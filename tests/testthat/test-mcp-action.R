tool_def <- function(name, params = list(), required = character()) {
  list(
    name = name,
    description = "a tool",
    inputSchema = list(
      type = "object",
      properties = params,
      required = as.list(required)
    )
  )
}

client_with <- function(tools) {
  calls <- new.env(parent = emptyenv())
  calls$list <- list()
  list(
    server_info = list(name = "fake"),
    list_tools = function() tools,
    call_tool_data = function(tool, arguments = list()) {
      calls$list[[length(calls$list) + 1L]] <- list(tool = tool, arguments = arguments)
      list(ok = TRUE)
    },
    calls = function() calls$list
  )
}

test_that("into comes from the single required parameter", {
  client <- client_with(list(tool_def("get_dataset",
                                      list(dataset_id = list(type = "string")),
                                      "dataset_id")))
  act <- mcp_action(client, "get_dataset")
  expect_equal(attr(act, "mcp_action")$into, "dataset_id")
  expect_equal(attr(act, "mcp_action")$as, "lines")
})

test_that("as follows the schema type", {
  client <- client_with(list(
    tool_def("arrays", list(genes = list(type = "array")), "genes"),
    tool_def("strings", list(text = list(type = "string")), "text"),
    tool_def("numbers", list(limit = list(type = "integer")), "limit")
  ))
  expect_equal(attr(mcp_action(client, "arrays"), "mcp_action")$as, "array")
  expect_equal(attr(mcp_action(client, "strings"), "mcp_action")$as, "lines")
  expect_equal(attr(mcp_action(client, "numbers"), "mcp_action")$as, "scalar")
})

test_that("the value is written the way the server wants it", {
  client <- client_with(list(
    tool_def("arrays", list(genes = list(type = "array")), "genes"),
    tool_def("strings", list(text = list(type = "string")), "text")
  ))

  mcp_action(client, "arrays")(c("A", "B"))
  expect_true(inherits(client$calls()[[1]]$arguments$genes, "AsIs"))

  mcp_action(client, "strings")(c("A", "B"))
  expect_equal(client$calls()[[2]]$arguments$text, "A\rB")
})

test_that("fixed arguments are kept and call arguments win", {
  client <- client_with(list(tool_def("q", list(a = list(type = "string"),
                                                b = list(type = "string")), "a")))
  act <- mcp_action(client, "q", fixed = list(b = "fixed", a = "from-fixed"))
  act("from-call")
  args <- client$calls()[[1]]$arguments
  expect_equal(args$a, "from-call")
  expect_equal(args$b, "fixed")
})

test_that("a tool with no parameters is callable without a value", {
  client <- client_with(list(tool_def("info")))
  act <- mcp_action(client, "info")
  expect_true(is.na(attr(act, "mcp_action")$into))
  act()
  expect_equal(client$calls()[[1]]$tool, "info")
  expect_error(act("x"), "takes no arguments")
})

test_that("ambiguous schemas ask for into instead of guessing", {
  client <- client_with(list(
    tool_def("two_required", list(a = list(type = "string"), b = list(type = "string")),
             c("a", "b")),
    tool_def("all_optional", list(a = list(type = "string"), b = list(type = "string")))
  ))
  expect_error(mcp_action(client, "two_required"), "several required arguments")
  expect_error(mcp_action(client, "all_optional"), "no required argument")
  expect_equal(attr(mcp_action(client, "two_required", into = "b"), "mcp_action")$into, "b")
})

test_that("an unknown tool suggests the nearest name", {
  client <- client_with(list(tool_def("get_dataset", list(dataset_id = list(type = "string")),
                                      "dataset_id")))
  expect_error(mcp_action(client, "get_datasets"), "Did you mean: get_dataset")
  expect_error(mcp_action(client, "nothing_like_it"), "Available: get_dataset")
})

test_that("an unknown argument lists the real ones", {
  client <- client_with(list(tool_def("q", list(a = list(type = "string")))))
  expect_error(mcp_action(client, "q", into = "zzz"), "Available: a")
})

test_that("scalar arguments refuse a vector", {
  client <- client_with(list(tool_def("n", list(limit = list(type = "integer")), "limit")))
  expect_error(mcp_action(client, "n")(c(1, 2)), "takes one value")
})

test_that("an action prints what it will do", {
  client <- client_with(list(tool_def("get_dataset",
                                      list(dataset_id = list(type = "string")),
                                      "dataset_id")))
  out <- capture.output(print(mcp_action(client, "get_dataset")))
  expect_match(out, "get_dataset -> dataset_id")
})
