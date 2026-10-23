test_that("mcp_run binds the clients by name and returns the final list", {
  clients <- list(gps = list(id = "gps-client"), string = list(id = "string-client"))
  code <- '
a <- gps$id
b <- string$id
list(one = a, two = b)
'
  out <- mcp_run(code, clients)
  expect_equal(out, list(one = "gps-client", two = "string-client"))
})

test_that("mcp_run refuses code that does not end with a list", {
  expect_error(mcp_run('x <- 1\nx', list(gps = list())), "named list of results")
  expect_error(mcp_run('data.frame(a = 1)', list(gps = list())), "named list of results")
})

test_that("mcp_run validates its input", {
  expect_error(mcp_run(character(0), list(a = 1)), "`code` must be a string")
  expect_error(mcp_run("list()", list(1)), "named list")
})

test_that("mcp_explain takes what mcp_run returns", {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    generate_text = function(model, prompt, system, temperature) {
      seen$prompt <- prompt
      list(text = "ok")
    },
    .package = "aisdk.mcp"
  )
  results <- mcp_run("list(perturbation = data.frame(gene = 'HIF1A'))", list(gps = list()))
  expect_equal(mcp_explain(results), "ok")
  expect_match(seen$prompt, "## perturbation")
})

test_that("the intent travels from mcp_intent through mcp_run to mcp_explain", {
  clients <- list(gps = list(), gt = list(), string = list())
  code <- 'list(perturbation = data.frame(gene = "HIF1A"))'
  attr(code, "intent") <- "这个模块说明了什么"
  results <- mcp_run(code, clients)
  expect_equal(attr(results, "intent"), "这个模块说明了什么")

  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    generate_text = function(model, prompt, system, temperature) {
      seen$prompt <- prompt
      list(text = "ok")
    },
    .package = "aisdk.mcp"
  )
  expect_equal(mcp_explain(results), "ok")
  expect_match(seen$prompt, "这个模块说明了什么")
})

test_that("the final list is checked when asked", {
  clients <- list(gps = list(list_tools = function() list()))
  expect_match(mcp_check("x <- 1", clients, require_list = TRUE),
               "must end with a named list")
  expect_match(mcp_check("list(1, 2)", clients, require_list = TRUE),
               "needs a name")
  expect_match(mcp_check("list(a = 1, a = 2)", clients, require_list = TRUE),
               "repeats a name")
  expect_equal(mcp_check("list(a = 1)", clients, require_list = TRUE), character())
})
