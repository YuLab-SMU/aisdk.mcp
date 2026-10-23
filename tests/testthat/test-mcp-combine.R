fake_server <- function(name, tools) {
  list(
    server_info = list(name = name),
    as_sdk_tools = function() lapply(tools, function(n) list(name = n))
  )
}

names_of <- function(tools) vapply(tools, function(t) t$name, character(1))

test_that("tool names are prefixed with the server's own name", {
  a <- fake_server("GPSAdb", c("query_gene", "search_datasets"))
  b <- fake_server("UniProt", c("search"))

  combined <- combine_mcp_tools(a, b)
  expect_equal(
    names_of(combined),
    c("GPSAdb__query_gene", "GPSAdb__search_datasets", "UniProt__search")
  )
})

test_that("argument names choose the prefix", {
  a <- fake_server("GPSAdb", "query_gene")
  b <- fake_server("Something else", "search")

  combined <- combine_mcp_tools(gpsadb = a, uniprot = b)
  expect_equal(names_of(combined), c("gpsadb__query_gene", "uniprot__search"))
})

test_that("labels are sanitised into valid tool names", {
  weird <- fake_server("My Server !!", "x")
  combined <- combine_mcp_tools(weird)
  expect_equal(names_of(combined), "My_Server__x")
  expect_match(names_of(combined), "^[A-Za-z][A-Za-z0-9_]*__")
})

test_that("two servers with the same name do not collide", {
  a <- fake_server("gpsadb", "query_gene")
  b <- fake_server("gpsadb", "query_gene")

  combined <- combine_mcp_tools(a, b)
  expect_equal(names_of(combined), c("gpsadb__query_gene", "gpsadb_2__query_gene"))
})

test_that("a single list of clients is accepted", {
  clients <- list(fake_server("a", "x"), fake_server("b", "y"))
  expect_equal(names_of(combine_mcp_tools(clients)), c("a__x", "b__y"))
})

test_that("prefix = FALSE keeps names and refuses duplicates", {
  a <- fake_server("a", c("x", "y"))
  b <- fake_server("b", c("y", "z"))

  expect_error(combine_mcp_tools(a, b, prefix = FALSE), "same tool name")
  expect_equal(
    names_of(combine_mcp_tools(a, fake_server("b", "w"), prefix = FALSE)),
    c("x", "y", "w")
  )
})

test_that("bad arguments are rejected", {
  expect_error(combine_mcp_tools(), "at least one MCP client")
  expect_error(combine_mcp_tools(list(a = 1)), "not an MCP client")
})

test_that("the server's real tools combine with a prefixed gpsadb client", {
  skip_if_not_installed("gpsadb")
  skip_on_cran()
  skip_if_offline()

  gps <- gpsadb::gpsadb_tools()          # already an aisdk tool list
  combined <- combine_mcp_tools(
    gpsadb = list(as_sdk_tools = function() gps),
    other = list(as_sdk_tools = function() list(list(name = "search")))
  )
  expect_true("gpsadb__query_gene" %in% names_of(combined))
  expect_true("other__search" %in% names_of(combined))
})
