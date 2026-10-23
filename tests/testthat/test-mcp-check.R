schema_tool <- function(name, params, required = character()) {
  list(name = name, description = "d",
       inputSchema = list(type = "object",
                          properties = stats::setNames(lapply(params, function(type) list(type = type)), names(params)),
                          required = as.list(required)))
}

check_clients <- function() {
  list(
    gps = list(server_info = list(name = "GPSAdb"),
               list_tools = function() list(
                 schema_tool("query_gene", list(gene = "string", limit = "integer"), "gene"),
                 schema_tool("get_database_info", list())
               ),
               call_tool_data = function(...) list()),
    gt = list(server_info = list(name = "GTBAdb"),
              list_tools = function() list(
                schema_tool("correlate_two",
                            list(feature1 = "string", feature2 = "string",
                                 datasource = "string", category1 = "string"),
                            c("feature1", "feature2", "datasource"))
              ),
              call_tool_data = function(...) list())
  )
}

test_that("a correct call passes", {
  code <- '
act <- mcp_action(gps, "query_gene", into = "gene")
act("CDKN1A")
cor <- mcp_action(gt, "correlate_two", into = "feature1",
                  fixed = list(feature2 = "PGK1", datasource = "gtex"))
"HIF1A" |> cor()
'
  expect_equal(mcp_check(code, check_clients()), character())
})

test_that("an unknown tool is caught with a suggestion", {
  problems <- mcp_check('mcp_action(gps, "query_genes", into = "gene")', check_clients())
  expect_match(problems, "no tool `query_genes`")
  expect_match(problems, "query_gene")
})

test_that("an unknown argument and a missing required one are caught", {
  problems <- mcp_check(
    'mcp_action(gt, "correlate_two", into = "feature1", fixed = list(datum = "x"))',
    check_clients()
  )
  expect_true(any(grepl("no argument `datum`", problems)))
  expect_true(any(grepl("needs `feature2`, `datasource`", problems)))
})

test_that("an unknown client and a bad into are caught", {
  expect_true(any(grepl("not one of the clients",
                        mcp_check('mcp_action(nope, "query_gene")', check_clients()))))
  problems <- mcp_check('mcp_action(gps, "query_gene", into = "zzz")', check_clients())
  expect_true(any(grepl("no argument `zzz`", problems)))
})

test_that("code that does not parse is reported, not thrown", {
  expect_match(mcp_check("f <- function(", check_clients()), "does not parse")
})

test_that("mcp_intent repairs the code once when the schema check fails", {
  bad <- '```r\nact <- mcp_action(gps, "query_genes", into = "gene")\nact("CDKN1A")\n```'
  good <- '```r\nact <- mcp_action(gps, "query_gene", into = "gene")\nlist(hits = act("CDKN1A"))\n```'
  model <- MockModel$new(list(
    list(text = bad, finish_reason = "stop", usage = list(total_tokens = 10)),
    list(text = good, finish_reason = "stop", usage = list(total_tokens = 10))
  ))
  code <- mcp_intent("find CDKN1A", check_clients(), model = model)
  expect_match(code, "query_gene")
  expect_null(attr(code, "problems"))
})

test_that("mcp_intent reports what it could not fix", {
  bad <- '```r\nact <- mcp_action(gps, "query_genes", into = "gene")\nact("CDKN1A")\n```'
  model <- MockModel$new(list(
    list(text = bad, finish_reason = "stop", usage = list(total_tokens = 10)),
    list(text = bad, finish_reason = "stop", usage = list(total_tokens = 10))
  ))
  code <- mcp_intent("find CDKN1A", check_clients(), model = model)
  expect_true(any(grepl("query_genes", attr(code, "problems"))))
})

test_that("a value outside the schema's enum is caught", {
  client <- list(
    gt = list(server_info = list(name = "GTBAdb"),
              list_tools = function() list(
                list(name = "run_gtba", description = "d",
                     inputSchema = list(
                       type = "object",
                       properties = list(
                         library = list(type = "string",
                                        enum = list("Hallmarks", "KEGG")),
                         feature = list(type = "string")),
                       required = list("feature", "library")))),
              call_tool_data = function(...) list())
  )
  problems <- mcp_check(
    'mcp_action(gt, "run_gtba", into = "feature", fixed = list(library = "Hallmark"))',
    client
  )
  expect_true(any(grepl("must be one of: Hallmarks, KEGG", problems)))
})

test_that("discovery reads zero-argument list_* tools and skips the others", {
  client <- list(
    gt = list(server_info = list(name = "GTBAdb"),
              list_tools = function() list(
                list(name = "list_subtypes", description = "d",
                     inputSchema = list(type = "object", properties = list())),
                list(name = "list_features", description = "d",
                     inputSchema = list(type = "object",
                                        properties = list(q = list(type = "string")),
                                        required = list("q")))
              ),
              call_tool_data = function(tool, arguments) {
                if (identical(tool, "list_subtypes")) {
                  list(items = list(list(id = "tcga_tumor"), list(id = "gtex")))
                } else {
                  stop("should not be called")
                }
              })
  )
  out <- mcp_read_listings(client)
  expect_match(out, "list_subtypes")
  expect_match(out, "gtex")
  expect_false(grepl("list_features", out))
  expect_null(mcp_read_listings(list(gt = list(list_tools = function() list()))))
})

test_that("a tool called as a bare function is caught", {
  client <- list(
    string = list(server_info = list(name = "STRING"),
                  list_tools = function() list(
                    list(name = "string_ppi_enrichment", description = "d",
                         inputSchema = list(type = "object", properties = list())),
                    list(name = "string_visual_network", description = "d",
                         inputSchema = list(type = "object", properties = list()))
                  ),
                  call_tool_data = function(...) list())
  )
  problems <- mcp_check('string_ppi_enrichment(identifiers = "A")', client)
  expect_match(problems, "is a tool on `string`")
  expect_match(problems, "mcp_action\\(string, \"string_ppi_enrichment\"\\)")

  # a user function that happens to share a tool's name is left alone
  code <- 'string_ppi_enrichment <- function(x) x\nstring_ppi_enrichment(1)'
  expect_equal(mcp_check(code, client), character())
})
