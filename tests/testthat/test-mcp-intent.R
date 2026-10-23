fake_two_servers <- function() {
  tools <- list(
    list(name = "query_gene", description = "Perturbations that change a gene",
         inputSchema = list(type = "object",
                            properties = list(gene = list(type = "string"),
                                              limit = list(type = "integer")),
                            required = list("gene"))),
    list(name = "string_ppi_enrichment", description = "Is this set connected?",
         inputSchema = list(type = "object",
                            properties = list(identifiers = list(type = "string"),
                                              species = list(type = "string")),
                            required = list("identifiers")))
  )
  list(
    gps = list(server_info = list(name = "GPSAdb"), list_tools = function() tools[1],
               call_tool_data = function(...) list()),
    string = list(server_info = list(name = "STRING"), list_tools = function() tools[2],
                  call_tool_data = function(...) list())
  )
}

model_reply <- function(text) {
  MockModel$new(list(list(text = text, finish_reason = "stop",
                          usage = list(total_tokens = 10))))
}

test_that("the catalog lists clients, tools, parameters and types", {
  text <- mcp_catalog_text(fake_two_servers())
  expect_match(text, "# gps")
  expect_match(text, "# string")
  expect_match(text, "query_gene")
  expect_match(text, "gene \\(string, required\\)")
  expect_match(text, "string_ppi_enrichment")
})

test_that("mcp_intent returns the code block, never runs it", {
  code <- mcp_intent(
    "which perturbations change CDKN1A",
    fake_two_servers(),
    model = model_reply("Here you go:\n```r\nlist(hits = mcp_action(gps, \"query_gene\")(\"CDKN1A\"))\nlist(hits = act(\"CDKN1A\"))\n```\n")
  )
  expect_type(code, "character")
  expect_match(code, "mcp_action\\(gps, \"query_gene\"\\)")
  expect_false(grepl("```", code))
})

test_that("code that does not parse is refused", {
  expect_error(
    mcp_intent("x", fake_two_servers(),
               model = model_reply("```r\nf <- function(\n```")),
    "does not parse"
  )
})

test_that("text without a code fence is taken as the code", {
  code <- mcp_intent("x", fake_two_servers(),
                     model = model_reply("list(hits = mcp_action(gps, \"query_gene\")(\"CDKN1A\"))\n"))
  expect_match(code, "query_gene")
})

test_that("inputs are validated", {
  expect_error(mcp_intent(character(0), fake_two_servers()), "one sentence")
  expect_error(mcp_intent("x", list(fake_two_servers()$gps)), "name the clients")
  expect_error(mcp_intent("x", list(a = 1)), "not an MCP client")
})

test_that("project context is passed to the model", {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    generate_text = function(model, prompt, system, temperature) {
      seen$prompt <- prompt
      list(text = "```r\nlist(x = 1)\n```")
    },
    .package = "aisdk.mcp"
  )
  code <- mcp_intent("do a thing", fake_two_servers(),
                     context = "prefer gpsadb_de() when it fits")
  expect_match(code, "list\\(x = 1\\)")
  expect_match(seen$prompt, "prefer gpsadb_de")
  expect_match(seen$prompt, "query_gene")
})
