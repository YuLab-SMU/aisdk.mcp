test_that("the material carries names, tables, payloads and meta", {
  deg <- data.frame(gene = c("HIF1A", "PGK1"), log2fc = c(-4.28, -2.17))
  payload <- list(network_summary = list(edges_above_cutoff = 109L))
  text <- mcp_result_text(deg, 2)
  expect_match(text, "HIF1A")
  expect_match(text, "PGK1")
  expect_match(mcp_result_text(payload), "edges_above_cutoff")

  wrapped <- list(data = deg, meta = list(total = 2L))
  expect_match(mcp_result_text(wrapped), "meta: total")
  expect_match(mcp_result_text(wrapped), "HIF1A")
})

test_that("mcp_explain sends every named block and returns the text", {
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    generate_text = function(model, prompt, system, temperature) {
      seen$prompt <- prompt
      seen$system <- system
      list(text = "  两件事一致。  ")
    },
    .package = "aisdk.mcp"
  )

  text <- mcp_explain(
    list(perturbation = data.frame(gene = "HIF1A", log2fc = -4.28),
         ppi = list(edges = 1L)),
    question = "does the module hold up?"
  )
  expect_equal(text, "两件事一致。")
  expect_match(seen$prompt, "## perturbation")
  expect_match(seen$prompt, "## ppi")
  expect_match(seen$prompt, "does the module hold up")
  expect_match(seen$system, "Never invent a value")
})

test_that("mcp_explain validates its input", {
  expect_error(mcp_explain(list()), "must be a list")
  expect_error(mcp_explain("x"), "must be a list")
})
