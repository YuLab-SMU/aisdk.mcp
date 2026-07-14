# Test MCP Client
# Note: Most client tests require mocking processx, which is complex.
# These tests focus on the class structure and helper methods.

test_that("McpClient class exists", {
  expect_true(R6::is.R6Class(McpClient))
})

test_that("create_mcp_client function exists", {
  expect_true(is.function(create_mcp_client))
})

test_that("McpClient exposes prompts/list and prompts/get methods", {
  # Method presence check (behavior is covered via the request builders in
  # test-mcp-utils.R; full transport tests need a live server, per this file's
  # convention of not mocking processx).
  expect_true(all(c("list_prompts", "get_prompt") %in% names(McpClient$public_methods)))
})

# Integration tests would require a real MCP server or extensive mocking
# These are documented as examples in the demo script
