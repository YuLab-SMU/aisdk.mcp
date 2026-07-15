# MCP sampling: a server asks the client to generate (sampling/createMessage).

test_that("mcp_client_capabilities advertises sampling only when a handler is set", {
  expect_length(mcp_client_capabilities(FALSE), 0)
  caps <- mcp_client_capabilities(TRUE)
  expect_false(is.null(caps$sampling))
})

test_that("mcp_sampling_messages_to_aisdk converts and drops empty content", {
  mcp_msgs <- list(
    list(role = "user", content = list(type = "text", text = "What is 2+2?")),
    list(role = "assistant", content = list(type = "text", text = "4")),
    list(role = "user", content = list(type = "image", data = "...")) # no text -> dropped
  )
  conv <- mcp_sampling_messages_to_aisdk(mcp_msgs)
  expect_length(conv, 2)
  expect_equal(vapply(conv, function(m) m$role, character(1)), c("user", "assistant"))
  expect_equal(conv[[1]]$content, "What is 2+2?")
  expect_length(mcp_sampling_messages_to_aisdk(NULL), 0)
})

test_that("mcp_stop_reason maps aisdk finish reasons to MCP", {
  expect_equal(mcp_stop_reason("stop"), "endTurn")
  expect_equal(mcp_stop_reason("length"), "maxTokens")
  expect_equal(mcp_stop_reason("stop_sequence"), "stopSequence")
  expect_equal(mcp_stop_reason(NULL), "endTurn")     # default
  expect_equal(mcp_stop_reason("something_else"), "endTurn")
})

test_that("mcp_build_sampling_result yields a valid CreateMessageResult", {
  r <- mcp_build_sampling_result("hi", "openai:gpt-4o", "endTurn")
  expect_equal(r$role, "assistant")
  expect_equal(r$content$type, "text")
  expect_equal(r$content$text, "hi")
  expect_equal(r$model, "openai:gpt-4o")
  expect_equal(r$stopReason, "endTurn")
})

test_that("mcp_handle_server_request routes ping, sampling, and errors", {
  source(test_path("helper-mock.R"))

  # ping -> empty success result
  p <- mcp_handle_server_request(list(method = "ping", id = 9L))
  expect_equal(p$id, 9L)
  expect_false(is.null(p$result))
  expect_null(p$error)

  # sampling with a handler -> success carrying the generated message
  req <- list(jsonrpc = "2.0", method = "sampling/createMessage", id = 7L,
              params = list(messages = list(list(role = "user",
                                                 content = list(type = "text", text = "hi")))))
  model <- MockModel$new(list(list(text = "generated!", finish_reason = "stop",
                                   usage = list(total_tokens = 5))))
  ok <- mcp_handle_server_request(req, mcp_sampling_handler(model))
  expect_equal(ok$id, 7L)
  expect_equal(ok$result$content$text, "generated!")
  expect_equal(ok$result$role, "assistant")

  # sampling without a handler -> method not found
  none <- mcp_handle_server_request(req, NULL)
  expect_equal(none$error$code, JSONRPC_METHOD_NOT_FOUND)

  # unknown server method -> method not found
  unk <- mcp_handle_server_request(list(method = "roots/list", id = 3L))
  expect_equal(unk$error$code, JSONRPC_METHOD_NOT_FOUND)

  # a throwing handler becomes an internal error, never signals out
  bad <- mcp_handle_server_request(req, function(params) stop("boom"))
  expect_equal(bad$error$code, JSONRPC_INTERNAL_ERROR)
  expect_match(bad$error$message, "boom")
})

test_that("mcp_sampling_handler runs the model and forwards request options", {
  source(test_path("helper-mock.R"))
  model <- MockModel$new(list(list(text = "The answer is 42.", finish_reason = "length",
                                   usage = list(total_tokens = 5))))
  handler <- mcp_sampling_handler(model)
  result <- handler(list(
    messages = list(list(role = "user", content = list(type = "text", text = "q"))),
    systemPrompt = "Be concise",
    maxTokens = 128
  ))
  expect_equal(result$content$text, "The answer is 42.")
  expect_equal(result$stopReason, "maxTokens")           # length -> maxTokens
  expect_equal(result$model, "mock:mock-model")
  expect_equal(model$last_params$max_tokens, 128)         # maxTokens forwarded
})

test_that("McpClient carries a sampling_handler field and create_mcp_client accepts it", {
  expect_true("sampling_handler" %in% names(McpClient$public_fields))
  expect_true("sampling_handler" %in% names(formals(create_mcp_client)))
})
