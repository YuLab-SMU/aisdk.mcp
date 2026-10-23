# Streamable HTTP transport. Behaviour is exercised through an injected
# `transport`, so these tests never touch the network; the live endpoint is
# covered by the opt-in smoke test at the end.

# A transport that answers from a canned lookup table, recording every message
# the client sends so tests can assert on the wire format.
fake_transport <- function(responses) {
  state <- new.env(parent = emptyenv())
  state$messages <- list()
  fn <- function(message) {
    state$messages[[length(state$messages) + 1L]] <- message
    if (is.null(message$id)) return(invisible(NULL))
    answer <- responses[[message$method]]
    if (is.null(answer)) {
      return(list(
        jsonrpc = "2.0",
        id = message$id,
        error = list(code = -32601L, message = "method not found")
      ))
    }
    if (is.function(answer)) answer <- answer(message)
    c(list(jsonrpc = "2.0", id = message$id), answer)
  }
  attr(fn, "state") <- state
  fn
}

standard_responses <- function() {
  list(
    initialize = function(msg) {
      list(result = list(
        protocolVersion = msg$params$protocolVersion,
        capabilities = list(tools = list(listChanged = FALSE)),
        serverInfo = list(name = "fake-server", version = "1.2.3"),
        instructions = "use the tools"
      ))
    },
    `notifications/initialized` = function(msg) list(result = list()),
    `tools/list` = list(result = list(tools = list(
      list(
        name = "get_number",
        description = "Return structured data",
        inputSchema = list(
          type = "object",
          properties = list(who = list(type = "string")),
          required = list("who"),
          additionalProperties = FALSE
        )
      ),
      list(
        name = "text_only",
        description = "Return JSON as text (no structuredContent)",
        inputSchema = list(type = "object", properties = list())
      ),
      list(
        name = "boom",
        description = "Always fails",
        inputSchema = list(type = "object", properties = list())
      )
    ))),
    `tools/call` = function(msg) {
      switch(
        msg$params$name,
        get_number = list(result = list(
          content = list(list(
            type = "text",
            text = '{"n":42,"label":"answer"}'
          )),
          structuredContent = list(n = 42L, label = "answer")
        )),
        text_only = list(result = list(
          content = list(list(type = "text", text = '{"a":1,"b":[2,3]}'))
        )),
        boom = list(result = list(
          content = list(list(type = "text", text = "gene X is not in GPSAdb")),
          isError = TRUE
        )),
        list(error = list(code = -32602L, message = 'unknown tool "x"'))
      )
    }
  )
}

new_fake_client <- function(protocol_version = "2025-06-18") {
  transport <- fake_transport(standard_responses())
  client <- create_mcp_http_client(
    "https://example.invalid/mcp",
    protocol_version = protocol_version,
    transport = transport
  )
  list(client = client, transport = transport)
}

test_that("create_mcp_http_client returns a connected client", {
  fx <- new_fake_client()
  expect_s3_class(fx$client, "McpHttpClient")
  expect_true(inherits(fx$client, "McpClient"))
  expect_equal(fx$client$server_info$name, "fake-server")
  expect_equal(fx$client$instructions, "use the tools")
  expect_true(fx$client$is_alive())
  expect_null(fx$client$process)  # no child process for this transport
})

test_that("handshake requests the configured protocol version", {
  fx <- new_fake_client(protocol_version = "2024-11-05")
  msgs <- sent_messages(fx$transport)
  expect_equal(msgs[[1]]$method, "initialize")
  expect_equal(msgs[[1]]$params$protocolVersion, "2024-11-05")
  expect_equal(msgs[[1]]$params$clientInfo$name, "r-ai-sdk-http")
  # the initialized notification follows, without an id
  expect_equal(msgs[[2]]$method, "notifications/initialized")
  expect_null(msgs[[2]]$id)
})

test_that("request ids are unique and increasing", {
  fx <- new_fake_client()
  fx$client$list_tools()
  fx$client$list_tools()
  ids <- vapply(sent_messages(fx$transport), function(m) m$id %||% NA_integer_, integer(1))
  ids <- ids[!is.na(ids)]
  expect_gte(length(ids), 3L)
  expect_false(anyDuplicated(ids) > 0)
  expect_true(all(diff(ids) > 0))
})

test_that("list_tools and as_sdk_tools expose the server tools", {
  fx <- new_fake_client()
  tools <- fx$client$list_tools()
  expect_length(tools, 3L)
  expect_equal(
    vapply(tools, function(t) t$name, character(1)),
    c("get_number", "text_only", "boom")
  )

  sdk <- fx$client$as_sdk_tools()
  expect_length(sdk, 3L)
  expect_equal(
    vapply(sdk, function(t) t$name, character(1)),
    c("get_number", "text_only", "boom")
  )
})

test_that("call_tool returns the raw MCP result", {
  fx <- new_fake_client()
  res <- fx$client$call_tool("get_number", list(who = "r"))
  expect_equal(res$structuredContent$n, 42L)
  # arguments reach the server untouched
  call <- sent_messages(fx$transport)[[3]]
  expect_equal(call$params$name, "get_number")
  expect_equal(call$params$arguments$who, "r")
})

test_that("a tool call with no arguments sends an object, not an array", {
  # `list()` would serialize as `[]`; strict servers reject that for a tool
  # declared with no parameters ("cannot unmarshal array into Go value ...").
  fx <- new_fake_client()
  fx$client$call_tool("text_only")
  call <- sent_messages(fx$transport)[[3]]
  expect_identical(
    as.character(jsonlite::toJSON(call$params$arguments, auto_unbox = TRUE)),
    "{}"
  )
})

test_that("call_tool_data prefers structuredContent", {
  fx <- new_fake_client()
  payload <- fx$client$call_tool_data("get_number")
  expect_equal(payload$n, 42L)
  expect_equal(payload$label, "answer")
})

test_that("call_tool_data falls back to parsing the text block", {
  fx <- new_fake_client()
  payload <- fx$client$call_tool_data("text_only")
  expect_equal(payload$a, 1L)
  expect_equal(unlist(payload$b), c(2L, 3L))
})

test_that("a tool-level error raises mcp_tool_error with the server message", {
  fx <- new_fake_client()
  expect_error(
    fx$client$call_tool("boom"),
    "gene X is not in GPSAdb",
    class = "mcp_tool_error"
  )
  # call_tool_data goes through call_tool, so it inherits the behaviour
  expect_error(fx$client$call_tool_data("boom"), class = "mcp_tool_error")
})

test_that("a JSON-RPC error surfaces as an R error", {
  fx <- new_fake_client()
  expect_error(fx$client$call_tool("x"), "MCP tool error")
})

test_that("a closed client refuses further calls", {
  fx <- new_fake_client()
  fx$client$close()
  expect_false(fx$client$is_alive())
  expect_error(fx$client$list_tools(), class = "mcp_closed_error")
})

test_that("create_mcp_http_client validates transport", {
  expect_error(
    create_mcp_http_client("https://example.invalid/mcp", transport = 1),
    "must be a function or NULL"
  )
})

test_that("mcp_sse_payload reads the last JSON object on data lines", {
  sse <- paste(
    "event: message",
    'data: {"jsonrpc":"2.0","id":1,"result":{"first":true}}',
    "",
    "event: message",
    'data: {"jsonrpc":"2.0","id":2,"result":{"second":true}}',
    "",
    sep = "\n"
  )
  parsed <- mcp_sse_payload(sse)
  expect_true(parsed$result$second)

  expect_null(mcp_sse_payload("event: ping\n\n"))
  expect_null(mcp_sse_payload("data: {not json}\n\n"))
})

# --- live smoke test -------------------------------------------------------
# Opt-in: skipped on CRAN and when the network is unavailable. The endpoint is
# public (no key), so CI can enable it by running non-CRAN checks.

test_that("live: a public Streamable HTTP MCP server answers", {
  skip_on_cran()
  skip_if_offline()

  client <- create_mcp_http_client(
    "https://gpsadb.gzdlab.com/mcp",
    timeout = 30
  )
  on.exit(client$close(), add = TRUE)

  expect_true(grepl("gpsadb", client$server_info$name, ignore.case = TRUE))

  tools <- client$list_tools()
  names <- vapply(tools, function(t) t$name, character(1))
  expect_true(all(c("get_database_info", "run_fastgpsa") %in% names))

  info <- client$call_tool_data("get_database_info")
  expect_gt(info$datasets, 0)

  # unknown tools are JSON-RPC errors, unknown datasets are tool errors
  expect_error(client$call_tool("does_not_exist"), "MCP tool error")
  expect_error(
    client$call_tool("get_dataset", list(dataset_id = "D00000")),
    class = "mcp_tool_error"
  )
})
