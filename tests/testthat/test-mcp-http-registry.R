# Built-in HTTP server registry. Name resolution and argument pass-through run
# offline through the same injected `transport` the HTTP tests use; the
# endpoints themselves are covered by the opt-in live test at the end.

# Responses a registry entry's server would give: the expected tool plus one
# more, so a passing check cannot be a coincidence of length.
registry_responses <- function(tool_names) {
  tools <- lapply(tool_names, function(nm) {
    list(
      name = nm,
      description = paste("tool", nm),
      inputSchema = list(type = "object", properties = list())
    )
  })
  list(
    initialize = function(msg) {
      list(result = list(
        protocolVersion = msg$params$protocolVersion,
        capabilities = list(tools = list(listChanged = FALSE)),
        serverInfo = list(name = "registry-fake", version = "0.0.1")
      ))
    },
    `notifications/initialized` = function(msg) list(result = list()),
    `tools/list` = list(result = list(tools = tools))
  )
}

# A transport shaped like `fake_transport()`, but recording nothing extra: the
# registry tests assert on the resolved URL, which travels via the client.
recording_transport <- function(tool_names) {
  responses <- registry_responses(tool_names)
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

test_that("the registry lists every server with a URL and a maintainer", {
  servers <- list_mcp_http_servers()
  expect_s3_class(servers, "data.frame")
  expect_named(servers, c("db", "url", "title", "maintainer", "media"))
  expect_setequal(
    servers$db,
    c("gpsadb", "string", "interpro", "ols", "gwas", "gtbadb")
  )
  expect_false(anyDuplicated(servers$db) > 0)
  expect_true(all(grepl("^https://", servers$url)))
  expect_true(all(servers$media %in% c("json", "sse")))
  expect_true(all(nzchar(servers$maintainer)))
})

test_that("registry entries agree with the table they are built from", {
  servers <- list_mcp_http_servers()
  for (i in seq_len(nrow(servers))) {
    entry <- mcp_http_resolve(servers$db[i])
    expect_equal(entry$url, servers$url[i])
    expect_equal(entry$db, servers$db[i])
    expect_true(nzchar(entry$tools))
  }
})

test_that("db names are matched case-insensitively and through aliases", {
  expect_equal(mcp_http_resolve("GPSAdb")$db, "gpsadb")
  expect_equal(mcp_http_resolve("  STRING  ")$db, "string")
  expect_equal(mcp_http_resolve("stringdb")$db, "string")
  expect_equal(mcp_http_resolve("string_db")$db, "string")
  expect_equal(mcp_http_resolve("STRING-DB")$db, "string")
  expect_equal(mcp_http_resolve("gpsa")$db, "gpsadb")
})

test_that("an unknown db fails with the known names and the stdio pointer", {
  err <- tryCatch(mcp_http_resolve("ensembl"), error = function(e) e)
  expect_s3_class(err, "mcp_unknown_server_error")
  msg <- conditionMessage(err)
  expect_match(msg, 'Unknown MCP HTTP server: "ensembl"')
  expect_match(msg, "gpsadb, string, interpro, ols, gwas, gtbadb", fixed = TRUE)
  expect_match(msg, "create_mcp_client\\(\\)")
})

test_that("a non-string db is rejected", {
  expect_error(mcp_http_resolve(NULL), class = "mcp_unknown_server_error")
  expect_error(mcp_http_resolve(""), class = "mcp_unknown_server_error")
  expect_error(mcp_http_resolve(NA_character_), class = "mcp_unknown_server_error")
  expect_error(
    mcp_http_resolve(c("string", "gwas")),
    class = "mcp_unknown_server_error"
  )
})

test_that("create_mcp_http_client_for connects to the registered URL", {
  transport <- recording_transport(c("get_database_info", "search_datasets"))
  client <- create_mcp_http_client_for("gpsadb", transport = transport)
  on.exit(client$close(), add = TRUE)

  expect_s3_class(client, "McpHttpClient")
  expect_equal(client$url, "https://gpsadb.gzdlab.com/mcp")
  expect_equal(mcp_http_server_name(client), "gpsadb")
  expect_equal(client$server_info$name, "registry-fake")

  tools <- vapply(client$list_tools(), function(t) t$name, character(1))
  expect_true("get_database_info" %in% tools)
})

test_that("headers, timeout and protocol version reach the client", {
  transport <- recording_transport("gwascatalog_get_traits")
  client <- create_mcp_http_client_for(
    "gwas",
    headers = list("X-Test" = "1"),
    timeout = 7,
    protocol_version = "2024-11-05",
    transport = transport
  )
  on.exit(client$close(), add = TRUE)

  expect_equal(client$timeout, 7)
  expect_identical(client$headers, list("X-Test" = "1"))
  expect_equal(client$protocol_version, "2024-11-05")
  # and the handshake actually used it
  msgs <- sent_messages(transport)
  expect_equal(msgs[[1]]$params$protocolVersion, "2024-11-05")
})

test_that("validation accepts any entry whose expected tool is present", {
  servers <- list_mcp_http_servers()
  for (db in servers$db) {
    entry <- mcp_http_resolve(db)
    transport <- recording_transport(c(entry$tools, "another_tool"))
    client <- create_mcp_http_client_for(db, transport = transport)
    expect_equal(mcp_http_server_name(client), db)
    client$close()
  }
})

test_that("a repurposed endpoint fails loudly instead of returning tools", {
  # a server that answers with unrelated tools means the URL no longer points
  # at the database the registry entry names.
  transport <- recording_transport(c("search_domains", "get_entry"))
  expect_error(
    create_mcp_http_client_for("string", transport = transport),
    "no longer looks like",
    class = "mcp_endpoint_changed_error"
  )
})

test_that("validate_tools = FALSE skips the check", {
  transport <- recording_transport("unrelated_tool")
  client <- create_mcp_http_client_for(
    "string",
    validate_tools = FALSE,
    transport = transport
  )
  on.exit(client$close(), add = TRUE)

  expect_equal(mcp_http_server_name(client), "string")
  # the only requests are the handshake — no extra tools/list before the caller asks
  msgs <- sent_messages(transport)
  expect_equal(
    vapply(msgs, function(m) m$method, character(1)),
    c("initialize", "notifications/initialized")
  )
})

test_that("an unknown db is rejected before any transport is touched", {
  called <- FALSE
  transport <- function(message) {
    called <<- TRUE
    NULL
  }
  expect_error(
    create_mcp_http_client_for("ncbi", transport = transport),
    class = "mcp_unknown_server_error"
  )
  expect_false(called)
})

test_that("mcp_http_server_name maps URLs back to names", {
  expect_equal(mcp_http_server_name("https://gpsadb.gzdlab.com/mcp"), "gpsadb")
  # trailing slash and case differences do not matter
  expect_equal(mcp_http_server_name("https://MCP.string-db.org/"), "string")
  expect_equal(mcp_http_server_name("https://mcp.string-db.org"), "string")
  expect_equal(
    mcp_http_server_name("https://www.ebi.ac.uk/ols4/api/mcp"),
    "ols"
  )
  expect_error(
    mcp_http_server_name("https://example.org/mcp"),
    class = "mcp_unknown_server_error"
  )
  expect_error(mcp_http_server_name(NULL), class = "mcp_unknown_server_error")
})

test_that("mcp_http_server_name accepts the client itself", {
  transport <- recording_transport("string_resolve_proteins")
  client <- create_mcp_http_client_for("string", transport = transport)
  on.exit(client$close(), add = TRUE)
  expect_equal(mcp_http_server_name(client), "string")
})

# --- live smoke test -------------------------------------------------------
# One handshake per registered endpoint, opt-in like the HTTP smoke test.

test_that("live: every registered server answers a handshake", {
  skip_on_cran()
  skip_if_offline()

  servers <- list_mcp_http_servers()
  for (db in servers$db) {
    client <- create_mcp_http_client_for(db, timeout = 60)
    tools <- client$list_tools()
    client$close()
    expect_gt(length(tools), 0L)
  }
})
