#' @title MCP Streamable HTTP Client
#' @description
#' Connect to a remote MCP server over the Streamable HTTP transport
#' (MCP 2025-06-18): a single endpoint that answers JSON-RPC `POST`s directly.
#'
#' This is the transport most public MCP servers expose today (GPSAdb, and most
#' hosted tool servers). It is *not* the same as [McpSseClient], which speaks the
#' legacy 2024-11-05 HTTP+SSE transport (a long-lived `GET` stream announcing a
#' separate `POST` endpoint).
#'
#' `McpHttpClient` inherits [McpClient], so `list_tools()`, `call_tool()`,
#' `call_tool_data()`, `list_resources()`, `list_prompts()`, `get_prompt()` and
#' `as_sdk_tools()` behave exactly as they do for a local (stdio) server — only
#' the transport differs. There is no child process and therefore no `$process`.
#'
#' Server-initiated requests (`sampling/createMessage`, `roots/list`) are not
#' supported: the 2025-06-18 transport delivers those on the optional `GET` SSE
#' stream, which many servers (including those that answer `GET` with 405) do
#' not offer. A `sampling_handler` is therefore neither used nor advertised.
#' @name McpHttpClient
#' @export
NULL

#' MCP Streamable HTTP Client R6 Class
#'
#' @export
McpHttpClient <- R6::R6Class(
  "McpHttpClient",
  inherit = McpClient,
  public = list(
    #' @field url The MCP endpoint URL.
    url = NULL,

    #' @field headers Extra HTTP headers (auth, user agent, ...).
    headers = NULL,

    #' @field timeout Seconds allowed per request.
    timeout = NULL,

    #' @field protocol_version Protocol version requested at handshake; the
    #'   server echoes the version it will speak.
    protocol_version = NULL,

    #' @field session_id Session id issued by a stateful server
    #'   (`Mcp-Session-Id` header), echoed back on later requests. `NULL` for
    #'   stateless servers.
    session_id = NULL,

    #' @field instructions Free-text usage instructions sent by the server
    #'   (optional capability).
    instructions = NULL,

    #' @field closed Whether [close()] has been called.
    closed = FALSE,

    #' @description
    #' Connect to a remote MCP server over Streamable HTTP.
    #'
    #' @param url The MCP endpoint, e.g. `"https://gpsadb.gzdlab.com/mcp"`.
    #' @param headers Named list of extra HTTP headers.
    #' @param timeout Seconds allowed per request.
    #' @param protocol_version Protocol version to request.
    #' @param transport Optional `function(message)` returning the parsed
    #'   JSON-RPC response (or `NULL` for notifications). Supplying one bypasses
    #'   HTTP entirely — this is how the test suite exercises the client without
    #'   touching the network. Leave `NULL` in normal use.
    #' @return A new McpHttpClient object.
    initialize = function(url,
                          headers = list(),
                          timeout = 120,
                          protocol_version = "2025-06-18",
                          transport = NULL) {
      if (!is.null(transport) && !is.function(transport)) {
        stop("`transport` must be a function or NULL")
      }
      self$url <- url
      self$headers <- headers
      self$timeout <- timeout
      self$protocol_version <- protocol_version
      private$transport <- transport

      # Deliberately does NOT call super$initialize(): there is no child
      # process to start.  The handshake below is the whole setup.
      private$perform_handshake()
      invisible(self)
    },

    #' @description
    #' An HTTP client is alive until it is closed.
    #' @return `TRUE` while the client is usable.
    is_alive = function() {
      !isTRUE(self$closed)
    },

    #' @description
    #' Close the client. For this transport that only marks it unusable; the
    #' server holds no session to tear down (a stateful server expires it).
    #' @return Self, invisibly.
    close = function() {
      self$closed <- TRUE
      invisible(self)
    }
  ),

  private = list(
    transport = NULL,

    ensure_alive = function() {
      if (!self$is_alive()) {
        rlang::abort("MCP HTTP client is closed", class = "mcp_closed_error")
      }
    },

    perform_handshake = function() {
      resp <- private$send_request(mcp_initialize_request(
        client_info = list(name = "r-ai-sdk-http", version = "0.7.0"),
        capabilities = mcp_client_capabilities(FALSE),
        id = private$next_id(),
        protocol_version = self$protocol_version
      ))

      if (!is.null(resp$error)) {
        stop("MCP initialization failed: ", resp$error$message)
      }

      self$server_info <- resp$result$serverInfo
      self$capabilities <- resp$result$capabilities
      self$instructions <- resp$result$instructions

      # A stateless server answers 202 with an empty body; a server that
      # rejects the notification must not break the connection.
      tryCatch(
        private$send_notification(mcp_initialized_notification()),
        error = function(e) NULL
      )

      invisible(self)
    },

    send_request = function(request) {
      out <- private$roundtrip(request, expect_response = TRUE)
      if (is.null(out)) {
        rlang::abort("MCP server returned an empty response", class = "mcp_transport_error")
      }
      out
    },

    send_notification = function(notification) {
      private$roundtrip(notification, expect_response = FALSE)
    },

    roundtrip = function(message, expect_response) {
      if (!is.null(private$transport)) {
        out <- private$transport(message)
        if (!expect_response) return(invisible(NULL))
        return(out)
      }
      private$post(message, expect_response = expect_response)
    },

    # POST one JSON-RPC message and return the parsed response (or NULL).
    post = function(message, expect_response = TRUE) {
      hdrs <- c(list(
        Accept = "application/json, text/event-stream",
        `Content-Type` = "application/json"
      ), self$headers)
      if (!is.null(self$session_id)) hdrs[["Mcp-Session-Id"]] <- self$session_id

      req <- httr2::request(self$url)
      req <- httr2::req_headers(req, !!!hdrs)
      req <- httr2::req_body_raw(
        req,
        jsonlite::toJSON(message, auto_unbox = TRUE, null = "null")
      )
      req <- httr2::req_timeout(req, self$timeout)
      req <- httr2::req_retry(
        req,
        max_tries = 3,
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
        }
      )
      # Inspect status ourselves so a JSON-RPC error payload is not swallowed.
      req <- httr2::req_error(req, is_error = function(resp) FALSE)

      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)

      sid <- httr2::resp_header(resp, "Mcp-Session-Id")
      if (!is.null(sid) && nzchar(sid)) self$session_id <- sid

      # Notifications are answered with 202 and an empty body; httr2 refuses to
      # read an empty body, so read defensively.
      text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")

      if (status == 404L && !is.null(self$session_id)) {
        rlang::abort(
          paste0(
            "MCP session expired (HTTP 404) for session ", self$session_id,
            "; build a new client to re-initialize"
          ),
          class = "mcp_session_expired"
        )
      }
      if (status >= 400L) {
        rlang::abort(
          sprintf("MCP HTTP %d: %s", status,
                  substr(gsub("\\s+", " ", text), 1, 400)),
          class = "mcp_transport_error"
        )
      }
      if (!expect_response) return(invisible(NULL))

      ctype <- httr2::resp_header(resp, "content-type") %||% ""
      if (grepl("text/event-stream", ctype, fixed = TRUE)) {
        return(mcp_sse_payload(text))
      }
      if (!nzchar(text)) return(NULL)
      mcp_deserialize(text)
    }
  )
)

#' Create an MCP Streamable HTTP Client
#'
#' Connect to a remote MCP server whose tools are reached over a single
#' JSON-RPC `POST` endpoint (MCP 2025-06-18 Streamable HTTP).
#'
#' Use this instead of [create_mcp_sse_client()] unless the server documents the
#' legacy HTTP+SSE transport: a Streamable HTTP endpoint answers `GET` with 405.
#'
#' @param url The MCP endpoint, e.g. `"https://gpsadb.gzdlab.com/mcp"`.
#' @param headers Named list of extra HTTP headers (auth, user agent, ...).
#' @param timeout Seconds allowed per request.
#' @param protocol_version Protocol version to request; the server echoes the
#'   version it will speak.
#' @param transport Optional `function(message)` returning the parsed JSON-RPC
#'   response. Supplying one bypasses HTTP and is how the test suite runs
#'   without the network; leave `NULL` in normal use.
#' @return An [McpHttpClient] object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   client <- create_mcp_http_client("https://gpsadb.gzdlab.com/mcp")
#'   vapply(client$list_tools(), function(t) t$name, character(1))
#'   client$call_tool_data("get_database_info")
#'   client$close()
#' }
#' }
create_mcp_http_client <- function(url,
                                   headers = list(),
                                   timeout = 120,
                                   protocol_version = "2025-06-18",
                                   transport = NULL) {
  McpHttpClient$new(
    url,
    headers = headers,
    timeout = timeout,
    protocol_version = protocol_version,
    transport = transport
  )
}

# Some Streamable HTTP servers answer a single request with an SSE stream.
# Return the last parsable JSON object carried on a `data:` line.
mcp_sse_payload <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  data <- sub("^data:[[:space:]]?", "", lines[startsWith(lines, "data:")])
  data <- data[nzchar(data)]
  if (length(data) == 0) return(NULL)
  out <- NULL
  for (d in data) {
    msg <- mcp_deserialize(d)
    if (!is.null(msg)) out <- msg
  }
  out
}
