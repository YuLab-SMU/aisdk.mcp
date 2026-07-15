#' MCP Client
#'
#' Connect to and communicate with an MCP server process.
#'
#' @name McpClient
#' @export
NULL

#' MCP Client R6 Class
#'
#' Manages connection to an external MCP server via stdio.
#'
#' @export
McpClient <- R6::R6Class(
  "McpClient",
  public = list(
    #' @field process The processx process object
    process = NULL,

    #' @field server_info Information about the connected server
    server_info = NULL,

    #' @field capabilities Server capabilities
    capabilities = NULL,

    #' @field sampling_handler Optional `function(params)` answering server
    #'   `sampling/createMessage` requests (see [mcp_sampling_handler()]).
    sampling_handler = NULL,

    #' @description

    #' Create a new MCP Client
    #' @param command The command to run (e.g., "npx", "python")
    #' @param args Command arguments (e.g., c("-y", "@modelcontextprotocol/server-github"))
    #' @param env Environment variables as a named character vector
    #' @param sampling_handler Optional `function(params)` that answers a
    #'   server's `sampling/createMessage` requests by returning an MCP
    #'   CreateMessageResult. When supplied, the client advertises the `sampling`
    #'   capability. Use [mcp_sampling_handler()] to back it with an aisdk model.
    #' @param inherit_env Logical. A local MCP server is a third-party binary, so
    #'   by default (`FALSE`) it does NOT inherit this session's environment —
    #'   only a minimal set of non-secret system variables plus whatever you pass
    #'   in `env`, so your model API keys are never handed to the server. Set
    #'   `TRUE` to inherit the parent environment (still with recognised
    #'   credentials stripped) for a server you trust that needs it.
    #' @return A new McpClient object
    initialize = function(command, args = character(), env = NULL,
                          sampling_handler = NULL, inherit_env = FALSE) {
      if (!is.null(sampling_handler) && !is.function(sampling_handler)) {
        stop("`sampling_handler` must be a function or NULL")
      }
      self$sampling_handler <- sampling_handler

      # Build a deliberately minimal, secret-free environment for the child so a
      # local server never inherits this session's model API keys (see mcp_env).
      proc_env <- mcp_build_process_env(env, inherit_env = inherit_env)

      # Start the MCP server process
      self$process <- processx::process$new(
        command = command,
        args = args,
        stdin = "|",
        stdout = "|",
        stderr = "|",
        env = proc_env
      )

      # Perform MCP handshake
      private$perform_handshake()

      invisible(self)
    },

    #' @description
    #' List available tools from the MCP server
    #' @return A list of tool definitions
    list_tools = function() {
      private$ensure_alive()
      req <- mcp_tools_list_request(id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result$tools %||% list()
    },

    #' @description
    #' Call a tool on the MCP server
    #' @param name The tool name
    #' @param arguments Tool arguments as a named list
    #' @return The tool result
    call_tool = function(name, arguments = list()) {
      private$ensure_alive()
      req <- mcp_tools_call_request(name, arguments, id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP tool error: ", resp$error$message)
      }

      resp$result
    },

    #' @description
    #' List available resources from the MCP server
    #' @return A list of resource definitions
    list_resources = function() {
      private$ensure_alive()
      req <- mcp_resources_list_request(id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result$resources %||% list()
    },

    #' @description
    #' Read a resource from the MCP server
    #' @param uri The resource URI
    #' @return The resource contents
    read_resource = function(uri) {
      private$ensure_alive()
      req <- mcp_resources_read_request(uri, id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result
    },

    #' @description
    #' List available prompt templates from the MCP server (`prompts/list`).
    #' @return A list of prompt definitions (name, description, arguments).
    list_prompts = function() {
      private$ensure_alive()
      req <- mcp_prompts_list_request(id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result$prompts %||% list()
    },

    #' @description
    #' Get a prompt from the MCP server, rendered with the given arguments
    #' (`prompts/get`).
    #' @param name The prompt name.
    #' @param arguments A named list of prompt arguments (default none).
    #' @return The prompt result (description + messages).
    get_prompt = function(name, arguments = list()) {
      private$ensure_alive()
      args <- if (length(arguments) == 0) {
        structure(list(), names = character(0))
      } else {
        arguments
      }
      req <- mcp_prompts_get_request(name, args, id = private$next_id())
      resp <- private$send_request(req)

      if (!is.null(resp$error)) {
        stop("MCP error: ", resp$error$message)
      }

      resp$result
    },

    #' @description
    #' Check if the MCP server process is alive
    #' @return TRUE if alive, FALSE otherwise
    is_alive = function() {
      !is.null(self$process) && self$process$is_alive()
    },

    #' @description
    #' Close the MCP client connection
    close = function() {
      if (!is.null(self$process) && self$process$is_alive()) {
        self$process$kill()
      }
      invisible(self)
    },

    #' @description
    #' Convert MCP tools to SDK Tool objects
    #' @return A list of Tool objects
    as_sdk_tools = function() {
      mcp_tools <- self$list_tools()
      lapply(mcp_tools, function(t) {
        private$mcp_tool_to_sdk_tool(t)
      })
    }
  ),
  private = list(
    request_id = 0L,
    next_id = function() {
      private$request_id <- private$request_id + 1L
      private$request_id
    },
    ensure_alive = function() {
      if (!self$is_alive()) {
        stop("MCP server process is not running")
      }
    },
    perform_handshake = function() {
      # Send initialize request, advertising `sampling` when a handler is set so
      # the server may ask this client to generate on its behalf.
      init_req <- mcp_initialize_request(
        client_info = list(name = "r-ai-sdk", version = "0.7.0"),
        capabilities = mcp_client_capabilities(is.function(self$sampling_handler)),
        id = private$next_id()
      )

      resp <- private$send_request(init_req)

      if (!is.null(resp$error)) {
        stop("MCP initialization failed: ", resp$error$message)
      }

      self$server_info <- resp$result$serverInfo
      self$capabilities <- resp$result$capabilities

      # Send initialized notification
      notif <- mcp_initialized_notification()
      private$send_notification(notif)

      invisible(self)
    },
    send_request = function(request) {
      json_str <- paste0(mcp_serialize(request), "\n")
      self$process$write_input(json_str)

      # Read response (blocking with timeout)
      response_str <- private$read_response()
      mcp_deserialize(response_str)
    },
    send_notification = function(notification) {
      json_str <- paste0(mcp_serialize(notification), "\n")
      self$process$write_input(json_str)
    },
    write_message = function(message) {
      self$process$write_input(paste0(mcp_serialize(message), "\n"))
    },
    read_response = function(timeout_ms = 60000) {
      # Read lines from stdout until the response to our request arrives. The
      # transport is full-duplex: a server may interleave its own requests (e.g.
      # `sampling/createMessage`) before replying. Those are handled inline — we
      # answer them and keep reading — instead of being mistaken for the reply.
      start_time <- Sys.time()

      while (TRUE) {
        # Check timeout
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
        if (elapsed > timeout_ms) {
          stop("MCP response timeout")
        }

        # Try to read
        chunk <- self$process$read_output_lines(n = 1)
        if (length(chunk) > 0 && nzchar(chunk)) {
          msg <- mcp_deserialize(chunk)

          # A server->client request carries `method` AND `id`: answer it and
          # keep waiting for our own response.
          if (is.list(msg) && !is.null(msg$method) && !is.null(msg$id)) {
            reply <- mcp_handle_server_request(msg, self$sampling_handler)
            private$write_message(reply)
            next
          }
          # A server notification (`method`, no `id`) is not addressed to us.
          if (is.list(msg) && !is.null(msg$method)) {
            next
          }

          # Otherwise this is the response we were waiting for.
          return(chunk)
        }

        # Small sleep to avoid busy waiting
        Sys.sleep(0.01)
      }
    },
    mcp_tool_to_sdk_tool = function(mcp_tool) {
      # Create a closure that calls back to this client
      client <- self
      tool_name <- mcp_tool$name

      tool(
        name = mcp_tool$name,
        description = mcp_tool$description %||% "",
        parameters = private$schema_from_mcp(mcp_tool$inputSchema),
        execute = function(args) {
          result <- client$call_tool(tool_name, args)
          # Extract text content if present
          if (!is.null(result$content)) {
            texts <- sapply(result$content, function(c) {
              if (c$type == "text") c$text else ""
            })
            paste(texts, collapse = "\n")
          } else {
            jsonlite::toJSON(result, auto_unbox = TRUE)
          }
        }
      )
    },
    schema_from_mcp = function(input_schema) {
      # Convert MCP input schema to SDK schema
      # MCP uses standard JSON Schema format
      if (is.null(input_schema)) {
        return(z_object())
      }

      # For now, pass through as raw schema
      # The SDK tool system will handle it
      structure(
        input_schema,
        class = c("z_schema", "z_object")
      )
    }
  )
)

#' Create an MCP Client
#'
#' Convenience function to create and connect to an MCP server.
#'
#' @param command The command to run the MCP server
#' @param args Command arguments
#' @param env Environment variables
#' @param sampling_handler Optional `function(params)` answering server
#'   `sampling/createMessage` requests; see [mcp_sampling_handler()]. When set,
#'   the client advertises the `sampling` capability.
#' @param inherit_env Logical. By default (`FALSE`) the spawned server does NOT
#'   inherit this session's environment (so your model API keys stay private) —
#'   only non-secret system variables plus `env`. Set `TRUE` to inherit the
#'   parent environment with credentials stripped, for a trusted server.
#' @return An McpClient object
#' @export
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Connect to GitHub MCP server
#'   client <- create_mcp_client(
#'     "npx",
#'     c("-y", "@modelcontextprotocol/server-github"),
#'     env = c(GITHUB_PERSONAL_ACCESS_TOKEN = Sys.getenv("GITHUB_TOKEN"))
#'   )
#'
#'   # List available tools
#'   tools <- client$list_tools()
#'
#'   # Use tools with generate_text
#'   result <- generate_text(
#'     model = "openai:gpt-4o",
#'     prompt = "List my GitHub repos",
#'     tools = client$as_sdk_tools()
#'   )
#'
#'   client$close()
#' }
#' }
create_mcp_client <- function(command, args = character(), env = NULL,
                              sampling_handler = NULL, inherit_env = FALSE) {
  McpClient$new(command, args, env,
                sampling_handler = sampling_handler, inherit_env = inherit_env)
}
