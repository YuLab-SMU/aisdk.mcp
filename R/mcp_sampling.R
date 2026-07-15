#' MCP Sampling (server-initiated generation)
#'
#' The Model Context Protocol lets a *server* ask the connected *client* to run
#' an LLM generation on its behalf (`sampling/createMessage`). These helpers let
#' an [McpClient] advertise the `sampling` capability and answer those requests
#' by running an aisdk model — so a server can reason with the client's model
#' without holding its own API keys.
#'
#' @name mcp_sampling
#' @keywords internal
NULL

#' Build the client capabilities object for the initialize handshake
#'
#' @param has_sampling Whether the client will answer `sampling/createMessage`.
#' @return A capabilities list (empty, or advertising `sampling`).
#' @keywords internal
mcp_client_capabilities <- function(has_sampling = FALSE) {
  if (isTRUE(has_sampling)) {
    return(list(sampling = structure(list(), names = character(0))))
  }
  structure(list(), names = character(0))
}

#' Convert MCP sampling messages to aisdk message format
#'
#' MCP messages are `{role, content: {type: "text", text}}`; aisdk wants
#' `{role, content}`. Non-text content blocks are best-effort flattened to their
#' text (or dropped when they carry none).
#'
#' @param mcp_messages A list of MCP `SamplingMessage` objects.
#' @return A list of aisdk messages.
#' @keywords internal
mcp_sampling_messages_to_aisdk <- function(mcp_messages) {
  out <- lapply(mcp_messages %||% list(), function(m) {
    content <- m$content
    text <- if (is.list(content)) {
      content$text %||% ""
    } else {
      as.character(content %||% "")
    }
    list(role = m$role %||% "user", content = text)
  })
  # Drop any message that ended up empty (e.g. an image-only block we can't map).
  Filter(function(m) nzchar(m$content %||% ""), out)
}

#' Map an aisdk finish_reason to an MCP sampling stopReason
#' @keywords internal
mcp_stop_reason <- function(finish_reason) {
  switch(finish_reason %||% "stop",
    stop = "endTurn",
    end_turn = "endTurn",
    length = "maxTokens",
    max_tokens = "maxTokens",
    stop_sequence = "stopSequence",
    "endTurn"
  )
}

#' Build an MCP CreateMessageResult
#'
#' @param text The generated text.
#' @param model The model id string that produced it.
#' @param stop_reason The MCP stopReason (e.g. "endTurn").
#' @return An MCP `CreateMessageResult` list.
#' @keywords internal
mcp_build_sampling_result <- function(text, model = "unknown", stop_reason = "endTurn") {
  list(
    role = "assistant",
    content = list(type = "text", text = text %||% ""),
    model = model %||% "unknown",
    stopReason = stop_reason %||% "endTurn"
  )
}

#' Dispatch a server-initiated request to the right client handler
#'
#' A client must answer certain server->client requests. This turns one such
#' request into the JSON-RPC response to send back: `ping` (liveness),
#' `sampling/createMessage` (routed to `sampling_handler`), and a
#' method-not-found error for anything else.
#'
#' @param msg The deserialized JSON-RPC request from the server.
#' @param sampling_handler Optional `function(params)` returning a
#'   CreateMessageResult; `NULL` if the client did not register one.
#' @return A JSON-RPC response list (never signals; handler errors become a
#'   JSON-RPC internal error so the server always gets a reply).
#' @keywords internal
mcp_handle_server_request <- function(msg, sampling_handler = NULL) {
  method <- msg$method
  id <- msg$id

  if (identical(method, "ping")) {
    return(jsonrpc_response(structure(list(), names = character(0)), id))
  }

  if (identical(method, "sampling/createMessage")) {
    if (!is.function(sampling_handler)) {
      return(jsonrpc_error(
        JSONRPC_METHOD_NOT_FOUND,
        "This client did not register a sampling handler.",
        id
      ))
    }
    result <- tryCatch(sampling_handler(msg$params %||% list()), error = function(e) e)
    if (inherits(result, "condition")) {
      return(jsonrpc_error(
        JSONRPC_INTERNAL_ERROR,
        paste0("Sampling handler failed: ", conditionMessage(result)),
        id
      ))
    }
    return(jsonrpc_response(result, id))
  }

  jsonrpc_error(
    JSONRPC_METHOD_NOT_FOUND,
    paste0("Method not supported by this client: ", method %||% "<none>"),
    id
  )
}

#' Extract a model id string from a model object or id
#' @keywords internal
mcp_model_id <- function(model) {
  if (inherits(model, "LanguageModelV1")) {
    return(paste0(model$provider %||% "", ":", model$model_id %||% ""))
  }
  if (is.character(model) && length(model) == 1) {
    return(model)
  }
  "unknown"
}

#' Create an aisdk-backed sampling handler
#'
#' Returns a `function(params)` suitable as an [McpClient]'s `sampling_handler`:
#' it converts an incoming `sampling/createMessage` request into an aisdk
#' [aisdk::generate_text()] call and formats the reply as an MCP
#' `CreateMessageResult`. The server's `modelPreferences` are advisory — the
#' `model` you pass here is authoritative, which keeps API keys on the client.
#'
#' @param model An aisdk model object or id string used to answer sampling
#'   requests (e.g. `"anthropic:claude-sonnet-5"`).
#' @param ... Extra arguments forwarded to [aisdk::generate_text()] on every
#'   sampling call (e.g. `temperature`).
#' @return A function of one argument (the request params).
#' @export
mcp_sampling_handler <- function(model = NULL, ...) {
  force(model)
  extra <- list(...)
  function(params) {
    params <- params %||% list()
    messages <- mcp_sampling_messages_to_aisdk(params$messages)
    if (length(messages) == 0) {
      messages <- list(list(role = "user", content = ""))
    }
    args <- c(
      list(
        model = model,
        prompt = messages,
        system = params$systemPrompt,
        max_tokens = params$maxTokens,
        temperature = params$temperature
      ),
      extra
    )
    args <- args[!vapply(args, is.null, logical(1))]
    result <- do.call(generate_text, args)
    mcp_build_sampling_result(
      text = result$text,
      model = mcp_model_id(model),
      stop_reason = mcp_stop_reason(result$finish_reason)
    )
  }
}
