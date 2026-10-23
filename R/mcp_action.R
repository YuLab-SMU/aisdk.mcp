#' Bind an MCP tool into a data-first function
#'
#' One MCP call, with everything that does not change pinned once: the client,
#' the tool, which argument receives the value, and how a vector is written.
#' After that you only supply the value, so it drops into a pipe.
#'
#' The two things you would otherwise have to look up are inferred from the
#' server's own schema: `into` from the first required parameter, `as` from that
#' parameter's type. Say them explicitly only when the inference is wrong.
#'
#' @param client An MCP client.
#' @param tool Tool name on that server.
#' @param into The argument that receives the value. Defaults to the tool's only
#'   required parameter (its only parameter when nothing is required).
#' @param as How to write a vector: `"array"` for a JSON array (GPSAdb),
#'   `"lines"` for one carriage-return separated string (STRING), `"scalar"` for
#'   a single number or flag. Defaults to the parameter's schema type.
#' @param fixed Named list of arguments sent with every call.
#' @return A function of `(value = NULL, ...)`. Calling it performs the request
#'   and returns the tool payload.
#' @seealso [mcp_intent()] to have a model write the pipeline, and
#'   [combine_mcp_tools()] to hand tools to an agent instead.
#' @examplesIf interactive()
#' gps <- create_mcp_http_client("https://gpsadb.gzdlab.com/mcp")
#' query <- mcp_action(gps, "query_gene", fixed = list(direction = "up"))
#' query("CDKN1A")$total
#' @export
mcp_action <- function(client, tool, into = NULL, as = NULL, fixed = list()) {
  if (!is.function(tryCatch(client$list_tools, error = function(e) NULL))) {
    stop("`client` must be an MCP client (it has `list_tools()`)", call. = FALSE)
  }
  if (!is.character(tool) || length(tool) != 1L || !nzchar(tool)) {
    stop("`tool` must be a single tool name", call. = FALSE)
  }
  tools <- client$list_tools()
  names_all <- vapply(tools, function(t) t$name %||% "", character(1))
  hit <- match(tool, names_all)
  if (is.na(hit)) {
    near <- names_all[agrep(tool, names_all, max.distance = 0.35, ignore.case = TRUE)]
    stop(
      sprintf("no tool `%s` on this server.", tool),
      if (length(near)) {
        paste0(" Did you mean: ", paste(near, collapse = ", "), "?")
      } else {
        paste0(" Available: ", paste(names_all, collapse = ", "))
      },
      call. = FALSE
    )
  }

  schema <- tools[[hit]]$inputSchema %||% list()
  props <- schema$properties %||% list()
  required <- as.character(unlist(schema$required %||% list()))
  into <- resolve_into(into, names(props), required, tool)
  as <- resolve_as(as, if (is.na(into)) NULL else props[[into]]$type %||% NULL)

  act <- function(value = NULL, ...) {
    arguments <- utils::modifyList(fixed, list(...))
    if (!is.null(value)) {
      if (is.na(into)) {
        stop(sprintf("`%s` takes no arguments; call it without a value", tool),
             call. = FALSE)
      }
      arguments[[into]] <- format_mcp_value(value, as)
    }
    client$call_tool_data(tool, arguments)
  }
  attr(act, "mcp_action") <- list(
    server = as.character(tryCatch(client$server_info$name, error = function(e) "server"))[[1]],
    tool = tool,
    into = into,
    as = as
  )
  class(act) <- c("mcp_action", "function")
  act
}

#' @keywords internal
resolve_into <- function(into, params, required, tool) {
  if (!length(params)) {
    if (!is.null(into)) {
      stop(sprintf("`%s` takes no arguments, so `into` has nothing to bind to", tool),
           call. = FALSE)
    }
    return(NA_character_)
  }
  if (!is.null(into)) {
    if (!into %in% params) {
      stop(sprintf("`%s` has no argument `%s`. Available: %s", tool, into,
                   paste(params, collapse = ", ")), call. = FALSE)
    }
    return(into)
  }
  if (length(required) == 1L) return(required)
  if (length(required) > 1L) {
    stop(sprintf("`%s` has several required arguments; say `into =`: %s", tool,
                 paste(required, collapse = ", ")), call. = FALSE)
  }
  if (length(params) == 1L) return(params)
  stop(sprintf("`%s` has no required argument; say `into =`: %s", tool,
               paste(params, collapse = ", ")), call. = FALSE)
}

#' @keywords internal
resolve_as <- function(as, type) {
  allowed <- c("array", "lines", "scalar")
  if (!is.null(as)) return(match.arg(as, allowed))
  switch(type %||% "string",
    array = "array",
    string = "lines",
    "scalar"
  )
}

#' @keywords internal
format_mcp_value <- function(value, as = "lines") {
  if (identical(as, "array")) return(I(as.character(value)))
  if (identical(as, "lines")) return(paste(as.character(value), collapse = "\r"))
  if (length(value) != 1L) {
    stop("`as = \"scalar\"` takes one value, got ", length(value), call. = FALSE)
  }
  as.character(value)[[1]]
}

#' @export
print.mcp_action <- function(x, ...) {
  info <- attr(x, "mcp_action")
  cat(sprintf(
    "<mcp_action %s :: %s -> %s (%s)>\n",
    info$server, info$tool,
    if (is.na(info$into)) "(no value)" else info$into,
    info$as
  ))
  invisible(x)
}
