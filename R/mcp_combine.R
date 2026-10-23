#' Combine tools from several MCP servers
#'
#' One agent can talk to several MCP servers at once, and this is the piece that
#' makes it work: it collects each client's tools into a single `tools=` list.
#'
#' Tool names are flat across a session, so two servers that both expose
#' `query_gene` or `search` would shadow each other. By default every tool is
#' therefore prefixed with its server's name (`gpsadb__query_gene`), and a name
#' that would still collide gets a numeric suffix. Pass `prefix = FALSE` to keep
#' the server's own names; that is only safe when you know they are unique and
#' the function will refuse duplicates rather than let one silently win.
#'
#' @param ... MCP clients ([McpClient], [McpHttpClient] or [McpSseClient]).
#'   Name them to choose the prefix yourself
#'   (`combine_mcp_tools(gpsadb = a, uniprot = b)`); otherwise the server's own
#'   advertised name is used. A single unnamed list of clients is also accepted.
#' @param prefix Logical. Prefix tool names with their server (default `TRUE`).
#' @return A flat list of `aisdk` tool objects, ready for the `tools=` argument
#'   of [aisdk::generate_text()].
#' @seealso [McpClient]
#' @examples
#' \dontrun{
#' gpsadb <- create_mcp_http_client("https://gpsadb.gzdlab.com/mcp")
#' other <- create_mcp_http_client("https://example.org/mcp")
#' combined <- combine_mcp_tools(gpsadb = gpsadb, other = other)
#' vapply(combined, function(t) t$name, character(1))
#' # "gpsadb__query_gene", "other__search", ...
#' }
#' @export
combine_mcp_tools <- function(..., prefix = TRUE) {
  clients <- list(...)
  if (length(clients) == 1L && is.list(clients[[1]]) &&
      is.null(names(clients)) && !has_tools(clients[[1]])) {
    clients <- clients[[1]]
  }
  if (!length(clients)) {
    stop("supply at least one MCP client", call. = FALSE)
  }
  labels <- names(clients)
  if (is.null(labels)) labels <- rep(NA_character_, length(clients))

  used <- character(0)
  out <- list()
  for (i in seq_along(clients)) {
    client <- clients[[i]]
    if (!has_tools(client)) {
      stop(sprintf("argument %d is not an MCP client (no `as_sdk_tools()`)", i),
           call. = FALSE)
    }
    tools <- client$as_sdk_tools()
    if (isTRUE(prefix)) {
      label <- labels[[i]]
      if (is.na(label) || !nzchar(label)) label <- server_label(client, i)
      label <- unique_label(sanitise_label(label), used)
      used <- c(used, label)
      tools <- prefix_tool_names(tools, label)
    }
    out <- c(out, tools)
  }

  if (!isTRUE(prefix)) {
    names_seen <- vapply(out, function(t) t$name, character(1))
    duplicated_names <- unique(names_seen[duplicated(names_seen)])
    if (length(duplicated_names)) {
      stop(
        "two servers expose the same tool name(s): ",
        paste(duplicated_names, collapse = ", "),
        ". Keep `prefix = TRUE` or rename the servers.",
        call. = FALSE
      )
    }
  }
  out
}

#' @keywords internal
has_tools <- function(x) {
  is.function(tryCatch(x$as_sdk_tools, error = function(e) NULL))
}

#' @keywords internal
server_label <- function(client, i) {
  name <- tryCatch(client$server_info$name, error = function(e) NULL)
  if (is.null(name) || !length(name) || is.na(name) || !nzchar(name)) {
    name <- paste0("server", i)
  }
  as.character(name)[1]
}

#' @keywords internal
sanitise_label <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- "server"
  if (!grepl("^[A-Za-z]", x)) x <- paste0("s", x)
  substr(x, 1, 32)
}

#' @keywords internal
unique_label <- function(label, used) {
  if (!label %in% used) return(label)
  i <- 2L
  while (paste0(label, "_", i) %in% used) i <- i + 1L
  paste0(label, "_", i)
}

#' @keywords internal
prefix_tool_names <- function(tools, label) {
  for (i in seq_along(tools)) {
    tools[[i]]$name <- paste0(label, "__", tools[[i]]$name)
  }
  tools
}
