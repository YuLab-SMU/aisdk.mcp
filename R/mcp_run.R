#' Run generated code and return what it collected
#'
#' [mcp_intent()] writes code that ends with one named list of its results, so
#' the pipeline has a single output you can hand to [mcp_explain()]. Plain
#' `eval()` gives you whatever the last line happened to be; this runs the code
#' in its own environment, with the clients bound by name, and insists on that
#' list.
#'
#' It runs the code you give it, so read it first — the same rule as `eval()`.
#' Nothing is printed or saved by this function; library calls and client calls
#' in the code do their own work.
#'
#' @param code R code as a string, as returned by [mcp_intent()].
#' @param clients Named list of MCP clients, bound by name in the code's
#'   environment (`list(gps = ..., string = ...)` makes `gps` and `string`).
#' @param envir Where functions the code calls (its `library()` packages, your
#'   helpers) are looked up. Defaults to the caller.
#' @return The named list the code ended with, carrying the intent as
#'   `attr(, "intent")` when the code came from [mcp_intent()].
#' @seealso [mcp_intent()], [mcp_explain()]
#' @examples
#' \dontrun{
#' code <- mcp_intent("...", clients = clients)
#' results <- mcp_run(code, clients)
#' mcp_explain(results, question = "...")
#' }
#' @export
mcp_run <- function(code, clients, envir = parent.frame()) {
  if (!is.character(code) || !length(code)) {
    stop("`code` must be a string", call. = FALSE)
  }
  if (!is.list(clients) || is.null(names(clients))) {
    stop("`clients` must be a named list", call. = FALSE)
  }
  env <- list2env(clients, parent = envir)
  value <- eval(parse(text = paste(code, collapse = "\n")), envir = env)
  attr(value, "intent") <- attr(code, "intent")
  if (!is.list(value) || is.data.frame(value)) {
    stop(
      "the code did not end with a named list of results",
      " (got ", paste(class(value), collapse = "/"), "). ",
      "Ask for one, or end the code with `list(...)` yourself.",
      call. = FALSE
    )
  }
  value
}
