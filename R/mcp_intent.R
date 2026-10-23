#' Let a model write an MCP pipeline from a sentence
#'
#' You should not have to read a server's tool list before you can use it.
#' Describe what you want, hand over the clients, and get back R code that does
#' it — code you can read, keep and re-run, unlike a one-off agent answer.
#'
#' The model only sees the tools the servers actually advertise, with their
#' parameters and types, and is told to call nothing else. The code is returned
#' as text: **it is never evaluated here.** Read it, then run it yourself.
#'
#' @param intent What you want, in one sentence.
#' @param clients Named list of MCP clients; the names become the variable names
#'   in the generated code (`list(gps = ..., string = ...)` gives `gps` and
#'   `string`).
#' @param model An aisdk model or id string. `NULL` uses the model configured
#'   with [aisdk::set_model()], so usually you do not pass it.
#' @param context Extra instructions for the model, e.g. the typed functions of a
#'   package that it should prefer over raw `mcp_action()` calls.
#' @return The generated code, as one string.
#' @seealso [mcp_action()] for the primitive it writes, and
#'   [combine_mcp_tools()] if you want the agent to call the tools itself.
#' @examples
#' \dontrun{
#' gps <- create_mcp_http_client("https://gpsadb.gzdlab.com/mcp")
#' code <- mcp_intent(
#'   "Which perturbations change CDKN1A, and how many are in Hep G2?",
#'   clients = list(gps = gps)
#' )
#' cat(code)
#' }
#' @export
mcp_intent <- function(intent, clients, model = NULL, context = NULL) {
  if (!is.character(intent) || length(intent) != 1L || !nzchar(intent)) {
    stop("`intent` must be one sentence", call. = FALSE)
  }
  if (!is.list(clients) || !length(clients)) {
    stop("`clients` must be a named list of MCP clients", call. = FALSE)
  }
  client_names <- names(clients)
  if (is.null(client_names) || any(!nzchar(client_names))) {
    stop("name the clients, e.g. list(gps = ..., string = ...)", call. = FALSE)
  }

  prompt <- paste0(
    mcp_catalog_text(clients),
    if (!is.null(context)) paste0("\n\nProject context (follow it):\n", context),
    "\n\nTask: ", intent, "\n"
  )
  reply <- generate_text(
    model = model,
    prompt = prompt,
    system = mcp_intent_system(),
    temperature = 0.2
  )
  code <- extract_r_code(reply$text %||% "")
  problem <- tryCatch({
    parse(text = code)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(problem)) {
    stop("the model returned code that does not parse: ", problem,
         "\n---\n", code, call. = FALSE)
  }
  code
}

#' @keywords internal
mcp_intent_system <- function() {
  paste(
    "You write short, runnable R code that calls MCP tools.",
    "Return exactly one fenced R code block and nothing else."
  )
}

#' @keywords internal
mcp_catalog_text <- function(clients) {
  blocks <- lapply(names(clients), function(name) {
    client <- clients[[name]]
    if (!is.function(tryCatch(client$list_tools, error = function(e) NULL))) {
      stop(sprintf("`%s` is not an MCP client", name), call. = FALSE)
    }
    tools <- client$list_tools()
    lines <- vapply(tools, function(tool) {
      schema <- tool$inputSchema %||% list()
      props <- schema$properties %||% list()
      required <- as.character(unlist(schema$required %||% list()))
      params <- if (!length(props)) {
        "(none)"
      } else {
        paste(sprintf(
          "%s (%s%s)", names(props),
          vapply(props, function(p) p$type %||% "string", character(1)),
          ifelse(names(props) %in% required, ", required", "")
        ), collapse = ", ")
      }
      sprintf("- %s: %s\n  parameters: %s",
              tool$name, first_line(tool$description %||% ""), params)
    }, character(1))
    paste0("# ", name, "\n", paste(lines, collapse = "\n"))
  })
  paste(c(
    "Available clients and their tools (read from the live servers).",
    "",
    unlist(blocks),
    "",
    "Write R code that uses only these clients and tools. The clients are",
    "already connected and available as variables with exactly the names above.",
    "",
    "The only call form you need:",
    "",
    "  act <- mcp_action(client, \"tool_name\", into = \"argument\", as = \"lines\")",
    "  value |> act()",
    "",
    "Rules:",
    "- Never invent a tool or an argument; use the names listed above.",
    "- `into` must be one of that tool's parameters.",
    "- `as` is \"array\" for a JSON array, \"lines\" for one carriage-return",
    "  separated string, \"scalar\" for one number or flag. Omit it when the",
    "  parameter type makes it obvious.",
    "- Use the pipe `|>` to pass a value from one step to the next.",
    "- Add one short comment per step.",
    "- No file writes, no installs, no network calls other than these clients."
  ), collapse = "\n")
}

#' @keywords internal
first_line <- function(text) {
  strsplit(as.character(text), "\n", fixed = TRUE)[[1]][1]
}

#' @keywords internal
extract_r_code <- function(text) {
  parts <- strsplit(as.character(text), "```", fixed = TRUE)[[1]]
  if (length(parts) < 2L) return(trimws(text))
  block <- parts[[2]]
  sub("^[ \t]*[rR][ \t]*\n", "", block) |> trimws()
}
