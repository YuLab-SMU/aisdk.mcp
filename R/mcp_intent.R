#' Let a model write an MCP pipeline from a sentence
#'
#' You should not have to read a server's tool list before you can use it.
#' Describe what you want, hand over the clients, and get back R code that does
#' it — code you can read, keep and re-run, unlike a one-off agent answer.
#'
#' The model only sees the tools the servers actually advertise, with their
#' parameters and types, and is told to call nothing else. Before returning, the
#' code is checked against those same schemas by [mcp_check()] — unknown tools,
#' unknown arguments and missing required arguments are found there, not by a
#' second opinion. If the check fails, the model gets one chance to repair the
#' code with the exact problems quoted back at it. What still does not check out
#' is attached as `attr(code, "problems")`.
#'
#' The code is returned as text: **it is never evaluated here.** Read it, then
#' run it yourself.
#'
#' @param intent What you want, in one sentence.
#' @param clients Named list of MCP clients; the names become the variable names
#'   in the generated code (`list(gps = ..., string = ...)` gives `gps` and
#'   `string`).
#' @param model An aisdk model or id string. `NULL` uses the model configured
#'   with [aisdk::set_model()], so usually you do not pass it.
#' @param context Extra instructions for the model, e.g. the typed functions of a
#'   package that it should prefer over raw `mcp_action()` calls.
#' @param discover Read the servers' `list_*` tools before writing, so identifier
#'   values (datasource, library, species) come from the server instead of the
#'   model's guess. A few extra read-only calls; `FALSE` skips them.
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
mcp_intent <- function(intent, clients, model = NULL, context = NULL,
                       discover = TRUE) {
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

  known <- if (isTRUE(discover)) mcp_read_listings(clients) else NULL
  prompt <- paste0(
    mcp_catalog_text(clients),
    if (!is.null(known)) {
      paste0("\n\nKnown identifiers (read from the servers' listing tools; use",
             " these exact strings):\n", known)
    },
    if (!is.null(context)) paste0("\n\nProject context (follow it):\n", context),
    "\n\nTask: ", intent, "\n"
  )
  code <- mcp_intent_ask(prompt, model)
  problems <- mcp_check(code, clients, require_list = TRUE)

  if (length(problems)) {
    repaired <- mcp_intent_ask(paste0(
      prompt,
      "\n\nThe code you wrote does not match the tools' schemas:\n",
      paste0("- ", problems, collapse = "\n"),
      "\nReturn the complete corrected code in one fenced R code block."
    ), model)
    if (!inherits(tryCatch(parse(text = repaired), error = function(e) e), "error")) {
      code <- repaired
      problems <- mcp_check(code, clients, require_list = TRUE)
    }
  }

  attr(code, "intent") <- intent
  if (length(problems)) attr(code, "problems") <- problems
  code
}

#' @keywords internal
mcp_intent_ask <- function(prompt, model) {
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
        paste(vapply(names(props), function(name) {
          prop <- props[[name]]
          type <- prop$type %||% "string"
          enum <- prop$enum %||% list()
          if (length(enum)) {
            type <- paste0(type, ", one of: ", paste(unlist(enum), collapse = ", "))
          }
          sprintf("%s (%s%s)", name, type,
                  if (name %in% required) ", required" else "")
        }, character(1)), collapse = "; ")
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
    "The call form:",
    "",
    "  act <- mcp_action(client, \"tool_name\", into = \"argument\",",
    "                    fixed = list(other_argument = \"value\"))",
    "  value |> act()",
    "",
    "A tool that needs several arguments: bind the rest with `fixed` when you",
    "create the action, not one action per argument. Assign the action first,",
    "then pipe the value into it.",
    "",
    "Rules:",
    "- Never invent a tool or an argument; use the names listed above.",
    "- When an argument takes an identifier (a datasource, a species, a library)",
    "  and its valid values are not listed above, call the tool that lists them",
    "  and take the value from its result. Never hardcode a guessed identifier.",
    "- `into` must be one of that tool's parameters.",
    "- `as` is \"array\" for a JSON array, \"lines\" for one carriage-return",
    "  separated string, \"scalar\" for one number or flag. Omit it when the",
    "  parameter type makes it obvious.",
    "- Use the pipe `|>` to pass a value from one step to the next.",
    "- Only functions named in the Project context above exist as bare",
    "  functions. Every server tool goes through mcp_action(), always.",
    "- Add one short comment per step.",
    "- End the code with a single named `list(...)` of the results, one element",
    "  per step, named after what it shows (not `res1`, `res2`). Do not print",
    "  at the end: the list is the output.",
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

#' Read the servers' zero-argument `list_*` tools once
#'
#' Identifier values are the one thing a tool schema often does not state, so
#' they cannot be checked statically. Reading the listing tools up front gives
#' the model the real strings instead of a guess.
#'
#' @param clients Named list of MCP clients.
#' @param max_tools How many listing tools to call per client.
#' @return A character block, or `NULL` when there is nothing to read.
#' @keywords internal
mcp_read_listings <- function(clients, max_tools = 2L) {
  blocks <- character()
  for (name in names(clients)) {
    tools <- tryCatch(clients[[name]]$list_tools(), error = function(e) NULL)
    if (is.null(tools)) next
    pick <- Filter(function(tool) {
      grepl("^list_", tool$name %||% "") &&
        !length(unlist(tool$inputSchema$required %||% list()))
    }, tools)
    for (tool in utils::head(pick, max_tools)) {
      result <- tryCatch(clients[[name]]$call_tool_data(tool$name, list()),
                         error = function(e) NULL)
      if (is.null(result)) next
      text <- paste(utils::capture.output(
        utils::str(result, max.level = 3, list.len = 6, give.attr = FALSE)
      ), collapse = "\n")
      blocks <- c(blocks, paste0("# ", name, " :: ", tool$name, "()\n",
                                 substr(text, 1, 1200)))
    }
  }
  if (!length(blocks)) return(NULL)
  paste(blocks, collapse = "\n\n")
}
