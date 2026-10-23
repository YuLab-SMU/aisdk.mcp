#' Check generated code against the servers' schemas
#'
#' Reads R code without running it and reports calls that cannot work: a tool
#' that does not exist, an argument the tool does not take, a required argument
#' left out. This is the deterministic half of [mcp_intent()]'s self-check — the
#' model can be wrong, the schema cannot.
#'
#' Only `mcp_action()` calls are checked, and only where the arguments are
#' literals. Anything it cannot see is reported as nothing, never as a guess.
#'
#' @param code R code as a string (or a character vector of lines).
#' @param clients Named list of MCP clients, the same one given to
#'   [mcp_intent()].
#' @param require_list Require the code to end with a named `list(...)`, the
#'   shape [mcp_run()] and [mcp_explain()] need. [mcp_intent()] sets this.
#' @return A character vector of problems, empty when nothing was found.
#' @seealso [mcp_intent()]
#' @examples
#' \dontrun{
#' code <- 'act <- mcp_action(gps, "query_gene", into = "gene"); act("CDKN1A")'
#' mcp_check(code, list(gps = create_mcp_http_client("https://gpsadb.gzdlab.com/mcp")))
#' }
#' @export
mcp_check <- function(code, clients, require_list = FALSE) {
  expr <- tryCatch(parse(text = paste(code, collapse = "\n"), keep.source = FALSE),
                   error = function(e) e)
  if (inherits(expr, "error")) {
    return(paste0("does not parse: ", conditionMessage(expr)))
  }
  if (!is.list(clients) || is.null(names(clients))) {
    stop("`clients` must be a named list", call. = FALSE)
  }
  tool_index <- mcp_tool_index(clients)
  defined <- mcp_defined_names(expr)
  problems <- character()
  walk_calls <- function(node) {
    if (!is.call(node)) return(invisible(NULL))
    head <- node[[1]]
    if (is.name(head)) {
      fn_name <- as.character(head)
      if (identical(fn_name, "mcp_action")) {
        problems <<- c(problems, check_one_action(as.list(node)[-1], clients))
      } else if (fn_name %in% names(tool_index) && !fn_name %in% defined) {
        problems <<- c(problems, sprintf(
          "`%s` is a tool on `%s`; call it with mcp_action(%s, \"%s\")",
          fn_name, tool_index[[fn_name]], tool_index[[fn_name]], fn_name))
      }
    }
    for (part in as.list(node)) {
      if (!missing(part) && is.call(part)) walk_calls(part)
    }
    invisible(NULL)
  }
  for (e in expr) walk_calls(e)

  if (isTRUE(require_list)) {
    last <- expr[[length(expr)]]
    if (!is.call(last) || !identical(as.character(last[[1]]), "list")) {
      problems <- c(problems,
                    "the code must end with a named list(...) of its results")
    } else {
      element_names <- names(as.list(last)[-1])
      if (is.null(element_names) || any(!nzchar(element_names))) {
        problems <- c(problems, "every element of the final list needs a name")
      } else if (anyDuplicated(element_names)) {
        problems <- c(problems, sprintf(
          "the final list repeats a name: %s",
          paste(unique(element_names[duplicated(element_names)]), collapse = ", ")))
      }
    }
  }
  problems
}

#' @keywords internal
check_one_action <- function(args, clients) {
  names_arg <- names(args)
  pick <- function(name, position) {
    hit <- match(name, names_arg)
    if (!is.na(hit)) return(args[[hit]])
    if (length(args) >= position) return(args[[position]])
    NULL
  }
  client_arg <- pick("client", 1L)
  tool_arg <- pick("tool", 2L)
  into_arg <- pick("into", 3L)
  fixed_arg <- pick("fixed", 5L)

  if (!is.name(client_arg)) return(character())
  client <- as.character(client_arg)
  if (!client %in% names(clients)) {
    return(sprintf("`%s` is not one of the clients: %s", client,
                   paste(names(clients), collapse = ", ")))
  }
  if (!is.character(tool_arg) || length(tool_arg) != 1L) return(character())

  client_obj <- clients[[client]]
  tools <- tryCatch(client_obj$list_tools(), error = function(e) NULL)
  if (is.null(tools)) {
    return(sprintf("`%s` is not an MCP client", client))
  }
  names_all <- vapply(tools, function(t) t$name %||% "", character(1))
  hit <- match(tool_arg, names_all)
  if (is.na(hit)) {
    near <- names_all[agrep(tool_arg, names_all, max.distance = 0.35, ignore.case = TRUE)]
    return(sprintf("`%s` has no tool `%s`%s", client, tool_arg,
                   if (length(near)) paste0(" (did you mean: ",
                                            paste(near, collapse = ", "), "?)")
                   else paste0(" (available: ", paste(names_all, collapse = ", "), ")")))
  }

  schema <- tools[[hit]]$inputSchema %||% list()
  props <- names(schema$properties %||% list())
  required <- as.character(unlist(schema$required %||% list()))
  out <- character()

  into <- if (is.character(into_arg) && length(into_arg) == 1L) into_arg else NA_character_
  if (!is.na(into) && nzchar(into) && !into %in% props) {
    out <- c(out, sprintf("`%s` has no argument `%s` (available: %s)",
                          tool_arg, into, paste(props, collapse = ", ")))
  }

  fixed_names <- if (is.call(fixed_arg) && identical(as.character(fixed_arg[[1]]), "list")) {
    names(as.list(fixed_arg)[-1])
  } else {
    NULL
  }
  if (!is.null(fixed_names)) {
    unknown <- setdiff(fixed_names, props)
    if (length(unknown)) {
      out <- c(out, sprintf("`%s` has no argument%s %s (available: %s)",
                            tool_arg, if (length(unknown) > 1) "s" else "",
                            paste0("`", unknown, "`", collapse = ", "),
                            paste(props, collapse = ", ")))
    }
    fixed_values <- as.list(fixed_arg)[-1]
    for (i in seq_along(fixed_names)) {
      allowed <- schema$properties[[fixed_names[[i]]]]$enum
      value <- fixed_values[[i]]
      if (!is.null(allowed) && is.character(value) && length(value) == 1L &&
          !value %in% unlist(allowed)) {
        out <- c(out, sprintf("`%s` for `%s` must be one of: %s",
                              fixed_names[[i]], tool_arg,
                              paste(unlist(allowed), collapse = ", ")))
      }
    }
  }

  # `into` is often left out: mcp_action() infers it from the single required
  # parameter (or the only parameter), so that argument is covered either way.
  inferred <- if (!is.na(into)) {
    into
  } else if (length(required) == 1L) {
    required
  } else if (length(props) == 1L) {
    props
  } else {
    NA_character_
  }
  supplied <- c(inferred, fixed_names)
  missing <- setdiff(required, supplied)
  if (length(missing)) {
    out <- c(out, sprintf("`%s` needs %s", tool_arg,
                          paste0("`", missing, "`", collapse = ", ")))
  }
  out
}

#' @keywords internal
mcp_tool_index <- function(clients) {
  index <- character()
  for (name in names(clients)) {
    tools <- tryCatch(clients[[name]]$list_tools(), error = function(e) NULL)
    if (is.null(tools)) next
    tool_names <- vapply(tools, function(tool) tool$name %||% "", character(1))
    for (tool_name in tool_names) {
      if (nzchar(tool_name) && !tool_name %in% names(index)) {
        index[[tool_name]] <- name
      }
    }
  }
  index
}

#' @keywords internal
mcp_defined_names <- function(expr) {
  found <- character()
  walk <- function(node) {
    if (!is.call(node)) return(invisible(NULL))
    if (is.name(node[[1]]) && as.character(node[[1]]) %in% c("<-", "=", "->") &&
        is.name(node[[2]])) {
      found <<- c(found, as.character(node[[2]]))
    }
    for (part in as.list(node)) {
      if (!missing(part) && is.call(part)) walk(part)
    }
    invisible(NULL)
  }
  for (e in expr) walk(e)
  unique(found)
}
