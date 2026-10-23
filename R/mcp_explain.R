#' Turn a set of results into a short interpretation
#'
#' The last step of a multi-source pipeline: the queries answered their
#' questions, now something has to say what the answers mean together. The model
#' only sees the material you pass in, and is told to use nothing else.
#'
#' It writes prose, not code, and it does not query anything. Every number it
#' prints should be traceable to the results you handed it — check that when you
#' read it, the same way you check generated code.
#'
#' @param results A named list of results (data frames, result objects, or tool
#'   payloads), or a single result. Names become headings in the material.
#' @param question Optional: what you asked. Defaults to the intent carried by
#'   the results ([mcp_run()] passes it through), so a pipeline needs to state
#'   the question once.
#' @param model An aisdk model or id string. `NULL` uses the model configured
#'   with [aisdk::set_model()].
#' @param max_rows Rows of each table shown to the model.
#' @return The interpretation as one Markdown string.
#' @seealso [mcp_intent()] to write the pipeline, [mcp_action()] to run it.
#' @examples
#' \dontrun{
#' mcp_explain(
#'   list(perturbation = deg, coexpression = cor_df, ppi = net),
#'   question = "HIF1A knockdown, its coexpression module, and their interactions"
#' )
#' }
#' @export
mcp_explain <- function(results, question = NULL, model = NULL, max_rows = 5L) {
  if (!is.list(results) || !length(results)) {
    stop("`results` must be a list of results", call. = FALSE)
  }
  if (is.null(question)) question <- attr(results, "intent")
  if (is.null(names(results))) {
    names(results) <- paste0("result_", seq_along(results))
  }
  material <- paste(vapply(names(results), function(name) {
    paste0("## ", name, "\n", mcp_result_text(results[[name]], max_rows))
  }, character(1)), collapse = "\n\n")

  prompt <- paste0(
    if (!is.null(question)) paste0("The question was: ", question, "\n\n"),
    "Material (query results):\n\n", material
  )
  reply <- generate_text(
    model = model,
    prompt = prompt,
    system = paste(
      "You interpret query results for a scientist.",
      "Use only numbers, names and relationships that appear in the material.",
      "Never invent a value, a paper, a gene function or a mechanism.",
      "Say plainly what the results agree on and what they do not.",
      "Write in the same language as the question, or as the material when there",
      "is no question. Three short paragraphs, Markdown, no headings."
    ),
    temperature = 0.3
  )
  trimws(reply$text %||% "")
}

#' @keywords internal
mcp_result_text <- function(x, max_rows = 5L) {
  if (is.data.frame(x)) {
    return(paste(utils::capture.output(
      print(utils::head(x, max_rows))
    ), collapse = "\n"))
  }
  if (is.list(x) && is.data.frame(x$data)) {
    meta <- if (is.list(x$meta)) {
      paste0("meta: ", paste(names(x$meta), collapse = ", "), "\n")
    } else {
      ""
    }
    return(paste0(meta, paste(utils::capture.output(
      print(utils::head(x$data, max_rows))
    ), collapse = "\n")))
  }
  text <- paste(utils::capture.output(
    utils::str(x, max.level = 2, list.len = 8, give.attr = FALSE)
  ), collapse = "\n")
  substr(text, 1, 1500)
}
