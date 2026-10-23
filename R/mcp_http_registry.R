#' Built-in MCP HTTP servers
#'
#' Connect to a public MCP server that nobody needs to install: the table below
#' maps a short database name to a hosted Streamable HTTP endpoint. Every entry
#' is answered by the database's own maintainers, needs no API key, and was
#' verified with a full `initialize` + `tools/list` handshake — an HTTP 200 on
#' its own means nothing (EBI Search answers 200 with an HTML page).
#'
#' The survey behind this list, including the candidates that were probed and
#' rejected (PDBe, NCBI, UniProt, Ensembl, Reactome, ...), is in
#' `dev_docs/mcp-http-registry-survey.md`.
#'
#' @name mcp_http_servers
#' @keywords internal
NULL

# One row per server: where it lives, who runs it, what it answers with, and
# the leading tool name(s) its `tools/list` must contain. The expected tool is
# a drift check — these endpoints are community-facing services that can move
# or be replaced, and a silent URL swap would surface as nonsense answers.
#' @keywords internal
mcp_http_server_table <- list(
  gpsadb = list(
    url = "https://gpsadb.gzdlab.com/mcp",
    title = "GPSAdb: genetic perturbation RNA-seq",
    maintainer = "GPSAdb",
    media = "json",
    tools = "get_database_info"
  ),
  string = list(
    url = "https://mcp.string-db.org/",
    title = "STRING: protein-protein interaction networks",
    maintainer = "STRING Consortium",
    media = "sse",
    tools = "string_resolve_proteins"
  ),
  interpro = list(
    url = "https://www.ebi.ac.uk/interpro/mcp",
    title = "InterPro: protein families and domain architectures",
    maintainer = "EMBL-EBI",
    media = "json",
    tools = "searchDomainArchitectures"
  ),
  ols = list(
    url = "https://www.ebi.ac.uk/ols4/api/mcp",
    title = "EBI Ontology Lookup Service: biomedical ontologies",
    maintainer = "EMBL-EBI",
    media = "json",
    tools = "search"
  ),
  gwas = list(
    url = "https://www.ebi.ac.uk/gwas/mcp",
    title = "GWAS Catalog: trait, study and association records",
    maintainer = "EMBL-EBI",
    media = "sse",
    tools = "gwascatalog_get_traits"
  ),
  gtbadb = list(
    url = "https://gtbadb.gzdlab.com/mcp",
    title = "GTBAdb: gene coexpression across TCGA, GTEx and CCLE",
    maintainer = "GTBAdb",
    media = "json",
    tools = "get_database_info"
  )
)

# Spelling variants people actually type (`db = "STRING-DB"`, "stringdb").
#' @keywords internal
mcp_http_server_aliases <- c(
  stringdb = "string",
  string_db = "string",
  `string-db` = "string",
  gpsa = "gpsadb",
  gps_adb = "gpsadb"
)

#' List the Built-in MCP HTTP Servers
#'
#' Show the table [create_mcp_http_client_for()] resolves `db` against, so the
#' caller can discover names with R code instead of reading the docs.
#'
#' @return A data frame with one row per server and the columns `db`, `url`,
#'   `title`, `maintainer` and `media` (`"json"` or `"sse"` — the body framing
#'   the server answers with; the client handles both).
#' @seealso [create_mcp_http_client_for()]
#' @export
#' @examples
#' list_mcp_http_servers()
list_mcp_http_servers <- function() {
  rows <- lapply(names(mcp_http_server_table), function(db) {
    entry <- mcp_http_server_table[[db]]
    data.frame(
      db = db,
      url = entry$url,
      title = entry$title,
      maintainer = entry$maintainer,
      media = entry$media,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Resolve a Database Name to Its MCP Endpoint
#'
#' @param db A single server name, matched case-insensitively and ignoring the
#'   aliases registered in `mcp_http_server_aliases`.
#' @return The registry entry (a list with `url`, `title`, `maintainer`,
#'   `media`, `tools`).
#' @keywords internal
mcp_http_resolve <- function(db) {
  if (!is.character(db) || length(db) != 1L || is.na(db) || !nzchar(db)) {
    rlang::abort(
      "`db` must be a single non-empty string",
      class = "mcp_unknown_server_error"
    )
  }
  key <- tolower(trimws(db))
  if (key %in% names(mcp_http_server_aliases)) {
    key <- mcp_http_server_aliases[[key]]
  }
  entry <- mcp_http_server_table[[key]]
  if (is.null(entry)) {
    rlang::abort(
      c(
        paste0("Unknown MCP HTTP server: \"", db, "\"."),
        i = paste0(
          "Known servers: ",
          paste(names(mcp_http_server_table), collapse = ", "),
          ". See `list_mcp_http_servers()`."
        ),
        i = paste0(
          "Servers without a hosted endpoint (PDBe, NCBI, UniProt, Ensembl, ",
          "Reactome, ...) need a local stdio client: use ",
          "`create_mcp_client()`."
        )
      ),
      class = "mcp_unknown_server_error"
    )
  }
  c(entry, list(db = key))
}

#' Create an MCP Client for a Built-in HTTP Server
#'
#' One call instead of pasting an endpoint: the `db` name is looked up in the
#' built-in table (see [list_mcp_http_servers()]) and the URL is handed to
#' [create_mcp_http_client()]. Everything else — handshake, `list_tools()`,
#' `call_tool()`, `as_sdk_tools()` — is exactly what that client does.
#'
#' The handshake is finished by the time this function returns. Afterwards the
#' leading tool named in the registry is checked against `tools/list()`, so a
#' moved or repurposed endpoint fails here with `mcp_endpoint_changed_error`
#' instead of returning tools with the wrong meaning. Pass
#' `validate_tools = FALSE` to skip the extra request.
#'
#' @param db Server name, e.g. `"gpsadb"`, `"string"`, `"interpro"`, `"ols"`,
#'   `"gwas"`. Case-insensitive; a few spellings (`"stringdb"`) are accepted.
#' @param headers Named list of extra HTTP headers (auth, user agent, ...).
#' @param timeout Seconds allowed per request.
#' @param protocol_version Protocol version to request; the server echoes the
#'   version it will speak.
#' @param validate_tools Check that the server still advertises the tool the
#'   registry expects for this database. Defaults to `TRUE`.
#' @param transport Optional `function(message)` returning the parsed JSON-RPC
#'   response. Supplying one bypasses HTTP, which is how the test suite runs
#'   offline; leave `NULL` in normal use.
#' @return An [McpHttpClient]. The URL is `client$url`; the database name is
#'   recoverable with [mcp_http_server_name()].
#' @seealso [list_mcp_http_servers()], [mcp_http_server_name()],
#'   [create_mcp_http_client()]
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   string <- create_mcp_http_client_for("string")
#'   names <- vapply(string$list_tools(), function(t) t$name, character(1))
#'   string$call_tool_data("string_resolve_proteins", list(proteins = "TP53"))
#'   string$close()
#' }
#' }
create_mcp_http_client_for <- function(db,
                                       headers = list(),
                                       timeout = 120,
                                       protocol_version = "2025-06-18",
                                       validate_tools = TRUE,
                                       transport = NULL) {
  entry <- mcp_http_resolve(db)

  client <- create_mcp_http_client(
    entry$url,
    headers = headers,
    timeout = timeout,
    protocol_version = protocol_version,
    transport = transport
  )

  if (isTRUE(validate_tools)) {
    if (!mcp_http_endpoint_matches(client, entry)) {
      client$close()
      rlang::abort(
        c(
          paste0(
            "MCP endpoint for \"", entry$db, "\" no longer looks like ",
            entry$title, "."
          ),
          x = paste0(
            "Expected a tool named \"", entry$tools, "\" from ", entry$url, "."
          ),
          i = paste0(
            "The server moved or was repurposed; update the registry in ",
            "`mcp_http_server_table`."
          ),
          i = "Pass `validate_tools = FALSE` to use it anyway."
        ),
        class = "mcp_endpoint_changed_error"
      )
    }
  }
  client
}

#' Name of the Built-in Server Behind a Client
#'
#' Turn a client (or an endpoint URL) back into the short name the registry
#' uses, e.g. for naming the client in [mcp_intent()] or in a message.
#'
#' @param client An [McpHttpClient] or an endpoint URL.
#' @return The server name (a string). Fails with `mcp_unknown_server_error`
#'   for a URL that is not in the registry.
#' @seealso [create_mcp_http_client_for()]
#' @export
#' @examples
#' mcp_http_server_name("https://mcp.string-db.org/")
mcp_http_server_name <- function(client) {
  url <- if (is.character(client) && length(client) == 1L) {
    client
  } else {
    client$url
  }
  if (!is.character(url) || length(url) != 1L || is.na(url)) {
    rlang::abort(
      "`client` must be an MCP client or a URL",
      class = "mcp_unknown_server_error"
    )
  }
  for (db in names(mcp_http_server_table)) {
    if (identical(mcp_http_normalize_url(mcp_http_server_table[[db]]$url),
                  mcp_http_normalize_url(url))) {
      return(db)
    }
  }
  rlang::abort(
    c(
      paste0("No built-in MCP server has the endpoint ", url, "."),
      i = "See `list_mcp_http_servers()`."
    ),
    class = "mcp_unknown_server_error"
  )
}

# Trailing slashes are the one difference the registry entries disagree on
# (`mcp.string-db.org/` vs `.../mcp`), so compare without them.
#' @keywords internal
mcp_http_normalize_url <- function(url) {
  sub("/+$", "", tolower(trimws(url)))
}

#' Does the server still advertise the tool this entry expects?
#' @keywords internal
mcp_http_endpoint_matches <- function(client, entry) {
  names <- tryCatch(
    vapply(client$list_tools(), function(t) t$name, character(1)),
    error = function(e) NULL
  )
  !is.null(names) && entry$tools %in% names
}
