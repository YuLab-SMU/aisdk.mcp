#' Subprocess environment for a local MCP server
#'
#' Spawning a local (stdio) MCP server means running a third-party binary. If it
#' inherits this R session's full environment it also inherits every model API
#' key (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, ...), `GITHUB_TOKEN`, and any other
#' secret — which a malicious or compromised server could exfiltrate. These
#' helpers build a deliberately minimal, secret-free environment for the child.
#'
#' @name mcp_env
#' @keywords internal
NULL

# Non-secret system variables a subprocess genuinely needs to run (find its
# executable, locate HOME/TMP, set the locale). Matched case-insensitively so
# Windows spellings (SystemRoot, Path) are covered.
#' @keywords internal
mcp_safe_env_names <- c(
  "PATH", "HOME", "USER", "LOGNAME", "SHELL", "TERM", "LANG", "LC_ALL",
  "TMPDIR", "TZ", "PWD", "DISPLAY", "XDG_RUNTIME_DIR", "XDG_DATA_HOME",
  "XDG_CONFIG_HOME", "XDG_CACHE_HOME", "NODE_OPTIONS", "SSL_CERT_FILE",
  "SSL_CERT_DIR",
  # Windows essentials
  "SYSTEMROOT", "SYSTEMDRIVE", "WINDIR", "COMSPEC", "PATHEXT", "USERNAME",
  "USERPROFILE", "HOMEDRIVE", "HOMEPATH", "TEMP", "TMP", "APPDATA",
  "LOCALAPPDATA", "PROGRAMFILES", "PROGRAMFILES(X86)", "PROGRAMDATA",
  "COMMONPROGRAMFILES", "NUMBER_OF_PROCESSORS", "PROCESSOR_ARCHITECTURE"
)

#' Is an env var name safe (non-secret) to pass to a subprocess?
#' @keywords internal
mcp_is_safe_envvar <- function(name) {
  if (!nzchar(name)) {
    return(FALSE)
  }
  up <- toupper(name)
  if (up %in% mcp_safe_env_names) {
    return(TRUE)
  }
  # Locale variables (LC_CTYPE, LC_NUMERIC, ...) are safe and often required.
  grepl("^LC_", up)
}

# Known credential env vars + a pattern for the common secret-bearing shapes.
# Mirrors aisdk's own r_eval credential scrub so the two stay consistent.
#' @keywords internal
mcp_sensitive_env_explicit <- c(
  "OPENAI_API_KEY", "OPENAI_KEY", "ANTHROPIC_API_KEY", "ANTHROPIC_KEY",
  "GOOGLE_API_KEY", "GEMINI_API_KEY", "DEEPSEEK_API_KEY", "MOONSHOT_API_KEY",
  "GROQ_API_KEY", "XAI_API_KEY", "MISTRAL_API_KEY", "COHERE_API_KEY",
  "AZURE_OPENAI_KEY", "AZURE_OPENAI_API_KEY", "HUGGINGFACE_API_KEY", "HF_TOKEN",
  "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN",
  "GITHUB_TOKEN", "GH_TOKEN", "GITHUB_PERSONAL_ACCESS_TOKEN"
)

#' @keywords internal
mcp_sensitive_envvar_pattern <- "(?i)(api[_-]?key|token|secret|password|passwd|credential|access[_-]?key|private[_-]?key|bearer|cookie|_key$|pat$)"

#' Does an env var name look like a secret?
#' @keywords internal
mcp_is_sensitive_envvar <- function(name) {
  if (!nzchar(name)) {
    return(FALSE)
  }
  if (toupper(name) %in% mcp_sensitive_env_explicit) {
    return(TRUE)
  }
  grepl(mcp_sensitive_envvar_pattern, name, perl = TRUE)
}

#' Build the environment for a spawned MCP server process
#'
#' By default (`inherit_env = FALSE`) the child gets only a minimal allowlist of
#' non-secret system variables plus whatever the caller passes in `env` — so no
#' API key reaches the server unless it was shared explicitly. With
#' `inherit_env = TRUE` the child inherits the parent environment but with
#' recognised credentials stripped out (defence in depth). The caller's explicit
#' `env` always wins.
#'
#' @param env A named character vector of variables to pass (the caller's
#'   deliberate choice, e.g. the server's own token). `NULL` for none.
#' @param inherit_env Logical. `FALSE` (default, secure) passes only the safe
#'   allowlist; `TRUE` passes the parent environment minus secrets.
#' @return A named character vector suitable for `processx`'s `env`.
#' @keywords internal
mcp_build_process_env <- function(env = NULL, inherit_env = FALSE) {
  parent <- Sys.getenv()
  parent_names <- names(parent)

  if (isTRUE(inherit_env)) {
    keep <- !vapply(parent_names, mcp_is_sensitive_envvar, logical(1))
  } else {
    keep <- vapply(parent_names, mcp_is_safe_envvar, logical(1))
  }
  base <- parent[keep]

  extra <- env %||% character(0)
  if (length(extra) > 0) {
    if (is.null(names(extra)) || any(!nzchar(names(extra)))) {
      stop("`env` must be a fully named character vector")
    }
    # The caller's explicit values win over the inherited base.
    base <- base[setdiff(names(base), names(extra))]
    result <- c(base, extra)
  } else {
    result <- base
  }

  # An empty env would leave the child unable to resolve its own executable;
  # guarantee PATH at minimum.
  if (length(result) == 0 || !("PATH" %in% names(result) || "Path" %in% names(result))) {
    path <- Sys.getenv("PATH")
    if (nzchar(path) && !("PATH" %in% names(result))) {
      result <- c(result, PATH = path)
    }
  }
  result
}
