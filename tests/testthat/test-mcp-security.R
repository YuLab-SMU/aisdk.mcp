# Security: a local (stdio) MCP server is a third-party binary and must NOT
# inherit this session's model API keys. See R/mcp_env.R.

test_that("mcp_is_sensitive_envvar recognises credentials but not ordinary vars", {
  for (nm in c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GITHUB_TOKEN", "GH_TOKEN",
               "AWS_SECRET_ACCESS_KEY", "OPENAI_KEY", "MY_APP_TOKEN",
               "DB_PASSWORD", "SOME_SECRET", "A_BEARER", "SESSION_COOKIE")) {
    expect_true(mcp_is_sensitive_envvar(nm), info = nm)
  }
  for (nm in c("PATH", "HOME", "LANG", "MY_APP_MODE", "DATABASE_URL", "")) {
    expect_false(mcp_is_sensitive_envvar(nm), info = nm)
  }
})

test_that("mcp_is_safe_envvar allowlists only non-secret system vars", {
  for (nm in c("PATH", "HOME", "LANG", "LC_CTYPE", "TMPDIR", "SystemRoot", "Path")) {
    expect_true(mcp_is_safe_envvar(nm), info = nm)
  }
  for (nm in c("OPENAI_API_KEY", "MY_APP_MODE", "DATABASE_URL", "RANDOM_VAR", "")) {
    expect_false(mcp_is_safe_envvar(nm), info = nm)
  }
})

test_that("the secure default never leaks secrets but keeps PATH", {
  withr::local_envvar(c(
    OPENAI_API_KEY = "sk-SECRET", ANTHROPIC_API_KEY = "sk-ant-SECRET",
    GITHUB_TOKEN = "ghp_SECRET", MY_APP_MODE = "prod"
  ))
  e <- mcp_build_process_env(env = NULL, inherit_env = FALSE)

  expect_false("OPENAI_API_KEY" %in% names(e))
  expect_false("ANTHROPIC_API_KEY" %in% names(e))
  expect_false("GITHUB_TOKEN" %in% names(e))
  expect_false("MY_APP_MODE" %in% names(e))   # not allowlisted -> withheld
  expect_true("PATH" %in% names(e))            # subprocess can still run
})

test_that("explicitly shared env is passed through and overrides the base", {
  withr::local_envvar(c(OPENAI_API_KEY = "sk-SECRET"))
  e <- mcp_build_process_env(
    env = c(GITHUB_PERSONAL_ACCESS_TOKEN = "ghp_EXPLICIT", PATH = "/custom/bin"),
    inherit_env = FALSE
  )
  expect_equal(e[["GITHUB_PERSONAL_ACCESS_TOKEN"]], "ghp_EXPLICIT") # deliberately shared
  expect_equal(e[["PATH"]], "/custom/bin")                          # explicit wins over base
  expect_false("OPENAI_API_KEY" %in% names(e))                      # session secret still withheld
})

test_that("inherit_env=TRUE passes ordinary vars but still strips credentials", {
  withr::local_envvar(c(
    OPENAI_API_KEY = "sk-SECRET", GITHUB_TOKEN = "ghp_SECRET",
    MY_APP_MODE = "prod", DATABASE_URL = "postgres://x"
  ))
  e <- mcp_build_process_env(env = NULL, inherit_env = TRUE)

  expect_true("MY_APP_MODE" %in% names(e))       # ordinary vars flow through
  expect_true("DATABASE_URL" %in% names(e))
  expect_false("OPENAI_API_KEY" %in% names(e))   # defence in depth: secrets still stripped
  expect_false("GITHUB_TOKEN" %in% names(e))
})

test_that("mcp_build_process_env rejects an unnamed env and always yields PATH", {
  expect_error(mcp_build_process_env(env = c("no-name")), "named")
  e <- mcp_build_process_env(env = NULL, inherit_env = FALSE)
  expect_true("PATH" %in% names(e))
})

test_that("McpClient / create_mcp_client / connect expose inherit_env (default FALSE)", {
  expect_true("inherit_env" %in% names(formals(create_mcp_client)))
  expect_false(eval(formals(create_mcp_client)$inherit_env))
  # McpClient$new's inherit_env default is FALSE too.
  init_fmls <- formals(McpClient$public_methods$initialize)
  expect_true("inherit_env" %in% names(init_fmls))
  expect_false(eval(init_fmls$inherit_env))
})
