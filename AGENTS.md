# Development notes

## Generated files

`NAMESPACE` and `man/*.Rd` are generated from the roxygen comments in `R/`.
Do not hand-edit them.

```r
roxygen2::roxygenise(".")   # or devtools::document()
```

Docs follow whichever roxygen2 the machine has installed — no pinned version.
DESCRIPTION declares it in `Config/roxygen2/version`; regenerate after a
roxygen2 upgrade and take the resulting `.Rd` reflow in the same commit rather
than fighting it with a version pin. Do not add a `RoxygenNote` line: it would
state a second, competing version number.

If you regenerate after deleting `man/`, run `roxygenise()` **twice**: the R6
"Super class" link (`\link[aisdk.mcp:McpClient]`) resolves only against the
topic index built from an existing `man/`; the first pass leaves it as plain
`\code{}`.

## Tests

`tests/testthat/test-mcp-http.R` drives the Streamable HTTP client through an
injected `transport`, so the whole suite runs offline. One live smoke test
talks to the public GPSAdb endpoint and is skipped on CRAN.
