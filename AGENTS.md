# Development notes

## Generated files

`NAMESPACE` and `man/*.Rd` are generated from the roxygen comments in `R/`.
Do not hand-edit them.

```r
roxygen2::roxygenise(".")   # or devtools::document()
```

Docs are generated with roxygen2 **7.3.3** (see `RoxygenNote` in DESCRIPTION).
roxygen2 8.x rewrites every existing `.Rd` file (indentation, `\details`
folding) without changing content, so pin 7.3.3 unless you want that churn:

```r
remotes::install_version("roxygen2", "7.3.3", lib = "<some lib>")
```

If you regenerate after deleting `man/`, run `roxygenise()` **twice**: the R6
"Super class" link (`\link[aisdk.mcp:McpClient]`) resolves only against the
topic index built from an existing `man/`; the first pass leaves it as plain
`\code{}`.

## Tests

`tests/testthat/test-mcp-http.R` drives the Streamable HTTP client through an
injected `transport`, so the whole suite runs offline. One live smoke test
talks to the public GPSAdb endpoint and is skipped on CRAN.
