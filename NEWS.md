# aisdk.mcp (development version)

* **Security (behaviour change):** a local (stdio) MCP server no longer inherits
  this R session's full environment. Previously `McpClient` started the server
  with every environment variable — including `OPENAI_API_KEY`,
  `ANTHROPIC_API_KEY`, `GITHUB_TOKEN`, etc. — exposing your model keys to a
  third-party process. By default the server now receives only a minimal set of
  non-secret system variables plus whatever you pass in `env`. Share exactly
  what a server needs via `env`, or pass `inherit_env = TRUE`
  (`create_mcp_client()` / `McpClient$new()`) to inherit the parent environment
  with recognised credentials still stripped.
* Added MCP **sampling**: register `sampling_handler` (see
  `mcp_sampling_handler()`) so a server can ask the client to run a generation
  on its behalf without holding API keys.

# aisdk.mcp 0.1.0

* First release. Adds Model Context Protocol (MCP) support for the
  `aisdk` toolkit: a client for connecting to and using tools from local
  (stdio) or remote (SSE) MCP servers, a server for exposing R functions
  as MCP tools, and discovery helpers.
