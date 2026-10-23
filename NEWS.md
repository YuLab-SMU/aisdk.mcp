# aisdk.mcp 0.1.3

* New `mcp_action()`: bind one tool into a data-first function. It reads the
  server's schema to decide which argument receives the value (`into`) and how a
  vector is written (`as`: JSON array, carriage-return string, or scalar), so a
  cross-server step reduces to `value |> act()`. Unknown tool names and
  ambiguous schemas fail when the action is built, not on first use.
* New `mcp_intent()`: describe a pipeline in one sentence and get back R code
  that uses it, built only from the tools the servers actually advertise. The
  code is returned and never evaluated. `model = NULL` uses the aisdk default
  model, so no model argument is needed when one is configured. `context =`
  passes instructions (a package's typed functions, for instance) so the code
  prefers them over raw `mcp_action()` calls.

# aisdk.mcp 0.1.2

* New `combine_mcp_tools()`: collect the tools of several MCP clients into one
  `tools=` list. Tool names are flat in a session, so each server's tools are
  prefixed with its own advertised name (`gpsadb__query_gene`), and duplicate
  names get a numeric suffix. With `prefix = FALSE` the names are kept as they
  are and duplicates raise an error instead of one server silently shadowing
  another.

# aisdk.mcp 0.1.1

* New **Streamable HTTP** transport: `create_mcp_http_client()` /
  `McpHttpClient`, for the MCP 2025-06-18 single-POST-endpoint style that most
  public MCP servers use. `create_mcp_sse_client()` only speaks the older
  HTTP+SSE transport and gets HTTP 405 from such servers.
* New `call_tool_data()` returns a tool payload as plain R data, preferring MCP
  `structuredContent` and falling back to the text block.
* `call_tool()` now raises a `mcp_tool_error` condition when a tool reports
  `isError: true`, carrying the server's message.
* Fix: a tool called with no arguments sent `"arguments": []`; it now sends
  `{}`, which strict servers require.

# aisdk.mcp 0.1.0

* First release. Adds Model Context Protocol (MCP) support for the
  `aisdk` toolkit: a client for connecting to and using tools from local
  (stdio) or remote (SSE) MCP servers, a server for exposing R functions
  as MCP tools, and discovery helpers.
