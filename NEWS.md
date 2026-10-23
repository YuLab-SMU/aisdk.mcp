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
