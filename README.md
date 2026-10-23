# aisdk.mcp

Model Context Protocol (MCP) support for the
[aisdk](https://github.com/YuLab-SMU/aisdk) toolkit.

- **Client** — connect to and use tools from local (stdio) or remote MCP
  servers: `create_mcp_client()` (stdio), `create_mcp_sse_client()` (legacy
  HTTP+SSE), `create_mcp_http_client()` (Streamable HTTP).
- **Server** — expose R functions as MCP tools (`create_mcp_server()`).
- **Discovery** — discover and integrate external MCP tools into agents.

All three clients share one tool API, so a model does not care which transport
delivered the tools:

```r
client$list_tools()                     # what the server offers
client$call_tool("name", list(...))     # raw MCP result
client$call_tool_data("name", list(...))# payload as plain R data
client$as_sdk_tools()                   # hand them to an aisdk agent
```

A self-contained MCP protocol implementation; depends only on the core `aisdk`
contract (`tool`, `generate_text`, schema builders), which is why it extracts
cleanly with no changes to core.

## Installation

```r
# install.packages("remotes")
remotes::install_github("YuLab-SMU/aisdk")       # core
remotes::install_github("YuLab-SMU/aisdk.mcp")   # this package
```

## Which remote transport?

| Server advertises | Use |
|---|---|
| One endpoint answering JSON-RPC `POST` (MCP 2025-06-18 Streamable HTTP) | `create_mcp_http_client(url)` |
| A `GET` event stream that announces a separate `POST` endpoint (2024-11-05 HTTP+SSE) | `create_mcp_sse_client(url)` |

If in doubt, try Streamable HTTP first: a Streamable HTTP endpoint answers `GET`
with HTTP 405, while a legacy SSE endpoint accepts it. Most public/hosted MCP
servers today are Streamable HTTP.

## Usage

```r
library(aisdk)
library(aisdk.mcp)

# local (stdio) server
client <- create_mcp_client(command = "my-mcp-server", args = character(0))

# remote server over Streamable HTTP — no key needed for this public one
client <- create_mcp_http_client("https://gpsadb.gzdlab.com/mcp")
client$server_info$name          # "gpsadb"
client$list_tools()              # 11 tools
info <- client$call_tool_data("get_database_info")

# use the discovered tools with an aisdk agent...
generate_text(
  model = "deepseek:deepseek-chat",
  prompt = "How many perturbation datasets does GPSAdb hold?",
  tools = client$as_sdk_tools()
)

client$close()
```
