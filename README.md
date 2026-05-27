# aisdk.mcp

Model Context Protocol (MCP) support for the
[aisdk](https://github.com/YuLab-SMU/aisdk) toolkit.

- **Client** — connect to and use tools from local (stdio) or remote (SSE) MCP
  servers (`create_mcp_client()`, `create_mcp_sse_client()`).
- **Server** — expose R functions as MCP tools (`create_mcp_server()`).
- **Discovery** — discover and integrate external MCP tools into agents.

A self-contained MCP protocol implementation; depends only on the core `aisdk`
contract (`tool`, `generate_text`, schema builders), which is why it extracts
cleanly with no changes to core.

## Installation

```r
# install.packages("remotes")
remotes::install_github("YuLab-SMU/aisdk")       # core
remotes::install_github("YuLab-SMU/aisdk.mcp")   # this package
```

## Usage

```r
library(aisdk)
library(aisdk.mcp)

client <- create_mcp_client(command = "my-mcp-server", args = character(0))
# use the discovered tools with an aisdk agent...
```
