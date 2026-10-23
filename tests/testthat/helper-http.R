# Shared by test-mcp-http.R and test-mcp-http-registry.R: the fake transport
# records what the client sent on its "state" attribute.
sent_messages <- function(transport) attr(transport, "state")$messages
