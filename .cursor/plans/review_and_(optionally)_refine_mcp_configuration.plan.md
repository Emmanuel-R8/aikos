---
name: Review and (Optionally) Refine MCP Configuration
overview: ""
todos:
  - id: paths-and-deps
    content: Confirm `uvx`, Zotero MCP, SBCL, and Quicklisp are installed and reachable as configured; optionally document these prerequisites near the MCP config.
    status: pending
  - id: server-roles
    content: Decide which of `cl-mcp` (stdio) vs `cl-http-mcp` (HTTP) is your primary CL MCP server, and document intended usage.
    status: pending
  - id: port-collisions
    content: If needed, choose a stable, low-conflict port for `cl-http-mcp` and align both the Lisp server startup and the `url` field in `mcp.json`.
    status: pending
---

# Review and (Optionally) Refine MCP Configuration

## Overview

You asked for a review of your `~/.cursor/mcp.json` configuration. This plan summarizes what your current setup does and outlines small, optional refinements if you later decide you want changes.

## Current Configuration Summary

- **Overall structure**: `mcpServers` defines three servers: `zotero`, `cl-mcp` (stdio), and `cl-http-mcp` (HTTP).
- **`zotero` server**:
- **Command**: `uvx`
- **Args**: `"masaki39-zotero-mcp"`
- **Behavior**: Uses `uvx` (uv tool) to run the `masaki39-zotero-mcp` MCP server, which is a standard way to run this server on demand without a permanent virtualenv.
- **`cl-mcp` server (stdio)**:
- **Command**: `/usr/bin/env sbcl`
- **Args**:
  - `"--eval", "(load \"~/quicklisp/setup.lisp\")"`
  - `"--eval", "(ql:quickload :cl-mcp)"`
  - `"--eval", "(cl-mcp:run :transport :stdio)"`
- **Env**:
  - `MCP_PROJECT_ROOT` set to `/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp`.
- **Behavior**: Starts SBCL, loads Quicklisp, quickloads the `cl-mcp` system, and runs it with stdio transport, scoped to your Interlisp project via `MCP_PROJECT_ROOT`.
- **`cl-http-mcp` server (HTTP)**:
- **Command**: `/usr/bin/env sbcl`
- **Args**:
  - `"--eval", "(load \"~/quicklisp/setup.lisp\")"`
  - `"--eval", "(ql:quickload :cl-mcp)"`
  - `"--eval", "(cl-mcp:start-http-server :port 3000)"`
- **Env**:
  - `MCP_PROJECT_ROOT` set to the same Interlisp project path as above.
- **Type / URL**:
  - `"type": "http"`
  - `"url": "http://127.0.0.1:3000/mcp"`
- **Behavior**: Starts the same `cl-mcp` system but exposes it over HTTP on port 3000, with Cursor connecting to `http://127.0.0.1:3000/mcp`.

## Assessment

- **JSON validity**: The file is syntactically valid JSON with a clear `mcpServers` object and well-formed nested structures.
- **Command configuration**:
- `uvx`-based Zotero server is configured in a standard, idiomatic way for uv-based MCP servers.
- Both Common Lisp servers follow the usual Quicklisp pattern (`load ~/quicklisp/setup.lisp` then `ql:quickload`), and should work correctly with SBCL on a Unix-like system.
- **Environment scoping**:
- `MCP_PROJECT_ROOT` is explicitly set for both CL-based servers, correctly pointing at your Interlisp repo, which is appropriate when the server needs repo context.
- **HTTP server metadata**:
- `cl-http-mcp` correctly specifies `"type": "http"` and a loopback URL, which matches how Cursor expects HTTP MCP servers to be declared.

## Possible Refinements (Optional)

- **Refinement `paths-and-deps`**: Document or verify prerequisites
- Confirm that `uvx` is installed and that `masaki39-zotero-mcp` is available (e.g., via `uv tool install` or equivalent), and that `sbcl` plus Quicklisp are installed and reachable at `~/quicklisp/setup.lisp`.
- Optionally keep a short comment or README near this file (outside the JSON) explaining these dependencies for future maintenance.
- **Refinement `server-roles`**: Clarify intended usage of both CL servers
- Decide and document (in a README or similar) whether you will generally use `cl-mcp` (stdio) or `cl-http-mcp` (HTTP) in Cursor, so future you remembers why both exist.
- **Refinement `port-collisions`**: Guard against HTTP port conflicts
- If you often run other services on port 3000, you may want to standardize a different port for `cl-http-mcp` (and update the `url` accordingly) to avoid collisions.

## Implementation Notes (If You Later Want Changes)

If you decide to adjust anything later (e.g., change ports or project roots), updates would be made directly in `~/.cursor/mcp.json`, followed by restarting Cursor or reloading servers. No structural changes to the JSON schema are needed for the current use cases.
