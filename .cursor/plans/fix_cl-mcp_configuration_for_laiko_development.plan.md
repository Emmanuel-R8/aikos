---
name: Fix cl-mcp Configuration for Laiko Development
overview: Fix the cl-mcp MCP server configuration to work with Laiko development, addressing the libcrypto dependency issue and configuration problems.
todos:
  - id: check-openssl
    content: Check if OpenSSL/libcrypto is available in Nix environment and find its path
    status: pending
  - id: update-mcp-config
    content: Update ~/.cursor/mcp.json with correct configuration (HTTP transport recommended)
    status: pending
  - id: test-cl-mcp-load
    content: Test if cl-mcp can be loaded after fixing dependencies
    status: pending
  - id: start-http-server
    content: If using HTTP transport, start cl-mcp HTTP server in REPL
    status: pending
  - id: verify-connection
    content: Verify Claude Code can connect to cl-mcp server
    status: pending
---

# Fix cl-mcp Configuration for Laiko Development

## Issues Identified

1. **Missing libcrypto dependency**: `cl-mcp` fails to load because it can't find `libcrypto.so` (OpenSSL library)
2. **Configuration variable**: `${workspaceFolder}` may not expand correctly in Cursor's MCP config
3. **Transport choice**: HTTP transport might be better for Claude Code (as recommended in cl-mcp docs)

## Solutions

### Option 1: Fix libcrypto Dependency (Recommended for stdio transport)

The error shows that cl-mcp (or one of its dependencies like `usocket` or `hunchentoot`) requires OpenSSL's libcrypto. In a Nix environment, you need to ensure the library is available.

**Steps:**

1. Install OpenSSL in your Nix environment (if using nix-shell/flake)
2. Set `LD_LIBRARY_PATH` to include OpenSSL libraries
3. Update mcp.json with absolute path and proper environment

### Option 2: Use HTTP Transport (Recommended for Claude Code)

According to cl-mcp documentation, HTTP transport is recommended for Claude Code because:

- You can start the server manually in your REPL
- Both you and Claude Code can use the same Lisp runtime
- Avoids stdio transport issues

**Steps:**

1. Start cl-mcp HTTP server manually in your REPL
2. Configure mcp.json to connect via HTTP
3. No libcrypto dependency issues (HTTP server doesn't require it)

### Option 3: Use ros (Recommended by cl-mcp docs)

The cl-mcp documentation recommends using `ros run` instead of direct `sbcl` invocation.

## Implementation

### Fix 1: Update mcp.json Configuration

**Current config issues:**

- `${workspaceFolder}` variable may not expand
- Missing libcrypto in environment
- Direct sbcl invocation may not load Quicklisp properly

**Fixed configurations:**

#### Configuration A: Stdio Transport (with libcrypto fix)

```json
{
  "mcpServers": {
    "zotero": {
      "command": "uvx",
      "args": ["masaki39-zotero-mcp"]
    },
    "cl-mcp": {
      "command": "sbcl",
      "args": [
        "--eval",
        "(load \"~/quicklisp/setup.lisp\")",
        "--eval",
        "(ql:quickload :cl-mcp)",
        "--eval",
        "(cl-mcp:run :transport :stdio)"
      ],
      "env": {
        "MCP_PROJECT_ROOT": "/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp",
        "LD_LIBRARY_PATH": "/nix/store/.../lib:/usr/lib:/usr/local/lib"
      }
    }
  }
}
```

**Note**: You'll need to find the correct OpenSSL path in your Nix store for `LD_LIBRARY_PATH`.

#### Configuration B: HTTP Transport (Recommended)

```json
{
  "mcpServers": {
    "zotero": {
      "command": "uvx",
      "args": ["masaki39-zotero-mcp"]
    },
    "cl-mcp": {
      "type": "http",
      "url": "http://127.0.0.1:3000/mcp"
    }
  }
}
```

**Then start the server manually in your REPL:**

```lisp
(ql:quickload :cl-mcp)
(cl-mcp:start-http-server :port 3000)
```

#### Configuration C: Using ros (Alternative)

```json
{
  "mcpServers": {
    "zotero": {
      "command": "uvx",
      "args": ["masaki39-zotero-mcp"]
    },
    "cl-mcp": {
      "command": "ros",
      "args": ["run", "-s", "cl-mcp", "-e", "(cl-mcp:run :transport :stdio)"],
      "env": {
        "MCP_PROJECT_ROOT": "/home/emmanuel/Sync/Development/Emulation/_gits/Interlisp"
      }
    }
  }
}
```

### Fix 2: Resolve libcrypto Dependency

**For Nix environments:**

1. **Check if OpenSSL is available:**

```bash
 nix-env -qaP | grep openssl
 # Or in your flake.nix, ensure openssl is included
```

2. **Add to flake.nix** (if using Nix):

```nix
 {
   inputs = { ... };
   outputs = { ... }: {
     devShells.default = pkgs.mkShell {
       buildInputs = with pkgs; [
         sbcl
         openssl  # Add this
       ];
       shellHook = ''
         export LD_LIBRARY_PATH="${pkgs.openssl.out}/lib:$LD_LIBRARY_PATH"
       '';
     };
   };
 };
```

3. **Or set LD_LIBRARY_PATH manually:**

```bash
 export LD_LIBRARY_PATH=$(nix-build '<nixpkgs>' -A openssl.out --no-out-link)/lib:$LD_LIBRARY_PATH
```

### Fix 3: Verify Quicklisp Setup

Ensure Quicklisp can find cl-mcp:

```bash
# Test if cl-mcp can be loaded (after fixing libcrypto)
sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
     --eval "(ql:quickload :cl-mcp)" \
     --eval "(quit)"
```

## Recommended Approach

**For Laiko development, I recommend HTTP transport:**

1. **Start cl-mcp HTTP server in your REPL:**

```lisp
 (ql:quickload :cl-mcp)
 (cl-mcp:start-http-server :port 3000)
```

2. **Update ~/.cursor/mcp.json:**

```json
{
  "mcpServers": {
    "zotero": {
      "command": "uvx",
      "args": ["masaki39-zotero-mcp"]
    },
    "cl-mcp": {
      "type": "http",
      "url": "http://127.0.0.1:3000/mcp"
    }
  }
}
```

3. **Initialize project root in Claude Code:**

When Claude Code connects, it should call `fs-set-project-root` with your project path, or you can configure it to do so automatically.

## Testing

After configuration:

1. **Test HTTP server** (if using HTTP transport):

```bash
 curl http://127.0.0.1:3000/mcp
```

2. **Test stdio transport** (if using stdio):

```bash
 echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | \
   sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
        --eval "(ql:quickload :cl-mcp)" \
        --eval "(cl-mcp:run :transport :stdio)"
```

## Troubleshooting

- **libcrypto not found**: Use HTTP transport or add OpenSSL to your Nix environment
- **Quicklisp not loading**: Ensure `~/quicklisp/setup.lisp` exists and is loadable
- **Project root not set**: Call `fs-set-project-root` tool after connecting, or set `MCP_PROJECT_ROOT` env var
- **Connection refused**: Ensure HTTP server is running on the configured port
