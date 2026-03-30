# Contributing to HEngram

We welcome contributions to HEngram! This project is built for high efficiency on specific hardware targets (legacy x64 CPUs), so please follow these guidelines to maintain its architectural integrity.

## 🏗️ Architectural Principles

1.  **Haskell Core**: All MCP protocol handling, JSON-RPC serialization, and Redis interaction should remain in Haskell.
2.  **C++ Supervisor**: Avoid adding Python or high-level scripting dependencies for ML. Use `llama.cpp` bin-releases or similar bare-metal C++ solutions managed via `System.Process`.
3.  **Matryoshka Representation Learning (MRL)**: HEngram expects 384-dimensional embeddings. If you add a new model, ensure it supports truncation or is natively 384-dim.

## 🛠️ Development & Testing

-   **TDD**: Use `cabal test` for all logic changes. 
-   **Redis Schema**: Any change to `createRediSearchIndex` in `Redis.hs` requires manually dropping the index (`FT.DROPINDEX engram_index`) before the new schema can be applied.
-   **Dependencies**: Keep the dependency list minimal. Every new library is a liability on legacy hardware.

## 🚀 Submission Process

1.  Create a feature branch: `git checkout -b feat/your-feature`.
2.  Ensure `cabal build` passes and linting is clean.
3.  Execute all tests and ensure 100% green.
4.  Submit a Pull Request with a clear rationale.

## 🌍 Community

Respect the hardware constraints. HEngram exists to prove that high-performance AI integration is possible without modern GPUs or massive RAM.
