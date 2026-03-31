# Contributing to HEngram

We welcome contributions to HEngram! This project is built for high efficiency with a low hardware footprint, so please follow these guidelines to maintain its architectural integrity.

## 🏗️ Architectural Principles

1.  **Haskell Core**: All MCP protocol handling, JSON-RPC serialization, and Redis interaction should remain in Haskell.
2.  **C++ Supervisor**: Avoid adding Python or high-level scripting dependencies for ML. Use `llama-server` managed via `System.Process` for embedding inference.
3.  **Matryoshka Representation Learning (MRL)**: HEngram expects 384-dimensional embeddings. If you add a new model, ensure it supports truncation or is natively 384-dim.
4.  **Intelligence on Server**: Validation and normalization logic should be implemented in the Haskell layer (e.g., in `App.hs`) rather than relying on the LLM to provide perfectly formatted data.

## 🛠️ Development & Testing

-   **TDD**: Use `cabal test` for all logic changes. 
-   **Redis Schema**: Any change to the search index requires manually dropping it (`FT.DROPINDEX engram_index`) before the new schema can be applied.
-   **Dependencies**: Keep the dependency list minimal. Every new library is a liability.

## 🚀 Submission Process

1.  Create a feature branch: `git checkout -b feat/your-feature`.
2.  Ensure `cabal build` passes and linting is clean.
3.  Execute all tests and ensure they are all passing.
4.  Submit a Pull Request with a clear rationale.

## 🌍 Community

Respect the resource constraints. HEngram exists to prove that high-performance AI integration is possible with minimal hardware requirements.
