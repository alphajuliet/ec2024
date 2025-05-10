# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains solutions to the Everybody Codes 2024 programming competition challenges, primarily implemented in Clojure. Each solution file (q01.clj, q02.clj, etc.) corresponds to a specific challenge.

## Environment Setup

This project uses Clojure 1.12.0 and various dependencies managed through the Clojure CLI tools (deps.edn).

## Common Commands

### Running Solutions

To run a specific solution file:

```bash
clj -M src/q01.clj
```

Or using Babashka (bb):

```bash
bb src/q01.clj
```

### REPL Development

Start a REPL with CIDER nREPL middleware:

```bash
clj -M:repl
```

### Running Tests

Run all tests:

```bash
clj -M:test
```

## Code Structure

- `src/` - Contains all solution files (q01.clj, q02.clj, etc.) and utility functions
  - `qXX.clj` - Solution for question XX, usually with multiple parts
  - `util.clj` - Common utility functions used across multiple solutions
- `data/` - Contains input data files for each challenge
- `deps.edn` - Project dependencies and aliases
- `bb.edn` - Babashka configuration

## Key Dependencies

- `clojure.core.match` - Pattern matching library
- `clojure.math.combinatorics` - Combinatorial algorithms