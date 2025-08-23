# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an ABAP to JavaScript transpiler that converts ABAP 7.02 syntax to ES6. The project is a monorepo with multiple packages that handle different aspects of transpilation and runtime execution.

## AI Collaboration

For advanced AI-driven development practices, see [AI_COLLABORATION_GUIDE.md](./AI_COLLABORATION_GUIDE.md). Key principles:

### Parallel Development
- **Statement transpilers** can be developed independently by multiple AI agents
- **Expression handlers** are perfect for parallel implementation
- **Runtime functions** can be built as separate modules
- **Test generation** can happen alongside implementation

### Task Decomposition Strategy
When working on complex features:
1. Break into independent components (statements, expressions, runtime)
2. Use TodoWrite tool to track parallel tasks
3. Deploy specialized agents for each component
4. Integrate and verify with E2E tests

### Dataset Generation
The transpiler is ideal for generating ML training data:
- Extract ABAP→JS pairs at statement/expression level
- Build pattern mappings for language models
- Generate API surface documentation
- See `TRANSPILER_API_USAGE.md` for implementation details

## Development Commands

### Build and Compilation
```bash
# Install all dependencies (monorepo-wide)
npm run install

# Compile all packages
npm run compile

# Run tests with compilation
npm test

# Run linting
npm run lint
```

### Package-Specific Commands
```bash
# Link packages locally for development
npm run link-local

# Run performance tests
npm run performance

# Run regression tests
npm run regression

# Docker operations for testing infrastructure
npm run docker:start
npm run docker:stop
```

### Testing Individual Packages
```bash
# Test transpiler package
cd packages/transpiler && npm test

# Test runtime package  
cd packages/runtime && npm test

# Test CLI package
cd packages/cli && npm test
```

## Architecture

### Package Structure

The codebase is organized as a monorepo with the following key packages:

1. **`packages/transpiler/`** - Core transpilation engine
   - Converts ABAP AST (from abaplint) to JavaScript
   - Handles different ABAP object types (classes, function groups, tables, etc.)
   - Statement and expression transpilers in respective subdirectories

2. **`packages/runtime/`** - JavaScript runtime for transpiled ABAP
   - Implements ABAP built-in functions and data types
   - Provides database abstraction layer
   - Contains console/output handling

3. **`packages/cli/`** - Command-line interface
   - Executable for transpiling ABAP projects
   - Configuration management

4. **Database Drivers** (`packages/database-*`)
   - SQLite, PostgreSQL, and Snowflake implementations
   - All extend the base database interface from runtime

5. **`packages/rfc-client-soap-xml/`** - SOAP/XML RFC client implementation

6. **`web/transpiler/`** - Web interface for the transpiler playground

### Key Transpilation Flow

1. **Parsing**: Uses @abaplint/core to parse ABAP files into AST
2. **Validation**: Checks for unsupported syntax and features (see `validation.ts`)
3. **Handler Selection**: Different handlers for different object types:
   - `HandleABAP` for classes, interfaces, programs
   - `HandleFUGR` for function groups
   - `HandleTable` for database tables
   - Each handler in `packages/transpiler/src/handlers/`
4. **Statement/Expression Processing**: 
   - Statements handled by files in `statements/` directory
   - Expressions handled by files in `expressions/` directory
   - Each implements specific ABAP construct transpilation
5. **Output Generation**: Creates JavaScript modules with proper imports and exports

### Important Design Decisions

- All method/function calls are `await`ed (async by default)
- JavaScript constructors renamed to `constructor_` due to async requirement
- CLAS locals (implementation and definition) merged into single file
- Top-level await enabled via `.mjs` file extension
- Single-threaded execution model
- No runtime artifact creation - requires rebuild for changes
- Database buffering settings ignored - all data fetched from DB

### Testing Approach

- Unit tests use Mocha framework
- Tests located in `test/` directories within each package
- Test files correspond to ABAP features being tested
- Performance tests in `performance/` directory measure transpilation speed
- Integration tests verify full transpilation pipeline

## Key Files and Entry Points

- `packages/transpiler/src/index.ts` - Main transpiler entry point
- `packages/runtime/src/index.ts` - Runtime library exports
- `packages/cli/src/index.ts` - CLI implementation
- `packages/transpiler/src/validation.ts` - Validation rules and checks
- `packages/transpiler/src/unit_test.ts` - Unit test generation
- `packages/transpiler/src/db/` - Database schema generation

## Working with ABAP Objects

When implementing new ABAP features:

1. Add handler in `packages/transpiler/src/handlers/` if new object type
2. Add statement transpiler in `packages/transpiler/src/statements/` for new statements
3. Add expression transpiler in `packages/transpiler/src/expressions/` for new expressions
4. Implement runtime support in `packages/runtime/src/` if needed
5. Add corresponding tests in `test/` directories

## Database Support

The transpiler can generate database schemas for supported databases. Schema generators are in `packages/transpiler/src/db/schema_generation/`. When adding database support:

1. Implement database driver in new package following existing patterns
2. Add schema generator extending `DatabaseSchemaGenerator`
3. Update initialization scripts to handle new database type