# Acton Programming Language - Development Guide

Acton is an actor-based programming language with distributed computing capabilities. This guide helps AI assistants understand the codebase structure and development practices.

## Quick Start

### Essential Build Commands
```bash
make                           # Build everything
make dist/bin/actonc          # Build only the compiler (faster when working on compiler)
make test                     # Run all tests
make test-compiler            # Run compiler tests only
dist/bin/acton build          # Build an Acton project
dist/bin/acton test           # Test an Acton project
```

### Key Binaries
- `dist/bin/actonc` - The Acton compiler
- `dist/bin/acton` - The Acton CLI frontend tool (project management, testing)

Our normal users should use `acton` as the only tool (they don't need to know about actonc).

## Repository Overview

```
acton/
├── compiler/          # Haskell-based compiler → [See compiler/CLAUDE.md]
├── base/             # Standard library & RTS → [See base/CLAUDE.md]
├── backend/          # Distributed runtime → [See backend/CLAUDE.md]
├── cli/              # CLI tool → [See cli/CLAUDE.md]
├── test/             # Test suites
├── docs/             # Documentation (mdBook)
└── build.zig         # Main build configuration
```

## Component-Specific Guides

For detailed information about each component, see:
- **[compiler/CLAUDE.md](compiler/CLAUDE.md)** - Haskell compiler architecture, compilation pipeline
- **[base/CLAUDE.md](base/CLAUDE.md)** - Standard library modules, RTS, builtins
- **[backend/CLAUDE.md](backend/CLAUDE.md)** - Distributed database, actor persistence
- **[cli/CLAUDE.md](cli/CLAUDE.md)** - CLI commands, package management

## Language Concepts

### Actor Model
- Actors are concurrent entities with private state
- Communication via asynchronous message passing
- No shared memory between actors
- Actors can be distributed across nodes

### Type System
- Static typing with type inference
- Python-inspired syntax
- Support for generics
- Protocol-based polymorphism (similar to interfaces)

## Code Style Guidelines

### Acton Code
```acton
# Variables and functions: snake_case
def calculate_sum(values: list[int]) -> int:
    return sum(values)

# Classes and types: PascalCase
class DataProcessor:
    def process(self, data: str) -> None:
        pass

# Actors: PascalCase
actor WorkerActor:
    def handle_request(self, req: Request) -> None:
        pass
```

### Haskell Code (Compiler)
```haskell
-- Functions: camelCase
parseExpression :: Parser Expression

-- Types: PascalCase
data AstNode = Literal Int | Variable String

-- Explicit type signatures always
-- 2-space indentation for some functions
-- "Whiteboard layout" for other functions where very deep indent is used to lay out expressions almost like one would when writing beautiful math expressions on a whiteboard
```

### C Code (RTS/Backend)
```c
// Functions: snake_case with module prefix
int rts_actor_create(rts_actor_t *actor);

// Macros/Constants: UPPER_CASE
#define MAX_ACTORS 1024

// Structs: snake_case with _t suffix
typedef struct actor_state_t {
    // ...
} actor_state_t;
```

## Testing Philosophy

1. **Comprehensive Coverage**: Every feature should have tests
2. **Golden Tests**: For compiler error messages and output
3. **Performance Tests**: Track performance regressions
4. **Integration Tests**: Test distributed features

## Common Development Tasks

### Adding a New Builtin Type
1. Implement C code in `base/builtin/`
2. Add Acton interface in `base/src/__builtin__.act`
3. Update compiler type system if needed
4. Add tests in `test/builtins_auto/`

### Adding a Standard Library Module
1. Create `.act` file in `base/src/`
2. Add C extension in `.ext.c` if needed
3. Update `base/Acton.toml`
4. Add module tests

### Modifying the Compiler
1. Work in `compiler/lib/src/Acton/`
2. Run `make dist/bin/actonc` for quick rebuilds
3. Add test cases in `compiler/actonc/test/`
4. Update golden files if error messages change

## Build System

- Uses Zig build system (`build.zig` files)
- Integrates Haskell (Stack), C, and Acton compilation
- Supports cross-compilation
- Package management via `Acton.toml` files

## Important Files

- `Makefile` - Top-level build orchestration
- `build.zig` - Main Zig build configuration
- `compiler/stack.yaml` - Haskell dependencies
- `*/Acton.toml` - Acton package configurations

## Debugging Tips

1. **Compiler Issues**: Enable verbose output with `--debug`
2. **Runtime Issues**: Use `ACTON_LOG_LEVEL=debug`
3. **Actor Issues**: Monitor with `actonmon` utility
4. **Memory Issues**: Built-in GC statistics available

## Contributing Guidelines

1. Follow existing code style in each language
2. Add tests for new features
3. Update documentation as needed
4. Ensure `make test` passes before submitting changes
5. Keep commits focused and well-described

For detailed development workflow and release procedures, see [docs/dev.md](docs/dev.md)

## Git Commit Guidelines

When making commits:
- Do not include AI assistant attribution (no "Generated with Claude" or similar)
- Write commit messages as if you wrote the code yourself
- Focus on what the change does, not how it was created
- Try hard to use short summary messages (< 50 chars)
- **NEVER use `git add -A` or `git add .`** - always add files deliberately
- Be careful not to add generated files (like `package.yaml` which are generated from `.in` templates)

## Release Process

### Creating a New Release

1. **Create release branch**: Use format `release-vX.Y.Z` (note the `v` prefix!)
   ```bash
   git checkout -b release-v0.26.0
   ```

2. **Update version files**:
   - `version.mk` - Update VERSION (this controls the version for the entire project)
   - Note: Do NOT manually edit `package.yaml` files - they are generated from `package.yaml.in` templates

3. **Update CHANGELOG.md**:
   - Add new version section with **today's date** (format: YYYY-MM-DD)
   - Organize changes under: Added, Changed, Fixed, Documentation, Testing/CI
   - Add PR links for all referenced PRs at the bottom
   - Add version comparison link

4. **Create and merge PR**:
   ```bash
   git push -u origin release-vX.Y.Z
   gh pr create --title "Release vX.Y.Z"
   ```

5. **After PR merge**:
   - Create and push tag: `git tag vX.Y.Z && git push origin vX.Y.Z`
   - GitHub Actions will automatically create the release
   - Update Homebrew formula if needed

### Important Release Notes
- Always use `release-v` prefix for release branches (not just `release-`)
- Always use today's date in the changelog (not a future or past date)
- The changelog update process is documented in `docs/dev.md`
