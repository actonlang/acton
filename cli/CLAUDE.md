# Acton CLI - Command Line Interface

The Acton CLI (`acton`) is the primary tool for managing Acton projects, written in Acton itself. It provides project management, building, testing, and package management functionality.

## Quick Reference

### Directory Structure
```
cli/
├── src/
│   ├── acton.act           # Main CLI entry point
│   └── acton_cli/          # CLI modules
│       ├── deps.act        # Dependency management
│       └── github.act      # GitHub integration
├── out/                    # Compiled output
├── build.zig              # Build configuration
└── Acton.toml            # Package definition
```

### Common Commands
```bash
# Project commands
acton new <name>          # Create new project
acton build               # Build current project
acton test                # Run tests
acton run                 # Build and run

# Dependency management
acton pkg add <package>   # Add dependency
acton pkg remove <pkg>    # Remove dependency
acton pkg fetch           # Download dependencies

# Development
acton clean              # Clean build artifacts
acton check              # Type check without building
```

## CLI Architecture

### Command Structure
The CLI follows a subcommand pattern:
```
acton <command> [options] [arguments]
```

### Main Components

#### `acton.act` - Entry Point
- Argument parsing
- Command dispatch
- Error handling
- Help generation

#### `acton_cli/deps.act` - Dependency Management
- Package resolution
- Version management
- Lock file handling
- Fetching from repositories

#### `acton_cli/github.act` - GitHub Integration
- Repository discovery
- Release management
- Package publishing

## Working with the CLI

### Adding a New Command

1. **Define Command** in `src/acton.act`:
```acton
def cmd_mycommand(args: list[str]) -> None:
    """Description of my command"""
    # Parse specific arguments
    # Perform command action
    # Handle errors
```

2. **Register Command**:
```acton
commands = {
    "mycommand": (cmd_mycommand, "Short description"),
    # ... other commands
}
```

3. **Add Help Text**:
```acton
help_texts["mycommand"] = """
Usage: acton mycommand [options]

Detailed description of what the command does.

Options:
  --flag    Description of flag
  --option  Description of option
"""
```

### Project Management

#### Project Structure
```
my_project/
├── Acton.toml         # Project configuration
├── src/               # Source files
│   └── main.act       # Entry point
├── test/              # Test files
├── .acton.lock        # Dependency lock file
└── out/               # Build output
```

#### Acton.toml Format
```toml
[package]
name = "my_project"
version = "0.1.0"
authors = ["Name <email>"]
description = "Project description"

[dependencies]
stdlib = "1.0.0"
http_server = { git = "https://github.com/acton/http", tag = "v2.0" }
local_lib = { path = "../local_lib" }

[dev-dependencies]
test_framework = "1.0.0"

[build]
zig_dependencies = ["libcurl"]
```

### Dependency Resolution

#### Resolution Algorithm
1. Read `Acton.toml` dependencies
2. Check `.acton.lock` for pinned versions
3. Resolve version constraints
4. Fetch missing packages
5. Update lock file

#### Package Sources
- **Registry**: Default package registry
- **Git**: Direct from git repositories
- **Local**: File system paths

### Build Process

#### Build Stages
1. **Parse Project**: Read `Acton.toml`
2. **Resolve Dependencies**: Fetch and validate
3. **Compile**: Invoke `actonc` compiler
4. **Generate Zig Build**: Create `build.zig`
5. **Build C/Binary**: Use Zig to compile
6. **Link**: Create final executable

#### Build Cache
- Incremental compilation
- Dependency tracking
- Artifact caching

## Testing Integration

### Test Discovery
```acton
def discover_tests(test_dir: str) -> list[TestFile]:
    # Find all .act files in test/
    # Identify test functions
    # Group by test type
```

### Test Execution
```acton
def run_tests(tests: list[TestFile]) -> TestResults:
    # Compile test files
    # Execute test binaries
    # Collect results
    # Generate report
```

### Test Types
- **Unit Tests**: `test_*` functions
- **Actor Tests**: `_test_*` actor methods
- **Integration Tests**: Full program tests

## Package Management

### Adding Dependencies
```acton
def add_dependency(name: str, version: str) -> None:
    # Parse current Acton.toml
    # Add to dependencies section
    # Resolve version constraints
    # Update Acton.toml
    # Fetch package
    # Update lock file
```

### Publishing Packages
```acton
def publish_package() -> None:
    # Validate package
    # Build distribution
    # Upload to registry
    # Tag git repository
```

## Error Handling

### User-Friendly Errors
```acton
class CLIError(Exception):
    def __init__(self, message: str, hint: str = None):
        self.message = message
        self.hint = hint

# Usage
if not file_exists("Acton.toml"):
    raise CLIError(
        "No Acton.toml found in current directory",
        hint="Run 'acton new' to create a new project"
    )
```

### Error Formatting
- Clear error messages
- Contextual information
- Suggested fixes
- Help command references

## Configuration

### Global Configuration
```bash
~/.acton/config.toml
```

### Project Configuration
```bash
.acton/config.toml
```

### Environment Variables
```bash
ACTON_HOME          # Acton installation directory
ACTON_CACHE         # Build cache location
ACTON_REGISTRY      # Package registry URL
ACTON_PARALLELISM   # Build parallelism
```

## CLI Development

### Running from Source
```bash
# Build CLI
cd cli
../dist/bin/acton build

# Run CLI
./out/bin/acton --help
```

### Debugging
```acton
# Add debug prints
if debug_enabled():
    print(f"Debug: Processing {file}")

# Enable with environment
export ACTON_DEBUG=1
```

### Testing CLI Commands
```acton
actor test_cli_commands:
    def test_new_command():
        # Create temp directory
        # Run 'acton new test_project'
        # Verify project structure
        # Clean up
```

## Integration with Build System

### Invoking Compiler
```acton
def compile_project(src_files: list[str]) -> bool:
    cmd = [compiler_path] + compiler_args + src_files
    result = run_process(cmd)
    return result.exit_code == 0
```

### Zig Build Generation
```acton
def generate_build_zig(project: Project) -> str:
    # Generate build.zig content
    # Include dependencies
    # Set up link flags
    # Configure output
```

## Future Enhancements

Planned features:
- Interactive project templates
- Dependency vulnerability scanning
- Performance profiling integration
- Remote caching
- Workspace support
- Custom registries

## Common Issues

### Issue: Dependency Conflicts
- Check version constraints
- Use `acton pkg tree` to visualize
- Consider version pinning

### Issue: Build Failures
- Check compiler errors
- Verify dependencies installed
- Clear cache with `acton clean`

### Issue: Slow Builds
- Enable parallel builds
- Use build cache
- Consider splitting large projects