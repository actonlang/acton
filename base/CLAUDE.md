# Acton Base - Standard Library & Runtime System

The base directory contains the Acton standard library and runtime system (RTS), providing core functionality for all Acton programs.

## Quick Reference

### Directory Structure
```
base/
├── src/                  # Standard library modules (Acton)
├── builtin/             # Builtin types implementation (C)
├── rts/                 # Runtime system (C/Zig)
├── stdlib/              # Additional stdlib components
├── build.zig            # Build configuration
└── Acton.toml          # Package definition
```

### Build Commands
```bash
# Build base (included in main build)
make

# Run stdlib tests
make test-stdlib

# Build specific module
cd base && zig build
```

## Standard Library Modules (`src/`)

### Core Modules

#### `__builtin__.act`
- Fundamental types and functions
- Imported implicitly in all modules
- Interfaces to C builtins

#### `file.act` / `file.ext.c`
- File I/O operations
- Path manipulation
- Directory operations

#### `net.act` / `net.ext.c`
- TCP/UDP networking
- DNS resolution
- Socket operations

#### `process.act` / `process.ext.c`
- Process spawning
- Environment variables
- Signal handling

#### `time.act` / `time.ext.c`
- Time operations
- Sleep/delay functions
- Time formatting

### Data Processing

#### `json.act` / `json.ext.c`
- JSON parsing and generation
- Type-safe JSON handling

#### `xml.act` / `xml.ext.c`
- XML parsing
- DOM manipulation

#### `base64.act` / `base64.ext.c`
- Base64 encoding/decoding

### Advanced Modules

#### `http.act`
- HTTP client/server
- Request/response handling
- WebSocket support

#### `re.act` / `re.ext.c`
- Regular expressions
- Pattern matching
- String substitution

#### `numpy.act` / `numpy.c`
- Numerical arrays
- Mathematical operations
- Compatible subset of NumPy

## Builtin Types (`builtin/`)

### Type Implementation Pattern
Each builtin type typically has:
```c
// type_name.h - Header with struct definition
typedef struct {
    // ... internal representation
} ActonType;

// type_name.c - Implementation
// - Constructor functions
// - Method implementations
// - Protocol implementations
// - Memory management
```

### Core Types

#### Numeric Types
- `int.c/h` - Arbitrary precision integers
- `float.c/h` - 64-bit floating point
- `complex.c/h` - Complex numbers
- `i16.c/h`, `i32.c/h`, `i64.c/h` - Fixed-size integers
- `u16.c/h`, `u32.c/h`, `u64.c/h` - Unsigned integers

#### Collections
- `list.c/h` - Dynamic arrays
- `dict.c/h` - Hash maps
- `set.c/h` - Hash sets
- `tuple.c/h` - Immutable sequences

#### Text and Binary
- `str.c/h` - Unicode strings
- `bytes.c/h` - Binary data

### Memory Management
All builtin types follow Acton's memory model:
- Reference counting with cycle detection
- Automatic memory management
- C integration points

## Runtime System (`rts/`)

### Core Components

#### `rts.c/h`
- Main runtime initialization
- Actor system setup
- Scheduler initialization

#### `q.c/h`
- Actor message queues
- Lock-free implementations
- Message routing

#### `io.c/h`
- Async I/O with libuv
- File descriptors
- Network operations

#### `gc.zig`
- Garbage collector
- Reference counting
- Cycle detection

### Actor Runtime
- Lightweight actors
- Message passing
- Scheduling
- Distribution support

## Adding Standard Library Modules

### 1. Pure Acton Module
```acton
# src/mymodule.act
def my_function(x: int) -> int:
    return x * 2

class MyClass:
    def method(self) -> str:
        return "hello"
```

### 2. Module with C Extension
```acton
# src/mymodule.act
# C functions are declared as external
def native_function(x: int) -> int:
    NotImplemented
```

```c
// src/mymodule.ext.c
#include <acton.h>

B_int native_function(B_int x) {
    return intFromInt(asInt(x) * 2);
}
```

### 3. Update Configuration
```toml
# base/Acton.toml
[dependencies]
mymodule = { path = "src/mymodule.act" }
```

## Working with Builtins

### Adding Methods to Builtin Types

1. **Declare in Acton** (`__builtin__.act`):
```acton
extension list[T]:
    def my_new_method(self) -> T:
        NotImplemented
```

2. **Implement in C** (`list.c`):
```c
B_value list_my_new_method(B_list self) {
    // Implementation
    return result;
}
```

3. **Register Method** (`list.c`):
```c
static struct method_entry list_methods[] = {
    {"my_new_method", (void*)list_my_new_method},
    // ...
};
```

### Memory Management in C Extensions

```c
// Allocate Acton object
B_str result = (B_str)GC_MALLOC(sizeof(struct B_str));
result->_class = &str_class;

// Reference counting
INCREF(obj);  // Increase reference
DECREF(obj);  // Decrease reference

// Conversion helpers
int64_t val = asInt(acton_int);
B_int result = intFromInt(val);
```

## Testing

### Unit Tests
- Place in `test/stdlib_auto/`
- Naming: `test_module.act`
- Auto-discovered by test runner

### C Extension Tests
- Test through Acton interface
- Use `testing` module assertions
- Verify memory management

### Performance Tests
- Use `test/perf/` for benchmarks
- Compare against Python/C baselines
- Track regression

## Common Patterns

### Async Operations
```c
// In .ext.c file
void async_operation(B_actor self, B_callback cb) {
    // Start async work
    uv_work_t *req = malloc(sizeof(uv_work_t));
    req->data = cb;
    
    uv_queue_work(uv_default_loop(), req, 
                  do_work, after_work);
}
```

### Error Handling
```c
// Throw Acton exception from C
if (error_condition) {
    B_raise(B_ValueError, "Error message");
    return NULL;
}
```

### Protocol Implementation
```c
// Implement protocol for type
static B_bool str_eq(B_str self, B_value other) {
    if (other->_class != &str_class) 
        return B_False;
    return strcmp(self->data, ((B_str)other)->data) == 0 
           ? B_True : B_False;
}
```

## Performance Guidelines

1. **String Operations**
   - Use views when possible
   - Avoid unnecessary copies
   - Cache length calculations

2. **Collections**
   - Preallocate when size known
   - Use appropriate data structure
   - Consider memory locality

3. **I/O Operations**
   - Always async in actors
   - Buffer when appropriate
   - Use libuv efficiently

## Debugging

### Print Debugging
```c
// In C extensions
fprintf(stderr, "Debug: value=%ld\n", asInt(val));

// Force flush
fflush(stderr);
```

### GDB
```bash
# Run with GDB
gdb ./program
(gdb) break function_name
(gdb) run
```

### Memory Debugging
- Use Valgrind for leaks
- Enable GC debugging
- Check reference counts

## Integration Notes

- Standard library available to all Acton programs
- Modules lazy-loaded on import
- C extensions loaded dynamically
- Type information preserved at runtime