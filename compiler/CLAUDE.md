# Acton Compiler - Development Guide

The Acton compiler (`actonc`) is written in Haskell and transforms Acton source code into C code that can be compiled and linked with the runtime system.

## Quick Reference

### Build & Test
```bash
# Build just the compiler (fastest iteration)
make dist/bin/actonc

# Run compiler tests
make test-compiler

# Run specific test suite
cd compiler && stack test --test-arguments "--filter pattern"

# Build with profiling
cd compiler && stack build --profile
```

### Key Directories
```
compiler/
├── lib/                    # Main compiler library
│   ├── src/Acton/         # Core compiler modules
│   └── test/              # Compiler unit tests
├── actonc/                # Compiler executable
│   ├── Main.hs           # Entry point
│   └── test/             # Golden tests
└── lsp-server/           # Language server
```

## Compilation Pipeline

The compiler transforms Acton code through multiple stages:

```
Source Code (.act)
    ↓ Parser
AST (Abstract Syntax Tree)
    ↓ Type Checker
Typed AST
    ↓ Normalizer
Normalized AST
    ↓ Deactorizer
Deactorized AST
    ↓ CPS Transform
CPS Form
    ↓ Lambda Lifter
Lifted Form
    ↓ Boxing
Boxed Form
    ↓ Code Generator
C Code (.c, .h)
```

## Key Modules

### Core Pipeline (`compiler/lib/src/Acton/`)

#### Parser.hs
- Megaparsec-based parser
- Produces AST from source code
- Handles Python-like syntax with Acton extensions
- Entry point: `parseModule`

#### Types.hs & TypeEnv.hs
- Type system definitions
- Type inference engine
- Constraint solving
- Key types: `Type`, `Scheme`, `TyEnv`

#### Normalizer.hs
- Simplifies AST for easier processing
- Desugars complex expressions
- Handles pattern matching expansion

#### Deactorizer.hs
- Transforms actor methods into regular functions
- Converts actor state into explicit parameters
- Handles message passing semantics

#### CPS.hs
- Continuation-Passing Style transformation
- Makes control flow explicit
- Prepares for async operations

#### LambdaLifter.hs
- Lifts nested functions to top level
- Closure conversion
- Prepares for C code generation

#### Boxing.hs
- Converts between boxed/unboxed representations
- Handles primitive type optimizations

#### CodeGen.hs
- Generates C code from final AST
- Produces `.c` and `.h` files
- Integrates with runtime system

### Supporting Modules

#### Env.hs
- Environment management
- Module system handling
- Import resolution

#### Diagnostics.hs
- Error message generation
- Source location tracking
- Pretty printing errors

#### QuickType.hs
- Fast type checking for IDE support
- Incremental type inference

## Working with the Compiler

### Adding a New Language Feature

1. **Update Parser** (`Parser.hs`)
   - Add new syntax rules
   - Update AST types in `Syntax.hs`

2. **Update Type System** (`Types.hs`)
   - Add type rules for new construct
   - Update type inference

3. **Update Normalizer** (`Normalizer.hs`)
   - Add normalization rules
   - Ensure simplified form

4. **Update Code Generation** (`CodeGen.hs`)
   - Generate appropriate C code
   - Link with runtime if needed

5. **Add Tests**
   - Parser tests in `lib/test/`
   - Type error tests in `actonc/test/typeerrors/`
   - Golden tests for code generation

### Debugging the Compiler

```haskell
-- Add debug prints in any module
import Debug.Trace

-- In your code
traceShow ("Debug info", someValue) $ restOfExpression

-- Or use the built-in pretty printer
import Pretty
traceShow (renderDoc $ pp someAst) $ ...
```

### Error Messages

Error messages are crucial for user experience. When adding new errors:

1. Create descriptive error in `Diagnostics.hs`
2. Add golden test in `actonc/test/typeerrors/`
3. Include:
   - Clear description of the problem
   - Source location
   - Suggested fix if possible

Example:
```haskell
throwError $ TypeError loc $ 
  "Cannot unify types" <+> pp ty1 <+> "and" <+> pp ty2
```

## Type System Details

### Type Inference
- Hindley-Milner with extensions
- Row polymorphism for records
- Subtyping for actors
- Protocol constraints

### Special Types
- `World` - Represents side effects
- `Msg` - Actor message types
- `Cap` - Capability types
- `Ref` - Reference types

## Testing Strategy

### Unit Tests (`lib/test/`)
- Test individual compiler phases
- Use sydtest framework
- Fast, focused tests

### Golden Tests (`actonc/test/`)
- Compare compiler output against expected
- Syntax errors: `syntaxerrors/`
- Type errors: `typeerrors/`
- Update with: `--accept` flag

### Integration Tests
- Full compilation tests in `test/`
- Ensure generated C code works

## Common Tasks

### Adding a Builtin Function
1. Add signature in `Builtin.hs`
2. Add type in builtin environment
3. Generate C call in `CodeGen.hs`

### Improving Error Messages
1. Identify error location in type checker
2. Add case in `Diagnostics.hs`
3. Create golden test
4. Iterate on message clarity

### Optimizing Compilation
1. Profile with `stack build --profile`
2. Run with `+RTS -p`
3. Check `.prof` file
4. Focus on hot paths in type checker

## Haskell Style Guide

```haskell
-- Module header
module Acton.Parser
  ( parseModule
  , parseExpression
  , ParseError
  ) where

-- Explicit imports
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- Type signatures for all top-level functions
parseModule :: T.Text -> Either ParseError Module
parseModule input = ...

-- Use where clauses for clarity
processAst :: Ast -> Result
processAst ast = process initialEnv ast
  where
    initialEnv = mkEnv
    process env node = ...

-- Pattern match explicitly
compile :: Source -> Either Error Output
compile (Source file content) = do
  ast <- parse content
  typed <- typeCheck ast
  generateCode typed
```

### Whiteboard Layout Style

The Acton compiler uses a distinctive "whiteboard layout" style in many places, especially in the type system and constraint solving code. This style uses very deep indentation to create mathematical equation-like layouts:

```haskell
-- Function signatures and implementations aligned far from left margin
infTop                                  :: Env -> Suite -> TypeM (TEnv,Suite)
infTop env ss                           = do -- Implementation starts at column 41+
                                             pushFX fxPure tNone
                                             (cs,te,ss) <- infSuiteEnv env ss
                                             ss <- msubst ss
                                             return (te, ss)

-- Pattern matching with deep alignment
infEnv env (For l p e b els)
  | nodup p                             = do -- Guard at column 41
                                             (cs1,te,t1,p') <- infEnvT env p
                                             t2 <- newTVar
                                             return (cs1++cs2, [], result)

-- Where clauses with mathematical alignment
addTyping env n s t c                   = c {info = addT n (simp env s) t (info c)}
    where addT n s t (DfltInfo l m mbe ts)
                                        = DfltInfo l m mbe ((n,s,t):ts)
                                        -- Continuation aligned for visual clarity
```

**Key characteristics of whiteboard layout:**
1. **Deep indentation** (often columns 40-50) for do-blocks and expressions
2. **Vertical alignment** of related terms for easy visual comparison
3. **Mathematical appearance** - equations and expressions laid out like on a whiteboard
4. **Structural emphasis** - the layout highlights the logical structure of complex expressions
5. **Common in type inference** - especially prevalent in Types.hs, TypeEnv.hs, and constraint solving code

This style helps when working with complex mathematical algorithms by making the structure visually apparent, similar to how one would write equations on a whiteboard with careful spacing and alignment.

## Performance Considerations

1. **Parser Performance**
   - Use strict Text, not String
   - Careful with backtracking

2. **Type Checker Performance**
   - Cache type environments
   - Minimize constraint solving

3. **Code Generation**
   - Stream output when possible
   - Batch C file writes

## Integration with Build System

The compiler is invoked by:
- Zig build system for Acton projects
- CLI tool for user commands
- Test harness for testing

Key interfaces:
- Command line arguments
- JSON output for errors
- File system conventions