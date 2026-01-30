# Acton Compiler - Development Guide

The Acton compiler (`acton`) is written in Haskell and compiled Acton source code into C code that can be compiled and linked with the runtime system.

## Quick Reference

### Build & Test
```bash
# Build just the compiler (fastest iteration)
make dist/bin/acton

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
├── acton/                 # Compiler executable
│   ├── Main.hs           # Entry point
│   └── test/             # Snapshot tests
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
   - Type error tests in `acton/test/typeerrors/`
   - Snapshot tests for code generation

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

### Testing Documentation Generation

Documentation generation tests are located in `test/test_doc_printing/`. This directory contains various test files demonstrating different documentation scenarios.

To test documentation generation:
```bash
acton doc src/basics.act

# Whole project
acton doc
```

### Error Messages

Error messages are crucial for user experience. When adding new errors:

1. Create descriptive error in `Diagnostics.hs`
2. Add snapshot test in `acton/test/typeerrors/`
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

### Snapshot Tests (`acton/test/`)
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
3. Create snapshot test
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

## Acton Compiler Style Guide

These patterns represent the ideal coding style for the Acton compiler. These principles apply broadly across the entire codebase and should be followed in all contributions:

### 1. **Simplify and Remove Dead Code**
```haskell
-- BAD: Keep unused data types, functions, or complex abstractions
data CompInfo = CompInfo Expr | CompWithWit Expr Expr  -- Dead code
normalize' env x = normalize env x  -- Unnecessary wrapper

-- GOOD: Keep only what's needed
type NormM a = State (Int,[(Name,PosPar,Expr)]) a
normalize env x = ...  -- Direct implementation
```

### 2. **Separate Concerns Clearly**
```haskell
-- BAD: Mix different responsibilities in one function
processExpr env expr = 
  case expr of
    Add x y -> let tx = typeOf x
                   ty = typeOf y
               in if tx == ty then ... else error ...
    Mul x y -> ...  -- Similar mixed logic

-- GOOD: Separate type checking from transformation
typeCheck env expr = ...  -- Just type checking
transform env expr = ...  -- Just transformation

-- Or factor out common patterns
processExpr env expr = transform env (typeCheck env expr)
```

### 3. **Use Generic Abstractions Over Ad-Hoc Solutions**
```haskell
-- BAD: Create specific functions for each use case
handleListError :: Error -> String
handleDictError :: Error -> String
handleSetError :: Error -> String

-- GOOD: One generic function that handles all cases
handleError :: Error -> String
handleError (TypeError t) = "Type error: " ++ show t
handleError (SyntaxError s) = "Syntax error: " ++ s
```

### 4. **Consistent Naming and Alignment**
```haskell
-- Use consistent variable names across the codebase:
-- env  - Environment
-- t    - Type
-- e    - Expression  
-- p    - Pattern or Parameter
-- n    - Name
-- l    - Location
-- s    - Statement or String
-- c    - Constraint or Context
-- w    - Witness
-- r    - Result or Row

-- Align related definitions for visual clarity
tInt              = tCon (TC qnInt [])
tFloat            = tCon (TC qnFloat [])
tString           = tCon (TC qnString [])
tBool             = tCon (TC qnBool [])
```

### 5. **Factor Out Common Patterns**
```haskell
-- BAD: Repeat similar code in multiple places
infer env (Add e1 e2) = do
  (cs1, t1, e1') <- infer env e1
  (cs2, t2, e2') <- infer env e2
  t <- newTVar
  return (cs1 ++ cs2 ++ [Cast t1 t, Cast t2 t], t, Add e1' e2')
  
infer env (Mul e1 e2) = do
  (cs1, t1, e1') <- infer env e1
  (cs2, t2, e2') <- infer env e2
  t <- newTVar
  return (cs1 ++ cs2 ++ [Cast t1 t, Cast t2 t], t, Mul e1' e2')

-- GOOD: Extract common binary operation pattern
inferBinOp env op e1 e2 = do
  (cs1, t1, e1') <- infer env e1
  (cs2, t2, e2') <- infer env e2
  t <- newTVar
  return (cs1 ++ cs2 ++ [Cast t1 t, Cast t2 t], t, op e1' e2')
```

### 6. **Clear Data Flow with Where Clauses**
```haskell
-- Structure where clauses to show data dependencies clearly
processExpr env expr = finalResult
  where 
    -- 1. Extract/compute base values
    baseType = typeOf env expr
    location = loc expr
    
    -- 2. Build intermediate results  
    constraints = gatherConstraints baseType
    simplified = simplify expr
    
    -- 3. Construct final result
    finalResult = Result simplified constraints location
    
-- Each binding depends only on earlier bindings
-- Clear progression from inputs to outputs
```

### 7. **Handle Each Case in Its Own Context**
```haskell
-- BAD: Share state across unrelated computations
processAll items = do
  state <- initState
  mapM (process state) items  -- Shared mutable state

-- GOOD: Each computation gets fresh context
processAll items = mapM processOne items
  where 
    processOne item = do
      state <- initState  -- Fresh state for each
      process state item
```

### 8. **Prefer Explicit Over Implicit**
```haskell
-- BAD: Rely on implicit behavior
findThing xs = head xs  -- Crashes on empty list

-- GOOD: Make expectations explicit
findThing xs = case xs of
  []    -> error "findThing: empty list"
  (x:_) -> x
  
-- Or better, use Maybe
findThing :: [a] -> Maybe a
findThing []    = Nothing
findThing (x:_) = Just x
```

### Key Principle: These Patterns Apply Everywhere

The above patterns aren't specific to any particular feature or module. They represent the ideal coding style throughout:
- **Parser**: Clean separation of lexing/parsing concerns
- **Type Checker**: Generic constraint handling, clear error messages
- **Normalizer**: Simple transformations, no unnecessary complexity
- **Code Generator**: Explicit mappings, clear output structure
- **All modules**: Consistent naming, factored patterns, clean data flow

When in doubt, look at recent high-quality commits in the repository for examples of how to refactor code to match these ideals.

## AI-Assisted Development Process

When working with AI assistants on the Acton compiler, follow this structured approach:

### 1. **Problem Discussion Phase**
- Thoroughly discuss the problem and solution requirements
- Identify affected compiler phases
- Consider edge cases and interactions with existing features

### 2. **Test-Driven Development**
```haskell
-- AI writes test cases based on requirements
-- Human reviews and approves test suite
-- Example: test/comprehensions/generic_test.act
def test_generic_list_comp():
    result = [x * 2 for x in [1, 2, 3]]
    testing.assertEqual(result, [2, 4, 6])
```

### 3. **Implementation Phase**
- AI implements code to pass tests
- Iteratively compile and run tests
- Fix errors until all tests pass

### 4. **Code Review and Cleanup Phase** ⚠️ **CRITICAL**
After getting tests to pass, AI MUST perform a holistic code review:

#### Checklist for AI Code Review:
- [ ] **Remove dead code** - Delete unused functions, data types, imports
- [ ] **Simplify complex patterns** - Look for opportunities to factor out common code
- [ ] **Check naming consistency** - Ensure variables and functions follow conventions
- [ ] **Verify error handling** - Proper error messages with source locations
- [ ] **Review data flow** - Ensure clean separation of concerns
- [ ] **Check integration** - Verify the change fits well with existing code
- [ ] **Update related code** - Ensure all affected areas are updated

#### Example Cleanup Pattern:
```haskell
-- BEFORE: Quick fix to pass tests
addCompWithWit :: (Name,PosPar,Expr,Expr) -> NormM ()
addComp :: (Name,PosPar,Expr) -> NormM ()
-- Two similar functions, ad-hoc solution

-- AFTER: Unified clean design
addComp :: (Name,PosPar,Expr) -> NormM ()
-- Single function, witness handled through annotation
```

### 5. **Final Review Before Commit**
- Ensure code follows the Acton compiler style guide
- Verify no regression in existing functionality
- Check that the solution is elegant and maintainable
- Confirm error messages are helpful

## Common Pitfalls to Avoid

1. **Tunnel Vision**: Don't focus only on making tests pass
2. **Ad-hoc Solutions**: Avoid quick fixes that complicate the codebase
3. **Inconsistent Patterns**: Match existing code style and patterns
4. **Missing Edge Cases**: Consider all variants (e.g., nested comprehensions)
5. **Poor Error Messages**: Always include helpful context in errors

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
