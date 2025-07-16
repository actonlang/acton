{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (toLower)


import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import qualified Acton.Printer as AP
import qualified Acton.DocPrinter as DocP
import qualified Acton.Env
import qualified Acton.Kinds
import qualified Acton.Types
import qualified Acton.Normalizer
import qualified Acton.Deactorizer
import qualified Acton.CPS
import qualified Acton.LambdaLifter
import qualified Acton.Boxing
import qualified Acton.CodeGen
import qualified Acton.Diagnostics as Diag
import Pretty (print)
import Test.Syd
import Test.Syd.Def.Golden (goldenTextFile)
import qualified Control.Monad.Trans.State.Strict as St
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Error.Diagnose (printDiagnostic, prettyDiagnostic, WithUnicode(..), TabSize(..), defaultStyle)
import Prettyprinter (unAnnotate, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>), joinPath, takeFileName, takeBaseName, takeDirectory, splitDirectories)
import System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing)
import Control.Monad (forM_, when, foldM)
import qualified Control.Exception as E
import Utils (SrcLoc(..))
import qualified System.IO.Unsafe




main :: IO ()
main = do
  let sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"
  env0 <- Acton.Env.initEnv sysTypesPath False

  sydTest $ do
    describe "Pass 1: Parser" $ do

      describe "Basic Syntax" $ do
        testParse env0 "syntax1"

      describe "Docstrings" $ do
        testParse env0 "docstrings"
        testDocstrings env0 "docstrings"

      describe "Module Structure" $ do
        describe "Module-level docstrings" $ do
          it "allows single-line docstring before import" $ do
            let input = "\"\"\"Module docstring\"\"\"\nimport math\n"
            case parseModuleTest input of
              Left err -> expectationFailure $ "Parse failed: " ++ err
              Right _ -> return ()

          it "allows multi-line docstring before import" $ do
            let input = "\"\"\"Module docstring\nwith multiple lines\"\"\"\nimport math\n"
            case parseModuleTest input of
              Left err -> expectationFailure $ "Parse failed: " ++ err
              Right _ -> return ()

        describe "Invalid module structure (before imports)" $ do
          testModuleParseError "module_var_before_import" "x = 42\nimport math\n"
          testModuleParseError "module_call_before_import" "print(\"hello\")\nimport math\n"
          testModuleParseError "module_number_before_import" "42\nimport math\n"
          testModuleParseError "module_list_before_import" "[1, 2, 3]\nimport math\n"
          testModuleParseError "module_dict_before_import" "{\"key\": \"value\"}\nimport math\n"
          testModuleParseError "module_if_before_import" "if True:\n    pass\nimport math\n"
          testModuleParseError "module_for_before_import" "for i in range(10):\n    pass\nimport math\n"
          testModuleParseError "module_class_before_import" "class Foo:\n    pass\nimport math\n"
          testModuleParseError "module_func_before_import" "def foo():\n    pass\nimport math\n"
          testModuleParseError "module_actor_before_import" "actor Foo():\n    pass\nimport math\n"

      describe "String Interpolation" $ do
        -- Note: In these tests, we use Haskell string literals which require escaping.
        -- The test format is: testParseOutput <Haskell literal> <expected parser output>
        --
        -- Examples of what the Haskell escapes mean in actual Acton code:
        --   Haskell literal: "\"\""           → Acton code: ""
        --   Haskell literal: "\"hello\""      → Acton code: "hello"
        --   Haskell literal: "'world'"        → Acton code: 'world'
        --   Haskell literal: "f\"x = {x}\""   → Acton code: f"x = {x}"
        --   Haskell literal: "\"x = {x}\""    → Acton code: "x = {x}"
        --
        -- Interpolation is a core feature in standard Acton strings.
        -- Both "string {expr}" and f"string {expr}" support interpolation

        -- Basic functionality (simplest to more complex)
        describe "Basic string literals" $ do
          testParseOutput "\"\"" "\"\""
          testParseOutput "\"plain text\"" "\"plain text\""
          testParseOutput "'hello world'" "\"hello world\""  -- single quotes convert to double

        describe "Basic interpolation" $ do
          testParseOutput "\"hello {name}\"" "\"hello %s\" % str(name)"  -- default interpolation
          testParseOutput "f\"hello {name}!\"" "\"hello %s!\" % str(name)"  -- with f-prefix
          testParseOutput "'Value: {x}'" "\"Value: %s\" % str(x)"  -- single quotes work too

        describe "Multiple interpolations" $ do
          testParseOutput "f\"Hello {name}, your score is {score}\"" "\"Hello %s, your score is %s\" % (str(name), str(score))"
          testParseOutput "\"First: {a}, Second: {b}, Third: {c}\"" "\"First: %s, Second: %s, Third: %s\" % (str(a), str(b), str(c))"

        describe "Expressions in interpolation" $ do
          testParseOutput "\"Sum: {a + b}\"" "\"Sum: %s\" % str(a + b)"  -- simple expression
          testParseOutput "f\"Calculation: {a * b // 2}\"" "\"Calculation: %s\" % str(a * b // 2)"  -- complex expression
          testParseOutput "f\"Result: {func({a: b})}\"" "\"Result: %s\" % str(func({a: b}))"  -- nested braces

        describe "Formatting specifications" $ do
          -- Width
          testParseOutput "f\"{num:5}\"" "\"%5s\" % str(num)"
          testParseOutput "f\"{num:10}\"" "\"%10s\" % str(num)"

          -- Alignment
          testParseOutput "f\"{name:<9}\"" "\"%-9s\" % str(name)"           -- left
          testParseOutput "f\"{name:>10}\"" "\"%10s\" % str(name)"         -- right
          testParseOutput "f\"{name:^10}\"" "\"%s\" % str(name).center(10)" -- center

          -- Zero padding
          testParseOutput "f\"{num:05}\"" "\"%05d\" % num"
          testParseOutput "f\"{num:010}\"" "\"%010d\" % num"

          -- Floating point precision
          testParseOutput "f\"{pi:.2f}\"" "\"%.2f\" % pi"
          testParseOutput "f\"{pi:.4f}\"" "\"%.4f\" % pi"
          testParseOutput "f\"{pi:.0f}\"" "\"%.0f\" % pi"
          testParseOutput "f\"{pi:10.2f}\"" "\"%10.2f\" % pi"

          -- Combined formatting
          testParseOutput "f\"{a:>10}:{b:^10}:{c:<10}\"" "\"%10s:%s:%-10s\" % (str(a), str(b).center(10), str(c))"
          testParseOutput "f\"{num:+08.2f}\"" "\"%08.2f\" % num"

          -- Spaces in format spec (various positions allowed)
          testParseOutput "f\"{ num : 10 }\"" "\"%10s\" % str(num)"  -- spaces everywhere

        describe "String quote variations" $ do
          -- Triple quotes support interpolation by default
          testParseOutput "\"\"\"hello {name}!\"\"\"" "\"hello %s!\" % str(name)"  -- triple double quotes
          testParseOutput "'''multi-line\n{value}'''" "\"multi-line\\\\n%s\" % str(value)"  -- triple single quotes
          testParseOutput "f\"\"\"Name: {name}\nAge: {age}\"\"\"" "\"Name: %s\\\\nAge: %s\" % (str(name), str(age))"  -- f-prefix with multi-line
          testParseOutput "\"\"\"Plain text without interpolation\"\"\"" "\"Plain text without interpolation\""  -- no braces = no interpolation

        describe "Special characters and escaping" $ do
          -- Escaped braces
          testParseOutput "f\"something but {{{substituted}}}\"" "\"something but {%s}\" % str(substituted)"

          -- Escaped quotes
          testParseOutput "f\"hello \\\"thing\\\"\"" "\"hello \\\"thing\\\"\""
          testParseOutput "f'hello \\'thing\\''" "\"hello 'thing'\""
          testParseOutput "f\"Value: {x} with \\\"quotes\\\"\"" "\"Value: %s with \\\"quotes\\\"\" % str(x)"

          -- Unicode
          testParseOutput "f\"Hello, {name}! 你好!\"" "\"Hello, %s! \\20320\\22909!\" % str(name)"

        describe "Slice expressions in interpolation" $ do
          -- Basic slice patterns
          testParseOutput "\"slice: {arr[1:3]}\"" "\"slice: %s\" % str(arr[1:3])"  -- basic range
          testParseOutput "\"slice: {arr[:5]}\"" "\"slice: %s\" % str(arr[:5])"  -- from start
          testParseOutput "\"slice: {arr[2:]}\"" "\"slice: %s\" % str(arr[2:])"  -- to end

          -- Step parameter
          testParseOutput "\"step: {arr[1:10:2]}\"" "\"step: %s\" % str(arr[1:10:2])"  -- with step
          testParseOutput "\"reverse: {arr[::-1]}\"" "\"reverse: %s\" % str(arr[::-1])"  -- negative step

          -- Complex expressions
          testParseOutput "\"complex: {arr[i+1:j*2]}\"" "\"complex: %s\" % str(arr[i + 1:j * 2])"  -- expressions in slice
          testParseOutput "\"nested: {matrix[i][j:k]}\"" "\"nested: %s\" % str(matrix[i][j:k])"  -- nested indexing

        describe "Nested string interpolation" $ do
          testParseOutput "\"outer {\"inner {x}\"}\"" "\"outer %s\" % str(\"inner %s\" % str(x))"
          testParseOutput "\"outer {'inner {x}'}\"" "\"outer %s\" % str(\"inner %s\" % str(x))"
          testParseOutput "'outer {\"inner {x}\"}'" "\"outer %s\" % str(\"inner %s\" % str(x))"
          testParseOutput "\"level1 {\"level2 {\"level3 {x}\"}\"}\""  "\"level1 %s\" % str(\"level2 %s\" % str(\"level3 %s\" % str(x)))"
          testParseOutput "\"outer {{literal}} {\"inner {x}\"}\"" "\"outer {literal} %s\" % str(\"inner %s\" % str(x))"

        describe "Escape sequences" $ do
          -- Standard escapes (preserved during parsing)
          testParseOutput "\"\\n\\t\\r\"" "\"\\\\n\\\\t\\\\r\""

          -- Hex escapes (note splitting behavior when followed by hex chars)
          testParseOutput "\"\\x48ello\"" "\"\\\\x48\" \"ello\""  -- splits to prevent C reading too many hex digits
          testParseOutput "\"\\x41BC\"" "\"\\\\x41\" \"BC\""

          -- Unicode escapes
          testParseOutput "\"\\u0041\"" "\"\\\\u0041\""  -- 4-digit unicode
          testParseOutput "\"\\U00000041\"" "\"\\\\U00000041\""  -- 8-digit unicode

          -- Octal escapes
          testParseOutput "\"\\123\"" "\"\\\\123\""  -- 3-digit octal
          testParseOutput "\"\\7\"" "\"\\\\7\""  -- 1-digit octal

          -- Mixed with interpolation
          testParseOutput "f\"Hello \\n{name}\\t!\"" "\"Hello \\\\n%s\\\\t!\" % str(name)"

      -- ==== OTHER STRING LITERAL TESTS ====
      describe "Other String Literals" $ do

        describe "Raw strings (no interpolation)" $ do
          testParseOutput "r\"hello {world}\"" "\"hello {world}\""
          testParseOutput "r'test {x} value'" "\"test {x} value\""
          testParseOutput "r\"\"\"multi\nline {no} interpolation\"\"\"" "\"multi\\\\nline {no} interpolation\""
          testParseOutput "r\"path\\to\\file\"" "\"path\\\\\\\\to\\\\\\\\file\""
          testParseOutput "r\"regex: \\x[0-9a-f]+\"" "\"regex: \\\\\\\\x[0-9a-f]+\""
          testParseOutput "r\"test \\n \\t \\r\"" "\"test \\\\\\\\n \\\\\\\\t \\\\\\\\r\""

        describe "Bytes literals" $ do
          -- Basic bytes
          testParseOutput "b\"hello\"" " bhello"
          testParseOutput "b\"\"\"multi\nline\"\"\"" " bmulti\\nline"

          -- Bytes with hex escapes (note splitting behavior)
          testParseOutput "b\"\\x48ello\"" " b\\x48  bello"  -- splits hex from following text
          testParseOutput "b\"\\x48\\x65llo\"" " b\\x48\\x65llo"  -- multiple hex escapes

          -- Other escapes in bytes
          testParseOutput "b\"Hello\\nWorld\"" " bHello\\nWorld"

        describe "Raw bytes literals" $ do
          testParseOutput "rb\"hello\"" " bhello"
          testParseOutput "rb'world'" " bworld"
          testParseOutput "rb\"\"\"multi\nline\"\"\"" " bmulti\\nline"

        describe "Legacy percent formatting (not interpolated)" $ do
          testParseOutput "\"hello %s\" % name" "\"hello %s\" % name"
          testParseOutput "\"Value: %d\" % count" "\"Value: %d\" % count"

      -- ==== ERROR HANDLING TESTS ====
      describe "String Parsing Errors" $ do
        describe "Basic string errors" $ do
          testModuleParseError "unclosed_string" "a = \"hello"
          testModuleParseError "unclosed_string_triple" "z = 1\na = \"\"\"hello\nb = 3\ndef foo():\n    pass"

        describe "Basic interpolation errors" $ do
          -- F-string errors
          testParseError "fstring_unclosed_brace" "f\"Unclosed brace: {name"
          testParseError "fstring_empty_expression" "f\"Empty expression {}\""
          testParseError "fstring_missing_expression" "f\"Missing expression {:10}\""
          testParseError "fstring_unbalanced_format" "f\"Unbalanced format {name:}:10}\""

          -- String interpolation errors (without f-prefix)
          testParseError "string_unclosed_brace" "\"Unclosed brace: {name"
          testParseError "tristring_unclosed_brace" "\"\"\"Unclosed brace: {name"
          testParseError "string_empty_expression" "\"Empty expression {}\""
          testParseError "string_missing_expression" "\"Missing expression {:10}\""

        describe "Format specification errors" $ do
          testParseError "fstring_invalid_format" "f\"Invalid format specifier {name:@Z}\""
          testParseError "fstring_invalid_alignment" "f\"{name:@10}\""
          testParseError "string_invalid_format" "\"{name:@Z}\""
          testParseError "fstring_empty_format_specifier" "f\"Empty format spec {x:}\""
          testParseError "fstring_missing_precision_digits_error" "f\"Missing precision digits {value:10.}\""
          testParseError "invalid_after_width" "\"value: {x:10@}\""
          testParseError "invalid_after_align" "\"value: {x:>10@}\""
          testParseError "invalid_after_precision" "\"value: {x:10.2@}\""
          testParseError "invalid_in_type_spec" "\"value: {x:10.2@f}\""

        describe "Nested interpolation errors" $ do
          testParseError "nested_unclosed_outer" "\"outer {\"inner"
          testParseError "nested_unclosed_inner" "\"outer {\"inner {x}\""
          testParseError "nested_empty_inner" "\"outer {\"inner {}\"}\""
          testParseError "nested_invalid_format" "\"outer {\"inner {x:@10}\"}\""
          testParseError "triple_nested_unclosed" "\"level1 {\"level2 {\"level3 {x"
          testParseError "mixed_quotes_unclosed" "\"outer {'inner {x"
          testParseError "escaped_brace_error" "\"outer {{broken {x}\""
          testParseError "nested_empty_format" "\"outer {\"inner {x:}\"}\""
          testParseError "nested_invalid_align" "\"outer {\"inner {x:@5}\"}\""
          testParseError "nested_bad_type_spec" "\"outer {\"inner {x:5@}\"}\""
          testParseError "deep_nesting_error" "\"a {\"b {\"c {\"d {\"e {f:@}\"}\"}\"}\"}\"}\""
          testParseError "alternating_quotes_error" "\"a {'b {\"c {'d {e'}\"}'}\""

        describe "Position-specific errors" $ do
          testParseError "error_at_start" "{x} at start"
          testParseError "error_at_end" "\"at end {x"
          testParseError "error_in_middle" "\"start {x:@} end\""
          testParseError "multiline_unclosed" "\"\"\"Line 1\nLine 2 {x\nLine 3\"\"\""
          testParseError "multiline_nested_error" "\"\"\"First line\n{\"inner\n  {broken}\n\"}\"\"\""
          testParseError "multiline_format_error" "\"\"\"\nValue: {x:@invalid}\nMore text\"\"\""
          testParseError "newline_in_expr" "\"value: {x\n}\""
          testParseError "tab_in_format" "\"value: {x:\t10}\""
          testParseError "unicode_format_error" "\"你好 {name:你}\""

        describe "Slice expression errors" $ do
          testParseError "slice_unclosed_bracket" "\"slice: {arr[1:3\""
          testParseError "slice_invalid_step" "\"slice: {arr[:::\""
          testParseError "slice_missing_bracket" "\"slice: {arr 1:3]}\""
          testParseError "slice_nested_error" "\"outer {arr[inner[}\""
          testParseError "slice_format_error" "\"slice: {arr[1:3]:@10}\""

      describe "Escape Sequence Errors" $ do

        describe "Hex escape errors" $ do
          testParseError "hex_incomplete_one_digit" "\"\\x4\""
          testParseError "hex_incomplete_no_digits" "\"\\x\""
          testParseError "hex_invalid_char" "\"\\xAG\""
          testParseError "hex_incomplete_in_interpolation" "f\"value: {x} and \\x4\""

        describe "Unicode escape errors" $ do
          testParseError "unicode_short_incomplete" "\"\\u123\""
          testParseError "unicode_short_no_digits" "\"\\u\""
          testParseError "unicode_short_invalid_char" "\"\\u123G\""
          testParseError "unicode_short_in_fstring" "f\"Hello \\u123\""
          testParseError "unicode_long_incomplete" "\"\\U1234567\""
          testParseError "unicode_long_no_digits" "\"\\U\""
          testParseError "unicode_long_invalid_char" "\"\\U1234567G\""
          testParseError "unicode_long_in_fstring" "f\"Hello \\U1234567\""

        describe "Octal escape errors" $ do
          testParseError "octal_out_of_range" "\"\\777\""
          testParseError "octal_invalid_first_digit" "\"\\8\""
          testParseError "octal_invalid_char" "\"\\12G\""
          testParseError "octal_out_of_range_in_fstring" "f\"value: {x} \\777\""

        describe "Unknown escape sequences" $ do
          testParseError "unknown_escape_p" "\"\\p\""
          testParseError "unknown_escape_z" "\"\\z\""
          testParseError "unknown_escape_in_fstring" "f\"unknown: \\q\""
          testParseError "unknown_escape_with_interpolation" "f\"value: {x} \\k\""

    describe "Documentation Generation" $ do
      testDocGen env0 ["bar", "foo"]

    describe "Pass 2: Kinds" $ do
      testKinds env0 "deact"

    describe "Pass 3: Types" $ do
      testTypes env0 "deact"
      testTypes env0 "test_discovery"

    describe "Pass 4: Normalizer" $ do
      testNorm env0 "deact"

    describe "Pass 5: Deactorizer" $ do
      testDeact env0 "deact"

    describe "Pass 6: CPS" $ do
      testCps env0 "cps_volatiles"

    describe "Pass 7: Lambda Lifting" $ do
      testLL env0 "deact"

    describe "Pass 8: Boxing" $ do
      testBoxing env0 "deact"

    describe "Pass 9: CodeGen" $ do
      testCodeGen env0 "deact"




parseActon :: String -> Either String String
parseActon input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.stmt P.initState) "" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right result -> Right $ concatMap (Pretty.print) result)
      handleFailFastError
  where
    inputWithNewline = if last input == '\n' then input else input ++ "\n"
    handleFailFastError :: P.FailFastError -> IO (Either String String)
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleFailFastError (P.FailFastError loc msg) =
      return $ Left $ formatFailFastError loc msg

    formatFailFastError :: SrcLoc -> String -> String
    formatFailFastError loc msg =
      let diagnostic = Diag.errorDiagnosticWithLoc "Syntax error" "test" inputWithNewline loc msg
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout

    offsetToLineCol :: Int -> String -> (Int, Int)
    offsetToLineCol offset s =
      let before = take offset s
          lineNum = length (filter (== '\n') before) + 1
          colNum = case reverse before of
                     [] -> 1
                     (c:cs) -> length (takeWhile (/= '\n') (c:cs)) + 1
      in (lineNum, colNum)

-- Helper function to parse a full module (for testing module-level constructs)
parseModuleTest :: String -> Either String String
parseModuleTest input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.file_input P.initState) "test.act" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right (imports, suite) -> Right $ "Module parsed successfully")
      handleFailFastError
  where
    inputWithNewline = if null input || last input == '\n' then input else input ++ "\n"
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test.act" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleFailFastError :: P.FailFastError -> IO (Either String String)
    handleFailFastError (P.FailFastError loc msg) =
      return $ Left $ formatFailFastError loc msg

    formatFailFastError :: SrcLoc -> String -> String
    formatFailFastError loc msg =
      let diagnostic = Diag.errorDiagnosticWithLoc "Parse error" "test.act" inputWithNewline loc msg
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout

-- Helper function to test module-level parser errors with golden files
testModuleParseError :: String -> String -> Spec
testModuleParseError testName input = do
  it testName $ do
    case parseModuleTest input of
      Left err -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "ERROR: " ++ err
      Right result -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "PARSED: " ++ result

-- Helper function to test parsing (just that it succeeds)
testParse env0 testname = do
  let dir = "test" </> "1-parse"

  (env, parsed) <- parseAct env0 testname

  genTests "Parse Check" dir testname parsed parsed

-- Helper function to test parsing with output validation
testParseOutput :: String -> String -> Spec
testParseOutput input expected = do
  it (show input) $ do
    case parseActon input of
      Left err -> expectationFailure $ "Parse failed: " ++ err
      Right output -> output `shouldBe` expected

-- Helper function to test parser errors with golden files
testParseError :: String -> String -> Spec
testParseError testName input = do
  it testName $ do
    case parseActon input of
      Left err -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "ERROR: " ++ err
      Right result -> goldenTextFile ("test/parser_golden/" ++ testName ++ ".golden") $
        return $ T.pack $ "PARSED: " ++ result

-- Helper function for documentation golden tests
-- Takes a list of module paths in dependency order
-- Examples: ["bar", "foo"] or ["utils/math", "utils/strings", "app"]
-- Generates golden files for all formats: .txt (ASCII), .md (Markdown), .html (HTML)
testDocGen :: Acton.Env.Env0 -> [String] -> Spec
testDocGen env0 modulePaths = do
  oldDir <- runIO getCurrentDirectory
  let goldenDir = oldDir </> "test" </> "doc-golden"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule moduleTypeEnv moduleDoc = nmod
          let newAccEnv = Acton.Env.addMod (S.modname parsed) moduleTypeEnv moduleDoc accEnv
          return (newAccEnv, accModules ++ [((takeFileName modulePath), parsed, nmod)])

    (finalEnv, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  -- Generate documentation for each module
  forM_ modules $ \(modName, parsed, nmod) -> do
    describe modName $ do
      it "generates ASCII documentation (plain)" $ do
        let asciiDoc = DocP.printAsciiDoc False nmod parsed
        goldenTextFile (goldenDir </> modName ++ ".txt") $ return $ T.pack asciiDoc

      it "generates ASCII documentation (styled)" $ do
        let asciiDoc = DocP.printAsciiDoc True nmod parsed
        goldenTextFile (goldenDir </> modName ++ "-color.txt") $ return $ T.pack asciiDoc

      it "generates Markdown documentation" $ do
        let mdDoc = DocP.printMdDoc nmod parsed
        goldenTextFile (goldenDir </> modName ++ ".md") $ return $ T.pack mdDoc

      it "generates HTML documentation" $ do
        let htmlDoc = DocP.printHtmlDoc nmod parsed
        goldenTextFile (goldenDir </> modName ++ ".html") $ return $ T.pack htmlDoc

-- Parse an Acton module by module path
-- Examples: "foo" -> src/foo.act, module foo
--           "foo/bar" -> src/foo/bar.act, module foo.bar
parseAct env0 modulePath = do
  let moduleComponents = splitDirectories modulePath
      moduleName = S.modName moduleComponents
      act_file = "test" </> "src" </> modulePath ++ ".act"
      sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"

  src <- liftIO $ readFile act_file
  parsed <- liftIO $ P.parseModule moduleName act_file src
  env <- liftIO $ Acton.Env.mkEnv [sysTypesPath] env0 parsed
  return (env, parsed)


genTests pass_name dir testname input_data output_data = do
  let input_golden = dir </> testname ++ ".input"
      output_golden = dir </> testname ++ ".output"

  describe testname $ do
    it ("Check " ++ pass_name ++ " input") $ do
      goldenTextFile input_golden $ return $ T.pack $ Pretty.print input_data
    it ("Check " ++ pass_name ++ " output") $ do
      goldenTextFile output_golden $ return $ T.pack $ Pretty.print output_data

-- pass 2 Kinds check
testKinds env0 testname = do
  let dir = "test" </> "2-kinds"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed

  genTests "Kinds Check" dir testname parsed kchecked

-- pass 3 Type check
testTypes env0 testname = do
  let dir = "test" </> "3-types"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod

  genTests "Type Check" dir testname kchecked tchecked

-- pass 4 Normalizer
testNorm env0 testname = do
  let dir = "test" </> "4-normalizer"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked

  genTests "Normalizer" dir testname tchecked normalized

-- pass 5 Deactorizer
testDeact env0 testname = do
  let dir = "test" </> "5-deactorizer"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized

  genTests "Deactorizer" dir testname normalized deacted

-- pass 6 CPS
testCps env0 testname = do
  let dir = "test" </> "6-cps"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, _) <- liftIO $ Acton.CPS.convert deactEnv deacted

  genTests "CPS" dir testname deacted cpstyled

-- pass 7 Lambda Lifting
testLL env0 testname = do
  let dir = "test" </> "7-lambdalifting"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, cpsEnv) <- liftIO $ Acton.CPS.convert deactEnv deacted
  (lifted,liftEnv) <- liftIO $ Acton.LambdaLifter.liftModule cpsEnv cpstyled

  genTests "Lambda Lifting" dir testname cpstyled lifted

-- pass 8 Boxing
testBoxing env0 testname = do
  let dir = "test" </> "8-boxing"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, cpsEnv) <- liftIO $ Acton.CPS.convert deactEnv deacted
  (lifted,liftEnv) <- liftIO $ Acton.LambdaLifter.liftModule cpsEnv cpstyled
  boxed <- liftIO $ Acton.Boxing.doBoxing liftEnv lifted

  genTests "Boxing" dir testname lifted boxed

-- pass 9 CodeGen
testCodeGen env0 testname = do
  let dir = "test" </> "9-codegen"

  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, cpsEnv) <- liftIO $ Acton.CPS.convert deactEnv deacted
  (lifted,liftEnv) <- liftIO $ Acton.LambdaLifter.liftModule cpsEnv cpstyled
  boxed <- liftIO $ Acton.Boxing.doBoxing liftEnv lifted
  (n,h,c) <- liftIO $ Acton.CodeGen.generate liftEnv "" boxed

  let pass_name = "CodeGen"
  let input_golden = dir </> testname ++ ".input"
      h_golden = dir </> testname ++ ".h"
      c_golden = dir </> testname ++ ".c"

  describe testname $ do
    it ("Check " ++ pass_name ++ " input") $ do
      goldenTextFile input_golden $ return $ T.pack $ Pretty.print boxed
    it ("Check " ++ pass_name ++ " .h output") $ do
      goldenTextFile h_golden $ return $ T.pack $ Pretty.print h
    it ("Check " ++ pass_name ++ " .c output") $ do
      goldenTextFile c_golden $ return $ T.pack $ Pretty.print c

testDocstrings :: Acton.Env.Env0 -> String -> Spec
testDocstrings env0 testname = do
  (env, parsed) <- parseAct env0 testname

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv, _) <- liftIO $ Acton.Types.reconstruct env kchecked
  let S.NModule tenv mdoc = nmod

  -- Extract docstrings from the parsed AST
  let S.Module _ _ stmts = parsed
      extractDeclDocstrings (S.Decl _ decls) = concatMap extractDocFromDecl decls
      extractDeclDocstrings _ = []

      extractDocFromDecl (S.Def _ n _ _ _ _ _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Class _ n _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Actor _ n _ _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Protocol _ n _ _ _ ddoc) = [(S.nstr n, ddoc)]
      extractDocFromDecl (S.Extension _ _ _ _ _ ddoc) = [("extension", ddoc)]

      docstrings = concatMap extractDeclDocstrings stmts

  describe testname $ do
    -- Basic functionality tests
    it "extracts module docstrings" $ do
      case mdoc of
        Just doc -> doc `shouldContain` "Test module"
        Nothing -> expectationFailure "Module docstring not extracted"

    it "extracts function docstrings" $ do
      case lookup "test_function" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test function"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles functions without docstrings" $ do
      case lookup "no_docstring_function" docstrings of
        Just Nothing -> return ()  -- Expected: no docstring
        Just (Just _) -> expectationFailure "Function should not have docstring"
        Nothing -> expectationFailure "Function not found"

    it "extracts class docstrings" $ do
      case lookup "TestClass" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test class"
        Just Nothing -> expectationFailure "Class should have docstring"
        Nothing -> expectationFailure "Class not found"

    it "extracts actor docstrings" $ do
      case lookup "TestActor" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test actor"
        Just Nothing -> expectationFailure "Actor should have docstring"
        Nothing -> expectationFailure "Actor not found"

    it "extracts protocol docstrings" $ do
      case lookup "TestProtocol" docstrings of
        Just (Just doc) -> doc `shouldContain` "Test protocol"
        Just Nothing -> expectationFailure "Protocol should have docstring"
        Nothing -> expectationFailure "Protocol not found"

    it "extracts extension docstrings" $ do
      case lookup "extension" docstrings of
        Just (Just doc) -> doc `shouldContain` "Extension"
        Just Nothing -> expectationFailure "Extension should have docstring"
        Nothing -> expectationFailure "Extension not found"

    -- Edge case tests
    it "ignores non-first string statements" $ do
      case lookup "function_with_non_first_string" docstrings of
        Just Nothing -> return ()  -- Expected: no docstring
        Just (Just _) -> expectationFailure "Non-first string should not be docstring"
        Nothing -> expectationFailure "Function not found"

    it "extracts only first string as docstring" $ do
      case lookup "function_with_multiple_strings" docstrings of
        Just (Just doc) -> do
          doc `shouldContain` "First string is docstring"
          when ("Second string" `isInfixOf` doc) $
            expectationFailure "Later strings should not be in docstring"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles single quote docstrings" $ do
      case lookup "function_with_single_quotes" docstrings of
        Just (Just doc) -> doc `shouldContain` "Single quote"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles triple single quote docstrings" $ do
      case lookup "function_with_triple_single_quotes" docstrings of
        Just (Just doc) -> doc `shouldContain` "Triple quote"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles mixed quotes in docstrings" $ do
      case lookup "function_with_mixed_quotes" docstrings of
        Just (Just doc) -> doc `shouldContain` "Mixed 'quotes'"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles empty docstrings" $ do
      case lookup "function_empty_docstring" docstrings of
        Just (Just doc) -> doc `shouldBe` ""
        Just Nothing -> expectationFailure "Function should have empty docstring"
        Nothing -> expectationFailure "Function not found"

    it "ignores strings in control flow" $ do
      case lookup "function_with_control_flow" docstrings of
        Just Nothing -> return ()  -- Expected: no docstring
        Just (Just _) -> expectationFailure "String in control flow should not be docstring"
        Nothing -> expectationFailure "Function not found"

    it "handles functions with just docstrings" $ do
      case lookup "function_just_docstring" docstrings of
        Just (Just doc) -> doc `shouldContain` "Just a docstring"
        Just Nothing -> expectationFailure "Function should have docstring"
        Nothing -> expectationFailure "Function not found"
