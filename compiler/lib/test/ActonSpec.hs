{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Data.Char (toLower)


import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import qualified Acton.Printer as AP
import qualified Acton.DocPrinter as DocP
import qualified Acton.Env
import Acton.Env (CompilationError(..))
import qualified Acton.Kinds
import qualified Acton.Types
import qualified Acton.TypeM
import Acton.TypeM (TypeError(..))
import qualified Acton.Normalizer
import qualified Acton.Deactorizer
import qualified Acton.CPS
import qualified Acton.LambdaLifter
import qualified Acton.Boxing
import qualified Acton.CodeGen
import qualified Acton.Diagnostics as Diag
import Pretty (print, prettyText)
import qualified Pretty
import Test.Syd
import Test.Syd.Def.Golden (goldenTextFile)
import qualified Control.Monad.Trans.State.Strict as St
import Text.Megaparsec (runParser, errorBundlePretty, ShowErrorComponent(..))
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Error.Diagnose (printDiagnostic, prettyDiagnostic, WithUnicode(..), TabSize(..), defaultStyle, addReport, addFile)
import Error.Diagnose.Report (Report(..))
import Prettyprinter (unAnnotate, layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath ((</>), joinPath, takeFileName, takeBaseName, takeDirectory, splitDirectories)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Monad (forM_, when, foldM)
import qualified Control.Exception as E
import Utils (SrcLoc(..), loc, prstr)
import qualified Acton.BuildSpec as BuildSpec
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified System.IO.Unsafe




main :: IO ()
main = do
  let sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"
  env0 <- Acton.Env.initEnv sysTypesPath False

  sydTest $ do
    describe "Pass 1: Parser" $ do

      describe "Basic Syntax" $ do
        testParse env0 ["syntax1"]

      describe "Docstrings" $ do
        testParse env0 ["docstrings"]
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

        describe "Triple quotes with 4-5 quotes (ending with quotes)" $ do
          -- Triple double quotes with 4 quotes (string ends with single ")
          testParseOutput "\"\"\"\"foo\"\"\"" "\"\\\"foo\""  -- """"foo""" -> "foo (1 quote in content)
          testParseOutput "\"\"\"\"hello world\"\"\"\"" "\"\\\"hello world\\\"\""  -- """""hello world""""" -> ""hello world"" (2 quotes in content)
          testParseOutput "r\"\"\"\"raw quote\"\"\"" "\"\\\"raw quote\""  -- raw string with 4 quotes
          testParseOutput "r\"\"\"\"\"raw two quotes\"\"\"" "\"\\\"\\\"raw two quotes\""  -- raw string with 5 quotes

          -- Triple single quotes with 4 quotes (string ends with single ')
          testParseOutput "''''bar'''" "\"'bar\""  -- ''''bar''' -> 'bar (1 quote in content)
          testParseOutput "'''''test me'''''" "\"''test me''\""  -- '''''test me''''' -> ''test me'' (2 quotes in content)
          testParseOutput "r''''raw single'''" "\"'raw single\""  -- raw string with 4 quotes
          testParseOutput "r'''''raw two singles'''" "\"''raw two singles\""  -- raw string with 5 quotes

          -- Interpolation with trailing quotes
          testParseOutput "\"\"\"\"value: {x}\"\"\"" "\"\\\"value: %s\" % str(x)"  -- interpolation with trailing quote
          testParseOutput "\"\"\"\"\"formatted {y}\"\"\"\"\"" "\"\\\"\\\"formatted %s\\\"\\\"\" % str(y)"  -- 2 quotes before and after
          testParseOutput "f''''x = {x}'''" "\"'x = %s\" % str(x)"  -- f-string with 4 single quotes
          testParseOutput "f'''''vals: {a}, {b}'''''" "\"''vals: %s, %s''\" % (str(a), str(b))"  -- f-string with 5 single quotes

        describe "Triple quotes with 6+ quotes (error cases)" $ do
          -- Triple double quotes with 6+ quotes should fail
          testParseError "triple_double_6quotes" "\"\"\"foo\"\"\"\"\"\""  -- 3 opening, 6 closing
          testParseError "triple_double_7quotes" "\"\"\"bar\"\"\"\"\"\"\""  -- 3 opening, 7 closing
          testParseError "triple_double_8quotes" "\"\"\"test\"\"\"\"\"\"\"\""  -- 3 opening, 8 closing
          testParseError "triple_double_10quotes" "\"\"\"content\"\"\"\"\"\"\"\"\"\""  -- 3 opening, 10 closing

          -- Triple single quotes with 6+ quotes should fail
          testParseError "triple_single_6quotes" "'''foo''''''"  -- 3 opening, 6 closing
          testParseError "triple_single_7quotes" "'''bar'''''''"  -- 3 opening, 7 closing
          testParseError "triple_single_8quotes" "'''test''''''''"  -- 3 opening, 8 closing
          testParseError "triple_single_10quotes" "'''content''''''''''"  -- 3 opening, 10 closing

          -- Raw strings with 6+ quotes should fail
          testParseError "raw_triple_double_6quotes" "r\"\"\"raw\"\"\"\"\"\""  -- raw with 6 closing quotes
          testParseError "raw_triple_single_6quotes" "r'''raw''''''"  -- raw with 6 closing quotes

          -- F-strings with 6+ quotes should fail
          testParseError "fstring_triple_double_6quotes" "f\"\"\"x={x}\"\"\"\"\"\""  -- f-string with 6 closing quotes
          testParseError "fstring_triple_single_6quotes" "f'''x={x}''''''"  -- f-string with 6 closing quotes

        describe "Special characters and escaping" $ do
          -- Escaped braces
          testParseOutput "f\"something but {{{substituted}}}\"" "\"something but {%s}\" % str(substituted)"
          testParseOutput "\"{{a}}\"" "\"{a}\""  -- Escaped braces should not trigger interpolation
          testParseOutput "\"{{\"" "\"{\""  -- Just escaped opening brace
          testParseOutput "\"}}\"" "\"}\""  -- Just escaped closing brace
          testParseOutput "f\"{{hello}}\"" "\"{hello}\""  -- f-string with escaped braces
          testParseOutput "\"{{hello}} {world}\"" "\"{hello} %s\" % str(world)"  -- Mix of escaped and interpolated

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
          testModuleParseError "unclosed_raw_string" "a = r\"hello"
          testModuleParseError "unclosed_bytes_string" "a = b\"hello"
          testModuleParseError "unclosed_raw_bytes_string" "a = rb\"hello"
          testModuleParseError "unclosed_raw_string_single" "a = r'hello"
          testModuleParseError "unclosed_bytes_string_single" "a = b'hello"
          testModuleParseError "unclosed_raw_bytes_string_single" "a = rb'hello"

        describe "Triple quote errors (6+ quotes)" $ do
          testModuleParseError "six_double_quotes" "a = \"\"\"\"\"\"test\"\"\"\"\"\""  -- 6 quotes each side
          testModuleParseError "six_single_quotes" "a = ''''''test''''''"  -- 6 quotes each side
          testModuleParseError "seven_double_quotes" "a = \"\"\"\"\"\"\"test\"\"\"\"\"\"\""  -- 7 quotes each side
          testModuleParseError "mixed_six_quotes_double" "a = \"\"\"\"\"\"mixed content with {interpolation}\"\"\"\"\"\""
          testModuleParseError "raw_six_quotes" "a = r\"\"\"\"\"\"raw test\"\"\"\"\"\""

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
      testKinds env0 ["deact"]

    describe "Pass 3: Types" $ do
      testTypes env0 ["deact"]
      testTypes env0 ["test_discovery"]

      testAttributesInitialization env0

    describe "Pass 4: Normalizer" $ do
      testNorm env0 ["deact"]

    describe "Pass 5: Deactorizer" $ do
      testDeact env0 ["deact", "deact_from_import"]

    describe "Pass 6: CPS" $ do
      testCps env0 ["cps_volatiles"]

    describe "Pass 7: Lambda Lifting" $ do
      testLL env0 ["deact"]

    describe "Pass 8: Boxing" $ do
      testBoxing env0 ["deact"]

    describe "Pass 9: CodeGen" $ do
      testCodeGen env0 ["deact"]
      testCodeGen env0 ["lines"]

    -- BuildSpec: parsing and update-in-place of Build.act (canonical layout)
    describe "BuildSpec" $ do
      it "parses canonical Build.act and dumps JSON" $ do
        let buildAct = unlines
              [ "# Canonical Build.act file"
              , "name = \"demo\""
              , ""
              , "# Dependencies section"
              , "dependencies = {"
              , "  \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "# Zig dependencies section"
              , "zig_dependencies = {"
              , "  \"z\": ("
              , "        url=\"https://z\","
              , "        hash=\"abcd\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              , ""
              , "# bla bla bla"
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            let json = BuildSpec.encodeBuildSpecJSON spec
            case BuildSpec.parseBuildSpecJSON json of
              Left err2 -> expectationFailure err2
              Right spec2 -> spec2 `shouldBe` spec

      it "updates Build.act in-place, preserving comments and main actor" $ do
        let buildAct0 = unlines
              [ "# Canonical Build.act file"
              , "name = \"demo\""
              , ""
              , "# Dependencies section (keep my comments)"
              , "dependencies = {"
              , "  \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "# Zig dependencies section"
              , "zig_dependencies = {"
              , "}"
              , ""
              , "# bla bla bla"
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"a\": {\"path\": \"deps/aa\"},\n    \"b\": {\"url\": \"u\", \"hash\": \"h\"}\n  },\n  \"zig_dependencies\": {\n    \"z\": {\"url\": \"zu\", \"hash\": \"zh\", \"artifacts\": [\"z\"]}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            let expected = unlines
                  [ "# Canonical Build.act file"
                  , "name = \"demo\""
                  , ""
                  , "# Dependencies section (keep my comments)"
                  , "dependencies = {"
                  , "    \"a\": ("
                  , "        path=\"deps/aa\""
                  , "    ),"
                  , "    \"b\": ("
                  , "        url=\"u\","
                  , "        hash=\"h\""
                  , "    )"
                  , "}"
                  , ""
                  , "# Zig dependencies section"
                  , "zig_dependencies = {"
                  , "    \"z\": ("
                  , "        url=\"zu\","
                  , "        hash=\"zh\","
                  , "        artifacts=[\"z\"]"
                  , "    )"
                  , "}"
                  , ""
                  , "# bla bla bla"
                  , "actor main(env: Env):"
                  , "    pass"
                  , ""
                  ]
            buildAct1 `shouldBe` expected

      it "parses Build.act with only dependencies (zig deps missing)" $ do
        let buildAct = unlines
              [ "# Only dependencies"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            -- deps present, zig deps empty
            BuildSpec.dependencies spec `shouldSatisfy` (not . null)
            BuildSpec.zig_dependencies spec `shouldSatisfy` null

      it "parses Build.act with only zig_dependencies (deps missing)" $ do
        let buildAct = unlines
              [ "# Only zig deps"
              , "zig_dependencies = {"
              , "    \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            -- zig present, deps empty
            BuildSpec.zig_dependencies spec `shouldSatisfy` (not . null)
            BuildSpec.dependencies spec `shouldSatisfy` null

      it "parses Build.act with no spec blocks (both optional)" $ do
        let buildAct = unlines
              [ "# No deps here"
              , "name = \"demo\""
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        case BuildSpec.parseBuildAct buildAct of
          Left err -> expectationFailure err
          Right (spec,_,_) -> do
            BuildSpec.dependencies spec `shouldSatisfy` null
            BuildSpec.zig_dependencies spec `shouldSatisfy` null

      it "appends missing zig_dependencies block when absent" $ do
        let buildAct0 = unlines
              [ "# Build with only dependencies"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"zig_dependencies\": {\n    \"z\": {\"url\": \"zu\", \"hash\": \"zh\", \"artifacts\": [\"z\"]}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- Original dependencies remain
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"a\": ("
              , "        path=\"deps/a\""
              , "    )"
              ])
            -- Missing zig_dependencies appended with expected formatting
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "zig_dependencies = {"
              , "  \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              ])

      it "appends missing dependencies block when absent" $ do
        let buildAct0 = unlines
              [ "# Build with only zig deps"
              , "zig_dependencies = {"
              , "    \"z\": (url=\"zu\", hash=\"zh\", artifacts=[\"z\"])"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"a\": {\"path\": \"deps/a\"}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- Original zig deps remain
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              ])
            -- Missing dependencies appended with expected formatting
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "dependencies = {"
              , "  \"a\": ("
              , "        path=\"deps/a\""
              , "    )"
              , "}"
              ])

      it "updates dependencies block with non-canonical whitespace, preserving outer line" $ do
        let buildAct0 = unlines
              [ "# Whitespace variant Build.act"
              , "dependencies  =   {"
              , ""
              , "  \"a\"  :  ( path = \"deps/a\" )"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"a\": {\"path\": \"deps/aa\"}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- Outer label line (with extra spaces) is preserved
            buildAct1 `shouldSatisfy` (isInfixOf "dependencies  =   {")
            -- Inner body is canonicalised and updated
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"a\": ("
              , "        path=\"deps/aa\""
              , "    )"
              ])

      it "updates only the overlaid dependency entry, preserving others" $ do
        let buildAct0 = unlines
              [ "# Multiple dependencies"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\"),"
              , "    \"b\": (path=\"deps/b\")"
              , "}"
              , ""
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {\n    \"b\": {\"path\": \"deps/bb\"}\n  }\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            -- JSON dependencies is treated as full set: only "b" remains, with updated path.
            buildAct1 `shouldSatisfy` (not . isInfixOf "    \"a\": (path=\"deps/a\"),")
            buildAct1 `shouldSatisfy` (isInfixOf $ unlines
              [ "    \"b\": ("
              , "        path=\"deps/bb\""
              , "    )"
              ])

      it "removes all dependencies and zig deps when new spec objects are empty" $ do
        let buildAct0 = unlines
              [ "# Canonical Build.act file"
              , "name = \"demo\""
              , ""
              , "# Dependencies section (keep my comments)"
              , "dependencies = {"
              , "    \"a\": (path=\"deps/a\")"
              , "}"
              , ""
              , "# Zig dependencies section"
              , "zig_dependencies = {"
              , "    \"z\": ("
              , "        url=\"zu\","
              , "        hash=\"zh\","
              , "        artifacts=[\"z\"]"
              , "    )"
              , "}"
              , ""
              , "# bla bla bla"
              , "actor main(env: Env):"
              , "    pass"
              , ""
              ]
        let newJson = "{\n  \"dependencies\": {},\n  \"zig_dependencies\": {}\n}\n"
        case BuildSpec.updateBuildActFromJSON buildAct0 (BL.fromStrict (B8.pack newJson)) of
          Left err -> expectationFailure err
          Right buildAct1 -> do
            let expected = unlines
                  [ "# Canonical Build.act file"
                  , "name = \"demo\""
                  , ""
                  , "# Dependencies section (keep my comments)"
                  , "dependencies = {"
                  , "}"
                  , ""
                  , "# Zig dependencies section"
                  , "zig_dependencies = {"
                  , "}"
                  , ""
                  , "# bla bla bla"
                  , "actor main(env: Env):"
                  , "    pass"
                  , ""
                  ]
            buildAct1 `shouldBe` expected


-- Helper function to format custom parse errors consistently
formatCustomParseError :: String -> String -> SrcLoc -> P.CustomParseError -> String
formatCustomParseError filename input loc err =
  let diagnostic = Diag.customParseErrorDiagnostic "Syntax error" filename input loc err
      doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
      layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
  in T.unpack $ renderStrict layout

parseActon :: String -> Either String String
parseActon input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.stmt P.initState) "" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right result -> Right $ concatMap (Pretty.print) result)
      handleCustomParseException
  where
    inputWithNewline = if last input == '\n' then input else input ++ "\n"
    handleCustomParseException :: P.CustomParseException -> IO (Either String String)
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleCustomParseException (P.CustomParseException loc err) =
      return $ Left $ formatCustomParseError "test" inputWithNewline loc err

-- Helper function to parse a full module (for testing module-level constructs)
parseModuleTest :: String -> Either String String
parseModuleTest input =
  System.IO.Unsafe.unsafePerformIO $
    E.catch
      (E.evaluate $ case runParser (St.evalStateT P.file_input P.initState) "test.act" inputWithNewline of
        Left err -> Left $ renderDiagnostic err
        Right (imports, suite) -> Right $ "Module parsed successfully")
      handleCustomParseException
  where
    inputWithNewline = if null input || last input == '\n' then input else input ++ "\n"
    renderDiagnostic err =
      let diagnostic = Diag.parseDiagnosticFromBundle "test.act" inputWithNewline err
          doc = prettyDiagnostic WithUnicode (TabSize 4) diagnostic
          layout = layoutPretty defaultLayoutOptions (unAnnotate doc)
      in T.unpack $ renderStrict layout
    handleCustomParseException :: P.CustomParseException -> IO (Either String String)
    handleCustomParseException (P.CustomParseException loc err) =
      return $ Left $ formatCustomParseError "test.act" inputWithNewline loc err

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
testParse :: Acton.Env.Env0 -> [String] -> Spec
testParse env0 modulePaths = do
  let dir = "test" </> "1-parse"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          return (accEnv, accModules ++ [(takeFileName modulePath, parsed)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, parsed) ->
    genTests "Parse Check" dir modName parsed parsed

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
testKinds :: Acton.Env.Env0 -> [String] -> Spec
testKinds env0 modulePaths = do
  let dir = "test" </> "2-kinds"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          return (accEnv, accModules ++ [(takeFileName modulePath, parsed, kchecked)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, parsed, kchecked) ->
    genTests "Kinds Check" dir modName parsed kchecked

-- pass 3 Type check
testTypes :: Acton.Env.Env0 -> [String] -> Spec
testTypes env0 modulePaths = do
  let dir = "test" </> "3-types"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, kchecked, tchecked)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, kchecked, tchecked) ->
    genTests "Type Check" dir modName kchecked tchecked

-- pass 4 Normalizer
testNorm :: Acton.Env.Env0 -> [String] -> Spec
testNorm env0 modulePaths = do
  let dir = "test" </> "4-normalizer"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, tchecked, normalized)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, tchecked, normalized) ->
    genTests "Normalizer" dir modName tchecked normalized

-- pass 5 Deactorizer
testDeact :: Acton.Env.Env0 -> [String] -> Spec
testDeact env0 modulePaths = do
  let dir = "test" </> "5-deactorizer"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, normalized, deacted)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, normalized, deacted) ->
    genTests "Deactorizer" dir modName normalized deacted

-- pass 6 CPS
testCps :: Acton.Env.Env0 -> [String] -> Spec
testCps env0 modulePaths = do
  let dir = "test" </> "6-cps"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, _) <- Acton.CPS.convert deactEnv deacted
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, deacted, cpstyled)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, deacted, cpstyled) ->
    genTests "CPS" dir modName deacted cpstyled

-- pass 7 Lambda Lifting
testLL :: Acton.Env.Env0 -> [String] -> Spec
testLL env0 modulePaths = do
  let dir = "test" </> "7-lambdalifting"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, cpsEnv) <- Acton.CPS.convert deactEnv deacted
          (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, cpstyled, lifted)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, cpstyled, lifted) ->
    genTests "Lambda Lifting" dir modName cpstyled lifted

-- pass 8 Boxing
testBoxing :: Acton.Env.Env0 -> [String] -> Spec
testBoxing env0 modulePaths = do
  let dir = "test" </> "8-boxing"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, cpsEnv) <- Acton.CPS.convert deactEnv deacted
          (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
          boxed <- Acton.Boxing.doBoxing liftEnv lifted
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, lifted, boxed)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, lifted, boxed) ->
    genTests "Boxing" dir modName lifted boxed

-- pass 9 CodeGen
testCodeGen :: Acton.Env.Env0 -> [String] -> Spec
testCodeGen env0 modulePaths = do
  let dir = "test" </> "9-codegen"

  modules <- runIO $ do
    let processModule (accEnv, accModules) modulePath = do
          (env, parsed) <- parseAct accEnv modulePath
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv, _) <- Acton.Types.reconstruct env kchecked
          let S.NModule tenv mdoc = nmod
          (normalized, normEnv) <- Acton.Normalizer.normalize typeEnv tchecked
          (deacted, deactEnv) <- Acton.Deactorizer.deactorize normEnv normalized
          (cpstyled, cpsEnv) <- Acton.CPS.convert deactEnv deacted
          (lifted,liftEnv) <- Acton.LambdaLifter.liftModule cpsEnv cpstyled
          boxed <- Acton.Boxing.doBoxing liftEnv lifted
          let act_file = "test" </> "src" </> modulePath ++ ".act"
          srcText <- readFile act_file
          let srcbase = "test" </> "src" </> modulePath
          (n,h,c) <- Acton.CodeGen.generate liftEnv srcbase srcText True boxed "test-hash"
          let newAccEnv = Acton.Env.addMod (S.modname parsed) tenv mdoc accEnv
          return (newAccEnv, accModules ++ [(takeFileName modulePath, boxed, n, h, c)])

    (_, modules) <- foldM processModule (env0, []) modulePaths
    return modules

  forM_ modules $ \(modName, boxed, n, h, c) -> do
    let pass_name = "CodeGen"
    let input_golden = dir </> modName ++ ".input"
        h_golden = dir </> modName ++ ".h"
        c_golden = dir </> modName ++ ".c"

    describe modName $ do
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

-- Tests for uninitialized class attribute checking

testAttributesInitialization :: Acton.Env.Env0 -> Spec
testAttributesInitialization env0 = do
  describe "Class Attribute Initialization Check" $ do
    testTypeSuccess env0 "class_init_attrs/init_basic"
    testTypeSuccess env0 "class_init_attrs/init_inferred_only"
    testTypeError   env0 "class_init_attrs/uninit_basic"
    testTypeError   env0 "class_init_attrs/uninit_basic_inferred"

    testTypeSuccess env0 "class_init_attrs/init_multiple"
    testTypeError   env0 "class_init_attrs/uninit_partial"

    testTypeSuccess env0 "class_init_attrs/init_inherited"
    testTypeError   env0 "class_init_attrs/uninit_inherited"
    testTypeSuccess env0 "class_init_attrs/init_parent_call"
    testTypeSuccess env0 "class_init_attrs/init_mixed_parent_self"
    testTypeSuccess env0 "class_init_attrs/init_grandparent_call"
    testTypeError   env0 "class_init_attrs/uninit_no_parent_init"
    testTypeError   env0 "class_init_attrs/uninit_grandparent"
    testTypeError   env0 "class_init_attrs/uninit_grandparent2"
    testTypeSuccess env0 "class_init_attrs/init_no_init_uses_parent"

    testTypeSuccess env0 "class_init_attrs/init_conditional"

    testTypeSuccess env0 "class_init_attrs/init_nested_if"
    testTypeSuccess env0 "class_init_attrs/init_nested_if_raise"
    testTypeError   env0 "class_init_attrs/uninit_nested_if"
    testTypeError   env0 "class_init_attrs/uninit_elif_missing"

    testTypeSuccess env0 "class_init_attrs/init_try_except"
    testTypeSuccess env0 "class_init_attrs/init_try_finally"
    testTypeSuccess env0 "class_init_attrs/init_try_except_finally"
    testTypeError   env0 "class_init_attrs/uninit_try_except"
    testTypeSuccess env0 "class_init_attrs/init_try_except_raise"
    testTypeSuccess env0 "class_init_attrs/init_try_else_combined"
    testTypeSuccess env0 "class_init_attrs/init_multiple_except_handlers"
    testTypeSuccess env0 "class_init_attrs/init_except_raise_after_assignment"
    testTypeSuccess env0 "class_init_attrs/init_try_inside_if"
    testTypeSuccess env0 "class_init_attrs/init_constant_condition"
    testTypeSuccess env0 "class_init_attrs/init_raise_after_assignment"
    testTypeSuccess env0 "class_init_attrs/init_both_branches_with_raise"
    testTypeSuccess env0 "class_init_attrs/init_elif_raise_after_assignment"
    testTypeSuccess env0 "class_init_attrs/init_after_statements"

    testTypeSuccess env0 "class_init_attrs/init_self_attr_access"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_in_expr"
    testTypeError   env0 "class_init_attrs/uninit_self_attr_access"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_conditional"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_in_loop"
    testTypeError   env0 "class_init_attrs/uninit_method_reference"

    testTypeSuccess env0 "class_init_attrs/init_function_call"
    testTypeSuccess env0 "class_init_attrs/init_nested_function_call"
    testTypeError   env0 "class_init_attrs/uninit_method_call"
    testTypeError   env0 "class_init_attrs/uninit_method_call_assign"
    testTypeSuccess env0 "class_init_attrs/init_actor"
    testTypeSuccess env0 "class_init_attrs/init_actor_call"

    testTypeSuccess env0 "class_init_attrs/init_notimplemented"
    testTypeSuccess env0 "class_init_attrs/init_notimpl_call"
    testTypeError   env0 "class_init_attrs/uninit_notimpl_call_other"
    testTypeError   env0 "class_init_attrs/uninit_augmented_assign"
    testTypeError   env0 "class_init_attrs/uninit_for_loop"
    testTypeSuccess env0 "class_init_attrs/init_after_loop"
    testTypeSuccess env0 "class_init_attrs/init_after_while"
    testTypeError   env0 "class_init_attrs/uninit_loop_references_self"
    testTypeError   env0 "class_init_attrs/uninit_init_in_method"
    testTypeError   env0 "class_init_attrs/uninit_return_early"

    testTypeError   env0 "class_init_attrs/uninit_self_reference"
    testTypeSuccess env0 "class_init_attrs/init_self_attr_reference"
    testTypeError   env0 "class_init_attrs/uninit_self_in_list"

    testTypeSuccess env0 "class_init_attrs/init_loop_break"
    testTypeSuccess env0 "class_init_attrs/init_loop_continue"
    testTypeSuccess env0 "class_init_attrs/init_assert"
    testTypeSuccess env0 "class_init_attrs/init_assert_with_self"
    testTypeError   env0 "class_init_attrs/uninit_assert_uninit_attr"
    testTypeSuccess env0 "class_init_attrs/init_delete"
    testTypeSuccess env0 "class_init_attrs/init_nested_function"
    testTypeError   env0 "class_init_attrs/uninit_nested_function_with_self"
    testTypeError   env0 "class_init_attrs/uninit_nested_function_escape"


-- Test a file that should produce a type error
-- Path can be "testname" or "subdir/testname"
testTypeError :: Acton.Env.Env0 -> String -> Spec
testTypeError env0 path = do
  let (subdir, testname) = case break (=='/') path of
                             (name, "") -> ("", name)
                             (dir, '/':name) -> (dir, name)
                             _ -> error $ "Invalid test path: " ++ path
      act_file = "test" </> "src" </> path ++ ".act"
      golden_file = "test" </> "3-types" </> path ++ ".golden"
      -- For error display, use just the basename like actonc does
      display_file = testname ++ ".act"

  it testname $ do
    goldenTextFile golden_file $ liftIO $ do
      -- Read the source file for error formatting
      srcContent <- readFile act_file

      result <- E.try $ do
        (env, parsed) <- parseAct env0 path
        kchecked <- Acton.Kinds.check env parsed
        (nmod, tchecked, typeEnv, mrefs) <- Acton.Types.reconstruct env kchecked
        -- Force evaluation to trigger any lazy exceptions
        E.evaluate $ length (show tchecked)
        return ()

      case result of
        Left (e :: E.SomeException) -> do
          -- Format the error like actonc does
          let diagnostic = case E.fromException e :: Maybe TypeError of
                Just typeErr ->
                  -- Use the typeReport function to format all TypeError variants with richer diagnostics
                  let report = Acton.TypeM.typeReport typeErr display_file srcContent
                      diag = addReport mempty report
                  in addFile diag display_file srcContent
                _ -> case E.fromException e :: Maybe CompilationError of
                  Just (IllegalSigOverride n) ->
                    Diag.actErrToDiagnostic "Compilation error" display_file srcContent (loc n) ("Illegal signature override: " ++ prettyText n)
                  Just (OtherError loc msg) ->
                    Diag.actErrToDiagnostic "Compilation error" display_file srcContent loc msg
                  Just compErr ->
                    -- For other compilation errors, use the default show instance
                    Diag.actErrToDiagnostic "Compilation error" display_file srcContent (loc compErr) (show compErr)
                  _ ->
                    -- For now, just use the default formatting for other errors
                    let diagnostic = addReport mempty $ Err (Just "error") (show e) [] []
                    in addFile diagnostic display_file srcContent

          -- Pretty print the diagnostic
          return $ T.pack $ show $
            unAnnotate (prettyDiagnostic WithoutUnicode (TabSize 4) diagnostic)
        Right _ ->
          return $ T.pack "ERROR: Expected type error but compilation succeeded"

-- Test a file that should type check successfully
-- Path can be "testname" or "subdir/testname"
testTypeSuccess :: Acton.Env.Env0 -> String -> Spec
testTypeSuccess env0 path = do
  let testname = takeBaseName path
      act_file = "test" </> "src" </> path ++ ".act"

  it testname $ do
    result <- E.try $ do
      (env, parsed) <- parseAct env0 path
      kchecked <- Acton.Kinds.check env parsed
      (nmod, tchecked, typeEnv, mrefs) <- Acton.Types.reconstruct env kchecked
      -- Force evaluation to trigger any lazy exceptions
      E.evaluate $ length (show tchecked)
      return ()

    case result of
      Left (e :: E.SomeException) ->
        expectationFailure $ "Expected success but got error: " ++ show e
      Right _ ->
        return ()
