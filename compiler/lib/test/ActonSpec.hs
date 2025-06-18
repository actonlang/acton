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
import Pretty (print)
import Test.Syd
import Test.Syd.Def.Golden (goldenTextFile)
import qualified Control.Monad.Trans.State.Strict as St
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import System.FilePath ((</>), joinPath, takeFileName, takeBaseName, takeDirectory)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Monad (forM_, when, foldM)




main :: IO ()
main = do
  let sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"
  env0 <- Acton.Env.initEnv sysTypesPath False

  sydTest $ do
    describe "Pass 1: Parser" $ do

      describe "Syntax" $ do
        testParse env0 "syntax1"

      describe "Module-level docstring tests" $ do
        describe "Valid docstrings before imports" $ do
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

        describe "Invalid expressions before imports (golden tests)" $ do
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

      describe "F-String Tests" $ do

        describe "Basic f-string syntax" $ do
          testParseOutput "f\"hello {a}!\"" "\"hello %s!\" % str(a)"
          testParseOutput "f\"\"" "\"\" % ()"
          testParseOutput "f\"plain text\"" "\"plain text\" % ()"

        describe "Variables and types" $ do
          testParseOutput "f\"Hello {name}, your score is {score}\"" "\"Hello %s, your score is %s\" % (str(name), str(score))"
          testParseOutput "f\"Types: {s}, {i}, {f}, {l}\"" "\"Types: %s, %s, %s, %s\" % (str(s), str(i), str(f), str(l))"
          testParseOutput "f\"None value: {n}\"" "\"None value: %s\" % str(n)"
          testParseOutput "f\"True: {t}, False: {f}\"" "\"True: %s, False: %s\" % (str(t), str(f))"

        describe "Alignment and formatting" $ do
          -- Alignment
          testParseOutput "f\"{name:^10}\"" "\"%s\" % str(name).center(10)"  -- center
          testParseOutput "f\"{name:<9}\"" "\"%-9s\" % str(name)"   -- left
          testParseOutput "f\"{name:>10}\"" "\"%10s\" % str(name)"  -- right
          testParseOutput "f\"{a:>10}:{b:^10}:{c:<10}\"" "\"%10s:%s:%-10s\" % (str(a), str(b).center(10), str(c))"  -- combined

          -- Numeric formatting
          testParseOutput "f\"{num:5}\"" "\"%5s\" % str(num)"        -- width
          testParseOutput "f\"{num:05}\"" "\"%05d\" % num"       -- zero padding
          testParseOutput "f\"{num:010}\"" "\"%010d\" % num"      -- zero padding with width
          testParseOutput "f\"{neg_num:05}\"" "\"%05d\" % neg_num"   -- negative with padding
          testParseOutput "f\"{pi:.2f}\"" "\"%.2f\" % pi"       -- float precision
          testParseOutput "f\"{pi:.4f}\"" "\"%.4f\" % pi"       -- float precision 4dp
          testParseOutput "f\"{pi:.0f}\"" "\"%.0f\" % pi"       -- float precision 0dp
          testParseOutput "f\"{pi:10.2f}\"" "\"%10.2f\" % pi"     -- float width and precision
          testParseOutput "f\"{num:+08.2f}\"" "\"%08.2f\" % num"   -- sign, width, precision

        describe "Expressions and escaping" $ do
          testParseOutput "f\"something but {{{substituted}}}\"" "\"something but {%s}\" % str(substituted)"  -- escaped braces
          testParseOutput "f\"Sum: {a + b}\"" "\"Sum: %s\" % str(a + b)"                    -- simple expression
          testParseOutput "f\"Calculation: {a * b // 2}\"" "\"Calculation: %s\" % str(a * b // 2)"       -- complex expression
          testParseOutput "f\"Result: {func({a: b})}\"" "\"Result: %s\" % str(func({a: b}))"          -- nested braces in expr

        describe "String variations" $ do
          -- Multiline
          testParseOutput "f\"\"\"Name: {name}\nAge: {age}\"\"\"" "\"Name: %s\\\\nAge: %s\" % (str(name), str(age))"
          testParseOutput "f\"\"\"\n    Name: {name}\n    Age: {age}\n    \"\"\"" "\"\\\\n    Name: %s\\\\n    Age: %s\\\\n    \" % (str(name), str(age))"

          -- Alternative quotes
          testParseOutput "f'hello {a}!'" "\"hello %s!\" % str(a)"
          testParseOutput "f'''hello {a}!'''" "\"hello %s!\" % str(a)"

          -- Spaces in format specifier
          testParseOutput "f\"{ num : 10 }\"" "\"%10s\" % str(num)"
          testParseOutput "f\"{num: 10}\"" "\"%10s\" % str(num)"
          testParseOutput "f\"{num :10}\"" "\"%10s\" % str(num)"
          testParseOutput "f\"{ name : >10 }\"" "\"%10s\" % str(name)"
          testParseOutput "f\"{ name : ^10 }\"" "\"%s\" % str(name).center(10)"
          testParseOutput "f\"{ name : <10 }\"" "\"%-10s\" % str(name)"

        describe "Special cases" $ do
          testParseOutput "f\"Hello, {name}! 你好!\"" "\"Hello, %s! \\20320\\22909!\" % str(name)"       -- Unicode
          testParseOutput "f\"Message: {greeting}!\"" "\"Message: %s!\" % str(greeting)"       -- Simple variable
          testParseOutput "f\"{name:@10}\"" "\"%s\" % str(name)"                -- Invalid format accepted

        describe "F-String Error Handling Golden Tests" $ do
          testParseError "fstring_unclosed_brace" "f\"Unclosed brace: {name"
          testParseError "fstring_empty_expression" "f\"Empty expression {}\""
          testParseError "fstring_missing_expression" "f\"Missing expression {:10}\""
          testParseError "fstring_unbalanced_format" "f\"Unbalanced format {name:}:10}\""
          testParseError "fstring_invalid_format" "f\"Invalid format specifier {name:@Z}\""


    describe "Documentation Printing" $ do
      -- Test documentation generation for modules
      -- TODO: Fix import resolution for foo module
      testDocFiles env0 ["bar"]

    describe "Pass 2: Kinds" $ do
      testKinds env0 "deact"

    describe "Pass 3: Types" $ do
      testTypes env0 "deact"

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
  case runParser (St.evalStateT P.stmt P.initState) "" inputWithNewline of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right $ concatMap (Pretty.print) result
  where
    inputWithNewline = if last input == '\n' then input else input ++ "\n"

-- Helper function to parse a full module (for testing module-level constructs)
parseModuleTest :: String -> Either String String
parseModuleTest input =
  case runParser (St.evalStateT P.file_input P.initState) "test.act" inputWithNewline of
    Left err -> Left $ errorBundlePretty err
    Right (imports, suite) -> Right $ "Module parsed successfully"
  where
    inputWithNewline = if null input || last input == '\n' then input else input ++ "\n"

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
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "1-parse"

  (env, parsed) <- parseAct env0 act_file

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
-- Takes a list of module names (without .act extension) in dependency order
-- and generates golden files for all three formats: .txt (ASCII), .md (Markdown), .html (HTML)
testDocFiles :: Acton.Env.Env0 -> [String] -> Spec
testDocFiles env0 moduleNames = do
  let testDir = "test"
      goldenDir = "doc-golden"
      sysTypesPath = ".." </> ".." </> ".." </> "dist" </> "base" </> "out" </> "types"

  -- Process modules in dependency order
  oldDir <- runIO getCurrentDirectory
  
  -- Parse and type-check all modules, building up the environment
  modules <- runIO $ do
    setCurrentDirectory (oldDir </> testDir)
    
    -- Process modules in order, accumulating environment
    let processModule (accEnv, accModules) modName = do
          let act_file = "src" </> modName ++ ".act"
          src <- readFile act_file
          let base = takeBaseName act_file
          parsed <- P.parseModule (S.modName [base]) act_file src
          env <- Acton.Env.mkEnv [sysTypesPath] accEnv parsed
          kchecked <- Acton.Kinds.check env parsed
          (nmod, tchecked, typeEnv) <- Acton.Types.reconstruct "" env kchecked
          let S.NModule tenv mdoc = nmod
          -- The new environment includes the processed module
          return (env, accModules ++ [(modName, parsed, nmod)])
    
    (finalEnv, mods) <- foldM processModule (env0, []) moduleNames
    setCurrentDirectory oldDir
    return mods
  
  -- Generate documentation for each module
  forM_ modules $ \(modName, parsed, nmod) -> do
    describe modName $ do
      it "generates ASCII documentation (plain)" $ do
        let asciiDoc = DocP.printAsciiDoc False nmod parsed
        goldenTextFile (testDir </> goldenDir </> modName ++ ".txt") $ return $ T.pack asciiDoc

      it "generates ASCII documentation (styled)" $ do
        let asciiDoc = DocP.printAsciiDoc True nmod parsed
        goldenTextFile (testDir </> goldenDir </> modName ++ "-color.txt") $ return $ T.pack asciiDoc

      it "generates Markdown documentation" $ do
        let mdDoc = DocP.printMdDoc nmod parsed
        goldenTextFile (testDir </> goldenDir </> modName ++ ".md") $ return $ T.pack mdDoc

      it "generates HTML documentation" $ do
        let htmlDoc = DocP.printHtmlDoc nmod parsed
        goldenTextFile (testDir </> goldenDir </> modName ++ ".html") $ return $ T.pack htmlDoc

parseAct env0 act_file = do
  let dir = takeDirectory act_file
      base = takeBaseName act_file
      sysTypesPath = ".." </> ".." </> "dist" </> "base" </> "out" </> "types"

  src <- liftIO $ readFile act_file
  parsed <- liftIO $ P.parseModule (S.modName [base]) act_file src
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
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "2-kinds"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed

  genTests "Kinds Check" dir testname parsed kchecked

-- pass 3 Type check
testTypes env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "3-types"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
  let S.NModule tenv mdoc = nmod

  genTests "Type Check" dir testname kchecked tchecked

-- pass 4 Normalizer
testNorm env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "4-normalizer"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked

  genTests "Normalizer" dir testname tchecked normalized

-- pass 5 Deactorizer
testDeact env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "5-deactorizer"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized

  genTests "Deactorizer" dir testname normalized deacted

-- pass 6 CPS
testCps env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "6-cps"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, _) <- liftIO $ Acton.CPS.convert deactEnv deacted

  genTests "CPS" dir testname deacted cpstyled

-- pass 7 Lambda Lifting
testLL env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "7-lambdalifting"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, cpsEnv) <- liftIO $ Acton.CPS.convert deactEnv deacted
  (lifted,liftEnv) <- liftIO $ Acton.LambdaLifter.liftModule cpsEnv cpstyled

  genTests "Lambda Lifting" dir testname cpstyled lifted

-- pass 8 Boxing
testBoxing env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "8-boxing"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
  let S.NModule tenv mdoc = nmod
  (normalized, normEnv) <- liftIO $ Acton.Normalizer.normalize typeEnv tchecked
  (deacted, deactEnv) <- liftIO $ Acton.Deactorizer.deactorize normEnv normalized
  (cpstyled, cpsEnv) <- liftIO $ Acton.CPS.convert deactEnv deacted
  (lifted,liftEnv) <- liftIO $ Acton.LambdaLifter.liftModule cpsEnv cpstyled
  boxed <- liftIO $ Acton.Boxing.doBoxing liftEnv lifted

  genTests "Boxing" dir testname lifted boxed

-- pass 9 CodeGen
testCodeGen env0 testname = do
  let act_file = "test" </> "src" </> testname ++ ".act"
      dir      = "test" </> "9-codegen"

  (env, parsed) <- parseAct env0 act_file

  kchecked <- liftIO $ Acton.Kinds.check env parsed
  (nmod, tchecked, typeEnv) <- liftIO $ Acton.Types.reconstruct "" env kchecked
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

