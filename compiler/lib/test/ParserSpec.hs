{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import qualified Acton.Printer as AP
import Pretty (print)
import Test.Syd
import qualified Control.Monad.Trans.State.Strict as St
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)

-- Generic parser runner for Acton source code
parseActon :: String -> Either String String
parseActon input =
  case runParser (St.evalStateT P.expr P.initState) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right $ Pretty.print result

-- Helper function to test parsing (just that it succeeds)
testParse :: String -> Spec
testParse input = do
  it (show input) $ do
    case parseActon input of
      Left err -> expectationFailure $ "Parse failed: " ++ err
      Right _ -> pure ()

-- Helper function to test parsing with output validation
testParseOutput :: String -> String -> Spec
testParseOutput input expected = do
  it (show input) $ do
    case parseActon input of
      Left err -> expectationFailure $ "Parse failed: " ++ err
      Right output -> output `shouldBe` expected

main :: IO ()
main = sydTest $ do
  describe "Acton Parser Tests" $ do

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
        testParseOutput "f\"He said \"hello\" to me\"" "\"He said \" % ()"    -- Unescaped quotes
