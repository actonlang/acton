module DeclarativeParserSpec (spec, main) where

import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import qualified Acton.Env as Env
import Acton.Printer ()
import Utils (SrcLoc(..), prstr)
import qualified Control.Exception as E
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Syd
import Text.Megaparsec (ParseErrorBundle)

main :: IO ()
main = sydTest spec

spec :: Spec
spec = describe "Declarative body grammars" $ do
  it "accepts actor bindings, signatures, methods and a terminal startup" $
    accepted $ unlines
      [ "actor Example(arg: int):"
      , "    \"\"\"Actor documentation\"\"\""
      , "    first, second = (arg, 2)"
      , "    var state: int = 0"
      , "    read: () -> int"
      , "    def read() -> int:"
      , "        return state"
      , "    proc def __startup__():"
      , "        for item in [first, second]:"
      , "            state += item"
      , "    __startup__()"
      , "class Following(object): pass"
      ]
  forM_ [ "actor Example(): pass"
        , "actor Example(): \"doc\"; var state = 0; value = 1"
        , "actor Example(): value = 1; __startup__()"
        , "actor Example(): \"doc\"; __startup__(); # trailing comment"
        , "actor Example():\n    def __startup__(): pass\n    __startup__();\n\n# comment\nclass Following(object): pass"
        , "actor Example():\n    value = await function()"
        ] $ \source -> it ("accepts " ++ takeWhile (/='\n') source) $ accepted source
  forM_ forbidden $ \statement -> do
    it ("rejects an actor-body " ++ firstLine statement) $
      rejected $ body "actor Example():" statement
    it ("rejects a module-level " ++ firstLine statement) $ rejected statement
  forM_ [ "var value"
        , "value = 1\nvalue = 2"
        , "var value = 1\nvalue = 2"
        , "value = 1\nvar value = 2"
        , "first, first = (1, 2)"
        , "_ = function()"
        , "[] = []"
        , "var _ = function()"
        , "class Nested(object): pass"
        ] $ \statement -> it ("rejects invalid actor declarations: " ++ firstLine statement) $
          rejected $ body "actor Example():" statement
  forM_ [ "__startup__()\nvalue = 1"
        , "__startup__()\npass"
        , "__startup__()\ndef later(): pass"
        , "__startup__(); value = 1"
        , "__startup__(); __startup__()"
        , "__startup__(1)"
        , "__startup__ = function\n__startup__(arg=1)"
        , "self.__startup__()"
        , "async __startup__()"
        , "await __startup__()"
        ] $ \statement -> it ("rejects invalid startup tails: " ++ firstLine statement) $
          rejected $ body "actor Example():" statement
  it "diagnoses a nonterminal startup at the body boundary" $ do
    errors <- failures $ body "actor Example():" "__startup__()\npass"
    forM_ errors $ \err -> err `shouldSatisfy` isInfixOf "StartupMustBeLast"
  forM_ ["class Example(object):", "protocol Example:", "extension value (Eq):"] $ \header -> do
    it ("accepts signatures and full method bodies in " ++ header) $
      accepted $ body header $ unlines
        [ "\"doc\""
        , "method: () -> None"
        , "def method(self):"
        , "    if True:"
        , "        for item in []:"
        , "            print(item)"
        , "pass"
        ]
    it ("accepts a native stub in " ++ header) $ accepted $ body header "NotImplemented"
    forM_ (forbidden ++ ["value = 1", "var value = 1", "__startup__()", "class Nested(object): pass"]) $ \statement ->
      it ("rejects " ++ firstLine statement ++ " in " ++ header) $ rejected $ body header statement
  it "keeps module bindings and adjacent declaration groups consistent across parsers" $
    accepted $ unlines
      [ "import math"
      , "left, right = (1, 2)"
      , "type Alias = int"
      , "def first(): return second()"
      , "def second(): return left"
      , "value: int"
      , "value = first()"
      , "actor Example(): pass"
      ]
  it "keeps executable suites unrestricted inside methods" $
    accepted $ unlines
      [ "actor Example():"
      , "    def method():"
      , "        def nested(): return 1"
      , "        with resource as item:"
      , "            try:"
      , "                while item:"
      , "                    if nested(): break"
      , "                    continue"
      , "            except Exception:"
      , "                raise ValueError()"
      , "            finally:"
      , "                assert True"
      , "        after 0: function()"
      , "        return"
      ]

  describe "Raise expressions" $ do
    forM_ [ "value = 1 if valid else raise ValueError()"
          , "actor Example(): value = 1 if valid else raise ValueError()"
          , "actor Example(): var value = (raise ValueError()) if invalid else 1"
          , "def f(): return raise ValueError()"
          , "def f(): raise first if condition else second"
          , "value = lambda: raise ValueError()"
          , "value = function(raise ValueError())"
          , "value = [(raise ValueError())]"
          ] $ \source -> it ("accepts " ++ source) $ accepted source
    it "preserves the precedence of a standalone conditional raise" $ do
      e <- initializer "raise first if condition else second"
      case e of
        S.Raise _ S.Cond{} _ -> return ()
        _ -> expectationFailure (show e)
    forM_ [ "1 if valid else raise error"
          , "(raise error) if invalid else 1"
          , "(raise error) + 1"
          , "(raise error) == value"
          , "lambda: raise error"
          ] $ \source -> it ("roundtrips " ++ source) $ do
            e <- initializer source
            printed <- initializer (prstr e)
            printed `shouldBe` e
    forM_ [ "value?.method(raise error)"
          , "value?[raise error]"
          , "value?[0:1](raise error)"
          ] $ \source -> it ("keeps optional-chain exits conditional: " ++ source) $ do
            e <- initializer source
            Env.exprRaises e `shouldBe` False
    let raise = S.eRaise (S.eVar (S.name "error"))
        value = S.eInt 1
    forM_ [ ("raise", raise, True)
          , ("strict operand", S.BinOp NoLoc value S.Plus raise, True)
          , ("conditional alternative", S.Cond NoLoc value (S.eBool True) raise, False)
          , ("both alternatives", S.Cond NoLoc raise (S.eBool True) raise, True)
          , ("short-circuit and", S.BinOp NoLoc value S.And raise, False)
          , ("short-circuit or", S.BinOp NoLoc value S.Or raise, False)
          , ("deferred lambda", S.Lambda NoLoc S.PosNIL S.KwdNIL raise S.fxPure, False)
          ] $ \(label,e,expected) -> it ("recognizes guaranteed exits: " ++ label) $
            Env.exprRaises e `shouldBe` expected

initializer :: String -> IO S.Expr
initializer source = do
  m <- P.parseModuleSerial (S.modName ["raise_expr"]) "raise_expr.act" ("value = " ++ source ++ "\n") Nothing
  case S.mbody m of
    [S.Assign _ _ e] -> return e
    _ -> expectationFailure (show m) >> return S.eNone

forbidden :: [String]
forbidden =
  [ "function()", "object.method()", "object.field = 1", "items[0] = 1"
  , "value += 1", "assert True", "raise ValueError()", "return", "del value"
  , "if True:\n    value = 1\nelse:\n    value = 2"
  , "while True:\n    pass", "for item in []:\n    pass"
  , "try:\n    pass\nexcept Exception:\n    pass"
  , "with resource as item:\n    pass", "after 0: function()"
  ]

body :: String -> String -> String
body header statements = unlines $ header : ["    " ++ l | l <- lines statements]

firstLine :: String -> String
firstLine = takeWhile (/='\n')

parsers :: [S.ModName -> String -> String -> Maybe (Int -> Int -> IO ()) -> IO S.Module]
parsers = [P.parseModuleSerial, P.parseModule]

accepted :: String -> IO ()
accepted source = do
  let src = source ++ "\n"
  asts <- mapM (\parse -> parse (S.modName ["declarative"]) "declarative.act" src Nothing) parsers
  head asts `shouldBe` last asts

failures :: String -> IO [String]
failures source = mapM check parsers
  where
    check parse = do
      result <- E.try $ parse (S.modName ["declarative"]) "declarative.act" (source ++ "\n") Nothing
      case result of
        Right _ -> expectationFailure "Expected a syntax error" >> return ""
        Left err
          | parserException err -> return $ show err
          | otherwise -> expectationFailure ("Unexpected exception: " ++ show err) >> return ""
    parserException err =
      case ( E.fromException err :: Maybe (ParseErrorBundle String P.CustomParseError)
           , E.fromException err :: Maybe P.CustomParseException
           , E.fromException err :: Maybe P.ContextError
           ) of
        (Just _, _, _) -> True
        (_, Just _, _) -> True
        (_, _, Just _) -> True
        _ -> False

rejected :: String -> IO ()
rejected source = failures source >> return ()
