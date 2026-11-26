module DeclarativeParserSpec (spec, main) where

import qualified Acton.Parser as P
import qualified Acton.Syntax as S
import qualified Acton.Env as Env
import qualified Acton.InitializationAnalysis as Init
import qualified Data.Map.Strict as M
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

  describe "Communication effect" $ do
    let hierarchy =
          [ (S.FXPure, [S.FXPure,S.FXMut,S.FXCom,S.FXProc,S.FXAction])
          , (S.FXMut, [S.FXMut,S.FXProc])
          , (S.FXProc, [S.FXProc])
          , (S.FXAction, [S.FXAction])
          , (S.FXCom, [S.FXCom,S.FXProc])
          ]
    forM_ hierarchy $ \(a,above) -> forM_ hierarchy $ \(b,_) ->
      it ("compares " ++ show a ++ " with " ++ show b) $ do
        Env.subeffect a b `shouldBe` (b `elem` above)
        Env.castEffect a b `shouldBe` ((b `elem` above && (a,b) /= (S.FXPure,S.FXAction)) || (a == S.FXMut && b == S.FXPure))
    it "accepts com annotations and contextual names" $
      accepted "com def com(send: int, spawn: int) -> int: return send + spawn"
    forM_ ["send", "spawn"] $ \word -> do
      it ("accepts " ++ word ++ " as an ordinary function") $
        accepted ("def " ++ word ++ "(): return 1\nvalue = " ++ word ++ "()")
      it ("rejects the retired " ++ word ++ " effect") $
        rejected (word ++ " def f(): pass")

  describe "Actor self-safety" $ do
    let declarations = unlines
          [ "class Failure:"
          , "    def __init__(self, value): self.value = value"
          , "class Explodes:"
          , "    def __init__(self, value): raise Failure(value)"
          , "class RaisesSelf:"
          , "    def __init__(self, value):"
          , "        self.value = value"
          , "        raise self"
          ]
    forM_ [ ("direct raise", "var state = raise Failure(self)", False)
          , ("aliased exception", "error = Failure(self)\nvar state = raise error", False)
          , ("exception stored on self", "error = Failure(self)\nvar state = raise self.error", False)
          , ("throwing constructor", "value = Explodes(self)", False)
          , ("constructor raising itself", "value = RaisesSelf(self)", False)
          , ("cyclic storage", "error = Failure(self)", True)
          , ("method-only use", "def method(): return self", True)
          , ("startup-only use", "proc def __startup__(): raise Failure(self)\n__startup__()", True)
          ] $ \(label,statements,expected) -> it ("checks zero-argument actors: " ++ label) $ do
            ast <- P.parseModuleSerial (S.modName ["self_safety"]) "self_safety.act"
                     (declarations ++ body "actor Example():" statements) Nothing
            let report = Init.analyzeModule ast
                q = S.GName (S.modName ["self_safety"]) (S.name "Example")
            M.lookup q (Init.selfSafety report) `shouldBe` Just expected
            M.lookup q (Init.parameterSafety report) `shouldBe` Just []
    it "uses imported constructor contracts to certify self forwarding" $ do
      provider <- P.parseModuleSerial (S.modName ["provider"]) "provider.act"
        "class Box:\n    def __init__(self, owner): self.owner = owner\n" Nothing
      consumer <- P.parseModuleSerial (S.modName ["consumer"]) "consumer.act"
        "from provider import Box\nactor Example():\n    box = Box(self)\n" Nothing
      let resolve (S.NoQ n) | n == S.name "Box" = S.GName (S.modName ["provider"]) n
          resolve q = q
          q = S.GName (S.modName ["consumer"]) (S.name "Example")
          local = Init.analyzeModules [(consumer,resolve)]
          linked = Init.analyzeModules [(consumer,resolve),(provider,id)]
      M.lookup q (Init.selfSafety local) `shouldBe` Just False
      M.lookup q (Init.selfSafety linked) `shouldBe` Just True

  describe "Advisory class self-safety" $ do
    let storage = "class Box:\n    def __init__(self, value): self.value = value\n"
    forM_ [ ("raising self", "raise self", False)
          , ("raising an alias", "alias = self\nraise alias", False)
          , ("raising a stored reference", "self.saved = self\nraise self.saved", False)
          , ("publishing self", "publish(self)", False)
          , ("observing a cycle", "self.saved = self\nvalue = self.saved.item", False)
          , ("cyclic storage", "self.saved = self", True)
          , ("forwarded cyclic storage", "self.saved = Box(self)", True)
          , ("ordinary initialization", "self.value = 1", True)
          ] $ \(label,statements,expected) -> it label $ do
            ast <- P.parseModuleSerial (S.modName ["class_safety"]) "class_safety.act"
                     (storage ++ body "class Example:" (body "def __init__(self):" statements)) Nothing
            let report = Init.analyzeModule ast
                q = S.GName (S.modName ["class_safety"]) (S.name "Example")
            M.lookup q (Init.selfSafety report) `shouldBe` Just expected
            M.lookup q (Init.parameterSafety report) `shouldBe` Just []
    it "keeps opaque initializers conservative without claiming an Acton self check" $ do
      ast <- P.parseModuleSerial (S.modName ["class_safety"]) "class_safety.act"
               "class Native:\n    def __init__(self, value): NotImplemented\nclass Inherited(Native): pass\n" Nothing
      let report = Init.analyzeModule ast
          q = S.GName (S.modName ["class_safety"]) (S.name "Native")
      M.lookup q (Init.selfSafety report) `shouldBe` Nothing
      M.lookup q (Init.parameterSafety report) `shouldBe` Just [(S.name "value",False)]
    it "checks constructor parameters even after the receiver is initialized" $ do
      ast <- P.parseModuleSerial (S.modName ["class_safety"]) "class_safety.act"
               "class Example:\n    def __init__(self, value):\n        self.ready = True\n        inspect(value)\n" Nothing
      let report = Init.analyzeModule ast
          q = S.GName (S.modName ["class_safety"]) (S.name "Example")
      M.lookup q (Init.parameterSafety report) `shouldBe` Just [(S.name "value",False)]

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
