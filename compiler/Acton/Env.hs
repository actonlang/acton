{-# LANGUAGE FlexibleInstances #-}
module Acton.Env where

import qualified Control.Exception
import Debug.Trace
import Acton.Syntax
import Acton.Names
import Utils
import Pretty
import InterfaceFiles
import Data.Typeable
import Data.Traversable
import System.FilePath.Posix (joinPath)
import System.Directory (doesFileExist)
import Control.Monad

mkEnv impPaths ifaces modul = getImports impPaths ifaces imps
  where Module _ imps _     = modul


type TEnv                   = [(Name,Type)]

instance Pretty TEnv where
    pretty tenv             = vcat (map pr tenv)
      where pr (n,t)        = pretty n <+> colon <+> pretty t

restrict vs                 = filter ((`elem` vs) . fst)

prune vs                    = filter ((`notElem` vs) . fst)

data Env a                  = Env { venv :: [(Name, Maybe a)], stvars :: [Name], ret :: Maybe a } 
                            deriving (Eq,Show)

instance Functor Env where
    fmap f env              = Env (fmap (fmap (fmap f)) (venv env))
                                  (stvars env) 
                                  (fmap f (ret env))

instance Foldable Env where
    foldr f z0 env          = foldr f z1 (catMaybes $ rng $ venv env)
      where z1              = foldr f z0 (ret env)

instance Traversable Env where
    traverse f env          = Env <$> traverse (traverse (traverse f)) (venv env) 
                                  <*> pure (stvars env) 
                                  <*> traverse f (ret env)

instance Subst (Env Type) where
    subst s env             = env{ venv = subst s (venv env), ret = subst s (ret env) }
    tyvars env              = tyvars (venv env)++ tyvars (ret env)
    

emptyEnv                    = Env { venv = [], stvars = [], ret = Nothing }

reserve vs env              = env { venv = [ (v, Nothing) | v <- vs ] ++ venv env }

define te env               = env { venv = [ (v, Just t) | (v,t) <- te ] ++ venv env }

newstate vs env             = env { stvars = vs }

reserved env v              = lookup v (venv env) == Just Nothing

findVar n env               = case lookup n (venv env) of
                                Nothing          -> Control.Exception.throw $ NameNotFound n
                                Just Nothing    -> Control.Exception.throw $ NameReserved n
                                Just (Just t) -> t

setReturn t env             = env { ret = Just t }

getReturn env               = fromJust $ ret env


----------------------------

builtins                    = [ 
                                (nm "record", schema [a] []
                                            (TFun RNil a (TRecord a))),
                                (nm "merge", schema [a,b,c] []
                                            (TFun RNil (RPos a (RPos b RNil)) c)),     -- very liberal type, for now!
                                (nm "sorted", schema [a,b] [CIn l0 0 a b]               -- contract requirement (containment)
                                            (TFun RNil (RPos b RNil) b)),
                                (nm "filter", schema [a, b] [CIn l0 0 a b]              -- contract requirement (containment)
                                            (TFun RNil (RPos (TFun RNil (RPos a RNil) TBool) (RPos b RNil)) b)),
                                (nm "id", schema [a] []
                                            (TFun RNil (RPos a RNil) TInt)),
                                (nm "len", schema [a,b] [CIn l0 0 a b]                  -- contract requirement (containment)
                                            (TFun RNil (RPos b RNil) TInt)),
                                (nm "dict", schema [a,b] []
                                            (TFun RNil (RPos (TDict a b) RNil) (TDict a b))),
                                (nm "sorteddict", schema [a,b] []
                                            (TFun RNil (RPos (TDict a b) RNil) (TDict a b))),
                                (nm "defaultdict", schema [a,b] []
                                            (TFun RNil (RPos (TFun RNil RNil b) RNil) (TDict a b))),
                                (nm "list", schema [a] []                               -- contract requirement (containment)
                                            (TFun RNil (RPos (TList a) RNil) (TList a))),
                                (nm "set", schema [a] []                                -- contract requirement (containment)
                                            (TFun RNil (RPos (TList a) RNil) (TSet a))),
                                (nm "sortedset", schema [a] []                                -- contract requirement (containment)
                                            (TFun RNil (RPos (TList a) RNil) (TSet a))),
                                (nm "sum", schema [] []
                                            (TFun RNil (RPos (TList TInt) RNil) TInt)),
                                (nm "enumerate", schema [a] []
                                            (TFun RNil (RPos (TList a) RNil) (TList (TTuple (RPos TInt (RPos a RNil)))))),
                                (nm "reversed", schema [a] []
                                            (TFun RNil (RPos (TList a) RNil) (TList a))),
                                (nm "min", schema [a] []
                                            (TFun RNil (RPos (TList a) RNil) a)),
                                (nm "max", schema [a] []
                                            (TFun RNil (RPos (TList a) RNil) a)),
                                (nm "iter", schema [a,b] [CIn l0 0 a b]
                                            (TFun RNil (RPos b RNil) (TList a))),
                                (nm "next", schema [a] []
                                            (TFun RNil (RPos (TList a) RNil) a)),
                                (nm "IPv4Address", schema [] []
                                            (TFun RNil (RPos TStr RNil) TInt)),
                                (nm "IPv4Network", schema [] []
                                            (TFun RNil (RPos TStr RNil) TInt)),
                                (nm "IPv6Address", schema [] []
                                            (TFun RNil (RPos TStr RNil) TInt)),
                                (nm "IPv6Network", schema [] []
                                            (TFun RNil (RPos TStr RNil) TInt)),
                                (nm "Decimal", schema [a,b] []
                                            (TFun RNil (RPos a RNil) b)),
                                (nm "abs", schema [] []
                                            (TFun RNil (RPos TInt RNil) TInt)),
                                (nm "all",  schema [a] []
                                            (TFun RNil a TBool)),
                                (nm "any",  schema [a] []
                                            (TFun RNil a TBool)),
                                (nm "int",  schema [a] []
                                            (TFun RNil a TInt)),
                                (nm "str", schema [a] []
                                            (TFun RNil a TStr)),
                                (nm "print", schema [a] []
                                            (TFun RNil a TNone)),
                                (nm "postpone", schema [a,b] []
                                            (TFun (syncFX RNil) (RPos TInt (RPos (TFun (asyncFX RNil) a (TMsg a)) a)) TNone)),
                                (nm "weakref", schema [a] []
                                            (TFun RNil (RPos a RNil) TInt)),
                                (nm "weakref_subscribe", schema [a, b] []
                                            (TFun RNil (RPos a (RPos b RNil)) TNone)),
                                (nm "weakref_unsubscribe", schema [a] []
                                            (TFun RNil (RPos a RNil) TNone)),
                                (nm "range", schema [] []
                                            (TFun RNil (RPos TInt (RPos TInt RNil)) (TList TInt))),
                                (nm "zip", schema [a,b] []
                                            (TFun RNil (RPos (TList a) (RPos (TList b) RNil)) (TList (TTuple (RPos a (RPos b RNil)))))),
                                (nm "__env__", schema [a] []
                                            (TFun RNil (RPos (TRecord a) RNil) (TRecord (
                                                RKwd (nm "create_external_api_process") 
                                                        (TSchema [[1],[2]] []
                                                            (TFun (syncFX (TVar [2])) (
                                                                RKwd (nm "executable_name") TStr (
                                                                RKwd (nm "args") (TList TStr) (
                                                                RKwd (nm "timeout") TInt (
                                                                RKwd (nm "node") TStr RNil)))) (TVar [1]))) (
                                                RKwd (nm "connect_external_api_process") (TFun (syncFX RNil) (
                                                                RKwd (nm "token") TStr RNil) TInt) RNil))))),
                                (nm "__get_current_placement__", schema [a] []
                                            (TFun (syncFX RNil) (RPos a RNil) (TSet TStr))),
                                (nm "__get_placement_constraints__", schema [a] []
                                            (TFun (syncFX RNil) (RPos a RNil) (TSet TStr))),
                                (nm "__set_placement_constraints__", schema [a] []
                                            (TFun (syncFX RNil) (RPos a (RPos (TSet TStr) RNil)) RNil)),
                                (nm "getattr", schema [a,b] []
                                            (TFun RNil (RPos (TRecord a) (RPos TStr RNil)) b)),
                                (nm "Exception", schema [a] []
                                            a),
                                (nm "ArithmeticError", schema [a] []
                                            a),
                                (nm "FloatingPointError", schema [a] []
                                            a),
                                (nm "OverflowError", schema [a] []
                                            a),
                                (nm "ZeroDivisionError", schema [a] []
                                            a),
                                (nm "AssertionError", schema [a] []
                                            a),
                                (nm "AttributeError", schema [a] []
                                            a),
                                (nm "LookupError", schema [a] []
                                            a),
                                (nm "IndexError", schema [a] []
                                            a),
                                (nm "KeyError", schema [a] []
                                            a),
                                (nm "ReferenceError", schema [a] []
                                            a),
                                (nm "TypeError", schema [a] []
                                            a),
                                (nm "ValueError", schema [a] []
                                            a),
                                (nm "StaleActorReferenceException", schema [a,b] []
                                            (TFun RNil (RPos a (RPos TStr b)) (TRecord (RKwd (nm "actor_id") a RNil))))
                              ]
  where a:b:c:_             = schemaTVars
        schema tvs cs t     = TSchema (unTVar tvs) cs t
        nm s                = Name l0 s


-- Import handling

getImports impPaths ifaces imps = getImp ifaces [] imps
  where getImp ifaces tenv []   = return (ifaces,tenv)
        getImp ifaces tenv (imp:imps)
                                = do (ifaces',tenv') <- importModule impPaths ifaces tenv imp 
                                     getImp ifaces' tenv' imps

-- NOTE: blend is still only approximate...

blend te []                     = te
blend te ((n,t):itms)
          | null hit            = blend ((n,t):te) itms
--          | otherwise           = blend ((n,mix t t'):te0) te1
          | otherwise           = blend (te0 ++ (n,mix t t'):te1) itms
  where (te0,hit)               = break ((==n) . fst) te
        (_,t'):te1              = hit
        mix t t'
          | t == t'             = t
          | otherwise           = case (t,t') of
                                    (TRecord r1,TRecord r2) -> TRecord (cat r1 r2)
        cat RNil r              = r
        cat (RPos t r1) r2      = RPos t (cat r1 r2)
        cat (RStar1 t r1) r2    = RStar1 t (cat r1 r2)
        cat (RKwd n t r1) r2    = RKwd n t (cat r1 r2)
        cat (RStar2 t r1) r2    = RStar2 t (cat r1 r2)

importModule :: (FilePath,FilePath) -> [(QName,Type)] -> TEnv -> Import -> IO ([(QName,Type)],TEnv)
importModule impPaths ifaces tenv s@Import{}
                                = modItems ifaces tenv (modules s)
  where modItems ifaces tenv []   = return (ifaces,tenv)
        modItems ifaces tenv (ModuleItem qname mbn : ms)
                                = do (ifaces',t) <- doImport impPaths ifaces qname
                                     modItems ifaces' (blend tenv (maybe [mkenv qname t] (\as -> [(as,t)]) mbn)) ms
        mkenv (QName n vs) t    = (n, mk vs t)
        mk [] t                 = t
        mk (n:ns) t             = TRecord (RKwd n (mk ns t) RNil)
importModule impPaths ifaces tenv s@FromImport{modul=ModRef (0,Just qname)}
                                = do (ifaces',t) <- doImport impPaths ifaces qname
                                     let tenv' = openRecord t
                                     return (ifaces',blend tenv [ pickItem tenv' item | item <- items s ])
  where pickItem tenv i@(ImportItem n as) = case lookup n tenv of
                                             Nothing -> err (loc n) ("Module " ++ render (pretty qname) ++ " does not export " ++ nstr n)
                                             Just t -> (maybe n id as,t)
importModule impPaths ifaces tenv s@FromImportAll{modul=ModRef (0,Just qname)}
                                = do  (ifaces',t) <- doImport impPaths ifaces qname
                                      return (ifaces',blend tenv (openRecord t))
importModule _ _ _      s       = illegalImport (loc s)

openRecord (TRecord row)      = open row
  where open RNil               = []
        open (RKwd n t row)     = (n,t) : open row
        open (RStar2 t row)     = openRecord t ++ open row
openRecord t                    = error "Internal: openRecord"

doImport (path,sysPath) ifaces qname
                                = case lookup qname ifaces of
                                      Just t -> return (ifaces,t)
                                      Nothing -> do
                                         found <- doesFileExist fpath
                                         if found
                                          then do t <- InterfaceFiles.readFile fpath
                                                  return ((qname,t):ifaces,t)
                                          else do found <- doesFileExist fpath2
                                                  unless found (fileNotFound qname)
                                            -- traceM ("# import " ++ prstr qname)
                                                  t <- InterfaceFiles.readFile  fpath2
                                                  return ((qname,t):ifaces,t)
  where fpath                   = joinPath (path : qpath qname) ++ ".ty"
        fpath2                  = joinPath (sysPath : qpath qname) ++ ".ty"
        qpath (QName n ns)      = nstr n : map nstr ns

-- Error handling ------------------------------------------------------------------------

data CheckerError               = FileNotFound QName
                                | NameNotFound Name
                                | NameReserved Name
                                | IllegalImport SrcLoc
                                | DuplicateImport Name
                                | OtherError SrcLoc String
                                deriving (Show)

instance Control.Exception.Exception CheckerError

checkerError (FileNotFound qname)       = (loc qname," Type interface file not found for " ++ render (pretty qname))
checkerError (NameNotFound n)           = (loc n," Name " ++ prstr n ++ " not in scope")
checkerError (NameReserved n)           = (loc n," Name " ++ prstr n ++ " not yet defined")
checkerError (IllegalImport l)          = (l," Relative import not supported")
checkerError (DuplicateImport n)        = (loc n," Duplicate import of name " ++ prstr n)
checkerError (OtherError l str)         = (l,str)

nameNotFound n                          = Control.Exception.throw $ NameNotFound n

fileNotFound qname                      = Control.Exception.throw $ FileNotFound qname

illegalImport loc                       = Control.Exception.throw $ IllegalImport loc

duplicateImport n                       = Control.Exception.throw $ DuplicateImport n

err l s                                 = Control.Exception.throw $ OtherError l s

err1 x s                                = err (loc x) (s ++ " " ++ prstr x)

err2 (x:_) s                            = err1 x s
