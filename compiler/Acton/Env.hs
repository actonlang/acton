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


type OTEnv                  = [(Name,OType)]

type TEnv                   = [(Name,TSchema)]

type Env                    = [(QName,NameInfo)]

data NameInfo               = NVar    (Maybe TSchema)
                            | NState  (Maybe Type)
                            | NClass  [TBind] [TCon] TEnv
                            | NProto  [TBind] [TCon] TEnv
                            | NExt    [TBind] [TCon] TEnv
                            | NTVar   [TCon]
                            | NModule Env
                            deriving (Show)


instance Pretty OTEnv where
    pretty tenv             = vcat (map pr tenv)
      where pr (n,t)        = pretty n <+> colon <+> pretty t

restrict vs                 = filter ((`elem` vs) . fst)

prune vs                    = filter ((`notElem` vs) . fst)

data OEnv                   = OEnv { venv :: [(Name, Maybe OType)], stvars :: [Name], ret :: Maybe OType }
                            deriving (Eq,Show)
instance Subst OEnv where
    subst s env             = env{ venv = subst s (venv env), ret = subst s (ret env) }
    tyvars env              = tyvars (venv env)++ tyvars (ret env)
    

emptyEnv                    = OEnv { venv = [], stvars = [], ret = Nothing }

reserve vs env              = env { venv = [ (v, Nothing) | v <- vs ] ++ venv env }

define te env               = env { venv = [ (v, Just t) | (v,t) <- te ] ++ venv env }

newstate vs env             = env { stvars = vs }

reserved env v              = lookup v (venv env) == Just Nothing

findVar n env               = case lookup n (venv env) of
                                Nothing       -> Control.Exception.throw $ NameNotFound n
                                Just Nothing  -> Control.Exception.throw $ NameReserved n
                                Just (Just t) -> t

setReturn t env             = env { ret = Just t }

getReturn env               = fromJust $ ret env


----------------------------

builtins                    :: Env
builtins                    = [ 
                                (builtinName "Sequence", NProto [a] [] []),
                                (builtinName "Mapping",  NProto [a,b] [] []),
                                (builtinName "Set",      NProto [a] [] [])
                              ]
  where a:b:c:_             = [ TBind (TV n) [] | n <- tvarSupply ]
        ta:tb:tc:_          = [ TVar NoLoc (TV n) | n <- tvarSupply ]

builtinName n               = qName ["__builtin__",n]


pSequence a                 = TCon NoLoc (TC (builtinName "Sequence") [a])
pMapping a b                = TCon NoLoc (TC (builtinName "Mapping") [a,b])
pSet a                      = TCon NoLoc (TC (builtinName "Set") [a])

old_builtins                = [
                                (nm "record", schema [a] []
                                            (OFun ONil a (ORecord a))),
                                (nm "merge", schema [a,b,c] []
                                            (OFun ONil (OPos a (OPos b ONil)) c)),     -- very liberal type, for now!
                                (nm "sorted", schema [a,b] [QIn l0 0 a b]               -- contract requirement (containment)
                                            (OFun ONil (OPos b ONil) b)),
                                (nm "filter", schema [a, b] [QIn l0 0 a b]              -- contract requirement (containment)
                                            (OFun ONil (OPos (OFun ONil (OPos a ONil) OBool) (OPos b ONil)) b)),
                                (nm "id", schema [a] []
                                            (OFun ONil (OPos a ONil) OInt)),
                                (nm "len", schema [a,b] [QIn l0 0 a b]                  -- contract requirement (containment)
                                            (OFun ONil (OPos b ONil) OInt)),
                                (nm "dict", schema [a,b] []
                                            (OFun ONil (OPos (ODict a b) ONil) (ODict a b))),
                                (nm "sorteddict", schema [a,b] []
                                            (OFun ONil (OPos (ODict a b) ONil) (ODict a b))),
                                (nm "defaultdict", schema [a,b] []
                                            (OFun ONil (OPos (OFun ONil ONil b) ONil) (ODict a b))),
                                (nm "list", schema [a] []                               -- contract requirement (containment)
                                            (OFun ONil (OPos (OList a) ONil) (OList a))),
                                (nm "set", schema [a] []                                -- contract requirement (containment)
                                            (OFun ONil (OPos (OList a) ONil) (OSet a))),
                                (nm "sortedset", schema [a] []                                -- contract requirement (containment)
                                            (OFun ONil (OPos (OList a) ONil) (OSet a))),
                                (nm "sum", schema [] []
                                            (OFun ONil (OPos (OList OInt) ONil) OInt)),
                                (nm "enumerate", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) (OList (OTuple (OPos OInt (OPos a ONil)))))),
                                (nm "reversed", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) (OList a))),
                                (nm "min", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) a)),
                                (nm "max", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) a)),
                                (nm "iter", schema [a,b] [QIn l0 0 a b]
                                            (OFun ONil (OPos b ONil) (OList a))),
                                (nm "next", schema [a] []
                                            (OFun ONil (OPos (OList a) ONil) a)),
                                (nm "IPv4Address", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "IPv4Network", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "IPv6Address", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "IPv6Network", schema [] []
                                            (OFun ONil (OPos OStr ONil) OInt)),
                                (nm "Decimal", schema [a,b] []
                                            (OFun ONil (OPos a ONil) b)),
                                (nm "abs", schema [] []
                                            (OFun ONil (OPos OInt ONil) OInt)),
                                (nm "all",  schema [a] []
                                            (OFun ONil a OBool)),
                                (nm "any",  schema [a] []
                                            (OFun ONil a OBool)),
                                (nm "int",  schema [a] []
                                            (OFun ONil a OInt)),
                                (nm "str", schema [a] []
                                            (OFun ONil a OStr)),
                                (nm "print", schema [a] []
                                            (OFun ONil a ONone)),
                                (nm "postpone", schema [a,b] []
                                            (OFun (syncFX ONil) (OPos OInt (OPos (OFun (asyncFX ONil) a (OMsg a)) a)) ONone)),
                                (nm "weakref", schema [a] []
                                            (OFun ONil (OPos a ONil) OInt)),
                                (nm "weakref_subscribe", schema [a, b] []
                                            (OFun ONil (OPos a (OPos b ONil)) ONone)),
                                (nm "weakref_unsubscribe", schema [a] []
                                            (OFun ONil (OPos a ONil) ONone)),
                                (nm "range", schema [] []
                                            (OFun ONil (OPos OInt (OPos OInt ONil)) (OList OInt))),
                                (nm "zip", schema [a,b] []
                                            (OFun ONil (OPos (OList a) (OPos (OList b) ONil)) (OList (OTuple (OPos a (OPos b ONil)))))),
                                (nm "__env__", schema [a] []
                                            (OFun ONil (OPos (ORecord a) ONil) (ORecord (
                                                OKwd (nm "create_external_api_process") 
                                                        (OSchema [[1],[2]] []
                                                            (OFun (syncFX (OVar [2])) (
                                                                OKwd (nm "executable_name") OStr (
                                                                OKwd (nm "args") (OList OStr) (
                                                                OKwd (nm "timeout") OInt (
                                                                OKwd (nm "node") OStr ONil)))) (OVar [1]))) (
                                                OKwd (nm "connect_external_api_process") (OFun (syncFX ONil) (
                                                                OKwd (nm "token") OStr ONil) OInt) ONil))))),
                                (nm "__get_current_placement__", schema [a] []
                                            (OFun (syncFX ONil) (OPos a ONil) (OSet OStr))),
                                (nm "__get_placement_constraints__", schema [a] []
                                            (OFun (syncFX ONil) (OPos a ONil) (OSet OStr))),
                                (nm "__set_placement_constraints__", schema [a] []
                                            (OFun (syncFX ONil) (OPos a (OPos (OSet OStr) ONil)) ONil)),
                                (nm "getattr", schema [a,b] []
                                            (OFun ONil (OPos (ORecord a) (OPos OStr ONil)) b)),
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
                                            (OFun ONil (OPos a (OPos OStr b)) (ORecord (OKwd (nm "actor_id") a ONil))))
                              ]
  where a:b:c:_             = schemaOVars
        schema tvs cs t     = OSchema (unOVar tvs) cs t
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
                                    (ORecord r1,ORecord r2) -> ORecord (cat r1 r2)
        cat ONil r              = r
        cat (OPos t r1) r2      = OPos t (cat r1 r2)
        cat (OStar1 t r1) r2    = OStar1 t (cat r1 r2)
        cat (OKwd n t r1) r2    = OKwd n t (cat r1 r2)
        cat (OStar2 t r1) r2    = OStar2 t (cat r1 r2)

importModule :: (FilePath,FilePath) -> [(QName,OType)] -> OTEnv -> Import -> IO ([(QName,OType)],OTEnv)
importModule impPaths ifaces tenv s@Import{}
                                = modItems ifaces tenv (modules s)
  where modItems ifaces tenv []   = return (ifaces,tenv)
        modItems ifaces tenv (ModuleItem qname mbn : ms)
                                = do (ifaces',t) <- doImport impPaths ifaces qname
                                     modItems ifaces' (blend tenv (maybe [mkenv qname t] (\as -> [(as,t)]) mbn)) ms
        mkenv (QName n vs) t    = (n, mk vs t)
        mk [] t                 = t
        mk (n:ns) t             = ORecord (OKwd n (mk ns t) ONil)
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

openRecord (ORecord row)      = open row
  where open ONil               = []
        open (OKwd n t row)     = (n,t) : open row
        open (OStar2 t row)     = openRecord t ++ open row
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
