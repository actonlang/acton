-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Acton.Normalizer where

import Acton.Syntax
import Acton.Names
import Acton.NameInfo
import Acton.Env
import Acton.QuickType
import Acton.Prim
import Acton.Builtin
import Acton.Transform (termsubst)
import Data.List
import Pretty
import Utils
import Control.Monad.State.Strict
import Debug.Trace

normalize                           :: Env0 -> Module -> IO (Module, Env0)
normalize env0 m                    = return (evalState (norm env m) (0,[]), env0')
  where env                         = normEnv env0
        env0'                       = convertModules (const []) (convEnv env) env0


--  Normalization:
--  X All module aliases are replaced by their original module name
--  X All parameters are positional
--  X Comprehensions are translated into loops
--  X String literals are concatenated and delimited by double quotes
--  X Tuple (and list) patterns are replaced by a var pattern followed by explicit element assignments
--  - With statemenmts are replaced by enter/exit prim calls + exception handling
--  X The assert statement is replaced by a prim call ASSERT
--  X Return without argument is replaced by return None
                                                                                                                 --  - The else branch of a while loop is replaced by an explicit if statement enclosing the loop
--  X Superclass lists are transitively closed


-- Normalizing monad
type NormM a                        = State (Int,[(Name,PosPar,Expr)]) a

newName                             :: String -> NormM Name
newName s                           = do (n,ts) <- get
                                         put (n+1,ts)
                                         return $ Internal NormPass s n

addComp                             :: (Name,PosPar,Expr) -> NormM ()
addComp (n,p,e)                     = state (\(i,ts) -> ((),(i,(n,p,e):ts)))

getComps                            :: NormM [(Name,PosPar,Expr)]
getComps                            = state (\(n,ts) -> (ts, (n,[])))

type NormEnv                        = EnvF NormX

data NormX                          = NormX {
                                        marksX :: [ContextMark],
                                        rtypeX :: Maybe Type,
                                        localScopeX :: Bool,
                                        capturedLocalsX :: [Name],
                                        lambdavarsX :: PosPar,
                                        classattrsX :: [Name],
                                        selfparamX :: Maybe Name
                                      }

data ContextMark                    = DROP | LOOP | FINAL deriving (Eq,Show)

setMarks c env                      = modX env $ \x -> x{ marksX = c }

pushMark c env                      = modX env $ \x -> x{ marksX = c :  marksX x }

marks env                           = marksX $ envX env

setRet t env                        = modX env $ \x -> x{ rtypeX = t }

getRet env                          = fromJust $ rtypeX $ envX env

enterFunctionScope ns env          = modX env $ \x -> x{ localScopeX = True,
                                                          capturedLocalsX = nub (ns ++ capturedLocalsX x) }

enterActorScope ns env             = modX env $ \x -> x{ localScopeX = True,
                                                          capturedLocalsX = nub (ns ++ capturedLocalsX x) }

advanceLocalEnv s env
  | localScopeX (envX env)          = modX env1 $ \x -> x{ capturedLocalsX = nub (bound s ++ capturedLocalsX x) }
  | otherwise                       = env1
  where env1                        = define (envOf s) env

advanceLocalSuite ss env           = foldl (flip advanceLocalEnv) env ss

addLambdavars p env                 = modX env $ \x -> x{ lambdavarsX = joinP (lambdavarsX x) p }
  where joinP PosNIL p              = p
        joinP (PosPar n mbt mbe p') p
                                    = PosPar n mbt mbe (joinP p' p)

getLambdavars env                   = lambdavarsX $ envX env

classattrs env                      = classattrsX $ envX env

selfparam env                       = selfparamX $ envX env

setClassAttrs ns env                = modX env $ \x -> x{ classattrsX = ns }

setSelfParam n env                  = modX env $ \x -> x{ selfparamX = Just n }

normEnv env0                        = setX env0 NormX{ marksX = [], rtypeX = Nothing, localScopeX = False,
                                                      capturedLocalsX = [],
                                                      lambdavarsX = PosNIL, classattrsX = [], selfparamX = Nothing }


-- Normalize terms ---------------------------------------------------------------------------------------

-- A generator bound to a local name need not lose its fusible structure.  If
-- the name has exactly one use, that use is one of the consumers handled
-- below, and the name is never rebound, retain the GeneratorExpr until its
-- consumer.  The outer iterator is still evaluated at the original assignment
-- point.  Ordinary function locals used by its deferred clauses are also
-- copied there, preserving Acton's capture-by-value closure semantics.  This
-- includes actor state: the deactorizer samples actor fields used freely by a
-- lambda when that lambda is constructed, and a forwarded generator must do
-- the same.  Only the plan which consumes these saved values moves forward.
-- Every less obvious case keeps the ordinary lazy iterator object.
forwardLocalGenerators             :: NormEnv -> Suite -> NormM Suite
forwardLocalGenerators env ss
  | not $ localScopeX $ envX env    = return ss
forwardLocalGenerators _ []        = return []
forwardLocalGenerators env (s:ss)
  | Just (n,gen) <- localGeneratorBinding s,
    localUses n ss == 1,
    n `notElem` assigned ss,
    Just (before,consumer,after,consumerEnv) <- findLocalGeneratorConsumer (advanceLocalEnv s env) n gen ss
                                    = do stored <- storeGeneratorSource env gen
                                         case stored of
                                             Just (sourceInits,gen') ->
                                                 case replaceLocalGeneratorConsumer consumerEnv n gen' consumer of
                                                     Just consumer' -> do
                                                         rest <- forwardLocalGenerators (advanceLocalSuite sourceInits env)
                                                                        (before ++ (consumer' : after))
                                                         return (sourceInits ++ rest)
                                                     Nothing -> keep
                                             Nothing -> keep
  where keep                        = do rest <- forwardLocalGenerators (advanceLocalEnv s env) ss
                                         return (s : rest)
forwardLocalGenerators env (s:ss)  = do
    rest <- forwardLocalGenerators (advanceLocalEnv s env) ss
    return (s : rest)

localGeneratorBinding             :: Stmt -> Maybe (Name,Expr)
localGeneratorBinding (Assign _ [PVar _ n _] gen@GeneratorExpr{})
                                    = Just (n,gen)
localGeneratorBinding _            = Nothing

localUses                          :: Name -> Suite -> Int
localUses n                        = length . filter (== n) . free

expressionUses                     :: Name -> Expr -> Int
expressionUses n                   = length . filter (== n) . free

storeGeneratorSource              :: NormEnv -> Expr -> NormM (Maybe (Suite,Expr))
storeGeneratorSource env gen@GeneratorExpr{}
                                    = do sourceName <- newName "gen_source"
                                         captureNames <- mapM captureName captures
                                         let sub = zip captures (map eVar captureNames)
                                             captureInits = zipWith captureInit captures captureNames
                                             gen' = termsubst sub gen
                                             captureEnv = advanceLocalSuite captureInits env
                                         return $ case gen' of
                                             GeneratorExpr l elem (CompFor cl p source co) ->
                                                 let sourceInit = sAssign (pVar sourceName $ typeOf captureEnv source) source
                                                     co' = CompFor cl p (eVar sourceName) co
                                                 in Just (captureInits ++ [sourceInit], GeneratorExpr l elem co')
                                             _ -> Nothing
  where captures                    = filter (not . isWitness) $
                                      nub (free gen) `intersect` capturedLocalsX (envX env)
        captureName _               = newName "gen_capture"
        captureInit old new         = sAssign (pVar new $ typeOf env (eVar old)) (eVar old)
storeGeneratorSource _ _           = return Nothing

findLocalGeneratorConsumer        :: NormEnv -> Name -> Expr -> Suite -> Maybe (Suite,Stmt,Suite,NormEnv)
findLocalGeneratorConsumer _ _ _ [] = Nothing
findLocalGeneratorConsumer env n gen (s:ss)
  | localUses n [s] == 0           = do
      (before,consumer,after,consumerEnv) <-
          findLocalGeneratorConsumer (advanceLocalEnv s env) n gen ss
      return (s:before,consumer,after,consumerEnv)
  | localUses n [s] == 1,
    Just _ <- replaceLocalGeneratorConsumer env n gen s
                                    = Just ([],s,ss,env)
  | otherwise                       = Nothing

replaceLocalGeneratorConsumer     :: NormEnv -> Name -> Expr -> Stmt -> Maybe Stmt
replaceLocalGeneratorConsumer env n gen (Assign l ps e)
  | localGeneratorConsumer env n gen e
                                    = Just $ Assign l ps e'
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (MutAssign l target e)
  | localGeneratorConsumer env n gen e
                                    = Just $ MutAssign l target e'
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (AugAssign l target op e)
  | localGeneratorConsumer env n gen e
                                    = Just $ AugAssign l target op e'
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (Assert l e msg)
  | localGeneratorConsumer env n gen e
                                    = Just $ Assert l e' msg
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (Expr l e)
  | localGeneratorConsumer env n gen e
                                    = Just $ Expr l e'
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (Return l (Just e))
  | localGeneratorConsumer env n gen e
                                    = Just $ Return l (Just e')
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (Raise l e)
  | localGeneratorConsumer env n gen e
                                    = Just $ Raise l e'
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen (VarAssign l ps e)
  | localGeneratorConsumer env n gen e
                                    = Just $ VarAssign l ps e'
  where e'                         = termsubst [(n,gen)] e
replaceLocalGeneratorConsumer env n gen s@(If _ bs _)
  | any branchConsumer bs          = Just $ termsubst [(n,gen)] s
  where branchConsumer (Branch e _)= localGeneratorConsumer env n gen e
replaceLocalGeneratorConsumer _ n gen (For l p source body els)
  | Just _ <- forGeneratorExpr source'
                                    = Just $ For l p source' body els
  where source'                    = termsubst [(n,gen)] source
replaceLocalGeneratorConsumer _ _ _ _ = Nothing

-- Follow the unique use through expression forms which evaluate their
-- children eagerly.  In particular, do not cross lambdas, comprehensions,
-- conditional expressions, or the short-circuiting and/or operators.  Once
-- the use reaches the iterable argument of a supported builtin, substitution
-- exposes the same direct fusion path used by a literal generator expression.
localGeneratorConsumer            :: NormEnv -> Name -> Expr -> Expr -> Bool
localGeneratorConsumer env n gen e
  | expressionUses n e /= 1        = False
localGeneratorConsumer env n gen e@Call{}
  | directLocalGeneratorConsumer env n gen e
                                    = True
localGeneratorConsumer env n gen (Call _ f p k)
                                    = localGeneratorConsumer env n gen f ||
                                      localGeneratorConsumerPos env n gen p ||
                                      localGeneratorConsumerKwd env n gen k
localGeneratorConsumer env n gen (TApp _ e _)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (Index _ e i)
                                    = localGeneratorConsumer env n gen e ||
                                      localGeneratorConsumer env n gen i
localGeneratorConsumer env n gen (IsInstance _ e _)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (BinOp _ e1 op e2)
  | op `notElem` [And,Or]          = localGeneratorConsumer env n gen e1 ||
                                      localGeneratorConsumer env n gen e2
localGeneratorConsumer env n gen (UnOp _ _ e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (Dot _ e _)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (Rest _ e _)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (DotI _ e _)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (RestI _ e _)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (Tuple _ p k)
                                    = localGeneratorConsumerPos env n gen p ||
                                      localGeneratorConsumerKwd env n gen k
localGeneratorConsumer env n gen (List _ es)
                                    = any (localGeneratorConsumerElem env n gen) es
localGeneratorConsumer env n gen (Dict _ as)
                                    = any (localGeneratorConsumerAssoc env n gen) as
localGeneratorConsumer env n gen (Set _ es)
                                    = any (localGeneratorConsumerElem env n gen) es
localGeneratorConsumer env n gen (Paren _ e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (Box _ e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer env n gen (UnBox _ e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumer _ _ _ _     = False

localGeneratorConsumerPos         :: NormEnv -> Name -> Expr -> PosArg -> Bool
localGeneratorConsumerPos env n gen (PosArg e p)
                                    = localGeneratorConsumer env n gen e ||
                                      localGeneratorConsumerPos env n gen p
localGeneratorConsumerPos env n gen (PosStar e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumerPos _ _ _ PosNil = False

localGeneratorConsumerKwd         :: NormEnv -> Name -> Expr -> KwdArg -> Bool
localGeneratorConsumerKwd env n gen (KwdArg _ e k)
                                    = localGeneratorConsumer env n gen e ||
                                      localGeneratorConsumerKwd env n gen k
localGeneratorConsumerKwd env n gen (KwdStar e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumerKwd _ _ _ KwdNil = False

localGeneratorConsumerElem        :: NormEnv -> Name -> Expr -> Elem -> Bool
localGeneratorConsumerElem env n gen (Elem e)
                                    = localGeneratorConsumer env n gen e
localGeneratorConsumerElem env n gen (Star e)
                                    = localGeneratorConsumer env n gen e

localGeneratorConsumerAssoc       :: NormEnv -> Name -> Expr -> Assoc -> Bool
localGeneratorConsumerAssoc env n gen (Assoc key value)
                                    = localGeneratorConsumer env n gen key ||
                                      localGeneratorConsumer env n gen value
localGeneratorConsumerAssoc env n gen (StarStar e)
                                    = localGeneratorConsumer env n gen e

directLocalGeneratorConsumer      :: NormEnv -> Name -> Expr -> Expr -> Bool
directLocalGeneratorConsumer env n gen call@(Call _ f p k)
  | Just input <- generatorConsumerInput env f args,
    expressionUses n input == 1    = directGeneratorConsumer env $ termsubst [(n,gen)] call
  where args                       = joinArg p k
directLocalGeneratorConsumer _ _ _ _ = False

generatorConsumerInput            :: NormEnv -> Expr -> PosArg -> Maybe Expr
generatorConsumerInput env f args
  | any (\builtin -> isBuiltinFunction env (name builtin) f)
        ["sum","max","min","max_def","min_def","set","dict"]
                                    = item 2
  | any (\builtin -> isBuiltinFunction env (name builtin) f) ["any","all","list"]
                                    = item 1
  | otherwise                       = Nothing
  where item i                      = do es <- fixedPosArgs args
                                         if i < length es then Just (es !! i) else Nothing

directGeneratorConsumer           :: NormEnv -> Expr -> Bool
directGeneratorConsumer env (Paren _ e)
                                    = directGeneratorConsumer env e
directGeneratorConsumer env (Call l f p k)
                                    = present (sumGeneratorCall env f args) ||
                                      present (boolGeneratorCall env f args) ||
                                      present (extremumGeneratorCall env f args) ||
                                      present (collectionGeneratorComp env l f args)
  where args                       = joinArg p k
        present Nothing            = False
        present (Just _)           = True
directGeneratorConsumer _ _        = False

-- Comprehensions deferred while normalizing an enclosing statement's expressions
-- (e.g. a branch condition) must be materialized before that statement, not
-- inside a nested suite of it, so shield any comprehensions pending on entry
-- from the getComps drain below.
normSuite env ss                    = do pending <- getComps
                                         ss0 <- forwardLocalGenerators env ss
                                         ss' <- normSuite' env ss0
                                         mapM_ addComp (reverse pending)
                                         return ss'

normSuite' env []                   = return []
normSuite' env (s : ss)             = do s' <- norm' env s
                                         comps <- getComps
                                         ss' <- normSuite' (advanceLocalEnv s env) ss
                                         defs <- mapM mkCompFun comps
                                         return (concat defs ++ s' ++ ss')
  where mkCompFun (f,lambound,comp) = do w <- newName "w"
                                         r <- newName "res"
                                         let env0 = define (envOf lambound) env
                                             fx = fxOf env0 comp
                                             (tw,w1,tr,e0,stmt) = transComp env0 w r comp
                                             body = sAssign (pVar w tw) w1 :
                                                    sAssign (pVar r tr) e0 :
                                                    stmt :
                                                    sReturn (eVar r) : []
                                         norm' env (sDef f lambound tr body fx)

        transComp env w r (ListComp _ (Elem e) co)
                                    = (tw, w1, tr, e0, compStmt co e1)
          where env1                = define (envOf co) env
                te                  = typeOf env1 e
                tr                  = tList te
                tw                  = tSequenceW tr te
                e0                  = List NoLoc []
                e1                  = eCall (eDot (eVar w) appendKW) [eVar r, e]
                w1                  = eCall (tApp (eQVar witSequenceList) [te]) []
        transComp env w r (SetComp _ (Elem annot_e) co)
                                    = (tw, w1, tr, e0, compStmt co e1)
          where env1                = define (envOf co) env
                te                  = typeOf env1 annot_e
                tr                  = tSet te
                tw                  = tSetW tr te
                (w0, e)             = unAnnot (tHashableW te) annot_e
                e0                  = eCall (tApp (eQVar primMkSet) [te]) [w0, Set NoLoc []]
                e1                  = eCall (eDot (eVar w) (name "add")) [eVar r, e]
                w1                  = eCall (tApp (eQVar witSetSet) [te]) [w0]
        transComp env w r (DictComp _ (Assoc annot_k v) co)
                                    = (tw, w1, tr, e0, compStmt co e1)
          where env1                = define (envOf co) env
                tk                  = typeOf env1 annot_k
                tv                  = typeOf env1 v
                tr                  = tDict tk tv
                tw                  = tMappingW tr tk tv
                (w0, k)             = unAnnot (tHashableW tk) annot_k
                e0                  = eCall (tApp (eQVar primMkDict) [tv,tk]) [w0, Dict NoLoc []]
                e1                  = eCall (eDot (eDot (eVar w) (Internal Witness "Indexed" 0)) setitemKW) [eVar r, k, v]
                w1                  = eCall (tApp (eQVar witMappingDict) [tk,tv]) [w0]

        compStmt (CompFor l p e c) x = For l p e [compStmt c x] []
        compStmt (CompIf l e c) x   = If l [Branch e [compStmt c x]] []
        compStmt (NoComp) x         = sExpr x


normPat                             :: NormEnv -> Pattern -> NormM (Pattern,Suite)
normPat env (PWild l a)             = do n <- newName "ignore"
                                         return (PVar l n $ conv env a,[])
normPat env (PVar l n a)            = return (PVar l n $ conv env a,[])
normPat env (PParen _ p)            = normPat env p
normPat env p@(PTuple _ pp kp)      = do v <- newName "tup"
                                         ss <- normSuite (define [(v, NVar t)] env) $ normPP v 0 pp ++ normKP v [] kp
                                         return (pVar v $ conv env t, ss)
  where normPP v n (PosPat p pp)    = Assign NoLoc [p] (DotI NoLoc (eVar v) n) : normPP v (n+1) pp
        normPP v n (PosPatStar p)   = [Assign NoLoc [p] (foldl (RestI NoLoc) (eVar v) [0..n-1])]
        normPP _ _ PosPatNil        = []
        normKP v ns (KwdPat n p kp) = Assign NoLoc [p] (Dot NoLoc (eVar v) n) : normKP v (n:ns) kp
        normKP v ns (KwdPatStar p)  = [Assign NoLoc [p] (foldl (Rest NoLoc) (eVar v) (reverse ns))]
        normKP _ _ KwdPatNil        = []
        t                           = typeOf env p
normPat env p@(PList _ ps pt)       = do v <- newName "lst"
                                         ss <- normSuite env $ normList v 0 ps pt
                                         return (pVar v $ conv env t, ss)
  where normList v n (p:ps) pt      = s : normList v (n+1) ps pt
          where s                   = Assign NoLoc [p] (eCall (tApp (eQVar primUGetItem) [te])
                                        [eVar v, Int NoLoc n (show n)])
        normList v n [] (Just p)    = [Assign NoLoc [p] (eCall (eDot sequenceWitness getsliceKW)
                                        [eVar v, eCall (eQVar qnSlice)
                                                       [Int NoLoc n (show n), None NoLoc, None NoLoc]])]
        normList v n [] Nothing     = []
        sequenceWitness             = eCall (tApp (eQVar witSequenceList) [te]) []
        te                          = case unalias env t of
                                          TCon _ (TC c [a]) | c == qnList -> a
                                          t' -> error ("normPat: expected list type, got " ++ prstr t')
        t                           = typeOf env p

plainPosPats                       :: PosPat -> Maybe [Pattern]
plainPosPats (PosPat p@(PVar _ _ _) ps)
                                    = (p :) <$> plainPosPats ps
plainPosPats PosPatNil              = Just []
plainPosPats _                      = Nothing

fixedPosArgs                       :: PosArg -> Maybe [Expr]
fixedPosArgs (PosArg e es)          = (e :) <$> fixedPosArgs es
fixedPosArgs PosNil                 = Just []
fixedPosArgs _                      = Nothing



class Norm a where
    norm                            :: NormEnv -> a -> NormM a
    norm'                           :: NormEnv -> a -> NormM [a]
    norm' env x                     = (:[]) <$> norm env x

instance (Norm a, EnvOf a) => Norm [a] where
    norm env []                     = return []
    norm env (a:as)                 = do as1 <- norm' env a
                                         as2 <- norm env1 as
                                         return (as1++as2)
      where env1                    = define (envOf a) env

instance Norm a => Norm (Maybe a) where
    norm env Nothing                = return Nothing
    norm env (Just a)               = Just <$> norm env a

instance Norm Module where
    norm env (Module m imps mdoc ss) = Module m imps mdoc <$> normSuite env ss

handle env x hs                     = do bs <- sequence [ branch e b | Handler e b <- hs ]
                                         return $ [sIf bs [sExpr $ eCall (eQVar primRAISE) [eVar x]]]
  where branch (ExceptAll _) b      = Branch (eBool True) <$> normSuite env b
        branch (Except _ y) b       = Branch (eIsInstance x y) <$> normSuite env b
        branch (ExceptAs _ y z) b   = Branch (eIsInstance x y) <$> (bind:) <$> normSuite env' b
          where env'                = define [(z,NVar t)] env
                bind                = sAssign (pVar z $ conv env t) (eVar x)
                t                   = tCon $ TC y []

exitContext env s
  | DROP:c <- marks env             = sDROP : exitContext (setMarks c env) s
  | FINAL:c <- marks env            = [sRAISE $ exn s]
  | LOOP:c <- marks env             = if s `elem` [sBreak,sContinue] then [s] else exitContext (setMarks c env) s
  | otherwise                       = [s]
  where exn (Break _)               = eCall (eQVar primBRK) []
        exn (Continue _)            = eCall (eQVar primCNT) []
        exn (Return _ (Just e))     = eCall (eQVar primRET) [e]

sDROP                               = sExpr (eCall (eQVar primDROP) [])
sPOP x                              = sAssign (pVar x tBaseException) (eCall (eQVar primPOP) [])
ePUSH                               = eCall (eQVar primPUSH) []
ePUSHF                              = eCall (eQVar primPUSHF) []
sSEQ                                = sExpr (eCall (eQVar primRAISE) [eCall (eQVar primSEQ) []])
sRAISE e                            = sExpr (eCall (eQVar primRAISE) [e])

-- TODO: maybe less approximation?
isVal Var{}                         = True
isVal Int{}                         = True
isVal Float{}                       = True
isVal Imaginary{}                   = True
isVal Bool{}                        = True
isVal None{}                        = True
isVal Strings{}                     = True
isVal BStrings{}                    = True
isVal Lambda{}                      = True
isVal (Call _ (TApp _ (Var _ n) _) (PosArg e PosNil) KwdNil)
                                    = n == primCAST && isVal e
isVal (TApp _ e _)                  = isVal e
isVal (Dot _ e _)                   = isVal e
isVal (DotI _ e _)                  = isVal e
isVal (Paren _ e)                   = isVal e
isVal _                             = False

instance Norm Stmt where
    norm env (Expr l e)             = Expr l <$> norm env e
    norm env (MutAssign l t e)      = MutAssign l <$> norm env t <*> norm env e
    norm env (Assert l e mbe)       = do e' <- normBool env e
                                         mbe' <- norm env mbe
                                         return $ Expr l $ eCall (eQVar primASSERT) [e', maybe eNone id mbe']
    norm env (Pass l)               = return $ Pass l
    norm env (Raise l e)            = do e' <- norm env e
                                         return $ Expr l $ eCall (eQVar primRAISE) [e']
    norm env (If l bs els)          = If l <$> norm env bs <*> normSuite env els
    norm env (While l e b els)      = While l (eBool True) <$> normSuite (pushMark LOOP env) (sIf1 e [sPass] (els++[sBreak]) : b) <*> return []
    norm env (Data l mbp ss)        = Data l <$> norm env mbp <*> normSuite env ss
    norm env (VarAssign l ps e)     = VarAssign l <$> norm env ps <*> norm env e
    norm env (After l now e e')     = After l now <$> norm env e <*> norm env e'
    norm env (Signature l ns t d)   = return $ Signature l ns (conv env t) d
    norm env s                      = error ("norm unexpected stmt: " ++ prstr s)

    norm' env (Decl l ds)           = do (eqs,ds) <- normDecls env ds
                                         return $ eqs ++ [Decl l ds]

    norm' env (Try l b [] els [])   = normSuite env (b ++ els)
    norm' env (Try l b hs els [])   = do b <- normSuite (pushMark DROP env) b
                                         els <- normSuite (define (envOf b) env) els
                                         x <- newName "x"
                                         hdl <- handle env x hs
                                         return [sIf [Branch ePUSH (b ++ sDROP : els)] (sPOP x : hdl)]
      where ePUSH                   = eCall (eQVar primPUSH) []
    norm' env (Try l b hs els fin)  = do ss <- norm' (pushMark FINAL env) try0
                                         x <- newName "xx"
                                         fin <- normSuite (define [(x,NVar tBaseException)] env) fin
                                         return [sIf [Branch ePUSHF (ss++mbseq)] (sPOP x : fin ++ relays x)]
      where try0                    = Try l b hs els []
            relays x                = iff [ Branch (eIsInstance x n) s | (n,s) <- map (relay x) ctrl, valid s] [sRAISE $ eVar x]
            relay _ SEQ             = (primSEQ, [sPass])
            relay _ BRK             = (primBRK, exitContext env sBreak)
            relay _ CNT             = (primCNT, exitContext env sContinue)
            relay x RET             = (primRET, downcast : exitContext env ret)
              where x'              = Derived x (globalName "RET")
                    downcast        = sAssign (pVar x' tRET) (eCAST tBaseException tRET (eVar x))
                    ret             = sReturn (eCAST tValue (getRet env) (eDot (eVar x') attrVal))
            valid [Expr{}]          = False
            valid [Assign{},Expr{}] = False
            valid _                 = True
            mbseq                   = if SEQ `elem` ctrl then [sSEQ] else []
            ctrl                    = nub (flows try0)
            iff [] els              = els
            iff bs els              = [sIf bs els]

    norm' env s@(Break l)           = return $ exitContext env s
    norm' env s@(Continue l)        = return $ exitContext env s
    norm' env (Return l Nothing)    = return $ exitContext env (Return l $ Just eNone)
    norm' env (Return l (Just e))   = do e <- norm env e
                                         case isVal e of
                                             True -> return $ retContext e
                                             False -> do
                                                 n <- newName "tmp"
                                                 return $ sAssign (pVar n $ conv env t) e : retContext (eVar n)
      where retContext e            = exitContext env $ Return l $ Just e
            t                       = typeOf env e

    -- A fixed tuple literal assigned to a fixed tuple of variables does not
    -- need a tuple object.  Evaluate every right-hand component first, so
    -- swaps and exceptions retain simultaneous-assignment semantics, and only
    -- then update the targets.  Boxing can subsequently give each temporary
    -- its unboxed representation where possible.
    norm' env (Assign l [PTuple _ pp KwdPatNil] (Tuple _ pa KwdNil))
      | Just ps <- plainPosPats pp
      , Just es <- fixedPosArgs pa
      , not (null ps)
      , length ps == length es      = do ns <- mapM (const $ newName "tmp") es
                                         let temps = [ Assign l [pVar n $ typeOf env e] e
                                                     | (n,e) <- zip ns es ]
                                             stores = [ Assign l [p] (eVar n)
                                                      | (p,n) <- zip ps ns ]
                                         normSuite env (temps ++ stores)
    norm' env (Assign l ps e)       = do e' <- norm env e
                                         (ps1,stmts) <- unzip <$> mapM (normPat env) ps
                                         ps2 <- norm env ps1
                                         let p'@(PVar _ n _) : ps' = ps2
                                         return $ Assign l [p'] e' : [ Assign l [p] (eVar n) | p <- ps' ] ++ concat stmts
    norm' env (For _ target source body els)
      | Just (result,co) <- forGeneratorExpr source
                                    = do plan <- iteratorPlan result co
                                         let env1 = define (iteratorPlanEnv plan) env
                                         fusedFor env1 target plan body els >>= normSuite env1
    norm' env s@(For l p e b els)
                                    = do i <- newName "iter"
                                         m <- newName "maybe"
                                         v <- newName "val"
                                         done <- newName "done"
                                         normSuite env (sAssign (pVar i $ conv env t) e : maybeLoop m v i done)
      where t@(TCon _ (TC c [t']))  = expTypeOf env e
            next i                  = eCall (eDot (eVar i) nextKW) []
            maybeBody m v i done    = [sAssign (pVar m (tMaybe t')) (next i),
                                       sIf [Branch (eIsInstance m qnJust) (maybeValBody m v)]
                                           (maybeDone done)]
            maybeValBody m v
               | isPVar p           = sAssign p (maybeVal m) : b
               | otherwise          = sAssign (pVar v t') (maybeVal m) : sAssign p (eVar v) : b
            maybeVal m              = eDot (eCAST (tMaybe t') (tJust t') (eVar m)) attrVal
            maybeDone done
               | null els           = [sBreak]
               | otherwise          = [sAssign (pVar done tBool) (eBool True), sBreak]
            maybeLoop m v i done
               | null els           = [While l (eBool True) (maybeBody m v i done) []]
               | otherwise          = [sAssign (pVar done tBool) (eBool False),
                                       While l (eBool True) (maybeBody m v i done) [],
                                       sIf [Branch (eVar done) els] []]
            isPVar PVar{}           = True
            isPVar _                = False
    {-
    with EXPRESSION as PATTERN:
        SUITE
    ===>
    $mgr = EXPRESSION
    $val = $mgr.__enter__()
    $exc = False
    try:
        PATTERN = $val
        SUITE
    except Exception as ex:
        $exc = True
        if not $mgr.__exit__(ex):
            raise
    finally:
        if not $exc:
            $mgr.__exit__(None)
    -}
    norm' env s@(With l (i:is) b)   = do notYet l s                     -- TODO: remove
                                         m <- newName "mgr"
                                         v <- newName "val"
                                         x <- newName "exc"
                                         (e,mbp,ss) <- normItem env i
                                         b' <- normSuite env1 (ss ++ b)
                                         return undefined
      where env1                    = define (envOf i) env
    norm' env (With l [] b)         = normSuite env b
    norm' env s                     = do s' <- norm env s
                                         return [s']

normItem env (WithItem e Nothing)   = do e' <- norm env e
                                         return (e', Nothing, [])
normItem env (WithItem e (Just p))  = do e' <- norm env e
                                         (p',ss) <- normPat env p
                                         return (e', Just p', ss)

normDecls env ds                    = do (pres, ds) <- unzip <$> mapM (normDecl env1 ns) ds
                                         pre <- normSuite env (concat pres)
                                         return (pre, ds)
      where env1                    = define (envOf ds) env
            ns                      = bound ds

normDecl env ns d@Class{}           = do d <- norm env1 d{ dbody = props ++ body }
                                         pre <- normSuite env pre
                                         return (pre, d)
      where (pre,te,body)           = fixupClassAttrs ns d
            env1                    = define (envOf pre ++ te) $ setClassAttrs (dom te) env
            props                   = [ Signature NoLoc [w] (monotype t) Property | (w,NVar t) <- te ]
normDecl env ns d                   = do d <- norm env d
                                         return ([], d)


-- The type-checker may leave witness bindings on the level of classes, even though our class
-- syntax does not yet support this in the same way as is does for actors. But the creation and
-- reduction of witnesses that mutually depend on classes becomes so much easier if we allow
-- ourselves to make use of this planned feature already today. The code below thus implements
-- class level bindings, albeit limited to witnesses, by transforming them into either global
-- binding prefixes (if the circular class dependencies actually got eliminated during witness
-- reduction), __init__ method locals (if they are only referenced during initialization) or
-- proper instance attributes (in the general case).


--                    dbody
--                   /     \
--                  /       \
--               eqs         defs
--              /   \       /    \
--             /     \   inits   dynamic
--           pre     dep
--                  /   \
--                 /     \
--               attr    local
fixupClassAttrs ns d0@Class{dname=n}
  | null eqs                        = ([], [], defs)
  | otherwise                       = --trace ("### Fixup class " ++ prstr n ++ ":") $
                                      --trace ("  # te:\n" ++ render (nest 8 $ vcat $ map pretty te)) $
                                      --trace ("  # pre: " ++ prstrs (bound pre)) $
                                      --trace ("  # attr: " ++ prstrs (bound attr)) $
                                      --trace ("  # local: " ++ prstrs (bound local)) $
                                      --trace ("  # defs:\n" ++ render (nest 4 $ vcat [ pretty d | Decl _ ds <- defs1, d <- ds, dname d `elem` [initKW, altInit]])) $
                                      (pre, te, defs1)
  where (eqs, defs)                 = split [] [] (dbody d0)
          where
            split eqs defs []       = (reverse eqs, reverse defs)
            split eqs defs (s:ss)   = case s of
                                        Assign _ [PVar _ (Internal Witness _ _) (Just _)] _ ->
                                            split(s:eqs) defs ss
                                        _ ->
                                            split eqs (s:defs) ss

        (dynref, initpar)           = dvars [] [] $ concat [ ds | Decl _ ds <- defs ]
          where
            dvars dyn ini []        = (dyn `intersect` bound eqs, ini)
            dvars dyn ini (d:ds)
              | dname d == initKW   = dvars dyn ([ n | n@(Internal Witness _ _) <- bound (pos d) ] ++ ini) ds
              | otherwise           = dvars (free d ++ dyn) ini ds

        (pre, dep)                  = split ns [] [] eqs
          where
            split ns pre dep []     = (reverse pre, reverse dep)
            split ns pre dep (eq:eqs)
              | null fvs            = split ns (eq:pre) dep eqs
              | otherwise           = split (bound eq ++ ns) pre (eq:dep) eqs
              where fvs             = free (expr eq) `intersect` (initpar++ns)

        (attr, local)               = split [] [] dep
          where
            split attr local []     = (reverse attr, reverse local)
            split attr local (eq:eqs)
              | null fvs            = split attr (eq:local) eqs
              | otherwise           = split (eq:attr) local eqs
              where fvs             = bound eq `intersect` (dynref++free eqs)

        initMeth                    = if altInit `elem` bound defs then altInit else initKW

        defs1                       = map (initS initMeth) defs
          where
            initS n (Decl l ds)     = Decl l $ map (initL . initD n) ds
            initS n s               = s

            initD n d@Def{}
              | dname d == n, Just self <- selfPar d
                                    = d{ dbody = [ sMutAssign (eDot (eVar self) w) e | Assign _ [PVar _ w _] e <- attr ] ++ dbody d }
              | deco d == Static    = d{ dbody = attr ++ dbody d }
            initD n d               = d

            initL d@Def{}
              | dname d == initKW   = d{ dbody = local ++ dbody d }
            initL d                 = d

        te                          = [ (w, NVar t) | Assign _ [PVar _ w (Just t)] _ <- attr ]


instance Norm Decl where
    norm env (Def l n q p k t b d x doc)
                                    = do p' <- joinPar <$> norm env0 p <*> norm (define (envOf p) env0) k
                                         b' <- normSuite env1 b
                                         return $ Def l n q p' KwdNIL (conv env t) (ret b') d x doc
      where env1                    = enterFunctionScope (dom $ envOf p ++ envOf k) $
                                      setMarks [] $ setRet t $ define (envOf p ++ envOf k) env0
            env0                    = defineTVars q env00
            env00                   = case p of
                                        PosPar self _ _ _ | not $ null $ classattrs env, d /= Static ->
                                            setSelfParam self env
                                        _ ->
                                            env
            ret b | fallsthru b     = b ++ [sReturn eNone]
                  | otherwise       = b
    norm env (Actor l n q p k b doc)
                                    = do p' <- joinPar <$> norm env0 p <*> norm (define (envOf p) env0) k
                                         b' <- normSuite env1 b
                                         return $ Actor l n q p' KwdNIL b' doc
      where env1                    = enterActorScope (dom $ envOf p ++ envOf k) $
                                      setMarks [] $ define (envOf p ++ envOf k) env0
            env0                    = define [(selfKW, NVar t0)] $ defineTVars q env
            t0                      = tCon $ TC (NoQ n) (map tVar $ qbound q)
    norm env (Class l n q as b doc) = Class l n q as <$> normSuite env1 b <*> return doc
      where env1                    = defineTVars (selfQuant (NoQ n) q) env
    norm env (Typedef l n q t doc)  = return $ Typedef l n q (conv env t) doc
    norm env d                      = error ("norm unexpected: " ++ prstr d)


catStrings ss                       = map (quote . escape '"') ss
  where escape c []                 = []
        escape c ('\\':x:xs)        = '\\' : x : escape c xs
        escape c (x:xs)
          | x == c                  = '\\' : x : escape c xs
          | otherwise               = x : escape c xs
        quote s                     = '"' : s ++ "\""


normInst env ts e                   = norm env e

normBool env e
  | t == tBool                      = norm env e
  | TOpt _ t' <- t, Var{} <- e      = return $ eBinOp (eCall (tApp (eQVar primISNOTNONE) [t']) [e]) And (eCall (eDot (eCAST t t' e) boolKW) [])
  | BinOp l e1 op e2 <- e,
    op `elem` [And,Or]              = do e1 <- normBool env e1
                                         e2 <- normBool env e2
                                         return $ BinOp l e1 op e2
  | otherwise                       = do e' <- norm env e
                                         return $ eCall (eDot e' boolKW) []
  where t                           = expTypeOf env e

instance Norm Expr where
    norm env (Var l (NoQ n))
      | n `elem` classattrs env,
        Just self <- selfparam env  = return $ eDot (eVar self) n
    norm env (Var l nm)             = return $ Var l nm
    norm env (Int l i s)            = Int l <$> return i <*> return s
    norm env (Float l f s)          = Float l <$> return f <*> return s
    norm env (Imaginary l i s)      = Imaginary l <$> return i <*> return s
    norm env (Bool l b)             = Bool l <$> return b
    norm env (None l)               = return $ None l
    norm env (NotImplemented l)     = return $ NotImplemented l
    norm env (Ellipsis l)           = return $ Ellipsis l
    norm env (Strings l ss)         = return $ Strings l (catStrings ss)
    norm env (BStrings l ss)        = return $ BStrings l (catStrings ss)
    norm env (Call l e p k)
      | Just (w,result,co,start) <- sumGeneratorCall env e (joinArg p k)
                                    = do plan <- iteratorPlan result co
                                         let env1 = define (iteratorPlanEnv plan) env
                                         fusedSum env1 w start plan >>= norm env1
    norm env (Call l e p k)
      | Just (kind,result,co) <- boolGeneratorCall env e (joinArg p k)
                                    = do plan <- iteratorPlan result co
                                         let env1 = define (iteratorPlanEnv plan) env
                                         fusedBool env1 kind plan >>= norm env1
    norm env (Call l e p k)
      | Just (kind,w,result,co,dflt) <- extremumGeneratorCall env e (joinArg p k)
                                    = do plan <- iteratorPlan result co
                                         let env1 = define (iteratorPlanEnv plan) env
                                         fusedExtremum env1 kind w dflt plan >>= norm env1
    norm env (Call l e p k)
      | Just comp <- collectionGeneratorComp env l e (joinArg p k)
                                    = norm env comp
    norm env (Call l e p k)         = Call l <$> norm env e <*> norm env (joinArg p k) <*> pure KwdNil
    norm env (TApp l e ts)          = TApp l <$> normInst env ts e <*> pure (conv env ts)
    norm env (Let l ss e)          = Let l <$> norm env ss <*> norm env1 e
      where env1                    = define (envOf ss) env
    norm env (Dot l (Var l' x) n)
      | NClass{} <- findQName x env = pure $ Dot l (Var l' x) n
    norm env (Dot l e n)
      | TTuple _ p k <- t,
        n `notElem` valueKWs        = DotI l <$> norm env e <*> pure (nargs p + narg n k)
      | otherwise                   = Dot l <$> norm env e <*> pure n
      where t                       = expTypeOf env e
    norm env (Async l e)            = Async l <$> norm env e
    norm env (Await l e)            = Await l <$> norm env e
    norm env (Cond l e1 e2 e3)      = Cond l <$> norm env e1 <*> normBool env e2 <*> norm env e3
    norm env (IsInstance l e c)     = IsInstance l <$> norm env e <*> pure c
    norm env (BinOp l e1 Or e2)     = BinOp l <$> norm env e1 <*> pure Or <*> norm env e2
    norm env (BinOp l e1 And e2)    = BinOp l <$> norm env e1 <*> pure And <*> norm env e2
    norm env (UnOp l Not e)         = UnOp l Not <$> normBool env e
    norm env (Rest l e n)           = RestI l <$> norm env e <*> pure (nargs p + narg n k)
      where TTuple _ p k            = expTypeOf env e
    norm env (DotI l e i)           = DotI l <$> norm env e <*> pure i
    norm env (RestI l e i)          = RestI l <$> norm env e <*> pure i
    norm env (Lambda l p k e fx)    = do p' <- joinPar <$> norm env p <*> norm (define (envOf p) env) k
                                         let env1 = define (envOf p ++ envOf k) (addLambdavars p' env)
                                         eta <$> (Lambda l p' KwdNIL <$> norm env1 e <*> pure fx)
    norm env (Yield l e)            = Yield l <$> norm env e
    norm env (YieldFrom l e)        = YieldFrom l <$> norm env e
    norm env (Tuple l ps ks)        = Tuple l <$> norm env (joinArg ps ks) <*> pure KwdNil
    norm env (List l es)            = List l <$> norm env es
    norm env e@ListComp{}           = deferComp env e
    norm env (Dict l as)            = Dict l <$> norm env as
    norm env e@DictComp{}           = deferComp env e
    norm env (Set l es)             = Set l <$> norm env es
    norm env e@SetComp{}            = deferComp env e
    norm env (GeneratorExpr _ (Elem e) co)
                                    = do plan <- iteratorPlan e co
                                         let env1 = define (iteratorPlanEnv plan) env
                                         lowerIteratorPlan env1 plan >>= norm env1
    norm env e@(GeneratorExpr l (Star _) _)
                                    = notYet l e
    norm env (Paren l e)            = norm env e
    norm env e                      = error ("norm unexpected: " ++ prstr e)

deferComp env e                     = do f <- newName "compfun"
                                         let p = getLambdavars env
                                         addComp (f,p,e)
                                         return (Call NoLoc (eVar f) (posarg $ map eVar $ pospars' p) KwdNil)

-- Retain the structure of a generator independently of its eventual
-- representation.  Escaping plans currently lower to the public lazy
-- combinators below; local consumers can instead turn the same plan into
-- nested loops without first allocating an iterator pipeline.
data IteratorPlan                  = IteratorYield Expr
                                   | IteratorFor Pattern Expr [Expr] IteratorPlan

-- Fused plans put the comprehension clauses directly into their surrounding
-- function.  Freshen every pattern first so a comprehension variable cannot
-- overwrite a same-named local outside the generator.  The substitution is
-- extended one clause at a time: a source sees preceding bindings, whereas
-- its own pattern is in scope only in the following tests and clauses.
iteratorPlan                       :: Expr -> Comp -> NormM IteratorPlan
iteratorPlan result                = build []
  where build subst (CompFor _ p source co)
                                    = do renaming <- mapM fresh (bound p)
                                         let subst' = [ (n,eVar n') | (n,n') <- renaming ] ++
                                                      [ pair | pair@(n,_) <- subst, n `notElem` bound p ]
                                             p' = renameIteratorPattern renaming p
                                             source' = termsubst subst source
                                             (tests,rest) = leadingTests co
                                             tests' = termsubst subst' tests
                                         rest' <- case rest of
                                                      NoComp -> return $ IteratorYield (termsubst subst' result)
                                                      CompFor{} -> build subst' rest
                                                      CompIf{} -> error "iteratorPlan: misplaced if-clause"
                                         return $ IteratorFor p' source' tests' rest'
        build _ co                 = error ("iteratorPlan: expected for-clause, got " ++ prstr co)
        fresh n                    = do n' <- newName "gen"
                                        return (n,n')

iteratorPlanEnv                   :: IteratorPlan -> [(Name,NameInfo)]
iteratorPlanEnv IteratorYield{}    = []
iteratorPlanEnv (IteratorFor p _ _ rest)
                                    = envOf p ++ iteratorPlanEnv rest

renameIteratorPattern             :: [(Name,Name)] -> Pattern -> Pattern
renameIteratorPattern ren (PWild l t)
                                    = PWild l t
renameIteratorPattern ren (PVar l n t)
                                    = PVar l (rename n) t
  where rename n                   = maybe n id (lookup n ren)
renameIteratorPattern ren (PParen l p)
                                    = PParen l (renameIteratorPattern ren p)
renameIteratorPattern ren (PTuple l p k)
                                    = PTuple l (renamePosPat ren p) (renameKwdPat ren k)
renameIteratorPattern ren (PList l ps p)
                                    = PList l (map (renameIteratorPattern ren) ps)
                                              (renameIteratorPattern ren <$> p)

renamePosPat                       :: [(Name,Name)] -> PosPat -> PosPat
renamePosPat ren (PosPat p ps)      = PosPat (renameIteratorPattern ren p) (renamePosPat ren ps)
renamePosPat ren (PosPatStar p)     = PosPatStar (renameIteratorPattern ren p)
renamePosPat _ PosPatNil            = PosPatNil

renameKwdPat                       :: [(Name,Name)] -> KwdPat -> KwdPat
renameKwdPat ren (KwdPat n p ps)    = KwdPat n (renameIteratorPattern ren p) (renameKwdPat ren ps)
renameKwdPat ren (KwdPatStar p)     = KwdPatStar (renameIteratorPattern ren p)
renameKwdPat _ KwdPatNil            = KwdPatNil

-- A generator expression is a lazy pipeline.  Each for-clause consumes an
-- Iterator; its immediately following if-clauses become a filter.  The last
-- for-clause maps to the result expression, while an earlier one flat-maps to
-- the pipeline for the remaining clauses.  Consequently only the outermost
-- iterator expression is evaluated when the generator expression is created.
lowerIteratorPlan                  :: NormEnv -> IteratorPlan -> NormM Expr
lowerIteratorPlan env (IteratorFor p source tests rest)
                                    = do source' <- case tests of
                                                        [] -> return source
                                                        _  -> do predicate <- generatorLambda env p ta (andExpr tests)
                                                                 return $ filterIterator ta predicate source
                                         case rest of
                                             IteratorYield result -> do
                                                 f <- generatorLambda env p ta result
                                                 return $ mapIterator ta tb f source'
                                             IteratorFor{} -> do
                                                 inner <- lowerIteratorPlan env rest
                                                 f <- generatorLambda env p ta inner
                                                 return $ flatmapIterator ta tb f source'
  where ta                          = typeOf env p
        tb                          = iteratorPlanType env rest
lowerIteratorPlan _ IteratorYield{} = error "lowerIteratorPlan: top-level yield"

iteratorPlanType                   :: NormEnv -> IteratorPlan -> Type
iteratorPlanType env (IteratorYield result)
                                    = typeOf env result
iteratorPlanType env (IteratorFor _ _ _ rest)
                                    = iteratorPlanType env rest

-- Recognize builtin sum with either its implicit zero or an explicit start.
-- The type checker has already inserted the Plus and Iterable witnesses and
-- expanded the omitted start argument, so the argument row is fixed here.
-- Calls to a shadowing function named sum do not match.
sumGeneratorCall                    :: NormEnv -> Expr -> PosArg -> Maybe (Expr,Expr,Comp,Maybe Expr)
sumGeneratorCall env f args
  | isBuiltinSum env f,
    Just [w,_,GeneratorExpr _ (Elem result) co,start] <- fixedPosArgs args
                                    = Just (w,result,co,if isNoneExpr start then Nothing else Just start)
  | otherwise                       = Nothing

isBuiltinSum                       :: NormEnv -> Expr -> Bool
isBuiltinSum env                    = isBuiltinFunction env (name "sum")

isBuiltinFunction                  :: NormEnv -> Name -> Expr -> Bool
isBuiltinFunction env builtin (TApp _ f _)
                                    = isBuiltinFunction env builtin f
isBuiltinFunction env builtin (Var _ n)
                                    = unalias env n == gBuiltin builtin
isBuiltinFunction _ _ _            = False

isNoneExpr                         :: Expr -> Bool
isNoneExpr None{}                   = True
isNoneExpr (Paren _ e)              = isNoneExpr e
isNoneExpr _                        = False

-- Consume a plan as nested loops.  Binding the outer source before creating
-- the accumulator preserves generator construction timing: the first source
-- is evaluated at the call site, while nested sources, filters, and the result
-- remain deferred until their surrounding loop reaches them.
fusedSum                           :: NormEnv -> Expr -> Maybe Expr -> IteratorPlan -> NormM Expr
fusedSum env witness start plan     = do sourceName <- newName "gen_source"
                                         accName <- newName "sum"
                                         let source = iteratorPlanSource plan
                                             sourceType = typeOf env source
                                             resultType = iteratorPlanType env plan
                                             plan' = setIteratorPlanSource (eVar sourceName) plan
                                             initSource = sAssign (pVar sourceName $ conv env sourceType) source
                                             initAcc = sAssign (pVar accName $ conv env resultType)
                                                                 (maybe (eCall (eDot witness zeroKW) []) id start)
                                             add result = [sAssign (pVar accName $ conv env resultType)
                                                                   (eCall (eDot witness iaddKW) [eVar accName,result])]
                                             loops = consumeIteratorPlan plan' add
                                         return $ eLet (initSource : initAcc : loops) (eVar accName)

data BoolGeneratorFold             = FoldAny | FoldAll

boolGeneratorCall                  :: NormEnv -> Expr -> PosArg -> Maybe (BoolGeneratorFold,Expr,Comp)
boolGeneratorCall env f args
  | Just [_,GeneratorExpr _ (Elem result) co] <- fixedPosArgs args,
    isBuiltinFunction env (name "any") f
                                    = Just (FoldAny,result,co)
  | Just [_,GeneratorExpr _ (Elem result) co] <- fixedPosArgs args,
    isBuiltinFunction env (name "all") f
                                    = Just (FoldAll,result,co)
  | otherwise                       = Nothing

-- any and all use the same direct-loop machinery as a fused for-loop, but
-- stop at the first decisive value.  The result and escape flag are raw bools;
-- the yielded value itself remains unboxed whenever its __bool__ path permits.
fusedBool                          :: NormEnv -> BoolGeneratorFold -> IteratorPlan -> NormM Expr
fusedBool env kind plan             = do sourceName <- newName "gen_source"
                                         resultName <- newName "bool_result"
                                         doneName <- newName "gen_done"
                                         let source = iteratorPlanSource plan
                                             sourceType = typeOf env source
                                             plan' = setIteratorPlanSource (eVar sourceName) plan
                                             initial = case kind of
                                                           FoldAny -> False
                                                           FoldAll -> True
                                             decisive = not initial
                                             initSource = sAssign (pVar sourceName $ conv env sourceType) source
                                             initResult = sAssign (pVar resultName tBool) (eBool initial)
                                             initDone = sAssign (pVar doneName tBool) (eBool False)
                                             truth result = eCall (eDot result boolKW) []
                                             decide = [ sAssign (pVar resultName tBool) (eBool decisive)
                                                      , sAssign (pVar doneName tBool) (eBool True)
                                                      , sBreak
                                                      ]
                                             emit result = case kind of
                                                               FoldAny -> [sIf1 (truth result) decide []]
                                                               FoldAll -> [sIf1 (truth result) [] decide]
                                             loops = consumeEscapingIteratorPlan doneName True plan' emit
                                         return $ eLet (initSource : initResult : initDone : loops) (eVar resultName)

data ExtremumGeneratorFold         = FoldMax | FoldMin

extremumGeneratorCall             :: NormEnv -> Expr -> PosArg -> Maybe (ExtremumGeneratorFold,Expr,Expr,Comp,Expr)
extremumGeneratorCall env f args
  | Just [w,_,GeneratorExpr _ (Elem result) co,dflt] <- fixedPosArgs args,
    rawDefault dflt,
    any (\builtin -> isBuiltinFunction env (name builtin) f) ["max","max_def"]
                                    = Just (FoldMax,w,result,co,dflt)
  | Just [w,_,GeneratorExpr _ (Elem result) co,dflt] <- fixedPosArgs args,
    rawDefault dflt,
    any (\builtin -> isBuiltinFunction env (name builtin) f) ["min","min_def"]
                                    = Just (FoldMin,w,result,co,dflt)
  | otherwise                       = Nothing
  where rawDefault e
          | isNoneExpr e            = False
          | TOpt{} <- unalias env (typeOf env e)
                                    = False
          | otherwise               = True

-- With a definite default, max/min have an initialized accumulator and need
-- no optional state.  Bind each yielded expression once before comparing it;
-- even a pure expression may be expensive and must retain iterator semantics.
fusedExtremum                     :: NormEnv -> ExtremumGeneratorFold -> Expr -> Expr -> IteratorPlan -> NormM Expr
fusedExtremum env kind witness dflt plan
                                    = do sourceName <- newName "gen_source"
                                         accName <- newName "extremum"
                                         candidateName <- newName "candidate"
                                         let source = iteratorPlanSource plan
                                             sourceType = typeOf env source
                                             resultType = iteratorPlanType env plan
                                             plan' = setIteratorPlanSource (eVar sourceName) plan
                                             initSource = sAssign (pVar sourceName $ conv env sourceType) source
                                             initAcc = sAssign (pVar accName $ conv env resultType) dflt
                                             comparison = case kind of FoldMax -> gtKW; FoldMin -> ltKW
                                             emit result = [ sAssign (pVar candidateName $ conv env resultType) result
                                                           , sIf1 (eCall (eDot witness comparison)
                                                                         [eVar candidateName,eVar accName])
                                                                  [sAssign (pVar accName $ conv env resultType)
                                                                           (eVar candidateName)] []
                                                           ]
                                             loops = consumeIteratorPlan plan' emit
                                         return $ eLet (initSource : initAcc : loops) (eVar accName)

-- Collection constructors already have equivalent eager comprehension
-- machinery.  Recasting list(generator), set(generator), and the pair-shaped
-- dict(generator) as comprehensions lets that machinery consume the clauses
-- directly.  Dict fusion also avoids constructing a temporary tuple for each
-- key/value pair; the boxed elements required by the collections remain.
collectionGeneratorComp           :: NormEnv -> SrcLoc -> Expr -> PosArg -> Maybe Expr
collectionGeneratorComp env l f args
  | Just [_,GeneratorExpr _ (Elem result) co] <- fixedPosArgs args,
    isBuiltinFunction env (name "list") f
                                    = Just $ ListComp l (Elem result) co
  | Just [hashWitness,_,GeneratorExpr _ (Elem result) co] <- fixedPosArgs args,
    isBuiltinFunction env (name "set") f
                                    = let env1 = define (envOf co) env
                                          resultType = typeOf env1 result
                                          result' = annot (tHashableW resultType) hashWitness resultType result
                                      in Just $ SetComp l (Elem result') co
  | Just [hashWitness,_,GeneratorExpr _ (Elem result) co] <- fixedPosArgs args,
    Just (key,value) <- generatorPair result,
    isBuiltinFunction env (name "dict") f
                                    = let env1 = define (envOf co) env
                                          keyType = typeOf env1 key
                                          key' = annot (tHashableW keyType) hashWitness keyType key
                                      in Just $ DictComp l (Assoc key' value) co
  | otherwise                       = Nothing

generatorPair                      :: Expr -> Maybe (Expr,Expr)
generatorPair (Tuple _ (PosArg key (PosArg value PosNil)) KwdNil)
                                    = Just (key,value)
generatorPair (Paren _ result)      = generatorPair result
generatorPair _                     = Nothing

-- A for-loop asks the Iterable witness for an Iterator before normalization.
-- Recover a generator expression from that compiler-inserted call so it can
-- be consumed directly instead of materializing its combinator pipeline.
forGeneratorExpr                   :: Expr -> Maybe (Expr,Comp)
forGeneratorExpr (Call _ (Dot _ _ n) (PosArg (GeneratorExpr _ (Elem result) co) PosNil) KwdNil)
  | n == iterKW                    = Just (result,co)
forGeneratorExpr _                 = Nothing

-- Inline a generator used immediately by a for-loop.  A consumer break must
-- escape every generated loop, not merely the innermost one, so a private flag
-- is propagated outward.  Continue naturally targets the innermost generated
-- loop (the next yielded value); clearing the flag also handles a continue in
-- finally overriding an earlier break.
fusedFor                           :: NormEnv -> Pattern -> IteratorPlan -> Suite -> Suite -> NormM Suite
fusedFor env target plan body els   = do sourceName <- newName "gen_source"
                                         doneName <- newName "gen_done"
                                         let source = iteratorPlanSource plan
                                             sourceType = typeOf env source
                                             plan' = setIteratorPlanSource (eVar sourceName) plan
                                             initSource = sAssign (pVar sourceName $ conv env sourceType) source
                                             initDone = sAssign (pVar doneName tBool) (eBool False)
                                             body' = markGeneratorLoopControl doneName body
                                             emit result = sAssign target result : body'
                                             loops = consumeEscapingIteratorPlan doneName True plan' emit
                                             finish
                                               | null els = []
                                               | otherwise = [sIf1 (UnOp NoLoc Not (eVar doneName)) els []]
                                         return $ initSource : initDone : loops ++ finish

consumeEscapingIteratorPlan       :: Name -> Bool -> IteratorPlan -> (Expr -> Suite) -> Suite
consumeEscapingIteratorPlan _ _ (IteratorYield result) emit
                                    = emit result
consumeEscapingIteratorPlan done top (IteratorFor p source tests rest) emit
                                    = loop : propagate
  where nested                     = consumeEscapingIteratorPlan done False rest emit
        body
          | null tests              = nested
          | otherwise               = [sIf1 (andExpr tests) nested []]
        loop                        = For NoLoc p source body []
        propagate
          | top                     = []
          | otherwise               = [sIf1 (eVar done) [sBreak] []]

markGeneratorLoopControl          :: Name -> Suite -> Suite
markGeneratorLoopControl done      = concatMap mark
  where mark (Break l)             = [sAssign (pVar done tBool) (eBool True), Break l]
        mark (Continue l)          = [sAssign (pVar done tBool) (eBool False), Continue l]
        mark (If l bs els)         = [If l [ Branch test (markGeneratorLoopControl done suite)
                                                  | Branch test suite <- bs ]
                                              (markGeneratorLoopControl done els)]
        -- Break and continue in a nested loop body belong to that loop.  Its
        -- else-suite executes outside it and still belongs to our consumer.
        mark (While l test suite els)
                                    = [While l test suite (markGeneratorLoopControl done els)]
        mark (For l p source suite els)
                                    = [For l p source suite (markGeneratorLoopControl done els)]
        mark (Try l suite hs els fin)
                                    = [Try l (markGeneratorLoopControl done suite)
                                             [ Handler ex (markGeneratorLoopControl done hsuite)
                                               | Handler ex hsuite <- hs ]
                                             (markGeneratorLoopControl done els)
                                             (markGeneratorLoopControl done fin)]
        mark (With l items suite)   = [With l items (markGeneratorLoopControl done suite)]
        mark stmt                   = [stmt]

iteratorPlanSource                 :: IteratorPlan -> Expr
iteratorPlanSource (IteratorFor _ source _ _)
                                    = source
iteratorPlanSource IteratorYield{}  = error "iteratorPlanSource: top-level yield"

setIteratorPlanSource              :: Expr -> IteratorPlan -> IteratorPlan
setIteratorPlanSource source (IteratorFor p _ tests rest)
                                    = IteratorFor p source tests rest
setIteratorPlanSource _ IteratorYield{}
                                    = error "setIteratorPlanSource: top-level yield"

consumeIteratorPlan               :: IteratorPlan -> (Expr -> Suite) -> Suite
consumeIteratorPlan (IteratorYield result) emit
                                    = emit result
consumeIteratorPlan (IteratorFor p source tests rest) emit
                                    = [For NoLoc p source body []]
  where nested                     = consumeIteratorPlan rest emit
        body
          | null tests              = nested
          | otherwise               = [sIf1 (andExpr tests) nested []]

leadingTests                       :: Comp -> ([Expr],Comp)
leadingTests (CompIf _ test co)     = let (tests,rest) = leadingTests co
                                      in (test:tests,rest)
leadingTests co                     = ([],co)

andExpr                            :: [Expr] -> Expr
andExpr [e]                         = e
andExpr (e:es)                      = eBinOp e And (andExpr es)
andExpr []                          = error "andExpr: empty test list"

generatorLambda                    :: NormEnv -> Pattern -> Type -> Expr -> NormM Expr
generatorLambda env (PVar _ n _) t body
                                    = return $ Lambda NoLoc (pospar [(n,t)]) KwdNIL body (fxOf env body)
generatorLambda env p t body        = do n <- newName "genitem"
                                         let body' = eLet [sAssign p (eVar n)] body
                                             env' = define [(n,NVar t)] env
                                         return $ Lambda NoLoc (pospar [(n,t)]) KwdNIL body' (fxOf env' body')

-- These calls are introduced after type inference.  Supply the already-known
-- Iterable witness explicitly and cast the concrete combinator object to its
-- public Iterator result type.
filterIterator                     :: Type -> Expr -> Expr -> Expr
filterIterator a predicate source   = eCAST (tFilter a) (tIterator a) call
  where call                        = eCall (tApp (eQVar qnFilter) [a,tIterator a])
                                          [iteratorWitness a,predicate,source]

mapIterator                        :: Type -> Type -> Expr -> Expr -> Expr
mapIterator a b f source            = eCAST (tMap a b) (tIterator b) call
  where call                        = eCall (tApp (eQVar qnMap) [a,b,tIterator a])
                                          [iteratorWitness a,f,source]

flatmapIterator                    :: Type -> Type -> Expr -> Expr -> Expr
flatmapIterator a b f source        = eCAST (tFlatmap a b) (tIterator b) call
  where call                        = eCall (tApp (eQVar qnFlatmap) [a,b,tIterator a])
                                          [iteratorWitness a,f,source]

iteratorWitness                    :: Type -> Expr
iteratorWitness a                   = eCall (tApp (eQVar witIterableIterator) [a]) []

eta (Lambda _ p KwdNIL (Call _ e p' KwdNil) fx)
  | eq1 p p'                        = e
  where
    eq1 (PosPar n _ _ p) (PosArg e p')  = eVar n == e && eq1 p p'
    eq1 (PosSTAR n _) (PosStar e)       = eVar n == e
    eq1 PosNIL PosNil                   = True
    eq1 _ _                             = False
eta e                               = e

nargs (TRow _ _ _ _ r)              = 1 + nargs r
nargs (TDefRow _ _ _ _ _ r)         = 1 + nargs r
nargs (TStar _ _ _)                 = 1
nargs (TNil _ _)                    = 0

narg n (TRow _ _ n' _ r)
  | n == n'                         = 0
  | otherwise                       = 1 + narg n r
narg n (TDefRow _ _ n' _ _ r)
  | n == n'                         = 0
  | otherwise                       = 1 + narg n r
narg n (TStar _ _ _)
  | n == attrKW                     = 0
narg n k                            = error ("### Bad narg " ++ prstr n ++ " " ++ prstr k)

instance Norm Pattern where
    norm env (PWild l a)            = return $ PWild l (conv env a)
    norm env (PVar l n a)           = return $ PVar l n (conv env a)
    norm env (PTuple l ps ks)       = PTuple l <$> norm env ps <*> norm env ks
    norm env (PList l ps p)         = PList l <$> norm env ps <*> norm env p        -- TODO: eliminate here
    norm env (PParen l p)           = norm env p

instance Norm Branch where
    norm env (Branch e ss)          = Branch <$> normBool env e <*> normSuite env ss

instance Norm Handler where
    norm env (Handler ex b)         = Handler ex <$> normSuite env1 b
      where env1                    = define (envOf ex) env

instance Norm PosPar where
    norm env (PosPar n t _ p)       = PosPar n (conv env t) Nothing <$> norm (define [(n,NVar $ fromJust t)] env) p
    norm env (PosSTAR n t)          = return $ PosSTAR n (conv env t)
    norm env PosNIL                 = return PosNIL

instance Norm KwdPar where
    norm env (KwdPar n t _ k)       = KwdPar n (conv env t) Nothing <$> norm (define [(n,NVar $ fromJust t)] env) k
    norm env (KwdSTAR n t)          = return $ KwdSTAR n (conv env t)
    norm env KwdNIL                 = return KwdNIL

joinPar (PosPar n t e p) k          = PosPar n t e (joinPar p k)
joinPar (PosSTAR n t) k             = PosPar n t Nothing (kwdToPosPar k)
joinPar PosNIL k                    = kwdToPosPar k

kwdToPosPar (KwdPar n t e k)        = PosPar n t e (kwdToPosPar k)
kwdToPosPar (KwdSTAR n t)           = PosPar n t Nothing PosNIL
kwdToPosPar KwdNIL                  = PosNIL

joinArg (PosArg e p) k              = PosArg e (joinArg p k)
joinArg (PosStar e) k               = PosArg e (kwdToPosArg k)
joinArg PosNil k                    = kwdToPosArg k

kwdToPosArg (KwdArg n e k)          = PosArg e (kwdToPosArg k)
kwdToPosArg (KwdStar e)             = PosArg e PosNil
kwdToPosArg KwdNil                  = PosNil


instance Norm PosArg where
    norm env (PosArg e p)           = PosArg <$> norm env e <*> norm env p
    norm env (PosStar e)            = PosStar <$> norm env e
    norm env PosNil                 = return PosNil

instance Norm KwdArg where
    norm env (KwdArg n e k)         = KwdArg n <$> norm env e <*> norm env k
    norm env (KwdStar e)            = KwdStar <$> norm env e
    norm env KwdNil                 = return KwdNil

instance Norm PosPat where
    norm env (PosPat p ps)          = PosPat <$> norm env p <*> norm env ps
    norm env (PosPatStar p)         = PosPatStar <$> norm env p
    norm env PosPatNil              = return PosPatNil

instance Norm KwdPat where
    norm env (KwdPat n p ps)        = KwdPat n <$> norm env p <*> norm env ps
    norm env (KwdPatStar p)         = KwdPatStar <$> norm env p
    norm env KwdPatNil              = return KwdPatNil

instance Norm Comp where
    norm env (CompFor l p e c)      = CompFor l <$> norm env p <*> norm env e <*> norm (define (envOf p) env) c
    norm env (CompIf l e c)         = CompIf l <$> normBool env e <*> norm env c
    norm env NoComp                 = return NoComp

instance Norm Elem where
    norm env (Elem e)               = Elem <$> norm env e
    norm env (Star e)               = Star <$> norm env e               -- TODO: eliminate here

instance Norm Assoc where
    norm env (Assoc k v)            = Assoc <$> norm env k <*> norm env v
    norm env (StarStar e)           = StarStar <$> norm env e           -- TODO: eliminate here


-- Convert function types ---------------------------------------------------------------------------------

convEnv env m (n, i)                = [(n, conv env i)]


class Conv a where
    conv                            :: NormEnv -> a -> a

instance (Conv a) => Conv [a] where
    conv env                        = map $ conv env

instance (Conv a) => Conv (Maybe a) where
    conv env                        = fmap $ conv env

instance (Conv a) => Conv (Name, a) where
    conv env (n, x)                 = (n, conv env x)

instance Conv NameInfo where
    conv env (NAct q p k te doc)    = NAct q (joinRow env p k) kwdNil (conv env te) doc
    conv env (NClass q ps te doc)   = NClass q (conv env ps) (conv env te) doc
    conv env (NType q t doc)        = NType q (conv env t) doc
    conv env (NSig sc dec doc)      = NSig (conv env sc) dec doc
    conv env (NDef sc dec doc)      = NDef (conv env sc) dec doc
    conv env (NVar t)               = NVar (conv env t)
    conv env (NSVar t)              = NSVar (conv env t)
    conv env ni                     = ni

instance Conv WTCon where
    conv env (w,c)                  = (w, conv env c)

instance Conv TSchema where
    conv env (TSchema l q t)        = TSchema l q (conv env t)

instance Conv Type where
    conv env (TFun l fx p k t)      = TFun l fx (joinRow env p k) kwdNil (conv env t)
    conv env (TCon l c)
      | Just t <- tExpand env c     = conv env t
      | otherwise                   = TCon l (conv env c)
    conv env (TTuple l p k)         = TTuple l (joinRow env p k) kwdNil
    conv env (TOpt l t)             = TOpt l (conv env t)
    conv env (TRow l k n t r)       = TRow l PRow nWild (conv env t) (conv env r)
    conv env (TDefRow l k n t _ r)  = TRow l PRow nWild (conv env t) (conv env r)
    conv env (TStar l k r)          = TRow l PRow nWild (TTuple l (conv env r) kwdNil) posNil
    conv env (TNil l k)             = TNil l PRow
    conv env t                      = t

instance Conv TCon where
    conv env (TC c ts)              = TC c (conv env ts)

-- Must mirror Syntax.tupleComponents, which the solver uses to derive tuple witnesses.
joinRow env (TRow l k n t p) r      = TRow l PRow nWild (conv env t) (joinRow env p r)
joinRow env (TDefRow l k n t _ p) r = TRow l PRow nWild (conv env t) (joinRow env p r)
joinRow env (TStar l k p) r         = TRow l PRow nWild (TTuple l (conv env p) kwdNil) (conv env r)
joinRow env (TNil _ _) r            = conv env r
-- To be removed:
joinRow env p (TNil _ _)            = conv env p
joinRow env p r                     = error ("##### joinRow " ++ prstr p ++ "  AND  " ++ prstr r)
