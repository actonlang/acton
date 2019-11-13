{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct,solverError) where

import Debug.Trace
import Data.Typeable
import qualified Control.Exception
import System.FilePath.Posix (joinPath)
import System.Directory (doesFileExist)
import Control.Monad
--import Control.Monad.State
import Data.Maybe (maybeToList)
import Data.Graph (SCC(..), stronglyConnComp)
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Env
import Acton.TypeM
import Acton.Solver
import qualified InterfaceFiles

reconstruct                             :: String -> Env -> Module -> IO (TEnv, SrcInfo)
reconstruct outname env modul           = do InterfaceFiles.writeFile (outname ++ ".ty") (unalias env1 te)
                                             return (te, info)
  where Module m _ suite                = modul
        env1                            = reserve (bound suite) env{ defaultmod = m }
        (te,info)                       = runTypeM $ (,) <$> infTop env1 suite <*> getDump

solverError                             = typeError

chkCycles (d@Class{} : ds)              = noforward (qual d) d ds && all (chkDecl d ds) (dbody d) && chkCycles ds
chkCycles (d@Protocol{} : ds)           = noforward (qual d) d ds && all (chkDecl d ds) (dbody d) && chkCycles ds
chkCycles (d : ds)                      = chkCycles ds
chkCycles []                            = True

chkDecl d ds s@Decl{}                   = chkCycles (decls s ++ ds)
chkDecl d ds s                          = noforward s d ds


noforward x d ds
  | not $ null vs                       = err2 vs "Illegal forward reference:"
  | otherwise                           = True
  where vs                              = free x `intersect` declnames (d:ds)

nodup x
  | not $ null vs                       = err2 vs "Duplicate names:"
  | otherwise                           = True
  where vs                              = duplicates (bound x)

noshadow svs x
  | not $ null vs                       = err2 vs "Illegal state shadowing:"
  | otherwise                           = True
  where vs                              = intersect (bound x) svs

noescape te                                                                                 -- TODO: check for escaping classes/protocols as well
--  | not $ null dangling                 = err2 dangling "Dangling type signature for"       -- TODO: turn on again!
  | otherwise                           = te
  where dangling                        = dom (nSigs te) \\ dom (nVars te)


-- Infer -------------------------------

infTop env ss                           = do pushFX fxNil
                                             te <- noescape <$> infEnv env ss
                                             popFX
                                             cs <- collectConstraints
                                             solve env cs
                                             msubst te

class Infer a where
    infer                               :: Env -> a -> TypeM Type

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM TEnv

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (TEnv,Type)

class InfData a where
    infData                             :: Env -> a -> TypeM TEnv


splitGen                                :: Env -> [TVar] -> TEnv -> Constraints -> TypeM (Constraints, TEnv)
splitGen env tvs te cs
  | null ambig_cs                       = return (fixed_cs, mapVars generalize te)
  | otherwise                           = do solve env ambig_cs
                                             cs1 <- simplify env (fixed_cs++gen_cs)
                                             te1 <- msubst te
                                             tvs1 <- fmap tyfree $ mapM msubst $ map tVar tvs
                                             splitGen env tvs1 te1 cs1
  where 
    (fixed_cs, cs')                     = partition (null . (\\tvs) . tyfree) cs
    (ambig_cs, gen_cs)                  = partition (ambig te . tyfree) cs'
    ambig te vs                         = or [ not $ null (vs \\ tyfree info) | (n, info) <- te ]
    q_new                               = mkBinds gen_cs
    generalize (TSchema l q t dec)      = closeFX $ TSchema l (subst s (q_new++q)) (subst s t) dec
      where s                           = tybound q_new `zip` map tVar (tvarSupply \\ tvs \\ tybound q)

mkBinds cs                              = collect [] $ catMaybes $ map bound cs
  where
    bound (Sub (TVar _ v) (TCon _ u))   = Just $ TBind v [u]
    bound (Impl (TVar _ v) u)           = Just $ TBind v [u]
    bound c                             = trace ("### Unreduced constraint: " ++ prstr c) $ Nothing
    collect vs []                       = []
    collect vs (TBind v us : q)
      | v `elem` vs                     = collect vs q
      | otherwise                       = TBind v (us ++ concat [ us' | TBind v' us' <- q, v' == v ]) : collect (v:vs) q
    

genTEnv                                 :: Env -> TEnv -> TypeM TEnv
genTEnv env te                          = do cs <- collectConstraints
                                             cs1 <- simplify env cs
                                             te1 <- msubst te
                                             tvs <- fmap tyfree $ mapM msubst $ map tVar $ tyfree env
                                             (cs2, te2) <- splitGen env tvs te1 cs1
                                             constrain cs2
                                             dump [ INS (loc v) t | (v, TSchema _ [] t _) <- nVars te1 ]
                                             dump [ GEN (loc v) t | (v, t) <- nVars te2 ]
                                             return te2

gen1                                    :: Env -> Name -> Type -> Decoration -> TypeM TSchema
gen1 env n t d                          = do te <- genTEnv env (nVar' n (monotype' t d))
                                             return $ snd $ head $ nVars te

commonTEnv                              :: Env -> [TEnv] -> TypeM TEnv
commonTEnv env []                       = return []
commonTEnv env tenvs                    = do unifyTEnv env tenvs vs
                                             return $ prune vs $ head tenvs
  where vs                              = foldr intersect [] $ map dom tenvs

unionTEnv                               :: Env -> [TEnv] -> TypeM TEnv
unionTEnv env tenvs                     = undefined                                 -- For data statements only. TODO: implement


infLiveEnv env x
  | fallsthru x                         = (Just . noescape) <$> infEnv env x
  | otherwise                           = (const Nothing . noescape) <$> infEnv env x

instance (InfEnv a) => InfEnv [a] where
    infEnv env []                       = return []
    infEnv env (s : ss)                 = do te1 <- infEnv env s
                                             te2 <- infEnv (define te1 env) ss
                                             return (nCombine te1 te2)

instance InfEnv Stmt where
    infEnv env (Expr _ e)               = do _ <- infer env e
                                             return []
    infEnv env (Assign _ pats e)
      | nodup pats                      = do t1 <- infer env e
                                             (te, t2) <- infEnvT env pats
                                             constrain [Sub t1 t2]
                                             return te
    infEnv env (AugAssign l pat (Op _ op) e)
                                        = do t1 <- infer env e
                                             t2 <- infer env pat
                                             constrain [Sub t1 t2, Impl t2 (protocol op)]
                                             return []
      where protocol PlusA              = cPlus
            protocol MinusA             = cMinus
            protocol MultA              = cNumber
            protocol PowA               = cNumber
            protocol DivA               = cNumber
            protocol ModA               = cReal
            protocol EuDivA             = cReal
            protocol ShiftLA            = cIntegral
            protocol ShiftRA            = cIntegral
            protocol BOrA               = cLogical
            protocol BXorA              = cLogical
            protocol BAndA              = cLogical
            protocol MMultA             = cMatrix
    infEnv env (Assert _ es)            = do mapM (inferBool env) es
                                             return []
    infEnv env (Pass _)                 = return []
    infEnv env (Delete _ pat)
      | nodup pat                       = do _ <- infer env pat                     -- TODO: constrain pat targets to opt type
                                             return []
    infEnv env (Return _ Nothing)       = do subFX (fxRet tNone tWild)
                                             return []
    infEnv env (Return l (Just e))      = do t <- infer env e
                                             subFX (fxRet t tWild)
                                             return []
    infEnv env (Raise _ Nothing)        = return []
    infEnv env (Raise _ (Just e))       = do _ <- infer env e
                                             return []
    infEnv env (Break _)                = return []
    infEnv env (Continue _)             = return []
    infEnv env (If _ bs els)            = do tes <- mapM (infLiveEnv env) bs
                                             te <- infLiveEnv env els
                                             commonTEnv env $ catMaybes (te:tes)
    infEnv env (While _ e b els)        = do inferBool env e
                                             _ <- noescape <$> infEnv env b
                                             _ <- noescape <$> infEnv env els
                                             return []
    infEnv env (For l p e b els)
      | nodup p                         = do (te, t1) <- infEnvT env p
                                             t2 <- infer env e
                                             _ <- noescape <$> infEnv (define te env) b
                                             _ <- noescape <$> infEnv env els
                                             constrain [Impl t2 (cCollection t1)]
                                             return []
    infEnv env (Try _ b hs els fin)     = do te <- infLiveEnv env (b ++ els)
                                             tes <- mapM (infLiveEnv env) hs
                                             te1 <- commonTEnv env $ catMaybes $ te:tes
                                             te2 <- noescape <$> infEnv (define te1 env) fin
                                             return (nCombine te1 te2)
    infEnv env (With _ items b)
      | nodup items                     = do te <- infEnv env items
                                             te1 <- noescape <$> infEnv (define te env) b
                                             return $ prune (dom te) te1

    infEnv env (VarAssign _ pats e)
      | nodup pats                      = do t1 <- infer env e
                                             (te, t2) <- infEnvT env pats
                                             constrain [Sub t1 t2]
                                             return (nState te)
    
    infEnv env (Decl _ ds)
      | nodup vs                        = do te1 <- infEnv (enterDecl env) ds
                                             when (not $ inDecl env) $
                                                 check (define te1 env) ds
                                             te2 <- genTEnv env te1
                                             return te2
      where vs                          = nub $ bound ds

    infEnv env (Data l _ _)             = notYet l "data syntax"
{-
    infEnv env (Data l Nothing b)       = do te <- infData env1 b
                                             let te1 = filter (notemp . fst) te1
                                             constrain [Equ (tRecord $ env2row tNil $ nVars te1) t]
                                             return []
      where t                           = undefined   -- WAS: getReturn env
            env1                        = reserve (filter istemp $ bound b) env

    infEnv env (Data l (Just p) b)
      | nodup p                         = do (te0, t) <- infEnvT env p
                                             (te1,te2) <- partition (istemp . fst) <$> infData env b
                                             constrain [Equ (tRecord $ env2row tNil $ nVars te2) t]
                                             return (nCombine te0 te1)

instance InfData [Stmt] where
    infData env []                      = return []
    infData env (s : ss)                = do te1 <- infData env s
                                             te2 <- infData (define (filter (istemp . fst) te1) env) ss
                                             unionTEnv env [te1,te2]

instance InfData Stmt where
    infData env (While _ e b els)       = do inferBool env e
                                             te1 <- infEnv env b
                                             te2 <- infEnv env els
                                             unionTEnv env [[], te1, te2]
    infData env (For _ p e b els)       = undefined
    infData env (If _ bs els)           = do tes <- mapM (infData env) bs
                                             te <- infData env els
                                             unionTEnv env (te:tes)
    infData env s                       = infEnv env s

instance InfData Branch where
    infData env (Branch e b)            = do inferBool env e
                                             infData env b
-}

instance InfEnv Decl where
    infEnv env (Actor _ n q p k t _)
      | nodup (p,k)                     = do t0 <- wellFormed env (extractSig env n q p k t NoMod)
                                             case reservedOrSig n env of
                                                Just Nothing   -> return (nVar' n t0)
                                                Just (Just t1) -> constrain [EquGen t0 t1] >> return (nVar' n t1)
                                                Nothing        -> illegalRedef n
    infEnv env (Def _ n q p k t _ m)
      | nodup (p,k)                     = do t0 <- wellFormed env (extractSig env n q p k t m)
                                             case reservedOrSig n env of
                                                Just Nothing   -> return (nVar' n t0)
                                                Just (Just t1) -> constrain [EquGen t0 t1] >> return (nVar' n t1)
                                                Nothing        -> illegalRedef n
    infEnv env (Class _ n q us b)
      | not $ reserved n env            = illegalRedef n
      | wf env q && wf env1 us          = do te <- noescape <$> infEnv env1 b
                                             return $ nClass n q (mro env1 us) te
      where env1                        = reserve (bound b) $ defineSelf n q $ defineTVars q $ block (stateScope env) env
            us                          = classBases env us
    infEnv env (Protocol _ n q us b)
      | not $ reserved n env            = illegalRedef n
      | wf env q && wf env1 us          = do te <- infEnv env1 b
                                             return $ nProto (defaultmod env) n q (mro env1 (protoBases env us)) te
      where env1                        = reserve (bound b) $ defineSelf n q $ defineTVars q $ block (stateScope env) env            
    infEnv env (Extension _ n q us b)
      | isProto env n                   = notYet (loc n) "Extension of a protocol"
      | wf env q && wf env1 us          = do w <- newName (noqual n)
                                             return $ nExt w n q (mro env1 (protoBases env us))
      where env1                        = reserve (bound b) $ defineSelf' n q $ defineTVars q $ block (stateScope env) env
            u                           = TC n [ tVar tv | TBind tv _ <- q ]
    infEnv env (Signature _ ns sc)
      | not $ null redefs               = illegalRedef (head redefs)
      | otherwise                       = do t0 <- wellFormed env sc
                                             return $ nSig ns t0
      where redefs                      = [ n | n <- ns, not $ reserved n env ]

classBases env []                       = []
classBases env (u:us)
  | isProto env (tcname u)              = u : protoBases env us
  | otherwise                           = u : protoBases env us

protoBases env []                       = []
protoBases env (u:us)
  | isProto env (tcname u)              = u : protoBases env us
  | otherwise                           = err1 u "Protocol expected"

mro env us                              = merge [] $ linearizations us ++ [us]
  where merge out lists
          | null heads                  = reverse out
          | h:_ <- good                 = merge (h:out) [ if match hd h then tl else hd:tl | (hd,tl) <- zip heads tails ]
          | otherwise                   = err2 heads "Inconsistent resolution order for"
          where (heads,tails)           = unzip [ (hd,tl) | hd:tl <- lists ]
                good                    = [ h | h <- heads, all (absent (tcname h)) tails]

        match u1 u2
          | u1 == u2                    = True
          | tcname u1 == tcname u2      = err2 [u1,u2] "Inconsistent instantiations of class/protocol"
          | otherwise                   = False

        absent n []                     = True
        absent n (u:us)
          | n == tcname u               = False
          | otherwise                   = absent n us

        linearizations []               = []
        linearizations (u : us)
          | all (entail env) cs         = (u:us') : linearizations us
          | otherwise                   = err1 u ("Type context too weak to entail")
          where (cs, us', _)            = findCon env u

extractSig env n q p k t m
  | null q                              = tSchema' [ TBind v [] | v <- tvs ] sig (extractDecoration m)
  | all (`elem` tybound q) tvs          = tSchema' q sig (extractDecoration m)
  | otherwise                           = err2 (tvs \\ tybound q) "Unbound type variable(s)in signature:"
  where
    tvs                                 = tyfree sig \\ tvarScope env
    sig                                 = tFun (extractFX m) prow krow (maybe tWild id t)
    (prow,krow)                         = extractPars m p k
    extractPars m (PosPar n t _ p) k
      | needsExtraParam m               = (extractP p, extractK k)
    extractPars m PosNIL (KwdPar n t _ k)
      | needsExtraParam m               = (extractP p, extractK k)
    extractPars (InstMeth _) p k        = err1 n "Missing 'self' parameter in definition of"
    extractPars ClassMeth p k           = err1 n "Missing 'class' parameter in definition of"
    extractPars _ p k                   = (extractP p, extractK k)
    needsExtraParam (InstMeth _)        = True
    needsExtraParam ClassMeth           = True
    needsExtraParam _                   = False
    extractP (PosPar n t _ p)           = posRow (maybe (monotype tWild) id t) (extractP p)
    extractP (PosSTAR n t)              = posVar Nothing        -- safe to ignore type (not schema) annotation t here
    extractP PosNIL                     = posNil
    extractK (KwdPar n t _ k)           = kwdRow n (maybe (monotype tWild) id t) (extractK k)
    extractK (KwdSTAR n t)              = kwdVar Nothing        -- safe to ignore type (not schema) annotation t here
    extractK KwdNIL                     = kwdNil
    extractFX (Sync _)                  = fxSync fxNil
    extractFX Async                     = fxAsync fxNil
    extractFX _                         = tWild

extractDecoration StaticMeth            = StaticMethod
extractDecoration ClassMeth             = ClassMethod
extractDecoration (InstMeth f)          = InstMethod f
extractDecoration _                     = NoDec


class Check a where
    check                               :: Env -> a -> TypeM ()

instance (Check a) => Check [a] where
    check env ds                        = mapM_ (check env) ds

instance Check Stmt where
    check env (Decl _ ds)               = check env ds
    check env s                         = return ()

instance Check Decl where
    check env (Actor l n q p k ann b)
      | noshadow svars p                = do pushFX (fxAct tWild)
                                             (te0, prow) <- infEnvT env p
                                             (te1, krow) <- infEnvT (define te0 env1) k
                                             te2 <- noescape <$> infEnv (define te1 (define te0 env1)) b
                                             te3 <- genTEnv env te2
                                             fx <- fxAct <$> newTVar
                                             checkAssump env n (tFun fx prow krow (tRecord $ env2row tNil $ nVars te3)) NoDec
      where svars                       = statedefs b
            env0                        = define envActorSelf $ defineTVars q $ block (stateScope env) env
            env1                        = reserve (bound (p,k) ++ bound b ++ svars) env0
            
    check env (Def l n q p k ann b (Sync _))
      | noshadow svars (p,k)            = do t <- newTVar
                                             pushFX (fxRet t tWild)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             (te0, prow) <- infEnvT env p
                                             (te1, krow) <- infEnvT (define te0 env1) k
                                             _ <- noescape <$> infEnv (define te1 (define te0 env1)) b
                                             popFX
                                             fx <- fxSync <$> newTVar
                                             checkAssump env n (tFun fx prow krow t) NoDec
      where svars                       = stateScope env
            env1                        = reserve (bound (p,k) ++ bound b \\ svars) $ defineTVars q env

    check env (Def l n q p k ann b Async)
      | noshadow svars (p,k)            = do t <- newTVar
                                             pushFX (fxRet t tWild)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             (te0, prow) <- infEnvT env p
                                             (te1, krow) <- infEnvT (define te0 env1) k
                                             _ <- noescape <$> infEnv (define te1 (define te0 env1)) b
                                             popFX
                                             fx <- fxAsync <$> newTVar
                                             checkAssump env n (tFun fx prow krow (tMsg t)) NoDec
      where svars                       = stateScope env
            env1                        = reserve (bound (p,k) ++ bound b \\ svars) $ defineTVars q env

    check env (Def l n q p k ann b modif)
                                        = do t <- newTVar
                                             fx <- newTVar
                                             pushFX (fxRet t fx)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             (te0, prow) <- infEnvT env p
                                             (te1, krow) <- infEnvT (define te0 env1) k
                                             _ <- noescape <$> infEnv (define te1 (define te0 env1)) b
                                             popFX
                                             (prow',krow') <- splitRows modif prow krow
                                             checkAssump env n (tFun fx prow' krow' t) (extractDecoration modif)
      where env1                        = reserve (bound (p,k) ++ bound b) $ defineTVars q $ block (stateScope env) env
            splitRows m p@(TNil _) k    = (,) <$> return p <*> splitRow m k
            splitRows m p k             = (,) <$> splitRow m p <*> return k
            splitRow (InstMeth _) (TRow _ n sc r)
                                        = constrain [EquGen sc (monotype tSelf)] >> return r
            splitRow (ClassMeth) (TRow _ n sc r)
                                        = constrain [EquGen sc (monotype (tAt (findSelf env)))] >> return r
            splitRow m r                = return r

    check env (Class l n _ _ b)         = do pushFX fxNil
                                             check (define te env1) b
                                             popFX
                                             checkBindings env False us te
      where env1                        = defineSelf n q $ defineTVars q $ block (stateScope env) env
            (q,us,te)                   = findClass (NoQual n) env

    check env (Protocol l n _ _ b)      = do pushFX fxNil
                                             check (define te env1) b
                                             popFX
                                             checkBindings env True us te
      where env1                        = defineSelf n q $ defineTVars q $ block (stateScope env) env
            (q,us,te)                   = findProto (NoQual n) env

    check env (Extension l n q us b)    = do pushFX fxNil
                                             te <- infEnv env1 b
                                             popFX
                                             checkBindings env False us te
      where env1                        = reserve (bound b) $ defineSelf' n q $ defineTVars q $ block (stateScope env) env
    check env (Signature l ns sc)       = return ()


checkBindings env proto us te
  | proto && (not $ null unsigs)        = lackSig unsigs
  | not proto && (not $ null undefs)    = lackDef undefs
  | otherwise                           = constrain refinements
  where tes                             = [ te' | u <- us, let (_,_,te') = findCon env u ]
        inherited                       = concatMap nSigs tes ++ concatMap nVars tes
        refinements                     = [ SubGen sc sc' | (n,sc) <- nVars te, Just sc' <- [lookup n inherited] ]
        undefs                          = (dom $ nSigs te ++ concatMap nSigs tes) \\ (dom $ nVars te ++ concatMap nVars tes)
        unsigs                          = dom te \\ (dom (nSigs te) ++ dom inherited)


checkAssump env n t d                   = case findVarType n env of
                                            (TSchema _ [] t' d')
                                               | matchDec d d' -> constrain [Sub t t']
                                               | otherwise -> err1 n ("Inconsistent decorations, " ++ show d ++ " vs " ++ prstr d' ++ ", for")
                                            sc' -> do
                                               sc <- gen1 env n t d         -- TODO: verify that generalizing one decl at a time is ok...
                                               constrain [EquGen sc sc']
  where matchDec NoDec _                = True
        matchDec (InstMethod False) _   = True
        matchDec (InstAttr False) _     = True
        matchDec d d'                   = d == d'

inferPure env e                         = do pushFX tNil
                                             t <- infer env e
                                             popFX
                                             return t

env2row                                 = foldl (\r (n,t) -> kwdRow n t r)           -- TODO: stabilize this...

instance InfEnv Branch where
    infEnv env (Branch e b)             = do inferBool env e
                                             noescape <$> infEnv env b

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do t <- infer env e
                                             constrain [Impl t cContextManager]
                                             return []
    infEnv env (WithItem e (Just p))    = do t1 <- infer env e
                                             (te, t2) <- infEnvT env p
                                             constrain [Equ t1 t2, Impl t1 cContextManager]
                                             return te

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do te <- infEnv env ex
                                             te1 <- noescape <$> infEnv (define te env) b
                                             return $ prune (dom te) te1

instance InfEnv Except where
    infEnv env (ExceptAll _)            = return []
    infEnv env (Except l x)             = do (cs,t) <- instantiate env $ classConSchema env x
                                             constrain (Sub t tException : cs)
                                             return []
    infEnv env (ExceptAs l x n)         = do (cs,t) <- instantiate env $ classConSchema env x
                                             constrain (Sub t tException : cs)
                                             return $ nVar n t

classConSchema env qn                   = tSchema q (tCon $ TC qn $ map tVar $ tybound q)
  where (q,_,_)                         = findClass qn env

instance Infer Expr where
    infer env (Var _ n)                 = do (cs,t) <- instantiate env $ openFX $ findVarType' n env
                                             constrain cs
                                             return t
    infer env (Int _ val s)             = return tInt
    infer env (Float _ val s)           = return tFloat
    infer env e@Imaginary{}             = notYetExpr e
    infer env (Bool _ val)              = return tBool
    infer env (None _)                  = return tNone
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env (Strings _ ss)            = return $ tUnion [ULit $ concat ss]
    infer env (BStrings _ ss)           = return tBytes
    infer env (Call l e ps ks)          = do t <- infer env e
                                             dump [INS (loc e) t]
                                             prow <- infer env ps
                                             krow <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX
                                             constrain [Sub t (tFun fx prow krow t0)]
                                             return t0
    infer env (Await l e)               = do t <- infer env e
                                             t0 <- newTVar
                                             fx <- fxSync <$> newTVar
                                             equFX fx
                                             constrain [Sub t (tMsg t0)]
                                             return t0
    infer env (Index l e [i])           = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newTVar
                                             constrain [Impl t (cIndexed ti t0)]
                                             return t0
    infer env (Slice l e [s])           = do t <- infer env e
                                             inferSlice env s
                                             constrain [Impl t cSliceable]
                                             return t
    infer env (Cond l e1 e e2)          = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             inferBool env e
                                             t0 <- newTVar
                                             constrain [Sub t1 t0, Sub t2 t0]
                                             return t0
    infer env (BinOp l e1 (Op _ op) e2)
      | op `elem` [Or,And]              = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [Impl t1 cBoolean, Impl t2 cBoolean]
                                             return tBool
      | otherwise                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             t <- newTVar
                                             constrain [Sub t1 t, Sub t2 t, Impl t (protocol op)]
                                             return t
      where protocol Plus               = cPlus
            protocol Minus              = cMinus
            protocol Mult               = cNumber
            protocol Pow                = cNumber
            protocol Div                = cNumber
            protocol Mod                = cReal
            protocol EuDiv              = cReal
            protocol ShiftL             = cIntegral
            protocol ShiftR             = cIntegral
            protocol BOr                = cLogical
            protocol BXor               = cLogical
            protocol BAnd               = cLogical
            protocol MMult              = cMatrix
    infer env (UnOp l (Op _ op) e)
      | op == Not                       = do t <- infer env e
                                             constrain [Impl t cBoolean]
                                             return tBool
      | otherwise                       = do t <- infer env e
                                             constrain [Impl t (protocol op)]
                                             return t
      where protocol UPlus              = cNumber
            protocol UMinus             = cNumber
            protocol BNot               = cIntegral
    infer env (CompOp l e ops)          = do t1 <- infer env e
                                             walk t1 ops
                                             return tBool
      where walk t0 []                     = return ()
            walk t0 (OpArg (Op l o) e:ops)
              | o `elem` [In,NotIn]     = do t1 <- infer env e
                                             constrain [Impl t1 (cCollection t0), Impl t0 cEq]
                                             walk t1 ops
                                             return ()
              | otherwise               = do t1 <- infer env e
                                             t <- newTVar
                                             constrain [Sub t0 t, Sub t1 t, Impl t (protocol o)]
                                             walk t ops
            protocol Eq                 = cEq
            protocol NEq                = cEq
            protocol LtGt               = cEq
            protocol Lt                 = cOrd
            protocol Gt                 = cOrd
            protocol LE                 = cOrd
            protocol GE                 = cOrd
            protocol Is                 = cIdentity
            protocol IsNot              = cIdentity
    infer env (Dot l e n)
      | Just m <- isModule env e        = infer env (Var l (QName m n))
      | otherwise                       = do t <- infer env e
                                             t0 <- newTVar
                                             constrain [Sel t n t0]
                                             return t0
    infer env (DotI l e i)              = do t <- infer env e
                                             t0 <- newTVar
                                             constrain [Sel t (rPos i) t0]
                                             return t0
    infer env (Lambda l p k e)
      | nodup (p,k)                     = do fx <- newTVar
                                             pushFX fx
                                             (te0, prow) <- infEnvT env1 p
                                             (te1, krow) <- infEnvT (define te0 env1) k
                                             t <- infer (define te1 (define te0 env1)) e
                                             popFX
                                             dump [INS l $ tFun fx prow krow t]
                                             return (tFun fx prow krow t)
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs)           = do prow <- infer env pargs
                                             return (tTuple prow)
    infer env (TupleComp l expr co)
      | nodup co                        = do te <- infEnv env co
                                             _ <- infer (define te env) expr
                                             prow <- newTVar
                                             return (tTuple prow)               -- !! Extreme short-cut, for now
    infer env (Record l kargs)          = do krow <- infer env kargs
                                             return (tRecord krow)
    infer env (RecordComp l n expr co)
      | nodup co                        = do te <- infEnv env co
                                             let env1 = define te env
                                             _ <- infer env1 (Var (nloc n) (NoQual n))
                                             _ <- infer env1 expr
                                             krow <- newTVar
                                             return (tRecord krow)              -- !! Extreme short-cut, for now
    infer env (List l es)               = do t0 <- newTVar
                                             infElems env es pSequence t0
    infer env (ListComp l e1 co)
      | nodup co                        = do te <- infEnv env co
                                             newTVar >>= infElems (define te env) [e1] pSequence
    infer env (Set l es)                = do t0 <- newTVar
                                             infElems env es pSet t0
    infer env (SetComp l e1 co)
      | nodup co                        = do te <- infEnv env co
                                             newTVar >>= infElems (define te env) [e1] pSet
    infer env (Dict l as)               = do tk <- newTVar
                                             tv <- newTVar
                                             infAssocs env as tk tv
    infer env (DictComp l a1 co)
      | nodup co                        = do te <- infEnv env co
                                             tk <- newTVar
                                             tv <- newTVar
                                             infAssocs (define te env) [a1] tk tv
    infer env (Paren l e)               = infer env e


isModule env e                          = fmap ModName $ mfilter (isMod env) $ fmap reverse $ dotChain e
  where dotChain (Var _ (NoQual n))     = Just [n]
        dotChain (Dot _ e n)            = fmap (n:) (dotChain e)
        dotChain _                      = Nothing


infElems env [] tc t0                   = return (tc t0)
infElems env (Elem e : es) tc t0        = do t <- infer env e
                                             constrain [Sub t t0]
                                             infElems env es tc t0
infElems env (Star e : es) tc t0        = do t <- infer env e
                                             constrain [Sub t (tc t0)]
                                             infElems env es tc t0

infAssocs env [] tk tv                  = return (pMapping tk tv)
infAssocs env (Assoc k v : as) tk tv    = do tk' <- infer env k
                                             tv' <- infer env v
                                             constrain [Sub tk' tk, Sub tv' tv]
                                             infAssocs env as tv tk
infAssocs env (StarStar e : as) tk tv   = do t <- infer env e
                                             constrain [Sub t (pMapping tk tv)]
                                             infAssocs env as tk tv

inferBool env e                         = do t <- infer env e
                                             constrain [Impl t cBoolean]
                                             return ()

inferSlice env (Sliz l e1 e2 e3)        = do ts <- mapM (infer env) es
                                             constrain [ Equ t tInt | (t,e) <- ts `zip` es ]
                                             return ()
  where es                              = concat $ map maybeToList (e1:e1:maybeToList e3)


instance (Infer a) => Infer (Maybe a) where
    infer env Nothing                   = newTVar
    infer env (Just x)                  = infer env x

instance InfEnvT PosPar where
    infEnvT env (PosPar n ann e p)      = do sc <- maybe (monotype <$> newTVar) (wellFormed env) ann
                                             t <- infer env e
                                             constrain [SubGen (monotype t) sc]
                                             (te,r) <- infEnvT (define (nVar' n sc) env) p
                                             return (nVar' n sc ++ te, posRow sc r)
    infEnvT env (PosSTAR n ann)         = do r <- newTVar
                                             t <- maybe newTVar (wellFormed env) ann
                                             constrain [Sub t (tTuple r)]
                                             return (nVar n t, r)
    infEnvT env PosNIL                  = return ([], posNil)

instance InfEnvT KwdPar where
    infEnvT env (KwdPar n ann e k)      = do sc <- maybe (monotype <$> newTVar) (wellFormed env) ann
                                             t <- infer env e
                                             constrain [SubGen (monotype t) sc]
                                             (te,r) <- infEnvT (define (nVar' n sc) env) k
                                             return (nVar' n sc ++ te, kwdRow n sc r)
    infEnvT env (KwdSTAR n ann)         = do r <- newTVar
                                             t <- maybe newTVar (wellFormed env) ann
                                             constrain [Sub t (tRecord r)]
                                             return (nVar n t, r)
    infEnvT env KwdNIL                  = return ([], kwdNil)

instance Infer PosArg where
    infer env (PosArg e p)              = do t <- infer env e
                                             prow <- infer env p
                                             sc <- gen1 env (rPos $ rowDepth prow + 1) t NoDec
                                             return (posRow sc prow)
    infer env (PosStar e)               = do t <- infer env e
                                             prow <- newTVar
                                             constrain [Sub t (tTuple prow)]
                                             return prow
    infer env PosNil                    = return posNil
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do t <- infer env e
                                             sc <- gen1 env n t NoDec
                                             krow <- infer env k
                                             return (kwdRow n sc krow)
    infer env (KwdStar e)               = do t <- infer env e
                                             krow <- newTVar
                                             constrain [Sub t (tRecord krow)]
                                             return krow
    infer env KwdNil                    = return kwdNil
    
instance InfEnv Comp where
    infEnv env NoComp                   = return []
    infEnv env (CompIf l e c)           = do inferBool env e
                                             infEnv env c
    infEnv env (CompFor l p e c)        = do (te1, t1) <- infEnvT env p
                                             t2 <- infer env e
                                             te2 <- infEnv (define te1 env) c
                                             constrain [Impl t2 (cCollection t1)]
                                             return (nCombine te1 te2)

instance Infer Exception where
    infer env (Exception e1 Nothing)    = do t1 <- infer env e1
                                             constrain [Sub t1 tException]
                                             return t1
    infer env (Exception e1 (Just e2))  = do t1 <- infer env e1
                                             constrain [Sub t1 tException]
                                             t2 <- infer env e2
                                             constrain [Sub t2 (tOpt tException)]
                                             return t1

instance InfEnvT [Pattern] where
    infEnvT env []                      = do t <- newTVar
                                             return ([], t)
    infEnvT env (p:ps)                  = do (te1, t1) <- infEnvT env p
                                             (te2, t2) <- infEnvT (define te1 env) ps
                                             constrain [Equ t1 t2]
                                             return (nCombine te1 te2, t1)

instance InfEnvT (Maybe Pattern) where
    infEnvT env Nothing                 = do t <- newTVar
                                             return ([], pSequence t)
    infEnvT env (Just p)                = do (te, t) <- infEnvT env p
                                             return (te, pSequence t)

instance InfEnvT PosPat where
    infEnvT env (PosPat p ps)           = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT (define te1 env) ps
                                             return (nCombine te1 te2, posRow (monotype t) r)
    infEnvT env (PosPatStar p)          = do (te, t) <- infEnvT env p
                                             r <- newTVar
                                             constrain [Equ t (tTuple r)]
                                             return (te, r)
    infEnvT env PosPatNil               = return ([], posNil)


instance InfEnvT KwdPat where
    infEnvT env (KwdPat n p ps)         = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT (define te1 env) ps
                                             return (nCombine te1 te2, kwdRow n (monotype t) r)
    infEnvT env (KwdPatStar p)          = do (te, t) <- infEnvT env p
                                             r <- newTVar
                                             constrain [Equ t (tRecord r)]
                                             return (te, r)
    infEnvT env KwdPatNil               = return ([], kwdNil)


instance InfEnvT Pattern where
    infEnvT env (PVar _ n ann)          = do t0 <- maybe newTVar (wellFormed env) ann
                                             case reservedOrSig n env of
                                                 Just Nothing ->
                                                     return (nVar n t0, t0)
                                                 Just (Just t1) -> do
                                                     constrain [EquGen (monotype t0) t1]
                                                     return (nVar' n t1, t0)        -- TODO: return scheme t1 instead
                                                 Nothing ->
                                                     case findVarType n env of
                                                         TSchema _ [] t _ -> return ([], t)
                                                         _ -> err1 n "Polymorphic variable not assignable:"
    infEnvT env (PIndex l e [i])        = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newTVar
                                             constrain [Impl t (cIndexed ti t0)]    -- TODO: ensure MutableIndexed
                                             equFX (fxMut tWild)
                                             return ([], t0)
    infEnvT env (PSlice l e [s])        = do t <- infer env e
                                             ti <- inferSlice env s
                                             constrain [Impl t cSliceable]          -- TODO: ensure MutableSliceable
                                             equFX (fxMut tWild)
                                             return ([], t)
    infEnvT env (PDot l e n)            = do t <- infer env e
                                             t0 <- newTVar
                                             constrain [Mut t n t0]
                                             equFX (fxMut tWild)
                                             return ([], t0)
    infEnvT env (PTuple _ ps)           = do (te, prow) <- infEnvT env ps
                                             return (te, tTuple prow)
--    infEnvT env (PRecord _ ps)          = do (te, krow) <- infEnvT env ps
--                                             return (te, tRecord krow)
    infEnvT env (PList _ ps p)          = do (te1, t1) <- infEnvT env ps
                                             (te2, t2) <- infEnvT (define te1 env) p
                                             constrain [Equ (pSequence t1) t2]
                                             return (nCombine te1 te2, t2)
    infEnvT env (PParen l p)            = infEnvT env p
    infEnvT env (PData l n es)          = do t0 <- newTVar
                                             t <- inferIxs env t0 es
                                             return (nVar n t0, t)

inferIxs env t0 []                      = return t0
inferIxs env t0 (i:is)                  = do t1 <- newTVar
                                             ti <- infer env i
                                             constrain [Impl t0 (cIndexed ti t1)]
                                             inferIxs env t1 is

instance Infer Pattern where
    infer env p                         = noenv <$> infEnvT env p
      where noenv ([], t)               = t
            noenv (te, _)               = nameNotFound (head (dom te))
                                             

-- Well-formed types ------------------------------------------------------

wellFormed env t
  | wfmd env True t                 = instwild t

wf env t                            = wfmd env False t

class WellFormed a where
    wfmd                            :: Env -> Bool -> a -> Bool
    instwild                        :: a -> TypeM a

instance WellFormed [TBind] where
    wfmd env w []                   = True
    wfmd env w (b:bs)               = wfmd env w b && wfmd env1 w bs
      where env1                    = defineTVars [b] env
    instwild                        = mapM instwild

instance WellFormed [TCon] where
    wfmd env w                      = all (wfmd env w)
    instwild                        = mapM instwild

instance WellFormed TSchema where
    wfmd env w (TSchema l [] t d)   = wfmd env1 w t
      where q                       = [ TBind tv [] | tv <- tyfree t \\ tvarScope env ]
            env1                    = defineTVars q env
    wfmd env w (TSchema l q t d)    = wfmd env False q && wfmd env1 w t
      where env1                    = defineTVars q env

    instwild (TSchema l q t d)      = TSchema l q <$> instwild t <*> return d

instance WellFormed TBind where
    wfmd env w (TBind tv us)        = all (wfmd env w) us
    instwild (TBind tv us)          = TBind tv <$> mapM instwild us

instance WellFormed TCon where
    wfmd env w (TC c ts)            = all (wfmd env w) ts
    instwild (TC c ts)              = TC c <$> mapM instwild ts

instance WellFormed Type where
    wfmd env False (TWild l)        = err l "Illegal wildcard type"
    wfmd env w (TVar _ tv)
      | tv `notElem` tvarScope env  = err1 tv "Unbound type variable"
    wfmd env w (TCon _ tc)          = wfmd env w tc
    wfmd env w (TAt _ tc)           = wfmd env w tc
    wfmd env w (TFun _ e p k t)     = wfmd env w e && wfmd env w p && wfmd env w k && wfmd env w t
    wfmd env w (TTuple _ p)         = wfmd env w p
    wfmd env w (TRecord _ k)        = wfmd env w k
    wfmd env w (TOpt _ t)           = wfmd env w t
    wfmd env w (TRow _ n t r)       = wfmd env w t && wfmd env w r
    wfmd env w t                    = True

    instwild (TWild _)              = newTVar
    instwild (TCon l tc)            = TCon l <$> instwild tc
    instwild (TAt l tc)             = TAt l <$> instwild tc
    instwild (TFun l e p k t)       = TFun l <$> instwild e <*> instwild p <*> instwild k <*> instwild t
    instwild (TTuple l p)           = TTuple l <$> instwild p
    instwild (TRecord l k)          = TRecord l <$> instwild k
    instwild (TOpt l t)             = TOpt l <$> instwild t
    instwild (TRow l n t r)         = TRow l n <$> instwild t <*> instwild r
    instwild t                      = return t


-- FX presentation ---------------------

openFX (TSchema l q (TFun l' fx p r t) dec)
  | Just fx1 <- open fx                 = TSchema l (TBind v [] : q) (TFun l' fx1 p r t) dec
  where open (TRow l n t fx)            = TRow l n t <$> open fx
        open (TNil l)                   = Just (TVar l v)
        open (TVar _ _)                 = Nothing
        v                               = head (tvarSupply \\ tybound q)
openFX t                                = t

closeFX (TSchema l q f@(TFun l' fx p r t) dec)
  | TVar _ v <- rowTail fx, sole v      = TSchema l (filter ((v`notElem`) . tybound) q) (TFun l' (subst [(v,tNil)] fx) p r t) dec
  where sole v                          = v `elem` tybound q && length (filter (==v) (tyfree q ++ tyfree f)) == 1
closeFX t                               = t
