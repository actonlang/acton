{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct,solverError) where

import Control.Monad
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Env
import Acton.Solver
import qualified InterfaceFiles

reconstruct                             :: String -> Env -> Module -> IO (TEnv, Module, SrcInfo)
reconstruct outname env modul           = do InterfaceFiles.writeFile (outname ++ ".ty") (unalias env1 te)
                                             return (te, modul', info)
  where Module m imp suite              = modul
        env1                            = reserve (bound suite) env
        ((te,suite'),info)              = runTypeM $ (,) <$> infTop env1 suite <*> getDump
        modul'                          = Module m imp suite'

solverError                             = typeError


nodup x
  | not $ null vs                       = err2 vs "Duplicate names:"
  | otherwise                           = True
  where vs                              = duplicates (bound x)

noshadow svs x
  | not $ null vs                       = err2 vs "Illegal state shadowing:"
  | otherwise                           = True
  where vs                              = intersect (bound x) svs

noescape (te,e)                                                                       -- TODO: check for escaping classes/protocols
  | otherwise                           = (te,e)


-- Infer -------------------------------

infTop                                  :: Env -> Suite -> TypeM (TEnv,Suite)
infTop env ss                           = do pushFX fxNil
                                             (te,ss1) <- infEnv env ss
                                             popFX
                                             cs <- collectConstraints
                                             solve cs
                                             te1 <- msubst te
                                             return (te1, ss1)

class Infer a where
    infer                               :: Env -> a -> TypeM (Type,a)

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM (TEnv,a)

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (TEnv,Type,a)

class InfData a where
    infData                             :: Env -> a -> TypeM TEnv


splitGen                                :: Env -> [TVar] -> TEnv -> Constraints -> TypeM (Constraints, TEnv)
splitGen env tvs te cs
  | null ambig_cs                       = return (fixed_cs, mapVars generalize te)
  | otherwise                           = do solve ambig_cs
                                             cs1 <- simplify (fixed_cs++gen_cs)
                                             te1 <- msubst te
                                             tvs1 <- msubstTV tvs
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
    bound (Sub env (TVar _ v) (TCon _ u))   = Just $ TBind v [u]                            -- TODO: capture env!!!!!!!!
    bound (Impl env (TVar _ v) u)           = Just $ TBind v [u]                            -- TODO: capture env!!!!!!!!
    bound c                             = trace ("### Unreduced constraint: " ++ prstr c) $ Nothing
    collect vs []                       = []
    collect vs (TBind v us : q)
      | v `elem` vs                     = collect vs q
      | otherwise                       = TBind v (us ++ concat [ us' | TBind v' us' <- q, v' == v ]) : collect (v:vs) q


genTEnv                                 :: Env -> TEnv -> TypeM TEnv
genTEnv env te                          = do cs <- collectConstraints
                                             cs1 <- simplify cs
                                             te1 <- msubst te
                                             tvs <- msubstTV (tyfree env)
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
  | fallsthru x                         = do (te,x') <- noescape <$> infEnv env x
                                             return (Just te, x')
  | otherwise                           = do (te,x') <- noescape <$> infEnv env x
                                             return (Nothing, x')

liveCombine te Nothing                  = te
liveCombine Nothing te'                 = te'
liveCombine (Just te) (Just te')        = Just $ nCombine te te'


instance (InfEnv a) => InfEnv [a] where
    infEnv env []                       = return (nEmpty, [])
    infEnv env (s : ss)                 = do (te1,s') <- infEnv env s
                                             (te2,ss') <- infEnv (define te1 env) ss
                                             return (nCombine te1 te2, s':ss')

instance InfEnv Stmt where
    infEnv env (Expr l e)               = do (_,e') <- infer env e
                                             return (nEmpty, Expr l e')
    infEnv env (Assign l pats e)
      | nodup pats                      = do (t1,e') <- infer env e
                                             (te,t2,pats') <- infEnvT env pats
                                             constrain [Sub env t1 t2]
                                             return (te, Assign l pats' e')
    infEnv env (Update l ts e)          = do (t1,e') <- infer env e
                                             (t2,ts') <- infer env ts
                                             constrain [Sub env t1 t2]
                                             return (nEmpty, Update l ts' e')
    infEnv env (IUpdate l t (Op _ o) e) = do (t1,t') <- infer env t
                                             (t2,e') <- infer env e
                                             w <- newName "AugOp"
                                             constrain [Sub env t2 t1, Impl env t1 (protocol o)]
                                             return ([], Update l [t'] (eCall (eDot (eVar w) (method o)) [t2e t',e']))
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
            method PlusA                = iaddKW
            method MinusA               = isubKW
            method MultA                = imulKW
            method PowA                 = ipowKW
            method DivA                 = itruedivKW
            method ModA                 = imodKW
            method EuDivA               = ifloordivKW
            method ShiftLA              = ilshiftKW
            method ShiftRA              = irshiftKW
            method BOrA                 = iorKW
            method BXorA                = ixorKW
            method BAndA                = iandKW
            method MMultA               = imatmulKW
            t2e (TaVar l n)             = Var l (NoQual n)
            t2e (TaIndex l e ix)        = Index l e ix
            t2e (TaSlice l e sl)        = Slice l e sl
            t2e (TaDot l e n)           = Dot l e n
            t2e (TaDotI l e i tl)       = DotI l e i tl
            t2e (TaParen l tg)          = Paren l (t2e tg)
            t2e (TaTuple l tgs)         = Tuple l (foldr PosArg PosNil $ map t2e tgs) KwdNil
    infEnv env (Assert l e1 e2)         = do e1' <- inferBool env e1
                                             (t,e2') <- infer env e2
                                             constrain [Sub env t tStr]
                                             return (nEmpty, Assert l e1' e2')
    infEnv env s@(Pass l)               = return ([], s)
    infEnv env (Delete l pat)           = undefined
--      | nodup pat                       = do (_,pat') <- infer env pat                 -- TODO: constrain pat targets to opt type
--                                            return (nEmpty, Delete l pat')
    infEnv env s@(Return l Nothing)     = do subFX env (fxRet tNone tWild)
                                             return (nEmpty, s)
    infEnv env (Return l (Just e))      = do (t,e') <- infer env e
                                             subFX env (fxRet t tWild)
                                             return (nEmpty, Return l (Just e'))
    infEnv env s@(Raise _ Nothing)      = return (nEmpty, s)
    infEnv env (Raise l (Just e))       = do (_,e') <- infer env e
                                             return (nEmpty, Raise l (Just e'))
    infEnv env s@(Break _)              = return (nEmpty, s)
    infEnv env s@(Continue _)           = return (nEmpty, s)
    infEnv env (If l bs els)            = do (tes,bs') <- fmap unzip $ mapM (infLiveEnv env) bs
                                             (te,els') <- infLiveEnv env els
                                             te1 <- commonTEnv env $ catMaybes (te:tes)
                                             return (te1, If l bs' els')
    infEnv env (While l e b els)        = do e' <- inferBool env e
                                             (_,b') <- noescape <$> infEnv env b
                                             (_,els') <- noescape <$> infEnv env els
                                             return (nEmpty, While l e' b' els')
    infEnv env (For l p e b els)
      | nodup p                         = do (te,t1,p') <- infEnvT env p
                                             (t2,e') <- infer env e
                                             (_,b') <- noescape <$> infEnv (define te env) b
                                             (_,els') <- noescape <$> infEnv env els
                                             constrain [Impl env t2 (cIterable t1)]
                                             return (nEmpty, For l p' e' b' els')
    infEnv env (Try l b hs els fin)     = do (te,b') <- infLiveEnv env b
                                             (te',els') <- infLiveEnv (maybe id define te $ env) els
                                             (tes,hs') <- fmap unzip $ mapM (infLiveEnv env) hs
                                             te1 <- commonTEnv env $ catMaybes $ (liveCombine te te'):tes
                                             (te2,fin') <- noescape <$> infEnv (define te1 env) fin
                                             return (nCombine te1 te2, Try l b' hs' els' fin')
    infEnv env (With l items b)
      | nodup items                     = do (te,items') <- infEnv env items
                                             (te1,b') <- noescape <$> infEnv (define te env) b
                                             return $ (prune (dom te) te1, With l items' b')

    infEnv env (VarAssign l pats e)
      | nodup pats                      = do (t1,e') <- infer env e
                                             (te,t2,pats') <- infEnvT env pats
                                             constrain [Sub env t1 t2]
                                             return (nState te, VarAssign l pats' e')
    
    infEnv env (After l e n ps ks)      = do (t1,e') <- infer env e
                                             constrain [Equ env t1 tInt]
                                             (cs,t2) <- instantiate env $ openFX $ findVarType n env
                                             constrain cs
                                             dump [INS (loc n) t2]
                                             (prow,ps') <- infer env ps
                                             (krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX                                       -- ..............
                                             constrain [Sub env t2 (tFun fx prow krow t0)]
                                             return (nEmpty, After l e' n ps' ks')
    
    infEnv env (Decl l ds)
      | nodup ds && noCheck env         = do (te1,ds1) <- infEnv env ds
                                             return (te1, Decl l ds1)
      | otherwise                       = do (te1,ds1) <- infEnv (setNoCheck env) ds
                                             ds2 <- check (define te1 env) ds1
                                             te2 <- genTEnv env te1
                                             return (te2, Decl l ds2)
            

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
    infEnv env d@(Actor _ n _ p k _ _)
      | not $ reservedOrSig n env       = illegalRedef n
      | nodup (p,k)                     = do sc <- extractSchema env d
                                             return (nVar' n sc, d)
    infEnv env d@(Def _ n _ p k _ _ _)
      | not $ reservedOrSig n env       = illegalRedef n
      | nodup (p,k)                     = do sc <- extractSchema env d
                                             return (nVar' n sc, d)
    infEnv env (Class l n q us b)
      | not $ reserved n env            = illegalRedef n
      | otherwise                       = do (te,b') <- noescape <$> infEnv env1 b
                                             return (nClass n q (mro env1 us1) te, Class l n q us1 b')
      where env1                        = reserve (bound b) $ defineSelf n q $ defineTVars q $ block (stateScope env) env
            us1                         = classBases env us
    infEnv env (Protocol l n q us b)
      | not $ reserved n env            = illegalRedef n
      | otherwise                       = do (te,b') <- infEnv env1 b
                                             return (nProto n q (mro env1 us1) te, Protocol l n q us1 b')
      where env1                        = reserve (bound b) $ defineSelf n q $ defineTVars q $ block (stateScope env) env  
            us1                         = protoBases env us
    infEnv env d@(Extension _ n q us b)
      | isProto env n                   = notYet (loc n) "Extension of a protocol"
      | otherwise                       = do w <- newName (nstr $ noqual n)
                                             return (nExt w n q (mro env1 (protoBases env us)), d)
      where env1                        = reserve (bound b) $ defineSelf' n q $ defineTVars q $ block (stateScope env) env
            u                           = TC n [ tVar tv | TBind tv _ <- q ]
    infEnv env d@(Signature _ ns sc)
      | not $ null redefs               = illegalRedef (head redefs)
      | otherwise                       = do return (nSig ns sc, d)
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
          | all entail cs               = (u:us') : linearizations us
          | otherwise                   = err1 u ("Type context too weak to entail")
          where (_,cs,us',_)            = findCon env u


class Check a where
    check                               :: Env -> a -> TypeM a

instance (Check a) => Check [a] where
    check env ds                        = mapM (check env) ds

instance Check Stmt where
    check env (Decl l ds)               = Decl l <$> check env ds
    check env s                         = return s

instance Check Decl where
    check env (Actor l n q p k ann b)
      | noshadow svars (p,k)            = do pushFX (fxAct tWild)
                                             (te0,prow,p') <- infEnvT env p
                                             (te1,krow,k') <- infEnvT (define te0 env1) k
                                             (te2,b') <- noescape <$> infEnv (define te1 (define te0 env1)) b
                                             te3 <- genTEnv env te2
                                             fx <- fxAct <$> newRowVar
                                             checkAssump env n (tFun fx prow krow (tRecord $ env2row kwdNil $ nVars te3))
                                             return $ Actor l n q p' k' ann b'
      where svars                       = statedefs b
            env0                        = define envActorSelf $ defineTVars q $ block (stateScope env) env
            env1                        = reserve (bound (p,k) ++ bound b ++ svars) env0

    check env (Def l n q p k ann b modif)
                                        = do t <- newTVar
                                             fx <- newRowVar
                                             pushFX (fxRet t fx)
                                             when (fallsthru b) (subFX env (fxRet tNone tWild))
                                             (te0,prow,p') <- infEnvT env p
                                             (te1,krow,k') <- infEnvT (define te0 env1) k
                                             (_,b') <- noescape <$> infEnv (define te1 (define te0 env1)) b
                                             popFX
                                             (prow',krow') <- splitRows modif prow krow
                                             checkAssump env n (tFun fx prow' krow' t)
                                             return $ Def l n q p' k' ann b' modif
      where env1                        = reserve (bound (p,k) ++ bound b) $ defineTVars q $ block (stateScope env) env
            splitRows m p@(TNil _ _) k  = (,) <$> return p <*> splitRow m k
            splitRows m p k             = (,) <$> splitRow m p <*> return k
            splitRow (ClassAttr _) (TRow _ _ n sc r)
                                        = constrain [Equ env (monotypeOf sc) tSelf] >> return r
            splitRow m r                = return r

    check env (Class l n q us b)        = do pushFX fxNil
                                             b' <- check (define te env1) b
                                             popFX
                                             checkBindings env False us te
                                             return $ Class l n q us b'
      where env1                        = defineSelf n q $ defineTVars q $ block (stateScope env) env
            (q,us,te)                   = findClass (NoQual n) env

    check env (Protocol l n q us b)     = do pushFX fxNil
                                             b' <- check (define te env1) b
                                             popFX
                                             checkBindings env True us te
                                             return $ Protocol l n q us b'         -- TODO: add Self to q
      where env1                        = defineSelf n q $ defineTVars q $ block (stateScope env) env
            (q,us,te)                   = findProto (NoQual n) env

    check env (Extension l n q us b)    = do pushFX fxNil
                                             (te,b') <- infEnv env1 b
                                             popFX
                                             checkBindings env False us te
                                             return $ Class l w [] us b'        -- TODO: properly mix in n and q in us......
      where env1                        = reserve (bound b) $ defineSelf' n q $ defineTVars q $ block (stateScope env) env
            Just w                      = findWitness env n q us
    check env d@(Signature l ns sc)     = return d


checkBindings env proto us te
  | proto && (not $ null unsigs)        = lackSig unsigs
  | not proto && (not $ null undefs)    = lackDef undefs
  | otherwise                           = constrain refinements
  where tes                             = [ te' | u <- us, let (_,_,_,te') = findCon env u ]
        inherited                       = concatMap nSigs tes ++ concatMap nVars tes
        refinements                     = [ SubGen env sc sc' | (n,sc) <- nSigs te, Just sc' <- [lookup n inherited] ]
        undefs                          = (dom $ csigs) \\ (dom $ nVars te ++ concatMap nVars tes)
        unsigs                          = dom te \\ (dom (nSigs te) ++ dom inherited)
        allsigs                         = nSigs te ++ concatMap nSigs tes
        (isigs,csigs)                   = partition isInstAttr allsigs
          where isInstAttr (n,sc)       = case sc of TSchema _ _ _ (InstAttr _) -> True; _ -> False


checkAssump env n t                     = case findVarType n env of
                                            (TSchema _ [] t' _) -> 
                                               constrain [Equ env t t']
                                            sc -> do
                                               sc' <- gen1 env n t (scdec sc)   -- TODO: verify that generalizing one decl at a time is ok
                                               constrain [EquGen env sc' sc]

inferPure env e                         = do pushFX fxNil
                                             t <- infer env e
                                             popFX
                                             return t

env2row                                 = foldl (\r (n,t) -> kwdRow n t r)           -- TODO: stabilize this...

instance InfEnv Branch where
    infEnv env (Branch e b)             = do e' <- inferBool env e
                                             (te,b') <- noescape <$> infEnv env b
                                             return (te, Branch e' b')

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do (t,e') <- infer env e
                                             constrain [Impl env t cContextManager]
                                             return (nEmpty, WithItem e' Nothing)
    infEnv env (WithItem e (Just p))    = do (t1,e') <- infer env e
                                             (te,t2,p') <- infEnvT env p
                                             constrain [Equ env t1 t2, Impl env t1 cContextManager]
                                             return (te, WithItem e' (Just p'))

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do (te,ex') <- infEnv env ex
                                             (te1,b') <- noescape <$> infEnv (define te env) b
                                             return (prune (dom te) te1, Handler ex' b')

instance InfEnv Except where
    infEnv env ex@(ExceptAll l)         = return (nEmpty, ex)
    infEnv env ex@(Except l x)          = do (q,tc) <- infException env x
                                             return (nEmpty, ex)
    infEnv env ex@(ExceptAs l x n)      = do (q,tc) <- infException env x
                                             return (nVar n (tCon tc) `nCombine` nTVars q, ex)

infException env x
  | Just (_,t) <- sub                   = return (q,tc)
  | otherwise                           = err1 x "Not an Exception sub-class:"
  where (_,q,tc)                        = findSkolemizedCon env x
        sub                             = findSubAxiom env tc qnException

instance Infer Expr where
    infer env (Var l n)                 = do (cs,t) <- instantiate env $ openFX $ findVarType' n env
                                             constrain cs
                                             return (t, Var l n)
    infer env e@(Int _ val s)           = return (tInt, e)
    infer env e@(Float _ val s)         = return (tFloat, e)
    infer env e@Imaginary{}             = notYetExpr e
    infer env e@(Bool _ val)            = return (tBool, e)
    infer env e@(None _)                = return (tNone, e)
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env e@(Strings _ ss)          = return $ (tUnion [ULit $ concat ss], e)
    infer env e@(BStrings _ ss)         = return (tBytes, e)
    infer env (Call l e ps ks)          = do (t,e') <- infer env e
                                             dump [INS (loc e) t]
                                             (prow,ps') <- infer env ps
                                             (krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX
                                             constrain [Sub env t (tFun fx prow krow t0)]
                                             return (t0, Call l e' ps' ks')
    infer env (Await l e)               = do (t,e') <- infer env e
                                             t0 <- newTVar
                                             fx <- fxAwait <$> newRowVar
                                             equFX env fx
                                             constrain [Sub env t (tMsg t0)]
                                             return (t0, Await l e')
    infer env (Index l e ixs)           = do (t,e') <- infer env e
                                             (ti,ix') <- infer env ix
                                             t0 <- newTVar
                                             w <- newName "Indexed"
                                             constrain [Impl env t (cIndexed ti t0)]
                                             return (t0, eCall (eDot (eVar w) getitemKW) [e',ix'])
      where ix | length ixs == 1        = head ixs
               | otherwise              = Tuple NoLoc (foldr PosArg PosNil ixs) KwdNil
    infer env (Slice l e slz)           = do (t,e') <- infer env e
                                             sl' <- inferSlice env sl
                                             w <- newName "Sliceable"
                                             constrain [Impl env t cSliceable]
                                             return (t, eCall (eDot (eVar w) getsliceKW) (e' : toArgs sl'))
      where sl | length slz == 1        = head slz
               | otherwise              = notYet l "Multidimensional slicing"
            toArgs (Sliz _ e1 e2 e3)    = map (maybe eNone id) [e1,e2,e3]
    infer env (Cond l e1 e e2)          = do (t1,e1') <- infer env e1
                                             (t2,e2') <- infer env e2
                                             e' <- inferBool env e
                                             t0 <- newTVar
                                             constrain [Sub env t1 t0, Sub env t2 t0]
                                             return (t0, Cond l e1' e' e2')
    infer env (BinOp l e1 o@(Op _ op) e2)
      | op `elem` [Or,And]              = do e1' <- inferBool env e1
                                             e2' <- inferBool env e2
                                             return (tBool, BinOp l e1' o e2')
      | otherwise                       = do (t1,e1') <- infer env e1
                                             (t2,e2') <- infer env e2
                                             t <- newTVar
                                             w <- newName "BinOp"
                                             constrain [Sub env t1 t, Sub env t2 t, Impl env t (protocol op)]
                                             return (t, eCall (eDot (eVar w) (method op)) [e1',e2'])
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
            method Plus                 = addKW
            method Minus                = subKW
            method Mult                 = mulKW
            method Pow                  = powKW
            method Div                  = truedivKW
            method Mod                  = modKW
            method EuDiv                = floordivKW
            method ShiftL               = lshiftKW
            method ShiftR               = rshiftKW
            method BOr                  = orKW
            method BXor                 = xorKW
            method BAnd                 = andKW
            method MMult                = matmulKW
    infer env (UnOp l o@(Op _ op) e)
      | op == Not                       = do e' <- inferBool env e
                                             return (tBool, UnOp l o e')
      | otherwise                       = do (t,e') <- infer env e
                                             w <- newName "UnOp"
                                             constrain [Impl env t (protocol op)]
                                             return (t, eCall (eDot (eVar w) (method op)) [e'])
      where protocol UPlus              = cNumber
            protocol UMinus             = cNumber
            protocol BNot               = cIntegral
            method UPlus                = posKW
            method UMinus               = negKW
            method BNot                 = invertKW
    infer env (CompOp l e1 [OpArg (Op _ op) e2])
      | op `elem` [In,NotIn]            = do (t1,e1') <- infer env e1
                                             (t2,e2') <- infer env e2
                                             w1 <- newName "Container"
                                             w2 <- newName "Comp"
                                             constrain [Impl env t2 (cContainer t1), Impl env t1 cEq]
                                             return (tBool, eCall (eDot (eVar w1) (method op)) [eVar w2, e1', e2'])
      | otherwise                       = do (t1,e1') <- infer env e1
                                             (t2,e2') <- infer env e2
                                             t <- newTVar
                                             w <- newName "Comp"
                                             constrain [Sub env t1 t, Sub env t2 t, Impl env t (protocol op)]
                                             return (tBool, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Eq                 = cEq
            protocol NEq                = cEq
            protocol LtGt               = cEq
            protocol Lt                 = cOrd
            protocol Gt                 = cOrd
            protocol LE                 = cOrd
            protocol GE                 = cOrd
            protocol Is                 = cIdentity
            protocol IsNot              = cIdentity
            method Eq                   = eqKW
            method NEq                  = neKW
            method LtGt                 = neKW
            method Lt                   = ltKW
            method Gt                   = gtKW
            method LE                   = leKW
            method GE                   = geKW
            method Is                   = isKW
            method IsNot                = isnotKW
            method In                   = containsKW
            method NotIn                = containsnotKW
    infer env (CompOp l e1 ops)         = notYet l "Comparison chaining"
    infer env (Dot l e n)
      | Just m <- isModule env e        = infer env (Var l (QName m n))
      | otherwise                       = do (t,e') <- infer env e
                                             t0 <- newTVar
                                             constrain [Sel env t n t0]
                                             return (t0, Dot l e' n)
    infer env (DotI l e i False)        = do (t,e') <- infer env e
                                             t0 <- newTVar
                                             constrain [Sel env t (rPos i) t0]
                                             return (t0, DotI l e' i False)
    infer env (DotI l e i True)         = undefined                                             
    infer env (Lambda l p k e)
      | nodup (p,k)                     = do fx <- newRowVar
                                             pushFX fx
                                             (te0, prow, p') <- infEnvT env1 p
                                             (te1, krow, k') <- infEnvT (define te0 env1) k
                                             (t,e') <- infer (define te1 (define te0 env1)) e
                                             popFX
                                             dump [INS l $ tFun fx prow krow t]
                                             return (tFun fx prow krow t, Lambda l p' k' e')
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs kargs)     = do (prow,pargs') <- infer env pargs
                                             (krow,kargs') <- infer env kargs
                                             return (TTuple NoLoc prow krow, Tuple l pargs' kargs')
    infer env (List l es)               = do t0 <- newTVar
                                             es' <- infElems env es pSequence t0
                                             return (pSequence t0, List l es')
    infer env (ListComp l e1 co)
      | nodup co                        = do (te,co') <- infEnv env co
                                             t0 <- newTVar
                                             es <- infElems (define te env) [e1] pSequence t0
                                             let [e1'] = es
                                             return (pSequence t0, ListComp l e1' co')
    infer env (Set l es)                = do t0 <- newTVar
                                             es'  <- infElems env es pSet t0
                                             return (pSet t0, Set l es')
    infer env (SetComp l e1 co)
      | nodup co                        = do (te,co') <- infEnv env co
                                             t0 <- newTVar
                                             es <- infElems (define te env) [e1] pSet t0
                                             let [e1'] = es
                                             return (pSet t0, SetComp l e1' co')
                                             
    infer env (Dict l as)               = do tk <- newTVar
                                             tv <- newTVar
                                             as' <- infAssocs env as tk tv
                                             return (pMapping tk tv, Dict l as')
    infer env (DictComp l a1 co)
      | nodup co                        = do (te,co') <- infEnv env co
                                             tk <- newTVar
                                             tv <- newTVar
                                             as <- infAssocs (define te env) [a1] tk tv
                                             let [a1'] = as
                                             return (pMapping tk tv, DictComp l a1' co')
    infer env (Paren l e)               = do (t,e') <- infer env e
                                             return (t, Paren l e')


isModule env e                          = fmap ModName $ mfilter (isMod env) $ fmap reverse $ dotChain e
  where dotChain (Var _ (NoQual n))     = Just [n]
        dotChain (Dot _ e n)            = fmap (n:) (dotChain e)
        dotChain _                      = Nothing


infElems env [] tc t0                   = return []
infElems env (Elem e : es) tc t0        = do (t,e') <- infer env e
                                             constrain [Sub env t t0]
                                             es' <- infElems env es tc t0
                                             return (Elem e' : es')
infElems env (Star e : es) tc t0        = do (t,e') <- infer env e
                                             constrain [Sub env t (tc t0)]
                                             es' <- infElems env es tc t0
                                             return (Star e' : es')

infAssocs env [] tk tv                  = return []
infAssocs env (Assoc k v : as) tk tv    = do (tk',k') <- infer env k
                                             (tv',v') <- infer env v
                                             constrain [Sub env tk' tk, Sub env tv' tv]
                                             as' <- infAssocs env as tv tk
                                             return (Assoc k' v' : as')
infAssocs env (StarStar e : as) tk tv   = do (t,e') <- infer env e
                                             constrain [Sub env t (pMapping tk tv)]
                                             as' <- infAssocs env as tk tv
                                             return (StarStar e' : as')

inferBool env e                         = do (t,e') <- infer env e
                                             w <- newName "Boolean"
                                             constrain [Impl env t cBoolean]
                                             return $ eCall (eDot (eVar w) boolKW) [e']

inferSlice env (Sliz l e1 e2 e3)        = do (t1,e1') <- infer env e1
                                             (t2,e2') <- infer env e2
                                             (t3,e3') <- infer env e3
                                             constrain [ Equ env t tInt | t <- [t1,t2,t3] ]
                                             return (Sliz l e1' e2' e3')

inferGen env e                          = do (t,e') <- infer env e
                                             sc <- gen1 env (name "_") t NoDec
                                             return (sc,e')

instance (Infer a) => Infer (Maybe a) where
    infer env Nothing                   = do t <- newTVar
                                             return (t, Nothing)
    infer env (Just x)                  = do (t,e') <- infer env x
                                             return (t, Just e')

instance InfEnvT PosPar where
    infEnvT env (PosPar n ann e p)      = do t <- maybe (monotype <$> newTVar) return ann
                                             (t',e') <- inferGen env e
                                             constrain [SubGen env t' t]
                                             (te,r,p') <- infEnvT (define (nVar' n t) env) p
                                             return (nVar' n t ++ te, posRow t r, PosPar n (Just t) e' p')
    infEnvT env (PosSTAR n ann)         = do t <- maybe (newTVarOfKind KRow) return ann
                                             r <- newRowVar
                                             constrain [Equ env t (tTuple r)]
                                             return (nVar n t, r, PosSTAR n (Just t))
    infEnvT env PosNIL                  = return (nEmpty, posNil, PosNIL)

instance InfEnvT KwdPar where
    infEnvT env (KwdPar n ann e k)      = do t <- maybe (monotype <$> newTVar) return ann
                                             (t',e') <- inferGen env e
                                             constrain [SubGen env t' t]
                                             (te,r,k') <- infEnvT (define (nVar' n t) env) k
                                             return (nVar' n t ++ te, kwdRow n t r, KwdPar n (Just t) e' k')
    infEnvT env (KwdSTAR n ann)         = do t <- maybe (newTVarOfKind KRow) return ann
                                             r <- newRowVar
                                             constrain [Equ env t (tRecord r)]
                                             return (nVar n t, r, KwdSTAR n (Just t))
    infEnvT env KwdNIL                  = return (nEmpty, kwdNil, KwdNIL)

instance Infer PosArg where
    infer env (PosArg e p)              = do (sc,e') <- inferGen env e
                                             (prow,p') <- infer env p
                                             return (posRow sc prow, PosArg e' p')
    infer env (PosStar e)               = do (t,e') <- infer env e
                                             prow <- newRowVar
                                             constrain [Equ env t (tTuple prow)]
                                             return (prow, PosStar e')
    infer env PosNil                    = return (posNil, PosNil)
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do (sc,e') <- inferGen env e
                                             (krow,k') <- infer env k
                                             return (kwdRow n sc krow, KwdArg n e' k')
    infer env (KwdStar e)               = do (t,e') <- infer env e
                                             krow <- newRowVar
                                             constrain [Equ env t (tRecord krow)]
                                             return (krow, KwdStar e')
    infer env KwdNil                    = return (kwdNil, KwdNil)
    
instance InfEnv Comp where
    infEnv env NoComp                   = return (nEmpty, NoComp)
    infEnv env (CompIf l e c)           = do e' <- inferBool env e
                                             (te,c') <- infEnv env c
                                             return (te, CompIf l e' c')
    infEnv env (CompFor l p e c)        = do (te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             (t2,e') <- infer env e
                                             (te2,c') <- infEnv (define te1 env) c
                                             constrain [Impl env t2 (cIterable t1)]
                                             return (nCombine te1 te2, CompFor l p' e' c')

instance Infer Exception where
    infer env (Exception e1 Nothing)    = do (t1,e1') <- infer env e1
                                             constrain [Sub env t1 tException]
                                             return (t1, Exception e1' Nothing)
    infer env (Exception e1 (Just e2))  = do (t1,e1') <- infer env e1
                                             constrain [Sub env t1 tException]
                                             (t2,e2') <- infer env e2
                                             constrain [Sub env t2 (tOpt tException)]
                                             return (t1, Exception e1' (Just e2'))

instance InfEnvT PosPat where
    infEnvT env (PosPat p ps)           = do (te1,t,p') <- infEnvT env p
                                             (te2,r,ps') <- infEnvT env ps
                                             return (nCombine te1 te2, posRow (monotype t) r, PosPat p' ps')
    infEnvT env (PosPatStar p)          = do (te,t,p') <- infEnvT env p
                                             r <- newRowVar
                                             constrain [Equ env t (tTuple r)]
                                             return (te, r, PosPatStar p')
    infEnvT env PosPatNil               = return (nEmpty, posNil, PosPatNil)


instance InfEnvT KwdPat where
    infEnvT env (KwdPat n p ps)         = do (te1,t,p') <- infEnvT env p
                                             (te2,r,ps') <- infEnvT env ps
                                             return (nCombine te1 te2, kwdRow n (monotype t) r, KwdPat n p' ps')
    infEnvT env (KwdPatStar p)          = do (te,t,p') <- infEnvT env p
                                             r <- newRowVar
                                             constrain [Equ env t (tRecord r)]
                                             return (te, r, KwdPatStar p')
    infEnvT env KwdPatNil               = return (nEmpty, kwdNil, KwdPatNil)


instance InfEnvT Pattern where
    infEnvT env (PVar l n ann)
      | reservedOrSig n env             = do t0 <- maybe newTVar return ann
                                             return (nVar n t0, t0, PVar l n (Just t0))
    infEnvT env (PVar l n Nothing)      = case findVarType n env of
                                             TSchema _ [] t _ -> return (nEmpty, t, PVar l n Nothing)
                                             _ -> err1 n "Polymorphic variable not assignable:"
    infEnvT env p@PVar{}                = typedReassign p
    infEnvT env (PTuple l ps ks)        = do (te1,prow,ps') <- infEnvT env ps
                                             (te2,krow,ks') <- infEnvT env ks
                                             return (nCombine te1 te2, TTuple NoLoc prow krow, PTuple l ps' ks')
    infEnvT env (PList l ps p)          = do (te1,t1,ps') <- infEnvT env ps
                                             (te2,t2,p') <- infEnvT (define te1 env) p
                                             constrain [Equ env (pSequence t1) t2]
                                             return (nCombine te1 te2, t2, PList l ps' p')
    infEnvT env (PParen l p)            = do (te,t,p') <- infEnvT env p
                                             return (te, t, PParen l p')
    infEnvT env (PData l n es)          = do t0 <- newTVar
                                             (t,es') <- inferIxs env t0 es
                                             return (nVar n t0, t, PData l n es')

instance InfEnvT (Maybe Pattern) where
    infEnvT env Nothing                 = do t <- newTVar
                                             return (nEmpty, pSequence t, Nothing)
    infEnvT env (Just p)                = do (te,t,p') <- infEnvT env p
                                             return (te, pSequence t, Just p')

instance InfEnvT [Pattern] where
    infEnvT env [p]                     = do (te,t,p') <- infEnvT env p
                                             return (te, t, [p'])
    infEnvT env (p:ps)                  = do (te1,t1,p') <- infEnvT env p
                                             (te2,t2,ps') <- infEnvT env ps
                                             constrain [Equ env t1 t2]
                                             return (nCombine te1 te2, t1, p':ps')

instance Infer [Target] where
    infer env [t]                       = do (t1,t') <- infer env t
                                             return (t1, [t'])
    infer env (t:ts)                    = do (t1,t') <- infer env t
                                             (t2,ts') <- infer env ts
                                             constrain [Equ env t1 t2]
                                             return (t1, t':ts')

instance Infer Target where
    infer env (TaVar l n)               = case findVarType n env of
                                             TSchema _ [] t _ -> return (t, TaVar l n)
                                             _ -> err1 n "Polymorphic variable not assignable:"

    infer env (TaIndex l e [i])         = do (t,e') <- infer env e
                                             (ti,i') <- infer env i
                                             t0 <- newTVar
                                             constrain [Impl env t (cIndexed ti t0), Sub env t tObject]
                                             equFX env (fxMut tWild tWild)
                                             return (t0, TaIndex l e' [i'])
    infer env (TaSlice l e [s])         = do (t,e') <- infer env e
                                             s' <- inferSlice env s
                                             constrain [Impl env t cSliceable, Sub env t tObject]
                                             equFX env (fxMut tWild tWild)
                                             return (t, TaSlice l e' [s'])
    infer env (TaDot l e n)             = do (t,e') <- infer env e
                                             t0 <- newTVar
                                             constrain [Mut env t n t0]
                                             equFX env (fxMut tWild tWild)
                                             return (t0, TaDot l e' n)
    infer env (TaDotI l e i tl)         = do (t,e') <- infer env e
                                             t0 <- newTVar
                                             --constrain [Mut env t n t0]
                                             equFX env (fxMut tWild tWild)
                                             return (t0, TaDotI l e' i tl)
    infer env (TaTuple l targs)         = do (ts,targs') <- unzip <$> mapM (infer env) targs
                                             return (tTuple (foldr posRow' posNil ts), TaTuple l targs')

inferIxs env t0 []                      = return (t0, [])
inferIxs env t0 (i:is)                  = do t1 <- newTVar
                                             (ti,i') <- infer env i
                                             constrain [Impl env t0 (cIndexed ti t1)]
                                             (t, is') <- inferIxs env t1 is
                                             return (t, i':is')



-- Extracting schemas from (actor and def) declarations

class ExtractT a where
    extractT                        :: a -> TypeM Type

instance ExtractT PosPar where
    extractT (PosPar n t _ p)       = posRow <$> maybe (monotype <$> newTVar) return t <*> extractT p
    extractT (PosSTAR n t)          = newTVar        -- safe to ignore type (not schema) annotation t here
    extractT PosNIL                 = return posNil

instance ExtractT KwdPar where
    extractT (KwdPar n t _ k)       = kwdRow n <$> maybe (monotype <$> newTVar) return t <*> extractT k
    extractT (KwdSTAR n t)          = newTVar        -- safe to ignore type (not schema) annotation t here
    extractT KwdNIL                 = return kwdNil

instance ExtractT Decl where
    extractT d@Def{}                = do fx <- newTVarOfKind KRow
                                         pr <- extractT $ pos d
                                         kr <- extractT $ kwd d
                                         let (prow,krow) = chop (deco d) pr kr
                                         tFun fx prow krow <$> maybe newTVar return (ann d)
      where 
        chop (ClassAttr _) p k      = chop1 p k
        chop _ p k                  = (p, k)
        chop1 (TRow _ _ n t p) k    = (p, k)
        chop1 TVar{} k              = missingSelf (dname d)
        chop1 p (TRow _ _ n t k)    = (p, k)
        chop1 _ _                   = missingSelf (dname d)
    extractT d@Actor{}              = do prow <- extractT $ pos d
                                         krow <- extractT $ kwd d
                                         tFun (fxAct fxNil) prow krow <$> maybe newTVar return (ann d)
    extractT _                      = newTVar

extractSchema env d                 = do sig <- extractT d
                                         let q | null (qual d) = [ TBind v [] | v <- nub (tyfree sig \\ tvarScope env), skolem v ]
                                               | otherwise     = qual d
                                         return $ TSchema NoLoc q sig (decoration d)
  where
    decoration d@Def{}              = deco d
    decoration Actor{}              = NoDec
    decoration _                    = NoDec


-- FX presentation ---------------------

openFX (TSchema l q (TFun l' fx p r t) dec)
  | Just fx1 <- open fx             = TSchema l (TBind v [] : q) (TFun l' fx1 p r t) dec
  where open (TRow l k n t fx)      = TRow l k n t <$> open fx
        open (TNil l _)             = Just (TVar l v)
        open (TVar _ _)             = Nothing
        v                           = head (tvarSupply \\ tybound q)
openFX t                            = t

closeFX (TSchema l q f@(TFun l' fx p r t) dec)
  | TVar _ v <- rowTail fx, sole v  = TSchema l (filter ((v`notElem`) . tybound) q) (TFun l' (subst [(v,fxNil)] fx) p r t) dec
  where sole v                      = v `elem` tybound q && length (filter (==v) (tyfree q ++ tyfree f)) == 1
closeFX t                           = t
