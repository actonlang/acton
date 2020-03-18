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


newWitness                              = newName "w"

-- Infer -------------------------------

infTop                                  :: Env -> Suite -> TypeM (TEnv,Suite)
infTop env ss                           = do pushFX fxNil
                                             (cs,te,ss1) <- infEnv env ss
                                             popFX
                                             solve cs
                                             te1 <- msubst te
                                             return (te1, ss1)

class Infer a where
    infer                               :: Env -> a -> TypeM (Constraints,Type,a)

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM (Constraints,TEnv,a)

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (Constraints,TEnv,Type,a)


splitGen                                :: Env -> [TVar] -> TEnv -> Constraints -> TypeM (Constraints, TEnv)
splitGen env tvs te cs
  | null ambig_cs                       = return (fixed_cs, map gen te)
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
    gen (n, NVar sc)                    = (n, NVar (generalize sc))
    gen (n, i)                          = (n, i)

mkBinds cs                              = collect [] $ catMaybes $ map bound cs
  where
    bound (Sub env (TVar _ v) (TCon _ u))   = Just $ TBind v [u]                            -- TODO: capture env!!!!!!!!
    bound (Impl w env (TVar _ v) u)         = Just $ TBind v [u]                            -- TODO: capture env!!!!!!!! preserve w!!!!!!!!!
    bound c                             = trace ("### Unreduced constraint: " ++ prstr c) $ Nothing
    collect vs []                       = []
    collect vs (TBind v us : q)
      | v `elem` vs                     = collect vs q
      | otherwise                       = TBind v (us ++ concat [ us' | TBind v' us' <- q, v' == v ]) : collect (v:vs) q


genTEnv                                 :: Env -> Constraints -> TEnv -> TypeM (Constraints,TEnv)
genTEnv env cs te                       = do cs1 <- simplify cs
                                             te1 <- msubst te
                                             tvs <- msubstTV (tyfree env)
                                             (cs2, te2) <- splitGen env tvs te1 cs1
                                             dump [ INS (loc v) t | (v, TSchema _ [] t _) <- nVars te1 ]
                                             dump [ GEN (loc v) t | (v, t) <- nVars te2 ]
                                             return (cs2, te2)

gen1                                    :: Env -> Name -> Constraints -> Type -> Decoration -> TypeM (Constraints,TSchema)
gen1 env n cs t d                       = do (cs',te) <- genTEnv env cs ([(n, NVar (monotype' t d))])
                                             return (cs', snd $ head $ nVars te)

commonTEnv                              :: Env -> [TEnv] -> TypeM (Constraints,TEnv)
commonTEnv env []                       = return ([], [])
commonTEnv env tenvs                    = do cs <- unifyTEnv env tenvs vs
                                             return (cs, prune vs $ head tenvs)
  where vs                              = foldr intersect [] $ map dom tenvs

unionTEnv                               :: Env -> [TEnv] -> TypeM TEnv
unionTEnv env tenvs                     = undefined                                 -- For data statements only. TODO: implement


infLiveEnv env x
  | fallsthru x                         = do (cs,te,x') <- infEnv env x
                                             return (cs, Just te, x')
  | otherwise                           = do (cs,te,x') <- infEnv env x
                                             return (cs, Nothing, x')

liveCombine te Nothing                  = te
liveCombine Nothing te'                 = te'
liveCombine (Just te) (Just te')        = Just $ te++te'


instance (InfEnv a) => InfEnv [a] where
    infEnv env []                       = return ([], [], [])
    infEnv env (s : ss)                 = do (cs1,te1,s') <- infEnv env s
                                             (cs2,te2,ss') <- infEnv (define te1 env) ss
                                             return (cs1++cs2, te1++te2, s':ss')

instance InfEnv Stmt where
    infEnv env (Expr l e)               = do (cs,_,e') <- infer env e
                                             return (cs, [], Expr l e')
    infEnv env (Assign l pats e)
      | nodup pats                      = do (cs1,t1,e') <- infer env e
                                             (cs2,te,t2,pats') <- infEnvT env pats
                                             return (Sub env t1 t2 : 
                                                     cs1++cs2, te, Assign l pats' e')
    infEnv env (Update l ts e)          = do (cs1,t1,e') <- infer env e
                                             (cs2,t2,ts') <- infer env ts
                                             return (Sub env t1 t2 : 
                                                     cs1++cs2, [], Update l ts' e')
    infEnv env (IUpdate l t (Op _ o) e) = do (cs1,t1,t') <- infer env t
                                             (cs2,t2,e') <- infer env e
                                             w <- newWitness
                                             return (Sub env t2 t1 : 
                                                     Impl w env t1 (protocol o) : 
                                                     cs1++cs2, [], Update l [t'] (eCall (eDot (eVar w) (method o)) [t2e t',e']))
      where protocol PlusA              = pPlus
            protocol MinusA             = pMinus
            protocol MultA              = pNumber
            protocol PowA               = pNumber
            protocol DivA               = pNumber
            protocol ModA               = pReal
            protocol EuDivA             = pReal
            protocol ShiftLA            = pIntegral
            protocol ShiftRA            = pIntegral
            protocol BOrA               = pLogical
            protocol BXorA              = pLogical
            protocol BAndA              = pLogical
            protocol MMultA             = pMatrix
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
    infEnv env (Assert l e1 e2)         = do (cs1,e1') <- inferBool env e1
                                             (cs2,t,e2') <- infer env e2
                                             return (Sub env t tStr : 
                                                     cs1++cs2, [], Assert l e1' e2')
    infEnv env s@(Pass l)               = return ([], [], s)
    infEnv env (Delete l pat)           = undefined
--      | nodup pat                       = do (cs,_,pat') <- infer env pat                 -- TODO: constrain pat targets to opt type
--                                            return (cs, [], Delete l pat')
    infEnv env s@(Return l Nothing)     = do cfx <- subFX env (fxRet tNone tWild)
                                             return ([cfx], [], s)
    infEnv env (Return l (Just e))      = do (cs1,t,e') <- infer env e
                                             cfx <- subFX env (fxRet t tWild)
                                             return (cfx : cs1, [], Return l (Just e'))
    infEnv env s@(Raise _ Nothing)      = return ([], [], s)
    infEnv env (Raise l (Just e))       = do (cs,_,e') <- infer env e
                                             return (cs, [], Raise l (Just e'))
    infEnv env s@(Break _)              = return ([], [], s)
    infEnv env s@(Continue _)           = return ([], [], s)
    infEnv env (If l bs els)            = do (css,tes,bs') <- fmap unzip3 $ mapM (infLiveEnv env) bs
                                             (cs0,te,els') <- infLiveEnv env els
                                             (cs1,te1) <- commonTEnv env $ catMaybes (te:tes)
                                             return (cs0++cs1++concat css, te1, If l bs' els')
    infEnv env (While l e b els)        = do (cs1,e') <- inferBool env e
                                             (cs2,_,b') <- infEnv env b
                                             (cs3,_,els') <- infEnv env els
                                             return (cs1++cs2++cs3, [], While l e' b' els')
    infEnv env (For l p e b els)
      | nodup p                         = do (cs1,te,t1,p') <- infEnvT env p
                                             (cs2,t2,e') <- infer env e
                                             (cs3,_,b') <- infEnv (define te env) b
                                             (cs4,_,els') <- infEnv env els
                                             w <- newWitness
                                             return (Impl w env t2 (pIterable t1) : 
                                                     cs1++cs2++cs3++cs4, [], For l p' e' b' els')               -- TODO: translate into while loop
    infEnv env (Try l b hs els fin)     = do (cs1,te,b') <- infLiveEnv env b
                                             (cs2,te',els') <- infLiveEnv (maybe id define te $ env) els
                                             (css,tes,hs') <- fmap unzip3 $ mapM (infLiveEnv env) hs
                                             (cs3,te1) <- commonTEnv env $ catMaybes $ (liveCombine te te'):tes
                                             (cs4,te2,fin') <- infEnv (define te1 env) fin
                                             return (cs1++cs2++cs3++cs4++concat css, te1++te2, Try l b' hs' els' fin')
    infEnv env (With l items b)
      | nodup items                     = do (cs1,te,items') <- infEnv env items
                                             (cs2,te1,b') <- infEnv (define te env) b
                                             return $ (cs1++cs2, prune (dom te) te1, With l items' b')

    infEnv env (VarAssign l pats e)
      | nodup pats                      = do (cs1,t1,e') <- infer env e
                                             (cs2,te,t2,pats') <- infEnvT env pats
                                             return (Sub env t1 t2 : 
                                                     cs1++cs2, [ (n,NSVar t) | (n,NVar t) <- te], VarAssign l pats' e')
    
    infEnv env (After l e n ps ks)      = do (cs1,t1,e') <- infer env e
                                             (cs2,t2) <- instantiate env $ openFX $ findVarType (NoQual n) env
                                             dump [INS (loc n) t2]
                                             (cs3,prow,ps') <- infer env ps
                                             (cs4,krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX                                       -- ..............
                                             return (Equ env t1 tInt : 
                                                     Sub env t2 (tFun fx prow krow t0) : 
                                                     cs1++cs2++cs3++cs4, [], After l e' n ps' ks')
    
    infEnv env (Decl l ds)
      | nodup ds && noCheck env         = do (cs1,te1,ds1) <- infEnv env ds
                                             return (cs1, te1, Decl l ds1)
      | otherwise                       = do (cs1,te1,ds1) <- infEnv (setNoCheck env) ds
                                             (cs2,ds2) <- check (define te1 env) ds1
                                             (cs3,te2) <- genTEnv env (cs1++cs2) te1
                                             return (cs3, te2, Decl l ds2)

    infEnv env d@(Signature _ ns sc)
      | not $ null redefs               = illegalRedef (head redefs)
      | otherwise                       = do return ([], [(n, NSig sc) | n <- ns], d)
      where redefs                      = [ n | n <- ns, not $ reserved n env ]

    infEnv env (Data l _ _)             = notYet l "data syntax"


instance InfEnv Decl where
    infEnv env d@(Actor _ n _ p k _ _)
      | not $ reservedOrSig n env       = illegalRedef n
      | nodup (p,k)                     = do sc <- extractSchema env d
                                             return ([], [(n, NVar sc)], d)
    infEnv env d@(Def _ n _ p k _ _ _)
      | not $ reservedOrSig n env       = illegalRedef n
      | nodup (p,k)                     = do sc <- extractSchema env d
                                             return ([], [(n, NVar sc)], d)
    infEnv env (Class l n q us b)
      | not $ reserved n env            = illegalRedef n
      | otherwise                       = do (cs,te,b') <- infEnv env1 b
                                             return (cs, [(n, NClass q (mro env1 us1) te)], Class l n q us1 b')
      where env1                        = reserve (bound b) $ defineSelf (NoQual n) q $ defineTVars q $ block (stateScope env) env
            us1                         = classBases env us
    infEnv env (Protocol l n q us b)
      | not $ reserved n env            = illegalRedef n
      | otherwise                       = do (cs,te,b') <- infEnv env1 b
                                             return (cs, [(n, NProto q (mro env1 us1) te)], Protocol l n q us1 b')
      where env1                        = reserve (bound b) $ defineSelf (NoQual n) q $ defineTVars q $ block (stateScope env) env  
            us1                         = protoBases env us
    infEnv env d@(Extension _ n q us b)
      | isProto n env                   = notYet (loc n) "Extension of a protocol"
      | otherwise                       = do w <- newWitness
                                             return ([], [(w, NExt n q (mro env1 (protoBases env us)))], d)
      where env1                        = reserve (bound b) $ defineSelf n q $ defineTVars q $ block (stateScope env) env
            u                           = TC n [ tVar tv | TBind tv _ <- q ]

classBases env []                       = []
classBases env (u:us)
  | isProto (tcname u) env              = u : protoBases env us
  | otherwise                           = u : protoBases env us

protoBases env []                       = []
protoBases env (u:us)
  | isProto (tcname u) env              = u : protoBases env us
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
        linearizations (u : us)         = (u:us') : linearizations us
          where (_,us',_)               = findCon env u


class Check a where
    check                               :: Env -> a -> TypeM (Constraints,a)

instance (Check a) => Check [a] where
    check env []                        = return ([], [])
    check env (d:ds)                    = do (cs1,d') <- check env d
                                             (cs2,ds') <- check env ds
                                             return (cs1++cs2, d':ds')

instance Check Stmt where
    check env (If l bs els)             = do (cs1,bs') <- check env bs
                                             (cs2,els') <- check env els
                                             return (cs1++cs2, If l bs' els')
    check env (Decl l ds)               = do (cs,ds') <- check env ds
                                             return (cs, Decl l ds')
    check env s                         = return ([], s)

instance Check Branch where
    check env (Branch e b)              = do (cs,b') <- check env b
                                             return (cs, Branch e b')

instance Check Decl where
    check env (Actor l n q p k ann b)
      | noshadow svars (p,k)            = do pushFX (fxAct tWild)
                                             (cs0,te0,prow,p') <- infEnvT env p
                                             (cs1,te1,krow,k') <- infEnvT (define te0 env1) k
                                             (cs2,te2,b') <- infEnv (define te1 (define te0 env1)) b
                                             popFX
                                             fx <- fxAct <$> newTVarOfKind XRow
                                             cs3 <- checkAssump env n (cs0++cs1++cs2) (tFun fx prow krow (tRecord $ env2row kwdNil $ nVars te2))
                                             return (cs3, Actor l n q p' k' ann b')
      where svars                       = statedefs b
            env0                        = define [(selfKW, NVar (monotype tRef))] $ defineTVars q $ block (stateScope env) env
            env1                        = reserve (bound (p,k) ++ bound b ++ svars) env0

    check env (Def l n q p k ann b mo)
                                        = do t <- newTVar
                                             fx <- newTVarOfKind XRow
                                             pushFX (fxRet t fx)
                                             csfx <- if fallsthru b then (:[]) <$> subFX env (fxRet tNone tWild) else pure []
                                             (cs0,te0,prow,p') <- infEnvT env p
                                             (cs1,te1,krow,k') <- infEnvT (define te0 env1) k
                                             (cs2,_,b') <- infEnv (define te1 (define te0 env1)) b
                                             popFX
                                             let (cs3,prow',krow') = split prow krow
                                             cs4 <- checkAssump env n (csfx++cs0++cs1++cs2++cs3) (tFun fx prow' krow' t)
                                             return (cs4, Def l n q p' k' ann b' mo)
      where env1                        = reserve (bound (p,k) ++ bound b) $ defineTVars q $ block (stateScope env) env
            split p k 
              | not $ isClassAttr mo    = ([], p, k)
            split (TRow _ _ n sc p) k   = ([Equ env (monotypeOf sc) tSelf], p, k)
            split p (TRow _ _ n sc k)   = ([Equ env (monotypeOf sc) tSelf], p, k)

    check env (Class l n q us b)        = do pushFX fxNil
                                             (cs1,b') <- check (define te env1) b
                                             popFX
                                             cs2 <- checkBindings env False us te
                                             return (cs1++cs2, Class l n q us b')
      where env1                        = defineSelf (NoQual n) q $ defineTVars q $ block (stateScope env) env
            NClass q us te              = findName n env

    check env (Protocol l n q us b)     = do pushFX fxNil
                                             (cs1,b') <- check (define te env1) b
                                             popFX
                                             cs2 <- checkBindings env True us te
                                             return (cs1++cs2, Protocol l n q us b')         -- TODO: add Self to q
      where env1                        = defineSelf (NoQual n) q $ defineTVars q $ block (stateScope env) env
            NProto q us te              = findName n env

    check env (Extension l n q us b)    = do pushFX fxNil
                                             (cs1,te,b') <- infEnv env1 b
                                             popFX
                                             cs2 <- checkBindings env False us te
                                             return (cs1++cs2, Class l w [] us b')        -- TODO: properly mix in n and q in us......
      where env1                        = reserve (bound b) $ defineSelf n q $ defineTVars q $ block (stateScope env) env
            w                           = locateWitness env n q us


checkBindings env proto us te
  | proto && (not $ null unsigs)        = lackSig unsigs
  | not proto && (not $ null undefs)    = lackDef undefs
  | otherwise                           = return refinements
  where tes                             = [ te' | u <- us, let (_,_,te') = findCon env u ]
        inherited                       = concatMap nSigs tes ++ concatMap nVars tes
        refinements                     = [ SubGen env sc sc' | (n,sc) <- nSigs te, Just sc' <- [lookup n inherited] ]
        undefs                          = (dom $ csigs) \\ (dom $ nVars te ++ concatMap nVars tes)
        unsigs                          = dom te \\ (dom (nSigs te) ++ dom inherited)
        allsigs                         = nSigs te ++ concatMap nSigs tes
        (isigs,csigs)                   = partition isInstAttr allsigs
          where isInstAttr (n,sc)       = case sc of TSchema _ _ _ (InstAttr _) -> True; _ -> False


checkAssump env n cs t                  = case findVarType (NoQual n) env of
                                            (TSchema _ [] t' _) -> 
                                               return (Equ env t t' : cs)
                                            sc -> do
                                               (cs',sc') <- gen1 env n cs t (scdec sc)   -- TODO: verify that generalizing one decl at a time is ok
                                               return (EquGen env sc' sc : cs')

inferPure env e                         = do pushFX fxNil
                                             (cs,t,e') <- infer env e
                                             popFX
                                             return (cs,t,e')

env2row                                 = foldl (\r (n,t) -> kwdRow n t r)           -- TODO: stabilize this...

instance InfEnv Branch where
    infEnv env (Branch e b)             = do (cs1,e') <- inferBool env e
                                             (cs2,te,b') <- infEnv env b
                                             return (cs1++cs2, te, Branch e' b')

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl w env t pContextManager :
                                                     cs, [], WithItem e' Nothing)           -- TODO: translate using w
    infEnv env (WithItem e (Just p))    = do (cs1,t1,e') <- infer env e
                                             (cs2,te,t2,p') <- infEnvT env p
                                             w <- newWitness
                                             return (Equ env t1 t2 :
                                                     Impl w env t1 pContextManager :
                                                     cs1++cs2, te, WithItem e' (Just p'))         -- TODO: translate using w

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do (cs1,te,ex') <- infEnv env ex
                                             (cs2,te1,b') <- infEnv (define te env) b
                                             return (cs1++cs2, prune (dom te) te1, Handler ex' b')

instance InfEnv Except where
    infEnv env ex@(ExceptAll l)         = return ([], [], ex)
    infEnv env ex@(Except l x)          = do tx <- infException env x
                                             return ([], [], ex)
    infEnv env ex@(ExceptAs l x n)      = do tx <- infException env x
                                             return ([], [(n, NVar $ monotype tx)], ex)

infException env x
  | Just (_,t) <- sub                   = return $ tCon tc
  | otherwise                           = err1 x "Not an Exception sub-class:"
  where tc                              = TC x []
        sub                             = findSubAxiom env tc qnException

instance Infer Expr where
    infer env (Var l n)                 = do (cs,t) <- instantiate env $ openFX $ findVarType n env
                                             return (cs, t, Var l n)
    infer env e@(Int _ val s)           = return ([], tInt, e)
    infer env e@(Float _ val s)         = return ([], tFloat, e)
    infer env e@Imaginary{}             = notYetExpr e
    infer env e@(Bool _ val)            = return ([], tBool, e)
    infer env e@(None _)                = return ([], tNone, e)
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env e@(Strings _ ss)          = return ([], tUnion [ULit $ concat ss], e)
    infer env e@(BStrings _ ss)         = return ([], tBytes, e)
    infer env (Call l e ps ks)          = do (cs1,t,e') <- infer env e
                                             dump [INS (loc e) t]
                                             (cs2,prow,ps') <- infer env ps
                                             (cs3,krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX
                                             return (Sub env t (tFun fx prow krow t0) :
                                                     cs1++cs2++cs3, t0, Call l e' ps' ks')
    infer env (Await l e)               = do (cs1,t,e') <- infer env e
                                             t0 <- newTVar
                                             fx <- fxAwait <$> newTVarOfKind XRow
                                             cfx <- equFX env fx
                                             return (Sub env t (tMsg t0) :
                                                     cfx :
                                                     cs1, t0, Await l e')
    infer env (Index l e ixs)           = do (cs1,t,e') <- infer env e
                                             (cs2,ti,ix') <- infer env ix
                                             t0 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t (pIndexed ti t0) :
                                                     cs1++cs2, t0, eCall (eDot (eVar w) getitemKW) [e',ix'])
      where ix | length ixs == 1        = head ixs
               | otherwise              = Tuple NoLoc (foldr PosArg PosNil ixs) KwdNil
    infer env (Slice l e slz)           = do (cs1,t,e') <- infer env e
                                             (cs2,sl') <- inferSlice env sl
                                             w <- newWitness
                                             return (Impl w env t pSliceable :
                                                     cs1++cs2, t, eCall (eDot (eVar w) getsliceKW) (e' : toArgs sl'))
      where sl | length slz == 1        = head slz
               | otherwise              = notYet l "Multidimensional slicing"
            toArgs (Sliz _ e1 e2 e3)    = map (maybe eNone id) [e1,e2,e3]
    infer env (Cond l e1 e e2)          = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             (cs3,e') <- inferBool env e
                                             t0 <- newTVar
                                             return (Sub env t1 t0 :
                                                     Sub env t2 t0 :
                                                     cs1++cs2++cs3, t0, Cond l e1' e' e2')
    infer env (BinOp l e1 o@(Op _ op) e2)
      | op `elem` [Or,And]              = do (cs1,e1') <- inferBool env e1
                                             (cs2,e2') <- inferBool env e2
                                             return (cs1++cs2, tBool, BinOp l e1' o e2')
      | otherwise                       = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             t <- newTVar
                                             w <- newWitness
                                             return (Sub env t1 t :
                                                     Sub env t2 t :
                                                     Impl w env t (protocol op) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Plus               = pPlus
            protocol Minus              = pMinus
            protocol Mult               = pNumber
            protocol Pow                = pNumber
            protocol Div                = pNumber
            protocol Mod                = pReal
            protocol EuDiv              = pReal
            protocol ShiftL             = pIntegral
            protocol ShiftR             = pIntegral
            protocol BOr                = pLogical
            protocol BXor               = pLogical
            protocol BAnd               = pLogical
            protocol MMult              = pMatrix
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
      | op == Not                       = do (cs,e') <- inferBool env e
                                             return (cs, tBool, UnOp l o e')
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl w env t (protocol op) :
                                                     cs, t, eCall (eDot (eVar w) (method op)) [e'])
      where protocol UPlus              = pNumber
            protocol UMinus             = pNumber
            protocol BNot               = pIntegral
            method UPlus                = posKW
            method UMinus               = negKW
            method BNot                 = invertKW
    infer env (CompOp l e1 [OpArg (Op _ op) e2])
      | op `elem` [In,NotIn]            = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             w1 <- newWitness
                                             w2 <- newWitness
                                             return (Impl w1 env t2 (pContainer t1) :
                                                     Impl w2 env t1 pEq :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w1) (method op)) [eVar w2, e1', e2'])
      | otherwise                       = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             t <- newTVar
                                             w <- newWitness
                                             return (Sub env t1 t :
                                                     Sub env t2 t :
                                                     Impl w env t (protocol op) :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Eq                 = pEq
            protocol NEq                = pEq
            protocol LtGt               = pEq
            protocol Lt                 = pOrd
            protocol Gt                 = pOrd
            protocol LE                 = pOrd
            protocol GE                 = pOrd
            protocol Is                 = pIdentity
            protocol IsNot              = pIdentity
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
--    infer env (Dot l (Var _ c) n)
--      | NClass q us te <- findQName c env = undefined  
--      | NProto q us te <- findQName c env = undefined
    infer env (Dot l e n)               = do (cs,t,e') <- infer env e
                                             t0 <- newTVar
                                             return (Sel env t n t0 :
                                                     cs, t0, Dot l e' n)
    infer env (DotI l e i False)        = do (cs,t,e') <- infer env e
                                             t0 <- newTVar
                                             return (Sel env t (rPos i) t0 :
                                                     cs, t0, DotI l e' i False)
    infer env e@(DotI l _ _ True)       = notYetExpr e
    infer env (Lambda l p k e)
      | nodup (p,k)                     = do fx <- newTVarOfKind XRow
                                             pushFX fx
                                             (cs0,te0, prow, p') <- infEnvT env1 p
                                             (cs1,te1, krow, k') <- infEnvT (define te0 env1) k
                                             (cs2,t,e') <- infer (define te1 (define te0 env1)) e
                                             popFX
                                             dump [INS l $ tFun fx prow krow t]
                                             return (cs0++cs1++cs2, tFun fx prow krow t, Lambda l p' k' e')
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs kargs)     = do (cs1,prow,pargs') <- infer env pargs
                                             (cs2,krow,kargs') <- infer env kargs
                                             return (cs1++cs2, TTuple NoLoc prow krow, Tuple l pargs' kargs')
    infer env (List l es)               = do t0 <- newTVar
                                             (cs,es') <- infElems env es pSequence t0
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t1 (pSequence t0) :
                                                     cs, t1, List l es')
    infer env (ListComp l e1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e1] pSequence t0
                                             let [e1'] = es
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t1 (pSequence t0) :
                                                     cs1++cs2, t1, ListComp l e1' co')
    infer env (Set l es)                = do t0 <- newTVar
                                             (cs,es')  <- infElems env es pSet t0
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t1 (pSet t0) :
                                                     cs, t1, Set l es')
    infer env (SetComp l e1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e1] pSet t0
                                             let [e1'] = es
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t1 (pSet t0) :
                                                     cs1++cs2, t1, SetComp l e1' co')
                                             
    infer env (Dict l as)               = do tk <- newTVar
                                             tv <- newTVar
                                             (cs,as') <- infAssocs env as tk tv
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t1 (pMapping tk tv) :
                                                     cs, t1, Dict l as')
    infer env (DictComp l a1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             tk <- newTVar
                                             tv <- newTVar
                                             (cs2,as) <- infAssocs (define te env) [a1] tk tv
                                             let [a1'] = as
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w env t1 (pMapping tk tv) :
                                                     cs1++cs2, t1, DictComp l a1' co')
    infer env (Paren l e)               = do (cs,t,e') <- infer env e
                                             return (cs, t, Paren l e')


isModule env e                          = fmap ModName $ mfilter (isMod env) $ fmap reverse $ dotChain e
  where dotChain (Var _ (NoQual n))     = Just [n]
        dotChain (Dot _ e n)            = fmap (n:) (dotChain e)
        dotChain _                      = Nothing


infElems env [] pc t0                   = return ([], [])
infElems env (Elem e : es) pc t0        = do (cs1,t,e') <- infer env e
                                             (cs2,es') <- infElems env es pc t0
                                             return (Sub env t t0 :
                                                     cs1++cs2, Elem e' : es')
infElems env (Star e : es) pc t0        = do (cs1,t,e') <- infer env e
                                             (cs2,es') <- infElems env es pc t0
                                             w <- newWitness
                                             return (Impl w env t (pc t0) :
                                                     cs1++cs2, Star e' : es')

infAssocs env [] tk tv                  = return ([], [])
infAssocs env (Assoc k v : as) tk tv    = do (cs1,tk',k') <- infer env k
                                             (cs2,tv',v') <- infer env v
                                             (cs3,as') <- infAssocs env as tv tk
                                             return (Sub env tk' tk :
                                                     Sub env tv' tv :
                                                     cs1++cs2++cs3, Assoc k' v' : as')
infAssocs env (StarStar e : as) tk tv   = do (cs1,t,e') <- infer env e
                                             (cs2,as') <- infAssocs env as tk tv
                                             w <- newWitness
                                             return (Impl w env t (pMapping tk tv) :
                                                     cs1++cs2, StarStar e' : as')

inferBool env e                         = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl w env t pBoolean :
                                                     cs, eCall (eDot (eVar w) boolKW) [e'])

inferSlice env (Sliz l e1 e2 e3)        = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             (cs3,t3,e3') <- infer env e3
                                             return (Sub env t1 tInt :
                                                     Sub env t2 tInt :
                                                     Sub env t3 tInt :
                                                     cs1++cs2++cs3, Sliz l e1' e2' e3')

inferGen env e                          = do (cs,t,e') <- infer env e
                                             (cs1,sc) <- gen1 env (name "_") cs t NoDec
                                             return (cs1, sc, e')

instance (Infer a) => Infer (Maybe a) where
    infer env Nothing                   = do t <- newTVar
                                             return ([], t, Nothing)
    infer env (Just x)                  = do (cs,t,e') <- infer env x
                                             return (cs, t, Just e')

instance InfEnvT PosPar where
    infEnvT env (PosPar n ann e p)      = do t <- maybe (monotype <$> newTVar) return ann
                                             (cs1,t',e') <- inferGen env e
                                             (cs2,te,r,p') <- infEnvT (define [(n, NVar t)] env) p
                                             return (SubGen env t' t :
                                                     cs1++cs2, (n, NVar t):te, posRow t r, PosPar n (Just t) e' p')
    infEnvT env (PosSTAR n ann)         = do t <- maybe newTVar return ann
                                             r <- newTVarOfKind KRow
                                             return (Equ env t (tTuple r) :
                                                     [], [(n, NVar $ monotype t)], r, PosSTAR n (Just t))
    infEnvT env PosNIL                  = return ([], [], posNil, PosNIL)

instance InfEnvT KwdPar where
    infEnvT env (KwdPar n ann e k)      = do t <- maybe (monotype <$> newTVar) return ann
                                             (cs1,t',e') <- inferGen env e
                                             (cs2,te,r,k') <- infEnvT (define [(n, NVar t)] env) k
                                             return (SubGen env t' t :
                                                     cs1++cs2, (n, NVar t):te, kwdRow n t r, KwdPar n (Just t) e' k')
    infEnvT env (KwdSTAR n ann)         = do t <- maybe newTVar return ann
                                             r <- newTVarOfKind KRow
                                             return (Equ env t (tRecord r) :
                                                     [], [(n, NVar $ monotype t)], r, KwdSTAR n (Just t))
    infEnvT env KwdNIL                  = return ([], [], kwdNil, KwdNIL)

instance Infer PosArg where
    infer env (PosArg e p)              = do (cs1,sc,e') <- inferGen env e
                                             (cs2,prow,p') <- infer env p
                                             return (cs1++cs2, posRow sc prow, PosArg e' p')
    infer env (PosStar e)               = do (cs,t,e') <- infer env e
                                             prow <- newTVarOfKind PRow
                                             return (Equ env t (tTuple prow) :
                                                     cs, prow, PosStar e')
    infer env PosNil                    = return ([], posNil, PosNil)
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do (cs1,sc,e') <- inferGen env e
                                             (cs2,krow,k') <- infer env k
                                             return (cs1++cs2, kwdRow n sc krow, KwdArg n e' k')
    infer env (KwdStar e)               = do (cs,t,e') <- infer env e
                                             krow <- newTVarOfKind KRow
                                             return (Equ env t (tRecord krow) :
                                                     cs, krow, KwdStar e')
    infer env KwdNil                    = return ([], kwdNil, KwdNil)
    
instance InfEnv Comp where
    infEnv env NoComp                   = return ([], [], NoComp)
    infEnv env (CompIf l e c)           = do (cs1,e') <- inferBool env e
                                             (cs2,te,c') <- infEnv env c
                                             return (cs1++cs2, te, CompIf l e' c')
    infEnv env (CompFor l p e c)        = do (cs1,te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             (cs2,t2,e') <- infer env e
                                             (cs3,te2,c') <- infEnv (define te1 env) c
                                             w <- newWitness
                                             return (Impl w env t2 (pIterable t1) :
                                                     cs1++cs2++cs3, te1++te2, CompFor l p' e' c')          -- TODO: translate using w...

instance Infer Exception where
    infer env (Exception e1 Nothing)    = do (cs1,t1,e1') <- infer env e1
                                             return (Sub env t1 tException :
                                                     cs1, t1, Exception e1' Nothing)
    infer env (Exception e1 (Just e2))  = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             return (Sub env t1 tException :
                                                     Sub env t2 (tOpt tException) :
                                                     cs1++cs2, t1, Exception e1' (Just e2'))

instance InfEnvT PosPat where
    infEnvT env (PosPat p ps)           = do (cs1,te1,t,p') <- infEnvT env p
                                             (cs2,te2,r,ps') <- infEnvT env ps
                                             return (cs1++cs2, te1++te2, posRow (monotype t) r, PosPat p' ps')
    infEnvT env (PosPatStar p)          = do (cs,te,t,p') <- infEnvT env p
                                             r <- newTVarOfKind PRow
                                             return (Equ env t (tTuple r) :
                                                     cs, te, r, PosPatStar p')
    infEnvT env PosPatNil               = return ([], [], posNil, PosPatNil)


instance InfEnvT KwdPat where
    infEnvT env (KwdPat n p ps)         = do (cs1,te1,t,p') <- infEnvT env p
                                             (cs2,te2,r,ps') <- infEnvT env ps
                                             return (cs1++cs2, te1++te2, kwdRow n (monotype t) r, KwdPat n p' ps')
    infEnvT env (KwdPatStar p)          = do (cs,te,t,p') <- infEnvT env p
                                             r <- newTVarOfKind KRow
                                             return (Equ env t (tRecord r) :
                                                     cs, te, r, KwdPatStar p')
    infEnvT env KwdPatNil               = return ([], [], kwdNil, KwdPatNil)


instance InfEnvT Pattern where
    infEnvT env (PVar l n ann)
      | reservedOrSig n env             = do t0 <- maybe newTVar return ann
                                             return ([], [(n, NVar $ monotype t0)], t0, PVar l n (Just t0))
    infEnvT env (PVar l n Nothing)      = case findVarType (NoQual n) env of
                                             TSchema _ [] t _ -> return ([], [], t, PVar l n Nothing)
                                             _ -> err1 n "Polymorphic variable not assignable:"
    infEnvT env p@PVar{}                = typedReassign p
    infEnvT env (PTuple l ps ks)        = do (cs1,te1,prow,ps') <- infEnvT env ps
                                             (cs2,te2,krow,ks') <- infEnvT env ks
                                             return (cs1++cs2, te1++te2, TTuple NoLoc prow krow, PTuple l ps' ks')
    infEnvT env (PList l ps p)          = do (cs1,te1,t1,ps') <- infEnvT env ps
                                             (cs2,te2,t2,p') <- infEnvT (define te1 env) p
                                             w <- newWitness
                                             return (Impl w env t2 (pSequence t1) :
                                                     cs1++cs2, te1++te2, t2, PList l ps' p')
    infEnvT env (PParen l p)            = do (cs,te,t,p') <- infEnvT env p
                                             return (cs, te, t, PParen l p')
    infEnvT env (PData l n es)          = notYet l "data syntax"


instance InfEnvT (Maybe Pattern) where
    infEnvT env Nothing                 = do t <- newTVar
                                             return ([], [], t, Nothing)
    infEnvT env (Just p)                = do (cs,te,t,p') <- infEnvT env p
                                             return (cs, te, t, Just p')

instance InfEnvT [Pattern] where
    infEnvT env [p]                     = do (cs,te,t,p') <- infEnvT env p
                                             return (cs, te, t, [p'])
    infEnvT env (p:ps)                  = do (cs1,te1,t1,p') <- infEnvT env p
                                             (cs2,te2,t2,ps') <- infEnvT env ps
                                             return (Equ env t1 t2 :
                                                     cs1++cs2, te1++te2, t1, p':ps')

instance Infer [Target] where
    infer env [t]                       = do (cs1,t1,t') <- infer env t
                                             return (cs1, t1, [t'])
    infer env (t:ts)                    = do (cs1,t1,t') <- infer env t
                                             (cs2,t2,ts') <- infer env ts
                                             return (Equ env t1 t2 :
                                                     cs1++cs2, t1, t':ts')

instance Infer Target where
    infer env (TaVar l n)               = case findVarType (NoQual n) env of
                                             TSchema _ [] t _ -> return ([], t, TaVar l n)
                                             _ -> err1 n "Polymorphic variable not assignable:"

    infer env (TaIndex l e [i])         = do (cs1,t,e') <- infer env e
                                             (cs2,ti,i') <- infer env i
                                             t0 <- newTVar
                                             w <- newWitness
                                             cfx <- equFX env (fxMut tWild tWild)
                                             return (Impl w env t (pIndexed ti t0) :
                                                     Sub env t tObject :
                                                     cfx :
                                                     cs1++cs2, t0, TaIndex l e' [i'])         -- TODO: translate using w...
    infer env (TaSlice l e [s])         = do (cs1,t,e') <- infer env e
                                             (cs2,s') <- inferSlice env s
                                             w <- newWitness
                                             cfx <- equFX env (fxMut tWild tWild)
                                             return (Impl w env t pSliceable :
                                                     Sub env t tObject :
                                                     cfx :
                                                     cs1++cs2, t, TaSlice l e' [s'])          -- TODO: translate using w
    infer env (TaDot l e n)             = do (cs,t,e') <- infer env e
                                             t0 <- newTVar
                                             cfx <- equFX env (fxMut tWild tWild)
                                             return (Mut env t n t0 :
                                                     cfx :
                                                     cs, t0, TaDot l e' n)
    infer env (TaDotI l e i tl)         = do (cs,t,e') <- infer env e
                                             t0 <- newTVar
                                             cfx <- equFX env (fxMut tWild tWild)
                                             return (--Mut env t n t0 :                     -- TODO: create MutI constraint
                                                     cfx :
                                                     cs, t0, TaDotI l e' i tl)
    infer env (TaTuple l targs)         = do (css,ts,targs') <- unzip3 <$> mapM (infer env) targs
                                             return (concat css, tTuple (foldr posRow' posNil ts), TaTuple l targs')



-- Extracting schemas from (actor and def) declarations

class ExtractT a where
    extractT                        :: a -> TypeM Type

instance ExtractT PosPar where
    extractT (PosPar n t _ p)       = posRow <$> maybe (monotype <$> newTVar) return t <*> extractT p
    extractT (PosSTAR n (Just (TTuple _ prow _)))
                                    = return prow
    extractT (PosSTAR n Nothing)    = newTVarOfKind PRow
    extractT PosNIL                 = return posNil

instance ExtractT KwdPar where
    extractT (KwdPar n t _ k)       = kwdRow n <$> maybe (monotype <$> newTVar) return t <*> extractT k
    extractT (KwdSTAR n (Just (TTuple _ _ krow)))
                                    = return krow
    extractT (KwdSTAR n Nothing)    = newTVarOfKind KRow
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
