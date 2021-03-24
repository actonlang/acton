{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct, showTyFile, typeError) where

import Control.Monad
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Prim
import Acton.Env
import Acton.Solver
import Acton.Subst
import Acton.Transform
import Acton.Converter
import Acton.TypeM
import Acton.TypeEnv
import qualified InterfaceFiles
import qualified Data.Map

reconstruct                             :: String -> Env0 -> Module -> IO (TEnv, Module, Env0)
reconstruct fname env0 (Module m i ss)  = do --traceM ("#################### original env0:")
                                             --traceM (render (pretty env0))
                                             InterfaceFiles.writeFile (fname ++ ".ty") (globalize (setMod m env2) te)
                                             --traceM ("#################### converted env0:")
                                             --traceM (render (pretty env0'))
                                             return (simp env2 te, Module m i ss1, env0')
  where env1                            = reserve (bound ss) (typeX env0)
        (te,ss1)                        = runTypeM $ infTop env1 ss
        env2                            = define te env0
        env0'                           = convEnvProtos env0

showTyFile env0 fname           = do te <- InterfaceFiles.readFile fname
                                     putStrLn ("\n#################################### Interface:\n")
                                     let env2 = define te env0
                                     putStrLn (vprint (simp env2 te))


nodup x
  | not $ null vs               = err2 vs "Duplicate names:"
  | otherwise                   = True
  where vs                      = duplicates (bound x)

class Simp a where
    simp                            :: Env0 -> a -> a

instance (Simp a) => Simp [a] where
    simp env                        = map (simp env)

instance Simp TSchema where
    simp env (TSchema l q t)        = TSchema l q' (subst s $ simp env' t)
      where (q', s)                 = simpQuant env (simp env' q) (tyfree t)
            env'                    = defineTVars (stripQual q) env

simpQuant env q vs0                 = (subst s [ Quant v ps | Quant v ps <- q2, not $ null ps ], s)
  where (q1,q2)                     = partition isEX q
        isEX (Quant v [p])          = length (filter (==v) vs) == 1
        isEX _                      = False
        vs                          = concat [ tyfree ps | Quant v ps <- q ] ++ vs0
        s                           = s1 ++ s2 ++ s3 ++ s4
        s1                          = [ (v, tCon p) | Quant v [p] <- q1 ]                       -- Inline existentials
        s2                          = univars `zip` beautyvars                                  -- Beautify univars
        s3                          = fxvars1 `zip` repeat fxPure                               -- Eliminate unconstrained effects
        s4                          = fxvars2 `zip` beautyfx                                    -- Beautify effects
        (fxvars,univars)            = partition ((==KFX) . tvkind) $ filter univar $ qbound q2
        (fxvars1,fxvars2)           = partition (\v -> length (filter (==v) vs) == 1) fxvars
        isFX v                      = tvkind v == KFX && length (filter (==v) vs) == 1
        beautyvars                  = map tVar $ tvarSupply \\ tvarScope env
        beautyfx                    = map tVar $ fxSupply \\ tvarScope env

instance Simp QBind where
    simp env (Quant v ps)           = Quant v (simp env ps)

instance Simp WTCon where
    simp env (w, c)                 = (w, simp env c)

instance Simp (Name, NameInfo) where
    simp env (n, NSig sc dec)       = (n, NSig (simp env sc) dec)
    simp env (n, NDef sc dec)       = (n, NDef (simp env sc) dec)
    simp env (n, NVar t)            = (n, NVar (simp env t))
    simp env (n, NSVar t)           = (n, NSVar (simp env t))
    simp env (n, NClass q us te)    = (n, NClass (simp env' q) (simp env' us) (simp env' te))
      where env'                    = defineTVars (stripQual q) env
    simp env (n, NProto q us te)    = (n, NProto (simp env' q) (simp env' us) (simp env' te))
      where env'                    = defineTVars (stripQual q) env
    simp env (n, NExt q c us te)    = (n, NExt q' (subst s $ simp env' c) (subst s $ simp env' us) (subst s $ simp env' te))
      where (q', s)                 = simpQuant env (simp env' q) (tyfree c ++ tyfree us ++ tyfree te)
            env'                    = defineTVars (stripQual q) env
    simp env (n, NAct q p k te)     = (n, NAct (simp env' q) (simp env' p) (simp env' k) (simp env' te))
      where env'                    = defineTVars (stripQual q) env
    simp env (n, i)                 = (n, i)

instance Simp Type where
    simp env (TCon l c)             = TCon l (simp env c)
    simp env (TFun l fx p k t)      = TFun l (simp env fx) (simp env p) (simp env k) (simp env t)
    simp env (TTuple l p k)         = TTuple l (simp env p) (simp env k)
    simp env (TOpt l t)             = TOpt l (simp env t)
    simp env (TRow l k n t r)       = TRow l k n (simp env t) (simp env r)
    simp env t                      = t

instance Simp TCon where
    simp env (TC n ts)              = TC (simp env n) (simp env ts)

instance Simp QName where
    simp env (GName m n)
      | Just m == thismod env       = NoQ n
    simp env n
      | not $ null aliases          = NoQ $ head aliases
      | otherwise                   = n
      where aliases                 = [ n1 | (n1, NAlias n2) <- names env, n2 == n ]

------------------------------

infTop                                  :: Env -> Suite -> TypeM (TEnv,Suite)
infTop env ss                           = do --traceM ("\n## infEnv top")
                                             pushFX fxPure tNone
                                             (cs,te,ss1) <- (if stub env then infEnv else infSuiteEnv) env ss
                                             ss1 <- msubst ss1
                                             popFX
                                             eq <- solveAll (define te env) te tNone cs
                                             te <- msubst te
                                             ss2 <- termred <$> msubst (bindWits eq ++ ss1)
                                             let s = [ (tv,repl (tvkind tv)) | tv <- tyfree te ]
                                                 te1 = subst s te
                                                 te2 = normTEnv $ if stub env then unSig te1 else te1
                                             return (te2, ss2)
  where repl KType                      = tNone 
        repl KFX                        = fxPure
        repl PRow                       = posNil
        repl KRow                       = kwdNil


class Infer a where
    infer                               :: Env -> a -> TypeM (Constraints,Type,a)

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM (Constraints,TEnv,a)

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (Constraints,TEnv,Type,a)


--------------------------------------------------------------------------------------------------------------------------

commonTEnv                              :: Env -> [TEnv] -> TypeM (Constraints,TEnv)
commonTEnv env []                       = return ([], [])
commonTEnv env (te:tes)                 = unifEnv tes (restrict te vs)
  where vs                              = foldr intersect (dom te) $ map dom tes
        l                               = length tes
        unifEnv tes []                  = return ([], [])
        unifEnv tes ((n,i):te)          = do t <- newTVar
                                             let (cs1,i') = unif n t i
                                             (cs2,te') <- unifEnv tes te
                                             return (cs1++cs2, (n,i'):te')
        unif n t0 (NVar t)
          | length ts == l              = ([ Cast t1 t0 | t1 <- t:ts ], NVar t0)
          where ts                      = [ t | te <- tes, Just (NVar t) <- [lookup n te] ]
        unif n t0 (NSVar t)
          | length ts == l              = ([ Cast t1 t0 | t1 <- t:ts ], NSVar t0)
          where ts                      = [ t | te <- tes, Just (NSVar t) <- [lookup n te] ]
{-
        unif n t0 (NDef sc d)
          | null (scbind sc) &&
            length ts == l              = ([ Cast t t0 | t <- ts ], NDef (monotype t0) d)
          where ts                      = [ sctype sc | te <- tes, Just (NDef sc d') <- [lookup n te], null (scbind sc), d==d' ]
        unif n t0 (NDef _ _)
          | length scs == l             = case findName n env of
                                             NReserved -> err1 n "Expected a common signature for"
                                             NSig sc d -> ([], NDef sc d)
          where scs                     = [ sc | te <- tes, Just (NDef sc d) <- [lookup n te] ]
-}
        unif n _ _                      = err1 n "Conflicting bindings for"

    

infSuiteEnv env ss                      = do (cs,te,ss') <- infEnv env ss
                                             let (sigs,terms) = sigTerms te
                                             case dom sigs \\ dom terms of
                                                [] -> return (cs, te, ss')
                                                ns -> err2 ns "Signature lacks subsequent binding"

infLiveEnv env x
  | fallsthru x                         = do (cs,te,x') <- infSuiteEnv env x
                                             return (cs, Just te, x')
  | otherwise                           = do (cs,te,x') <- infSuiteEnv env x
                                             return (cs, Nothing, x')

liveCombine te Nothing                  = te
liveCombine Nothing te'                 = te'
liveCombine (Just te) (Just te')        = Just $ te++te'

--------------------------------------------------------------------------------------------------------------------------

instance (InfEnv a) => InfEnv [a] where
    infEnv env []                       = return ([], [], [])
    infEnv env (s : ss)                 = do (cs1,te1,s1) <- infEnv env s
                                             let te1' = if inDecl env then noDefs te1 else te1      -- TODO: also stop class instantiation!
                                             (cs2,te2,ss2) <- infEnv (define te1' env) ss
                                             return (cs1++cs2, te1++te2, s1:ss2)

targetFX Var{}                          = return []
targetFX _                              = do st <- newTVar
                                             fx <- currFX
                                             return [Cast fxMut fx]

instance InfEnv Stmt where
    infEnv env (Expr l e)               = do (cs,_,e') <- infer env e
                                             return (cs, [], Expr l e')

    infEnv env (Assign l pats e)
      | nodup pats                      = do (cs1,te,t,pats') <- infEnvT env pats
                                             (cs2,e') <- inferSub env t e
                                             return (cs1++cs2, te, Assign l pats' e')


--infTarget env (Slice l e sl)            = do (cs1,sl') <- inferSlice env sl
--                                             (cs2,t,e') <- infer env e
--                                             t0 <- newTVar
--                                             w <- newWitness
--                                             return (Impl w t (pSliceable t0) :
--                                                     Cast t tObject :
--                                                     cs1++cs2, t, w, Slice l e' sl')

    infEnv env (MutAssign l tg e)
      | Slice _ e1 sl <- tg             = do cs0 <- targetFX tg
                                             (cs11,t,e1') <- infer env e1
                                             (cs12,sl') <- inferSlice env sl
                                             t0 <- newTVar
                                             w <- newWitness
                                             (cs2,t',e') <- infer env e
                                             w' <- newWitness
                                             let e2 = eCall (tApp (eDot (eVar w) setsliceKW) [t']) [e1', eVar w', sliz2exp sl, e']
                                             return ( Impl w t (pSliceable t0) :
                                                      Impl w' t' (pIterable t0) :
                                                      Cast t tObject :
                                                      cs0++cs11++cs12++cs2, [], Expr l e2 )
      | otherwise                       = do cs0 <- targetFX tg
                                             (cs1,t,w,tg') <- infTarget env tg
                                             (cs2,e') <- inferSub env t e
                                             return (cs0++cs1++cs2, [], assign w tg' e')
      where assign w (Index _ e ix) r   = Expr l $ eCall (eDot (eVar w) setitemKW) [e, ix, r]
            assign _ tg r               = MutAssign l tg r

    infEnv env (AugAssign l tg o e)     = do cs0 <- targetFX tg
                                             (cs1,t,w,lval) <- infTarget env tg
                                             (cs2,rval) <- inferSub env t tg
                                             (cs3,e') <- inferSub env t e
                                             w' <- newWitness
                                             return ( Impl w' t (protocol o) : 
                                                      cs0++cs1++cs2++cs3, [], assign w lval $ eCall (eDot (eVar w') (method o)) [rval,e'])
      where assign _ (Var l (NoQ n)) r  = Assign l [PVar NoLoc n Nothing] r
            assign w (Index l e ix) r   = Expr l $ eCall (eDot (eVar w) setitemKW) [e, ix, r]
            assign w (Slice l e sl) r   = Expr l $ eCall (eDot (eVar w) setsliceKW) [e, sliz2exp sl, r]
            assign _ tg r               = MutAssign l tg r
            
            protocol PlusA              = pPlus
            protocol MinusA             = pMinus
            protocol MultA              = pTimes
            protocol PowA               = pNumber
            protocol DivA               = pDiv
            protocol ModA               = pIntegral
            protocol EuDivA             = pIntegral
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
            
    infEnv env (Assert l e1 e2)         = do (cs1,_,_,e1') <- inferBool env e1
                                             (cs2,e2') <- inferSub env tStr e2
                                             return (cs1++cs2, [], Assert l e1' e2')
    infEnv env s@(Pass l)               = return ([], [], s)
    infEnv env (Delete l tg)            = do cs0 <- targetFX tg
                                             (cs1,t,w,tg') <- infTarget env tg
                                             return (constr tg' t ++ cs0 ++ cs1, [], delete w tg')
      where delete _ (Var _ (NoQ n))    = Assign l [PVar NoLoc n Nothing] eNone
            delete w (Index _ e ix)     = Expr l $ eCall (eDot (eVar w) delitemKW) [e, ix]
            delete w (Slice _ e sl)     = Expr l $ eCall (eDot (eVar w) delsliceKW) [e, sliz2exp sl]
            delete _ tg                 = MutAssign l tg eNone
            
            constr Var{} t              = [Cast tNone t]
            constr Dot{} t              = [Cast tNone t]
            constr _ t                  = []

    infEnv env s@(Return l Nothing)     = do t <- currRet
                                             return ([Cast tNone t], [], Return l Nothing)
    infEnv env (Return l (Just e))      = do t <- currRet
                                             (cs,e') <- inferSub env t e
                                             return (cs, [], Return l (Just e'))
    infEnv env s@(Raise _ Nothing)      = return ([], [], s)
    infEnv env (Raise l (Just e))       = do (cs,_,e') <- infer env e
                                             return (cs, [], Raise l (Just e'))
    infEnv env s@(Break _)              = return ([], [], s)
    infEnv env s@(Continue _)           = return ([], [], s)
    infEnv env (If l bs els)            = do (css,tes,bs') <- fmap unzip3 $ mapM (infLiveEnv env) bs
                                             (cs0,te,els') <- infLiveEnv env els
                                             (cs1,te1) <- commonTEnv env $ catMaybes (te:tes)
                                             return (cs0++cs1++concat css, te1, If l bs' els')
    infEnv env (While l e b els)        = do (cs1,env',s,e') <- inferBool env e
                                             (cs2,te1,b') <- infSuiteEnv env' b
                                             (cs3,te2,els') <- infSuiteEnv env els
                                             return (cs1++cs2++cs3, [], While l e' (termsubst s b') els')
    infEnv env (For l p e b els)
      | nodup p                         = do (cs1,te,t1,p') <- infEnvT env p
                                             t2 <- newTVar
                                             (cs2,e') <- inferSub env t2 e
                                             (cs3,te1,b') <- infSuiteEnv (define te env) b
                                             (cs4,te2,els') <- infSuiteEnv env els
                                             w <- newWitness
                                             return (Impl w t2 (pIterable t1) :
                                                     cs1++cs2++cs3++cs4, [], For l p' (eCall (eDot (eVar w) iterKW) [e']) b' els')
    infEnv env (Try l b hs els fin)     = do (cs1,te,b') <- infLiveEnv env b
                                             (cs2,te',els') <- infLiveEnv (maybe id define te $ env) els
                                             (css,tes,hs') <- fmap unzip3 $ mapM (infLiveEnv env) hs
                                             (cs3,te1) <- commonTEnv env $ catMaybes $ (liveCombine te te'):tes
                                             (cs4,te2,fin') <- infSuiteEnv (define te1 env) fin
                                             return (cs1++cs2++cs3++cs4++concat css, te1++te2, Try l b' hs' els' fin')
    infEnv env (With l items b)
      | nodup items                     = do (cs1,te,items') <- infEnv env items
                                             (cs2,te1,b') <- infSuiteEnv (define te env) b
                                             return $ (cs1++cs2, exclude te1 (dom te), With l items' b')

    infEnv env (VarAssign l pats e)
      | nodup pats                      = do (cs1,te,t,pats') <- infEnvT env pats
                                             (cs2,e') <- inferSub env t e
                                             return (cs1++cs2, [ (n,NSVar t) | (n,NVar t) <- te], VarAssign l pats' e')
    
    infEnv env (After l e1 e2)          = do (cs1,e1') <- inferSub env tInt e1
                                             (cs2,t,e2') <- infer env e2
                                             fx <- currFX
                                             return (Cast fxAction fx :
                                                     cs1++cs2, [], After l e1' e2')
    
    infEnv env (Signature l ns sc dec)
      | not $ null redefs               = illegalRedef (head redefs)
      | otherwise                       = return ([], [(n, NSig (unalias env sc) dec) | n <- ns], Signature l ns sc dec)
      where redefs                      = [ n | n <- ns, findName n env /= NReserved ]

    infEnv env (Data l _ _)             = notYet l "data syntax"

    infEnv env (Decl l ds)
      | inDecl env && nodup ds          = do (cs1,te1,ds1) <- infEnv env ds
                                             return (cs1, te1, Decl l ds1)
      | nodup ds                        = do (cs1,te1,ds1) <- infEnv (setInDecl env) ds
                                             (cs2,ds2) <- checkEnv (define te1 env) ds1
                                             (cs3,te2,ds3) <- genEnv env cs2 te1 ds2
                                             return (cs1++cs3, te2, Decl l ds3)


infTarget env (Var l (NoQ n))           = case findName n env of
                                             NReserved ->
                                                 err1 n "Variable not yet assigned"
                                             NSig (TSchema _ [] t') _ ->
                                                 err1 n "Variable not yet assigned"
                                             NVar t ->
                                                 return ([], t, name "_", Var l (NoQ n))
                                             NSVar t -> do
                                                 fx <- currFX
                                                 return ([Cast fxAction fx], t, name "_", Var l (NoQ n))
                                             _ -> 
                                                 err1 n "Variable not assignable:"
infTarget env (Index l e ix)            = do ti <- newTVar
                                             (cs1,ix') <- inferSub env ti ix
                                             (cs2,t,e') <- infer env e
                                             t0 <- newTVar
                                             w <- newWitness
                                             return (Impl w t (pIndexed ti t0) :
                                                     Cast t tObject :
                                                     cs1++cs2, t0, w, Index l e' ix')
--infTarget env (Slice l e sl)            = do (cs1,sl') <- inferSlice env sl
--                                             (cs2,t,e') <- infer env e
--                                             t0 <- newTVar
--                                             w <- newWitness
--                                             return (Impl w t (pSliceable t0) :
--                                                     Cast t tObject :
--                                                     cs1++cs2, t, w, Slice l e' sl')
infTarget env (Dot l e n)               = do (cs,t1,e') <- infer env e
                                             t2 <- newTVar
                                             return (Mut t1 n t2 :
                                                     Cast t1 tObject :
                                                     cs, t2, name "_", Dot l e' n)

sliz2exp (Sliz _ e1 e2 e3)              = eCall (eQVar qnSlice) $ map (maybe eNone id) [e1,e2,e3]

--------------------------------------------------------------------------------------------------------------------------

matchingDec n sc dec NoDec              = True
matchingDec n sc dec dec'
  | dec == dec'                         = True
  | otherwise                           = decorationMismatch n sc dec

matchDefAssumption env cs def           = do --traceM ("## matchDefAssumption " ++ prstr (dname def))
                                             (cs1, tvs) <- instQBinds env q1
                                             let eq0 = witSubst env q1 cs1
                                                 s = qbound q1 `zip` tvs           -- This cannot just be memoized in the global TypeM substitution,
                                             def <- msubstWith s def{ qbinds = [] } -- since the variables in (qbound q1) aren't necessarily globally unique
                                             let t1 = tFun (dfx def) (prowOf $ pos def) (krowOf $ kwd def) (fromJust $ ann def)
                                             (cs2,eq1) <- solveScoped env0 (qbound q0) [] t1 (Cast t1 (if inClass env then addSelf t0 (Just dec) else t0) : cs++cs1)
                                             checkNoEscape env (qbound q0)
                                             return (cs2, def{ qbinds = noqual env q0, pos = pos0 def, dbody = bindWits (eq0++eq1) ++ dbody def, dfx = fx t0 })
  where NDef (TSchema _ q0 t0) dec      = findName (dname def) env
        q1                              = qbinds def
        env0                            = defineTVars q1 $ defineTVars q0 env
        pos0 def
          | inClass env && dec/=Static  = case pos def of
                                            PosPar nSelf t' e' pos' -> PosPar nSelf t' e' $ qualWPar env q0 pos'
                                            _ -> err1 (dname def) "Missing self parameter"
          | otherwise                   = qualWPar env q0 (pos def)

--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Decl where
    infEnv env d@(Def _ n q p k a _ _ fx)
      | nodup (p,k)                     = case findName n env of
                                             NSig sc dec | matchingDec n sc dec (deco d) -> do
                                                 --traceM ("\n## infEnv (sig) def " ++ prstr (n, NDef sc dec))
                                                 return ([], [(n, NDef sc dec)], d)
                                             NReserved -> do
                                                 prow <- newTVarOfKind PRow
                                                 krow <- newTVarOfKind KRow
                                                 t <- tFun fx prow krow <$> newTVar
                                                 --traceM ("\n## infEnv def " ++ prstr (n, NDef (monotype t) (deco d)))
                                                 return ([], [(n, NDef (monotype t) (deco d))], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env d@(Actor _ n q p k b)
      | nodup (p,k)                     = case findName n env of
                                             NReserved -> do
                                                 te <- infActorEnv env b
                                                 prow <- newTVarOfKind PRow
                                                 krow <- newTVarOfKind KRow
                                                 --traceM ("\n## infEnv actor " ++ prstr (n, NAct q prow krow te))
                                                 return ([], [(n, NAct (unalias env q) prow krow te)], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env (Class l n q us b)
      | not $ null ps                   = notYet (loc n) "Classes with direct extensions"
      | otherwise                       = case findName n env of
                                             NReserved -> do
                                                 --traceM ("\n## infEnv class " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 te0 <- infProperties env as' b
                                                 (cs,te,b') <- infEnv env1 b
                                                 popFX
                                                 (cs1,eq1) <- solveScoped env1 (qbound q) te tNone cs
                                                 checkNoEscape env (qbound q)
                                                 (nterms,_,_) <- checkAttributes [] te' te
                                                 let te1 = stubAttributes env te
                                                 return (cs1, [(n, NClass q' as' (te0++te1))], Class l n q us (bindWits eq1 ++ props te0 ++ b'))
                                             _ -> illegalRedef n
      where env1                        = define (exclude (toSigs te') [initKW]) $ reserve (bound b) $ defineSelfOpaque $ defineTVars (stripQual q) env
            (as,ps)                     = mro2 env (unalias env us)
            q'                          = unalias env q
            as'                         = if null as && not (inBuiltin env && n == nValue) then [([Nothing],cValue)] else as
            te'                         = parentTEnv env as'
            props te0                   = [ Signature l0 [n] sc Property | (n,NSig sc Property) <- te0 ]
            
    infEnv env (Protocol l n q us b)    = case findName n env of
                                             NReserved -> do
                                                 --traceM ("\n## infEnv protocol " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 (cs,te,b') <- infEnv env1 b
                                                 popFX
                                                 (cs1,eq1) <- solveScoped env1 (qbound q) te tNone cs
                                                 checkNoEscape env (qbound q)
                                                 (nterms,_,sigs) <- checkAttributes [] te' te
                                                 let noself = [ n | (n, NSig sc Static) <- te, tvSelf `notElem` tyfree sc ]
                                                 when (not $ null nterms) $ err2 (dom nterms) "Method/attribute lacks signature:"
                                                 when (initKW `elem` sigs) $ err2 (filter (==initKW) sigs) "A protocol cannot define __init__"
                                                 when (not $ null noself) $ err2 noself "A static protocol signature must mention Self"
                                                 return (cs1, [(n, NProto q' ps te)], Protocol l n q us (bindWits eq1 ++ b'))
                                             _ -> illegalRedef n
      where env1                        = define (toSigs te') $ reserve (bound b) $ defineSelfOpaque $ defineTVars (stripQual q) env
            ps                          = mro1 env (unalias env us)
            q'                          = unalias env q
            te'                         = parentTEnv env ps

    infEnv env (Extension l q c us b)
      | isActor env n                   = notYet (loc n) "Extension of an actor"
      | isProto env n                   = notYet (loc n) "Extension of a protocol"
      | length us == 0                  = err (loc n) "Extension lacks a protocol"
--      | length us > 1                   = notYet (loc n) "Extensions with multiple protocols"
      | not $ null witsearch            = err (loc n) "Extension already exists"
      | otherwise                       = do --traceM ("\n## infEnv extension " ++ prstr n)
                                             pushFX fxPure tNone
                                             (cs,te,b') <- infEnv env1 b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 (qbound q) te tNone cs
                                             checkNoEscape env (qbound q)
                                             (nterms,asigs,sigs) <- checkAttributes final te' te
                                             when (not $ null nterms) $ err2 (dom nterms) "Method/attribute not in listed protocols:"
                                             when (not (null asigs || stub env)) $ err3 l asigs "Protocol method/attribute lacks implementation:"
                                             when (not $ null sigs) $ err2 sigs "Extension with new methods/attributes not supported"
                                             -- w <- newWitness
                                             return (cs1, [(extensionName (head us) n, NExt q' c ps te)], Extension l q c us (bindWits eq1 ++ b'))
      where TC n ts                     = c
            env1                        = define (toSigs te') $ reserve (bound b) $ defineSelfOpaque $ defineTVars (stripQual q) env
            witsearch                   = findWitness env n (implProto env $ head us)
            ps                          = mro1 env (unalias env us)     -- TODO: check that ps doesn't contradict any previous extension mro for c
            q'                          = unalias env q
            final                       = concat [ conAttrs env pn | (_, TC pn _) <- ps, hasWitness env n pn ]
            te'                         = parentTEnv env ps

--------------------------------------------------------------------------------------------------------------------------

checkAttributes final te' te
  | not $ null osigs                    = err2 osigs "Inherited signatures cannot be overridden:"
  | not $ null props                    = err2 props "Property attributes cannot have class-level definitions:"
  | not $ null nodef                    = err2 nodef "Methods finalized in a previous extension cannot be overridden:"
--  | not $ null nself                    = err0 nself "Negative Self in non-static method signature"
  | otherwise                           = return (nterms, abssigs, dom sigs)
  where (sigs,terms)                    = sigTerms te
        (sigs',terms')                  = sigTerms te'
        (allsigs,allterms)              = (sigs ++ sigs', terms ++ terms')
        nterms                          = exclude terms (dom allsigs)
        abssigs                         = dom allsigs \\ (dom allterms ++ final)
        osigs                           = (dom sigs `intersect` dom sigs') \\ [initKW]
        props                           = dom terms `intersect` dom (propSigs allsigs)
        nodef                           = dom terms `intersect` final
        nself                           = negself te

stripQual q                             = [ Quant v [] | Quant v us <- q ]

toSigs te                               = map makeSig te
  where makeSig (n, NDef sc dec)        = (n, NSig sc dec)
        makeSig (n, NVar t)             = (n, NSig (monotype t) Static)
        makeSig (n, i)                  = (n,i)


stubAttributes env te
  | stub env                            = te ++ stubSigs te
  | otherwise                           = te

stubSigs te                             = [ makeDef n sc dec | (n, NSig sc dec) <- te, dec /= Property ]
  where makeDef n (TSchema l q t) dec
          | TFun{} <- t                 = (n, NDef (TSchema l q $ addSelf t (Just dec)) dec)
          | otherwise                   = (n, NVar t)


--------------------------------------------------------------------------------------------------------------------------

solveAll env te tt cs                   = do --traceM ("\n\n### solveAll " ++ prstrs cs)
                                             (cs,eq) <- simplify env te tt cs
                                             (_,cs,_,eq) <- refine env cs te eq
                                             snd <$> solve env (const True) te tt eq cs

solveScoped env [] te tt cs             = simplify env te tt cs
solveScoped env vs te tt cs             = do --traceM ("\n\n### solveScoped " ++ prstrs vs)
                                             (cs,eq) <- simplify env te tt cs
                                             solve env (any (`elem` vs) . tyfree) te tt eq cs

checkNoEscape env []                    = return ()
checkNoEscape env vs                    = do fvs <- tyfree <$> msubst env
                                             let escaped = vs `intersect` fvs
                                             when (not $ null escaped) $ do
                                                 env1 <- msubst env
                                                 --traceM ("####### env:\n" ++ prstr env1)
                                                 err2 escaped "Escaping type variable"


wellformed                              :: (WellFormed a) => Env -> a -> TypeM ()
wellformed env x                        = do _ <- solveAll env [] tNone cs
                                             return ()
  where cs                              = wf env x

wellformedProtos                        :: Env -> [PCon] -> TypeM (Constraints, [(QName,[Expr])])
wellformedProtos env ps                 = do (css0, css1) <- unzip <$> mapM (wfProto env) ps
                                             _ <- solveAll env [] tNone (concat css0)
                                             return (concat css1, [ (tcname p, witsOf cs) | (p,cs) <- ps `zip` css1 ])


--------------------------------------------------------------------------------------------------------------------------

class Check a where
    checkEnv                            :: Env -> a -> TypeM (Constraints,a)
    checkEnv'                           :: Env -> a -> TypeM (Constraints,[a])
    checkEnv' env x                     = do (cs,x') <- checkEnv env x
                                             return (cs, [x'])

instance (Check a) => Check [a] where
    checkEnv env []                     = return ([], [])
    checkEnv env (d:ds)                 = do (cs1,d') <- checkEnv' env d
                                             (cs2,ds') <- checkEnv env ds
                                             return (cs1++cs2, d'++ds')

------------------

infActorEnv env ss                      = do dsigs <- mapM mkNDef (dvars ss \\ dom sigs)
                                             bsigs <- mapM mkNVar (pvars ss \\ dom (sigs++dsigs))
                                             return (sigs ++ dsigs ++ bsigs)
  where sigs                            = [ (n, NSig sc dec) | Signature _ ns sc dec <- ss, n <- ns, not $ isHidden n ]
        dvars ss                        = nub [ n | Decl _ ds <- ss, Def{dname=n} <- ds, not $ isHidden n ]
        mkNDef n                        = do t <- newTVar
                                             return (n, NDef (monotype $ t) NoDec)
        pvars ss                        = nub $ concat $ map pvs ss
          where pvs (Assign _ pats _)   = filter (not . isHidden) $ bound pats
                pvs (If _ bs els)       = foldr intersect (pvars els) [ pvars ss | Branch _ ss <- bs ]
                pvs _                   = []
        mkNVar n                        = do t <- newTVar
                                             return (n, NVar t)

matchActorAssumption env n0 p k te      = do --traceM ("## matchActorAssumption " ++ prstr n0)
                                             (cs,eq) <- simplify env te0 tNone [Cast (prowOf p) p0, Cast (krowOf k) k0]
                                             (css,eqs) <- unzip <$> mapM check1 te
                                             --traceM ("## matchActorAssumption returns " ++ prstrs (cs ++ concat css))
                                             return (cs ++ concat css, eq ++ concat eqs)
  where NAct _ p0 k0 te0                = findName n0 env
        check1 (n, i) | isHidden n      = return ([], [])
        check1 (n, NVar t)              = do --traceM ("## matchActorAssumption for attribute " ++ prstr n)
                                             unify env t t0
                                             return ([],[])
          where t0                      = case lookup n te0 of
                                             Just (NSig (TSchema _ _ t0) _) -> t0
                                             Just (NVar t0) -> t0
        check1 (n, NDef sc _)           = do (cs1,_,t) <- instantiate env sc
                                             --traceM ("## matchActorAssumption for method " ++ prstr n)
                                             unify env t t0
                                             (cs2,eq) <- solveScoped (defineTVars q env) (qbound q) te0 tNone cs1
                                             checkNoEscape env (qbound q)
                                             return (cs2, eq)
          where TSchema _ q t0          = case lookup n te0 of
                                             Just (NSig sc _) -> sc
                                             Just (NDef sc _) -> sc
        check1 (n, i)                   = return ([], [])


infProperties env as b
  | Just (self,ss) <- inits             = infProps self ss
  | otherwise                           = return []
  where inherited                       = concat $ map (conAttrs env . tcname . snd) as
        explicit                        = concat [ ns | Signature _ ns _ Property <- b ]
        inits                           = listToMaybe [ (x, dbody d) | Decl _ ds <- b, d@Def{pos=PosPar x _ _ _} <- ds, dname d == initKW ]
        infProps self (MutAssign _ (Dot _ (Var _ (NoQ x)) n) _ : b)
          | x /= self                   = return []
          | n `notElem` inherited,
            n `notElem` explicit        = do t <- newTVar
                                             te <- infProps self b
                                             return ((n,NSig (monotype t) Property) : te)
          | otherwise                   = infProps self b 
        infProps self (Expr _ (Call _ (Dot _ (Var _ c) n) _ _) : b)
          | isClass env c, n == initKW  = infProps self b
        infProps self _                 = return []

infDefBody env n (PosPar x _ _ _) b
  | inClass env && n == initKW          = infInitEnv env x b
infDefBody env _ _ b                    = do (cs,_,b') <- infSuiteEnv env b; return (cs, b')

infInitEnv env self (MutAssign l (Dot l' e1@(Var _ (NoQ x)) n) e2 : b)
  | x == self                           = do (cs1,t1,e1') <- infer env e1
                                             t2 <- newTVar
                                             (cs2,e2') <- inferSub env t2 e2
                                             (cs3,b') <- infInitEnv env self b
                                             return (Mut t1 n t2 : 
                                                     cs1++cs2++cs3, MutAssign l (Dot l' e1' n) e2' : b')
infInitEnv env self (Expr l e : b)
  | Call{fun=Dot _ (Var _ c) n} <- e,
    isClass env c, n == initKW          = do (cs1,_,e') <- infer env e
                                             (cs2,b') <- infInitEnv env self b
                                             return (cs1++cs2, Expr l e' : b')
infInitEnv env self b                   = do (cs,_,b') <- infSuiteEnv env b
                                             return (cs, b')

abstractDefs env q eq b                 = map absDef b
  where absDef (Decl l ds)              = Decl l (map absDef' ds)
        absDef (If l bs els)            = If l [ Branch e (map absDef ss) | Branch e ss <- bs ] (map absDef els)
        absDef stmt                     = stmt
        absDef' d@Def{}                 = d{ pos = pos1, dbody = bindWits eq ++ dbody d }
          where pos1
                  | deco d /= Static    = case pos d of
                                            PosPar nSelf t' e' pos' -> PosPar nSelf t' e' $ qualWPar env q pos'
                                            _ -> err1 (dname d) "Missing self parameter"
                  | otherwise           = qualWPar env q (pos d)


instance Check Decl where
    checkEnv env (Def l n q p k a b dec fx)
                                        = do --traceM ("## checkEnv def " ++ prstr n ++ " (q = [" ++ prstrs q ++ "])")
                                             t <- maybe newTVar return (unalias env a)
                                             pushFX fx t
                                             st <- newTVar
                                             wellformed env1 q
                                             wellformed env1 a
                                             (csp,te0,p') <- infEnv env1 p
                                             (csk,te1,k') <- infEnv (define te0 env1) k
                                             (csb,b') <- infDefBody (define te1 (define te0 env1)) n p' b
                                             popFX
                                             let cst = if fallsthru b then [Cast tNone t] else []
                                                 t1 = tFun fx (prowOf p') (krowOf k') t
                                             (cs1,eq1) <- solveScoped env1 tvs [] t1 (csp++csk++csb++cst)
                                             checkNoEscape env tvs
                                             -- At this point, n has the type given by its def annotations.
                                             -- Now check that this type is no less general than its recursion assumption in env.
                                             matchDefAssumption env cs1 (Def l n q (noDefaultsP p') (noDefaultsK k') (Just t)
                                                                         (bindWits eq1 ++ defaultsP p' ++ defaultsK k' ++ b') dec fx)
      where env1                        = reserve (bound (p,k) ++ bound b \\ stateScope env) $ defineTVars q $ setInDef env
            tvs                         = qbound q

    checkEnv env (Actor l n q p k b)    = do --traceM ("## checkEnv actor " ++ prstr n)
                                             pushFX fxAction tNone
                                             wellformed env1 q
                                             (csp,te1,p') <- infEnv env1 p
                                             (csk,te2,k') <- infEnv (define te1 env1) k
                                             (csb,te,b') <- infSuiteEnv (define te2 $ define te1 env1) b
                                             (cs0,eq0) <- matchActorAssumption env1 n p' k' te
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 tvs te tNone (csp++csk++csb++cs0)
                                             checkNoEscape env tvs
                                             fvs <- tyfree <$> msubst env
                                             return (cs1, Actor l n (noqual env q) (qualWPar env q $ noDefaultsP p') (noDefaultsK k')
                                                          (bindWits (eq1++eq0) ++ defsigs ++ defaultsP p' ++ defaultsK k' ++ b'))
      where env1                        = reserve (bound (p,k) ++ bound b) $ defineTVars q $
                                          define [(selfKW, NVar tRef)] $ reserve (statevars b) $ setInAct env
            tvs                         = qbound q
            defsigs                     = [ Signature NoLoc [n] sc dec | (n,NDef sc dec) <- te0 ]
            NAct _ _ _ te0              = findName n env

    checkEnv' env (Class l n q us b)    = do --traceM ("## checkEnv class " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             wellformed env1 us
                                             (csb,b') <- checkEnv env1 b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 tvs te tNone csb
                                             checkNoEscape env tvs
                                             return (cs1, [Class l n (noqual env q) (map snd as) (abstractDefs env q eq1 b')])
      where env1                        = define (subst s te) $ defineSelf (NoQ n) q $ defineTVars q $ setInClass env
            tvs                         = tvSelf : qbound q
            NClass _ as te              = findName n env
            s                           = [(tvSelf, tCon (TC (NoQ n) (map tVar $ qbound q)))]

    checkEnv' env (Protocol l n q us b) = do --traceM ("## checkEnv protocol " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             (csu,wmap) <- wellformedProtos env1 us
                                             (csb,b') <- checkEnv env1 b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 tvs te tNone (csu++csb)
                                             checkNoEscape env tvs
                                             b' <- msubst b'
                                             return (cs1, convProtocol env n q ps eq1 wmap b')
      where env1                        = define te $ defineSelf (NoQ n) q $ defineTVars q $ setInClass env
            tvs                         = tvSelf : qbound q
            NProto _ ps te              = findName n env

    checkEnv' env (Extension l q c us b)
                                        = do --traceM ("## checkEnv extension " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             (csu,wmap) <- wellformedProtos env1 us
                                             (csb,b') <- checkEnv env1 b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 tvs te tNone (csu++csb)
                                             checkNoEscape env tvs
                                             b' <- msubst b'
                                             return (cs1, convExtension env n' c q ps eq1 wmap b')
      where env1                        = define (subst s te) $ defineInst n ps thisKW' $ defineSelf n q $ defineTVars q $ setInClass env
            tvs                         = tvSelf : qbound q
            n                           = tcname c
            n'                          = extensionName (head us) n
            NExt _ _ ps te              = findName n' env
            s                           = [(tvSelf, tCon $ TC n (map tVar $ qbound q))]

    checkEnv' env x                     = do (cs,x') <- checkEnv env x
                                             return (cs, [x'])


instance Check Stmt where
    checkEnv env (If l bs els)          = do (cs1,bs') <- checkEnv env bs
                                             (cs2,els') <- checkEnv env els
                                             return (cs1++cs2, If l bs' els')
    checkEnv env (Decl l ds)            = do (cs,ds') <- checkEnv env ds
                                             return (cs, Decl l ds')
    checkEnv env (Signature l ns sc dec)
                                        = do wellformed env1 q
                                             wellformed env1 t
                                             return ([], Signature l ns sc' dec)
      where TSchema l q t               = sc
            sc' | null q                = sc
                | otherwise             = let TFun l' x p k t' = t in TSchema l (noqual env q) (TFun l' x (qualWRow env q p) k t')
            env1                        = defineTVars q env
    checkEnv env s                      = return ([], s)

instance Check Branch where
    checkEnv env (Branch e b)           = do (cs,b') <- checkEnv env b
                                             return (cs, Branch e b')


--------------------------------------------------------------------------------------------------------------------------

refine                                  :: Env -> Constraints -> TEnv -> Equations -> TypeM ([TVar], Constraints, TEnv, Equations)
refine env cs te eq
  | not $ null solve_vs                 = do --traceM ("  #solving cs : " ++ prstrs cs)
                                             (cs',eq') <- solve (define te env) (not . canQual) te tNone eq cs
                                             refineAgain cs' eq'
  | not $ null ambig_vs                 = do --traceM ("  #defaulting vs : " ++ prstrs ambig_vs)
                                             --traceM ("              cs : " ++ prstrs cs)
                                             (cs',eq') <- solve env ambig te tNone eq cs
                                             refineAgain cs' eq'
  | otherwise                           = do eq <- msubst eq
                                             return (gen_vs, cs, te, eq)
  where solve_vs                        = [ headvar c | c <- cs, not (canQual c) ]
        ambig_vs                        = tyfree cs \\ closeDepVars safe_vs cs

        safe_vs                         = if null def_vss then [] else nub $ foldr1 intersect def_vss
        def_vss                         = [ nub $ tyfree sc | (_, NDef sc _) <- te, null $ scbind sc ]
        gen_vs                          = nub (foldr union (tyfree cs) def_vss)
        
        canQual (Impl _ (TVar _ v) _)   = univar v
        canQual _                       = False

        ambig c                         = any (`elem` ambig_vs) (tyfree c)

        refineAgain cs eq               = do (cs,eq') <- simplify env te tNone cs
                                             te <- msubst te
                                             env <- msubst env
                                             refine env cs te (eq'++eq)

tyfixed te                              = tyfree $ filter (not . gen) te
  where gen (n, NDef sc _)              = null $ scbind sc
        gen _                           = False

qualify vs cs                           = let (q,wss) = unzip $ map qbind vs in (q, concat wss)
  where qbind v                         = (Quant v (casts ++ impls), wits)
          where casts                   = [ u | Cast (TVar _ v') (TCon _ u) <- cs, v == v' ]
                impls                   = [ p | Impl w (TVar _ v') p <- cs, v == v' ]
                wits                    = [ (w, impl2type t p) | Impl w t@(TVar _ v') p <- cs, v == v' ]

genEnv                                  :: Env -> Constraints -> TEnv -> [Decl] -> TypeM (Constraints,TEnv,[Decl])
genEnv env cs te ds0
  | onTop env                           = do te <- msubst te
                                             --traceM ("## genEnv 1\n" ++ render (nest 6 $ pretty te))
                                             (cs0,eq0) <- simplify env te tNone cs
                                             te <- msubst te
                                             env <- msubst env
                                             (gen_vs, gen_cs, te, eq1) <- refine env cs0 te eq0
                                             --traceM ("## genEnv 2 [" ++ prstrs gen_vs ++ "]\n" ++ render (nest 6 $ pretty te))
                                             let (q,ws) = qualify gen_vs gen_cs
                                                 te1 = map (generalize q) te
                                                 ds1 = map (abstract q ds0 ws eq1) ds0
                                             --traceM ("## genEnv 3 [" ++ prstrs gen_vs ++ "]\n" ++ render (nest 6 $ pretty te1))
                                             return ([], te1, ds1)
  | otherwise                           = do te <- msubst te
                                             --traceM ("## noGenEnv\n" ++ render (nest 6 $ pretty te))
                                             (cs0,eq0) <- simplify env te tNone cs
                                             te <- msubst te
                                             let ds1 = map (abstract [] ds0 [] eq0) ds0
                                             return (cs0, te, ds1)
  where
    generalize q (n, NDef sc d)
      | null $ scbind sc                = (n, NDef (tSchema q (sctype sc)) d)
    generalize q (n, i)                 = (n, i)

    abstract q ds ws eq d@Def{}
      | null $ qbinds d                 = d{ qbinds = noqual env q, 
                                             pos = wit2par ws (pos d),
                                             dbody = bindWits eq ++ wsubst ds q ws (dbody d) }
      | otherwise                       = d{ dbody = bindWits eq ++ wsubst ds q ws (dbody d) }
    abstract q ds ws eq d@Actor{}       = d{ dbody = bindWits eq ++ wsubst ds q ws (dbody d) }
    abstract q ds ws eq d               = d{ dbody = map bindInDef (wsubst ds q ws (dbody d)) }
      where bindInDef (Decl l ds')      = Decl l (map bindInDef' ds')
            bindInDef (If l bs els)     = If l [ Branch e (map bindInDef ss) | Branch e ss <- bs ] (map bindInDef els)
            bindInDef stmt              = stmt
            bindInDef' d@Def{}          = d{ dbody = bindWits eq ++ dbody d }
            bindInDef' d                = d{ dbody = map bindInDef (dbody d) }
            
    wsubst ds [] []                     = id
    wsubst ds q ws                      = termsubst s
      where s                           = [ (n, Lambda l0 p k (Call l0 (tApp (eVar n) tvs) (wit2arg ws (pArg p)) (kArg k)) fx) 
                                            | Def _ n [] p k _ _ _ fx <- ds ]
            tvs                         = map tVar $ qbound q


defaultsP (PosPar n (Just t) (Just e) p)
                                        = s : defaultsP p
  where s                               = sIf1 test [set] []
        test                            = eCall (tApp (eQVar primISNOTNONE) [t]) [eVar n]
        set                             = sAssign (pVar' n) e
defaultsP (PosPar n _ Nothing p)        = defaultsP p
defaultsP _                             = []

noDefaultsP (PosPar n t _ p)            = PosPar n t Nothing (noDefaultsP p)
noDefaultsP p                           = p

defaultsK (KwdPar n (Just t) (Just e) k)
                                        = s : defaultsK k
  where s                               = sIf1 test [set] []
        test                            = eCall (tApp (eQVar primISNOTNONE) [t]) [eVar n]
        set                             = sAssign (pVar' n) e
defaultsK (KwdPar n _ Nothing k)        = defaultsK k
defaultsK _                             = []

noDefaultsK (KwdPar n t _ k)            = KwdPar n t Nothing (noDefaultsK k)
noDefaultsK k                           = k



--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Branch where
    infEnv env (Branch e b)             = do (cs1,env',s,e') <- inferBool env e
                                             (cs2,te,b') <- infEnv env' b
                                             return (cs1++cs2, te, Branch e' (termsubst s b'))

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl w t pContextManager :
                                                     cs, [], WithItem e' Nothing)           -- TODO: translate using w
    infEnv env (WithItem e (Just p))    = do (cs1,t1,e') <- infer env e
                                             (cs2,te,t2,p') <- infEnvT env p
                                             w <- newWitness
                                             return (Cast t1 t2 :
                                                     Impl w t1 pContextManager :
                                                     cs1++cs2, te, WithItem e' (Just p'))         -- TODO: translate using w

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do (cs1,te,ex') <- infEnv env ex
                                             (cs2,te1,b') <- infEnv (define te env) b
                                             return (cs1++cs2, exclude te1 (dom te), Handler ex' b')

instance InfEnv Except where
    infEnv env (ExceptAll l)            = return ([], [], ExceptAll l)
    infEnv env (Except l x)             = return ([Cast t tException], [], Except l x)
      where t                           = tCon (TC x [])
    infEnv env (ExceptAs l x n)         = return ([Cast t tException], [(n, NVar t)], ExceptAs l x n)
      where t                           = tCon (TC x [])

instance Infer Expr where
    infer env x@(Var l n)               = case findQName n env of
                                            NVar t -> return ([], t, x)
                                            NSVar t -> do
                                                fx <- currFX
                                                return ([Cast fxAction fx], t, x)
                                            NDef sc d -> do 
                                                (cs,tvs,t) <- instantiate env sc
                                                return (cs, t, app t (tApp x tvs) $ witsOf cs)
                                            NClass q _ _ -> do
                                                (cs0,ts) <- instQBinds env q
                                                --traceM ("## Instantiating " ++ prstr n)
                                                when (not $ null $ abstractAttrs env n) (err1 n "Abstract class cannot be instantiated:")
                                                case findAttr env (TC n ts) initKW of
                                                    Just (_,sc,_) -> do
                                                        (cs1,tvs,t) <- instantiate env sc
                                                        let t0 = tCon $ TC (unalias env n) ts
                                                            t' = subst [(tvSelf,t0)] t{ restype = tSelf }
                                                        return (cs0++cs1, t', app t' (tApp x (ts++tvs)) $ witsOf (cs0++cs1))
                                            NAct q p k _ -> do
                                                (cs,tvs,t) <- instantiate env (tSchema q (tFun fxAction p k (tCon0 (unalias env n) q)))
                                                return (cs, t, app t (tApp x tvs) $ witsOf cs)
                                            NSig _ _ -> nameReserved n
                                            NReserved -> nameReserved n
                                            _ -> nameUnexpected n
    infer env e@(Int _ val s)           = do t <- newTVar
                                             w <- newWitness
                                             return ([Impl w t pNumber], t, eCall (eDot (eVar w) fromatomKW) [e])
    infer env e@(Float _ val s)         = do t <- newTVar
                                             w <- newWitness
                                             return ([Impl w t pRealFloat], t, eCall (eDot (eVar w) fromatomKW) [e])
    infer env e@Imaginary{}             = notYetExpr e
    infer env e@(Bool _ val)            = return ([], tBool, e)
    infer env e@(None _)                = return ([], tNone, e)
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env e@(Strings _ ss)          = return ([], tStr, e)
    infer env e@(BStrings _ ss)         = return ([], tBytes, e)
    infer env (Call l e ps ks)          = do (cs1,t,e') <- infer env e
                                             (cs2,prow,ps') <- infer env ps
                                             (cs3,krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX
                                             w <- newWitness
                                             return (Sub w t (tFun fx prow krow t0) :
                                                     cs1++cs2++cs3, t0, Call l (eCall (eVar w) [e']) ps' ks')
    infer env (TApp l e ts)             = internal l "Unexpected TApp in infer"
    infer env (Async l e)               = do (cs1,t,e') <- infer env e              -- TODO: expect an action type returning t
                                             fx <- currFX
                                             return (Cast fxAction fx :
                                                     cs1, tMsg t, Async l e')       -- TODO: produce action type returning Msg[t]
    infer env (Await l e)               = do t0 <- newTVar
                                             (cs1,e') <- inferSub env (tMsg t0) e
                                             fx <- currFX
                                             return (Cast fxAction fx :
                                                     cs1, t0, Await l e')
    infer env (Index l e ix)            = do ti <- newTVar
                                             (cs1,ix') <- inferSub env ti ix
                                             t0 <- newTVar
                                             w <- newWitness
                                             (cs2,t,e') <- infer env e
                                             return (Impl w t (pIndexed ti t0) :
                                                     cs1++cs2, t0, eCall (eDot (eVar w) getitemKW) [e', ix'])
    infer env (Slice l e sl)            = do (cs1,sl') <- inferSlice env sl
                                             (cs2,t,e') <- infer env e
                                             t0 <- newTVar
                                             w <- newWitness
                                             return (Impl w t (pSliceable t0) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) getsliceKW) [e', sliz2exp sl'])
    infer env (NDSlice l e slz)         = do (css,es) <- fmap unzip $ mapM (inferNDSlice env) slz
                                             t <- newTVar
                                             (cs,e') <- inferSub env (tNDArray t) e
                                             return (concat css++cs, (tNDArray t), eCall (eDot e' ndgetsliceKW) [List l (map Elem es)])
    infer env (Cond l e1 e e2)          = do t0 <- newTVar
                                             (cs0,env',s,e') <- inferBool env e
                                             (cs1,e1') <- inferSub env' t0 e1
                                             (cs2,e2') <- inferSub env t0 e2
                                             return (cs0++cs1++cs2, t0, Cond l (termsubst s e1') e' e2')
    infer env (IsInstance l e c)        = case findQName c env of
                                             NClass q _ _ -> do
                                                (cs,t,e') <- infer env e
                                                ts <- newTVars [ tvkind v | v <- qbound q ]
                                                return (Cast (tCon (TC (unalias env c) ts)) t :
                                                        cs, tBool, IsInstance l e' c)
                                             _ -> nameUnexpected c
    infer env (BinOp l s@Strings{} Mod e)
      | TRow _ _ _ t TNil{} <- prow     = do (cs,e') <- inferSub env t e
                                             return (cs, tStr, eCall formatF [s,eTuple [e']])
      | otherwise                       = do (cs,e') <- inferSub env tup e
                                             return (cs, tStr, eCall formatF [s,e'])
      where formatF                     = tApp (eQVar primFORMAT) [prow]
            tup                         = tTuple prow kwdNil
            prow                        = format $ concat $ sval s
            format []                   = posNil
            format ('%':s)              = nokey s
            format (c:s)                = format s
            nokey ('(':s)               = err l ("Mapping keys not supported in format strings")
            nokey s                     = flags s
            flags (f:s)
              | f `elem` "#0- +"        = flags s
            flags s                     = width s
            width ('*':s)               = posRow tInt (dot s)
            width (n:s)
              | n `elem` "123456789"    = dot (dropWhile (`elem` "0123456789") s)
            width s                     = dot s
            dot ('.':s)                 = prec s
            dot s                       = len s
            prec ('*':s)                = posRow tInt (len s)
            prec (n:s)
              | n `elem` "0123456789"   = len (dropWhile (`elem` "0123456789") s)
            prec s                      = len s
            len (l:s)
              | l `elem` "hlL"          = conv s
            len s                       = conv s
            conv (t:s)
              | t `elem` "diouxXc"      = posRow tInt (format s)
              | t `elem` "eEfFgG"       = posRow tFloat (format s)
              | t `elem` "rsa"          = posRow tStr (format s)
              | t == '%'                = format s
            conv (c:s)                  = err l ("Bad conversion character: " ++ [c])
            conv []                     = err l ("Bad conversion string")
    infer env (BinOp l e1 op e2)
      | op `elem` [Or,And]              = do (cs1,env1,s1,e1') <- inferBool env e1
                                             (cs2,env2,s2,e2') <- inferBool env1 e2
                                             return (cs1++cs2, tBool, BinOp l e1' op (termsubst s1 e2'))
      | otherwise                       = do t <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env (rtype op t) e2
                                             w <- newWitness
                                             return (Impl w t (protocol op) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Plus               = pPlus
            protocol Minus              = pMinus
            protocol Mult               = pTimes
            protocol Pow                = pNumber
            protocol Div                = pDiv
            protocol Mod                = pIntegral
            protocol EuDiv              = pIntegral
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
            rtype ShiftL t              = tInt
            rtype ShiftR t              = tInt
            rtype _ t                   = t
    infer env (UnOp l op e)
      | op == Not                       = do (cs,_,_,e') <- inferBool env e
                                             return (cs, tBool, UnOp l op e')
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl w t (protocol op) :
                                                     cs, t, eCall (eDot (eVar w) (method op)) [e'])
      where protocol UPlus              = pNumber
            protocol UMinus             = pNumber
            protocol BNot               = pIntegral
            method UPlus                = posKW
            method UMinus               = negKW
            method BNot                 = invertKW
    infer env (CompOp l e1 [OpArg op e2])
      | op `elem` [In,NotIn]            = do t1 <- newTVar
                                             (cs1,e1') <- inferSub env t1 e1
                                             t2 <- newTVar
                                             (cs2,e2') <- inferSub env t2 e2
                                             w <- newWitness
                                             return (Impl w t2 (pContainer t1) :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w) (method op)) [e2', e1'])
      | otherwise                       = do t <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t e2
                                             w <- newWitness
                                             return (Impl w t (protocol op) :
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

{-
          extension ndarray[float] (Number,Times,Plus,Minus):
              __neg__ : () -> ndarray[float]

          extension ndarray[int] (Number,Times,Plus,Minus):
              __neg__ : () -> ndarray[int]

          extension ndarray[float] (Real,Number,Times,Plus,Minus):
              __round__ : (?int) -> ndarray[float]

          extension ndarray[int] (Real,Number,Times,Plus,Minus):
              __round__ : (?int) -> mdarray[int]

          extension ndarray[A] (Sliceable[ndarray[A]], Indexed[int,ndarray[A]])):
              __getslice__ : (slice) -> ndarray[A]


-}

    infer env (Dot l x@(Var _ c) n)
      | NClass q us te <- cinfo         = do (cs0,ts) <- instQBinds env q
                                             let tc = TC c ts
                                             case findAttr env tc n of
                                                Just (_,sc,dec)
                                                  | dec == Just Property -> err l "Property attribute not selectable by class"
                                                  | abstractAttr env tc n -> err l "Abstract attribute not selectable by class"
                                                  | otherwise -> do
                                                      (cs1,tvs,t) <- instantiate env sc
                                                      let t' = subst [(tvSelf,tCon tc)] $ addSelf t dec
                                                      return (cs0++cs1, t', app2nd dec t' (tApp (Dot l x n) (ts++tvs)) $ witsOf (cs0++cs1))
                                                Nothing ->
                                                    case findWitness env c (hasAttr env n) of
                                                        Just wit -> do
                                                            (cs1,p,we) <- instWitness env ts wit
                                                            let Just (wf,sc,dec) = findAttr env p n
                                                            (cs2,tvs,t) <- instantiate env sc
                                                            let t' = subst [(tvSelf,tCon tc)] $ addSelf t dec
                                                            return (cs1++cs2, t', app t' (tApp (eDot (wf we) n) tvs) $ witsOf cs2)
                                                        Nothing -> err1 l "Attribute not found"
      | NProto q us te <- cinfo         = do (_,ts) <- instQBinds env q
                                             let tc = TC c ts
                                             case findAttr env tc n of
                                                Just (wf,sc,dec) -> do
                                                    (cs1,tvs,t) <- instantiate env sc
                                                    t0 <- newTVar
                                                    let t' = subst [(tvSelf,t0)] $ addSelf t dec
                                                    w <- newWitness
                                                    return (Impl w t0 tc :
                                                            cs1, t', app t' (tApp (Dot l (wf $ eVar w) n) tvs) $ witsOf cs1)
                                                Nothing -> err1 l "Attribute not found"
      where cinfo                       = findQName c env

    infer env (Dot l e n)
      | n == initKW                     = err1 n "__init__ cannot be selected by instance"
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             t0 <- newTVar
                                             return (Sel w t n t0 :
                                                     cs, t0, eCall (eVar w) [e'])

    infer env (Rest l e n)              = do p <- newTVarOfKind PRow
                                             k <- newTVarOfKind KRow
                                             t0 <- newTVar
                                             (cs,e') <- inferSub env (tTuple p (kwdRow n t0 k)) e
                                             return (cs, tTuple p k, Rest l e' n)

    infer env (DotI l e i)              = do (tup,ti,rest) <- tupleTemplate i
                                             (cs,e') <- inferSub env tup e
                                             return (cs, ti, DotI l e' i)

    infer env (RestI l e i)             = do (tup,ti,rest) <- tupleTemplate i
                                             (cs,e') <- inferSub env tup e
                                             return (cs, rest, RestI l e' i)
    infer env (Lambda l p k e fx)
      | nodup (p,k)                     = do pushFX fx tNone
                                             (cs0,te0,p') <- infEnv env1 p
                                             (cs1,te1,k') <- infEnv (define te0 env1) k
                                             (cs2,t,e') <- infer (define te1 (define te0 env1)) e
                                             popFX
                                             return (cs0++cs1++cs2, tFun fx (prowOf p') (krowOf k') t, Lambda l (noDefaultsP p') (noDefaultsK k') e' fx)
                                                     -- TODO: replace defaulted params with Conds
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs kargs)     = do (cs1,prow,pargs') <- infer env pargs
                                             (cs2,krow,kargs') <- infer env kargs
                                             return (cs1++cs2, TTuple NoLoc prow krow, Tuple l pargs' kargs')
    infer env (List l es)               = do t0 <- newTVar
                                             (cs,es') <- infElems env es t0
                                             t1 <- newTVar
                                             w1 <- newWitness
                                             let t2 = tList t0
                                                 w2 = eQVar primWCollectionList
                                             return (Impl w1 t1 (pSequence t0) :
                                                     cs, t1, eCall (tApp (eDot (eDot (eVar w1) (witAttr qnCollection)) fromiterKW) [t2]) [w2, List l es'])
    infer env (ListComp l e1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e1] t0
                                             let [e1'] = es
                                             t1 <- newTVar
                                             w1 <- newWitness
                                             let t2 = tList t0
                                                 w2 = eQVar primWCollectionList
                                             return (Impl w1 t1 (pSequence t0) :
                                                     cs1++cs2, t1, eCall (tApp (eDot (eDot (eVar w1) (witAttr qnCollection)) fromiterKW) [t2]) [w2, ListComp l e1' co'])
    infer env (Set l es)                = do t0 <- newTVar
                                             (cs,es')  <- infElems env es t0
                                             t1 <- newTVar
                                             w1 <- newWitness
                                             let t2 = tList t0
                                                 w2 = eQVar primWCollectionList
                                             return (Impl w1 t1 (pSet t0) :
                                                     cs, t1, eCall (tApp (eDot (eVar w1) fromiterKW) [t2]) [w2, List l es'])
    infer env (SetComp l e1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e1] t0
                                             let [e1'] = es
                                             t1 <- newTVar
                                             w1 <- newWitness
                                             let t2 = tList t0
                                                 w2 = eQVar primWCollectionList
                                             return (Impl w1 t1 (pSet t0) :
                                                     cs1++cs2, t1, eCall (tApp (eDot (eVar w1) fromiterKW) [t2]) [w2, ListComp l e1' co'])
                                             
    infer env (Dict l as)               = do tk <- newTVar
                                             tv <- newTVar
                                             (cs,as') <- infAssocs env as tk tv
                                             t1 <- newTVar
                                             w1 <- newWitness
                                             let t2 = tList (tTupleP (posRow tk $ posRow tv posNil))
                                                 w2 = eQVar primWCollectionList
                                             return (Impl w1 t1 (pMapping tk tv) :
                                                     cs, t1, eCall (tApp (eDot (eVar w1) fromiterKW) [t2]) [w2, List l as'])
    infer env (DictComp l a1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             tk <- newTVar
                                             tv <- newTVar
                                             (cs2,as) <- infAssocs (define te env) [a1] tk tv
                                             let [a1'] = as
                                             t1 <- newTVar
                                             w1 <- newWitness
                                             let t2 = tList (tTupleP (posRow tk $ posRow tv posNil))
                                                 w2 = eQVar primWCollectionList
                                             return (Impl w1 t1 (pMapping tk tv) :
                                                     cs1++cs2, t1, eCall (tApp (eDot (eVar w1) fromiterKW) [t2]) [w2, ListComp l a1' co'])
    infer env (Paren l e)               = do (cs,t,e') <- infer env e
                                             return (cs, t, Paren l e')



tupleTemplate i                         = do ts <- mapM (const newTVar) [0..i]
                                             p <- newTVarOfKind PRow
                                             k <- newTVarOfKind KRow
                                             let p0 = foldl (flip posRow) p ts
                                                 p1 = foldl (flip posRow) p (tail ts)
                                             return (TTuple NoLoc p0 k, head ts, TTuple NoLoc p1 k)

isModule env e                          = fmap ModName $ mfilter (isMod env) $ fmap reverse $ dotChain e
  where dotChain (Var _ (NoQ n))        = Just [n]
        dotChain (Dot _ e n)            = fmap (n:) (dotChain e)
        dotChain _                      = Nothing


infElems env [] t0                      = return ([], [])
infElems env (Elem e : es) t0           = do (cs1,e') <- inferSub env t0 e
                                             (cs2,es') <- infElems env es t0
                                             return (cs1++cs2, Elem e' : es')           -- TODO: translate using primitive Iterator
infElems env (Star e : es) t0           = do t1 <- newTVar
                                             (cs1,e') <- inferSub env t1 e
                                             (cs2,es') <- infElems env es t0
                                             w <- newWitness
                                             return (Impl w t1 (pIterable t0) :
                                                     cs1++cs2, Star e' : es')           -- TODO: translate using primitive Iterator
                                                     

infAssocs env [] tk tv                  = return ([], [])
infAssocs env (Assoc k v : as) tk tv    = do (cs1,k') <- inferSub env tk k
                                             (cs2,v') <- inferSub env tv v
                                             (cs3,as') <- infAssocs env as tv tk
                                             return (cs1++cs2++cs3, Elem (eTuple [k',v']) : as')    -- TODO: translate using primitive Iterator
infAssocs env (StarStar e : as) tk tv   = do t1 <- newTVar
                                             (cs1,e') <- inferSub env t1 e
                                             (cs2,as') <- infAssocs env as tk tv
                                             w <- newWitness
                                             return (Impl w t1 (pIterable $ tTupleP $ posRow tk $ posRow tv posNil) :
                                                     cs1++cs2, Star e' : as')                       -- TODO: translate using primitive Iterator


inferBool env (BinOp l e1 And e2)       = do (cs1,env1,s1,e1') <- inferBool env e1
                                             (cs2,env2,s2,e2') <- inferBool env1 e2
                                             return (cs1++cs2, env2, s1++s2, BinOp l e1' And (termsubst s1 e2'))
inferBool env (BinOp l e1 Or e2)        = do (cs1,_,_,e1') <- inferBool env e1
                                             (cs2,_,_,e2') <- inferBool env e2
                                             return (cs1++cs2, env, [], BinOp l e1' Or e2')
inferBool env (UnOp l Not e)            = do (cs,_,_,e') <- inferBool env e
                                             return (cs, env, [], UnOp l Not e')
inferBool env (CompOp l e1 [OpArg op e2])
  | Just n <- noneTest e1 op e2         = do t <- newTVar
                                             (cs1,e') <- inferSub env (tOpt t) (eVar n)
                                             return (cs1, define [(n,NVar t)] env, sCast n (tOpt t) t, eCall (tApp (eQVar primISNOTNONE) [t]) [e'])
inferBool env (IsInstance l e@(Var _ (NoQ n)) c)
                                        = case findQName c env of
                                             NClass q _ _ -> do
                                                (cs,t,e') <- infer env e
                                                ts <- newTVars [ tvkind v | v <- qbound q ]
                                                let tc = tCon (TC (unalias env c) ts)
                                                return (Cast tc t :
                                                        cs, define [(n,NVar tc)] env, sCast n t tc, IsInstance l e' c)
                                             _ -> nameUnexpected c
inferBool env e                         = do (cs,t,e') <- infer env e
                                             return (cs, env, [], eCall (eDot e' boolKW) [])

noneTest (Var _ (NoQ n)) IsNot None{}   = Just n
noneTest (Var _ (NoQ n)) NEq None{}     = Just n
noneTest None{} IsNot (Var _ (NoQ n))   = Just n
noneTest None{} NEq (Var _ (NoQ n))     = Just n
noneTest e op e'                        = Nothing

sCast n t t'                            = [(n, eCall (tApp (eQVar primCAST) [t,t']) [eVar n])]

inferSlice env (Sliz l e1 e2 e3)        = do (cs1,e1') <- inferSub env tInt e1
                                             (cs2,e2') <- inferSub env tInt e2
                                             (cs3,e3') <- inferSub env tInt e3
                                             return (cs1++cs2++cs3, Sliz l e1' e2' e3')

inferNDSlice env (NDExpr e)             = do (cs, e') <- inferSub env tInt e
                                             return (cs, eCall (eQVar qnNDIndex) [e'])
inferNDSlice env (NDSliz sl)            = do (cs, sl') <- inferSlice env sl
                                             return (cs, eCall (eQVar qnNDSlice) [sliz2exp sl'])


class InferSub a where
    inferSub                            :: Env -> Type -> a -> TypeM (Constraints,a)
    
instance InferSub Expr where
    inferSub env t e                    = do (cs,t',e') <- infer env e
                                             w <- newWitness
                                             return (Sub w t' t : cs, eCall (eVar w) [e'])

instance InferSub (Maybe Expr) where
    inferSub env t Nothing              = return ([], Nothing)
    inferSub env t (Just e)             = do (cs,e') <- inferSub env t e
                                             return (cs, Just e')


instance (Infer a) => Infer (Maybe a) where
    infer env Nothing                   = do t <- newTVar
                                             return ([], t, Nothing)
    infer env (Just x)                  = do (cs,t,e') <- infer env x
                                             return (cs, t, Just e')

instance InfEnv PosPar where
    infEnv env (PosPar n a Nothing p)   = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             (cs,te,p') <- infEnv (define [(n, NVar t)] env) p
                                             return (cs, (n, NVar t):te, PosPar n (Just t) Nothing p')
    infEnv env (PosPar n a (Just e) p)  = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,p') <- infEnv (define [(n, NVar t)] env) p
                                             return (cs1++cs2, (n, NVar t):te, PosPar n (Just t) (Just e') p')
    infEnv env (PosSTAR n a)            = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             r <- newTVarOfKind PRow
                                             return ([Cast t (tTupleP r)], [(n, NVar t)], PosSTAR n (Just $ tTupleP r))
    infEnv env PosNIL                   = return ([], [], PosNIL)

instance InfEnv KwdPar where
    infEnv env (KwdPar n a Nothing k)   = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             (cs,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (cs, (n, NVar t):te, KwdPar n (Just t) Nothing k')
    infEnv env (KwdPar n a (Just e) k)  = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (cs1++cs2, (n, NVar t):te, KwdPar n (Just t) (Just e') k')
    infEnv env (KwdSTAR n a)            = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             r <- newTVarOfKind KRow
                                             return ([Cast t (tTupleK r)], [(n, NVar t)], KwdSTAR n (Just $ tTupleK r))
    infEnv env KwdNIL                   = return ([], [], KwdNIL)

---------

instance Infer PosArg where
    infer env (PosArg e p)              = do (cs1,t,e') <- infer env e
                                             (cs2,prow,p') <- infer env p
                                             return (cs1++cs2, posRow t prow, PosArg e' p')
    infer env (PosStar e)               = do (cs,t,e') <- infer env e
                                             prow <- newTVarOfKind PRow
                                             krow <- newTVarOfKind KRow
                                             return (Cast t (tTuple prow krow) :
                                                     cs, prow, PosStar e')
    infer env PosNil                    = return ([], posNil, PosNil)
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do (cs1,t,e') <- infer env e
                                             (cs2,krow,k') <- infer env k
                                             return (cs1++cs2, kwdRow n t krow, KwdArg n e' k')
    infer env (KwdStar e)               = do (cs,t,e') <- infer env e
                                             prow <- newTVarOfKind PRow
                                             krow <- newTVarOfKind KRow
                                             return (Cast t (tTuple prow krow) :
                                                     cs, krow, KwdStar e')
    infer env KwdNil                    = return ([], kwdNil, KwdNil)

instance InfEnv Comp where
    infEnv env NoComp                   = return ([], [], NoComp)
    infEnv env (CompIf l e c)           = do (cs1,env',s,e') <- inferBool env e
                                             (cs2,te,c') <- infEnv env' c
                                             return (cs1++cs2, te, CompIf l e' (termsubst s c'))
    infEnv env (CompFor l p e c)        = do (cs1,te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             t2 <- newTVar
                                             (cs2,e') <- inferSub env t2 e
                                             (cs3,te2,c') <- infEnv (define te1 env) c
                                             w <- newWitness
                                             return (Impl w t2 (pIterable t1) :
                                                     cs1++cs2++cs3, te1++te2, CompFor l p' (eCall (eDot (eVar w) iterKW) [e']) c')

instance Infer Exception where
    infer env (Exception e1 Nothing)    = do (cs,t1,e1') <- infer env e1
                                             return (Cast t1 tException :
                                                     cs, t1, Exception e1' Nothing)
    infer env (Exception e1 (Just e2))  = do (cs1,t1,e1') <- infer env e1
                                             (cs2,t2,e2') <- infer env e2
                                             return (Cast t1 tException : 
                                                     Cast t2 tException : 
                                                     cs1++cs2, t1, Exception e1' (Just e2'))

instance InfEnvT PosPat where
    infEnvT env (PosPat p ps)           = do (cs1,te1,t,p') <- infEnvT env p
                                             (cs2,te2,r,ps') <- infEnvT env ps
                                             return (cs1++cs2, te1++te2, posRow t r, PosPat p' ps')
    infEnvT env (PosPatStar p)          = do (cs,te,t,p') <- infEnvT env p
                                             r <- newTVarOfKind PRow
                                             return (Cast t (tTupleP r) :
                                                     cs, te, r, PosPatStar p')
    infEnvT env PosPatNil               = return ([], [], posNil, PosPatNil)


instance InfEnvT KwdPat where
    infEnvT env (KwdPat n p ps)         = do (cs1,te1,t,p') <- infEnvT env p
                                             (cs2,te2,r,ps') <- infEnvT env ps
                                             return (cs1++cs2, te1++te2, kwdRow n t r, KwdPat n p' ps')
    infEnvT env (KwdPatStar p)          = do (cs,te,t,p') <- infEnvT env p
                                             r <- newTVarOfKind KRow
                                             return (Cast t (tTupleK r) :
                                                     cs, te, r, KwdPatStar p')
    infEnvT env KwdPatNil               = return ([], [], kwdNil, KwdPatNil)



instance InfEnvT Pattern where
    infEnvT env (PVar l n a)            = do t <- maybe newTVar return (unalias env a)
                                             wellformed env t
                                             case findName n env of
                                                 NReserved -> do
                                                     --traceM ("## infEnvT " ++ prstr n ++ " : " ++ prstr t)
                                                     return ([], [(n, NVar t)], t, PVar l n (Just t))
                                                 NSig (TSchema _ [] TFun{}) _ -> notYet l "Assignment to variable with function signature"
                                                 NSig (TSchema _ [] t') _ -> do
                                                     --traceM ("## infEnvT (sig) " ++ prstr n ++ " : " ++ prstr t ++ " < " ++ prstr t')
                                                     return ([Cast t t'], [(n, NVar t')], t, PVar l n (Just t))
                                                 NVar t' ->
                                                     return ([Cast t t'], [], t, PVar l n Nothing)
                                                 NSVar t' -> do
                                                     fx <- currFX
                                                     return (Cast fxAction fx :
                                                             Cast t t' : 
                                                             [], [], t, PVar l n Nothing)
                                                 _ -> 
                                                     err1 n "Variable not assignable:"
    infEnvT env (PTuple l ps ks)        = do (cs1,te1,prow,ps') <- infEnvT env ps
                                             (cs2,te2,krow,ks') <- infEnvT env ks
                                             return (cs1++cs2, te1++te2, TTuple NoLoc prow krow, PTuple l ps' ks')
    infEnvT env (PList l ps p)          = do (cs1,te1,t1,ps') <- infEnvT env ps
                                             (cs2,te2,t2,p') <- infEnvT (define te1 env) p
                                             w <- newWitness
                                             return (Impl w t2 (pSequence t1) :
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
    infEnvT env [p]                     = do (cs1,te1,t1,p') <- infEnvT env p
                                             return (cs1,te1,t1,[p'])
    infEnvT env (p:ps)                  = do (cs1,te1,t1,p') <- infEnvT env p
                                             (cs2,te2,t2,ps') <- infEnvT env ps
                                             unify env t1 t2
                                             return (cs1++cs2, te1++te2, t1, p':ps')


