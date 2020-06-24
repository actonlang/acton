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
import Acton.Transform
import qualified InterfaceFiles
import qualified Data.Map

reconstruct                             :: String -> Env -> Module -> IO (TEnv, Module)
reconstruct fname env (Module m imp ss) = do InterfaceFiles.writeFile (fname ++ ".ty") (unalias env2 te)
                                             return (te, Module m imp ss1)
  where env1                            = reserve (bound ss) env
        (te,ss1)                        = runTypeM $ infTop env1 ss
        env2                            = define te env1

solverError                             = typeError


nodup x
  | not $ null vs                       = err2 vs "Duplicate names:"
  | otherwise                           = True
  where vs                              = duplicates (bound x)


--------------------------------------------------------------------------------------------------------------------------

infTop                                  :: Env -> Suite -> TypeM (TEnv,Suite)
infTop env ss                           = do traceM ("\n## infEnv top")
                                             pushFX fxPure tNone
                                             (cs,te,ss1) <- (if inBuiltin env then infEnv else infSuiteEnv) env ss
                                             popFX
                                             eq <- solve env cs
                                             te1 <- msubst te
                                             return (te1, bindWits eq ++ ss1)

class Infer a where
    infer                               :: Env -> a -> TypeM (Constraints,Type,a)

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM (Constraints,TEnv,a)

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (Constraints,TEnv,Type,a)


--------------------------------------------------------------------------------------------------------------------------

commonTEnv                              :: Env -> [TEnv] -> TypeM (Constraints,TEnv)
commonTEnv env []                       = return ([], [])
commonTEnv env (te:tes)                 = unifEnv tes (restrict vs te)
  where vs                              = foldr intersect (dom te) $ map dom tes
        l                               = length tes
        unifEnv tes []                  = return ([], [])
        unifEnv tes ((n,i):te)          = do t <- newTVar
                                             let (cs1,i') = unif n t i
                                             (cs2,te') <- unifEnv tes te
                                             return (cs1++cs2, (n,i'):te')
        unif n t0 (NVar t)
          | length ts == l              = ([ Cast t t0 | t <- ts ], NVar t0)
          where ts                      = [ t | te <- tes, Just (NVar t) <- [lookup n te] ]
        unif n t0 (NSVar t)
          | length ts == l              = ([ Cast t t0 | t <- ts ], NSVar t0)
          where ts                      = [ t | te <- tes, Just (NSVar t) <- [lookup n te] ]
        unif n t0 (NDef sc d)
          | null (scbind sc) &&
            length ts == l              = ([ Cast t t0 | t <- ts ], NDef (monotype t0) d)
          where ts                      = [ sctype sc | te <- tes, Just (NDef sc d') <- [lookup n te], null (scbind sc), d==d' ]
        unif n t0 (NDef _ _)
          | length scs == l             = case findName n env of
                                             NReserved -> err1 n "Expected a common signature for"
                                             NSig sc d -> ([], NDef sc d)
          where scs                     = [ sc | te <- tes, Just (NDef sc d) <- [lookup n te] ]
        unif n _ _                      = err1 n "Inconsistent bindings for"

    

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
                                             let te1' = if inDecl env then noDefs te1 else te1  -- TODO: stop class instantiation!
                                             (cs2,te2,ss2) <- infEnv (define te1' env) ss
                                             return (cs1++cs2, te1++te2, s1:ss2)

instance InfEnv Stmt where
    infEnv env (Expr l e)               = do (cs,_,e') <- infer env e
                                             return (cs, [], Expr l e')
    infEnv env (Assign l pats e)
      | nodup pats                      = do (cs1,te,t,pats') <- infEnvT env pats
                                             (cs2,e') <- inferSub env t e
                                             return (cs1++cs2, te, Assign l pats' e')
    infEnv env (MutAssign l tg e)       = do (cs1,t0,tg') <- infTarget env tg
                                             (cs2,e') <- inferSub env (bound tg' t0) e
                                             return (cs1++cs2, [], assign tg' e')
      where bound Slice{} t             = tExist $ pIterable t
            bound _ t                   = t
            assign (Index _ e ix) rhs   = Expr l $ eCall (eDot (eDot e protoKW) setitemKW) [eDot e implKW, ix, rhs]
            assign (Slice _ e [sl]) rhs = Expr l $ eCall (eDot (eDot e protoKW) setsliceKW) (eDot e implKW : sliz2args sl ++ [rhs])
            assign tg rhs               = MutAssign l tg rhs
    infEnv env (AugAssign l tg (Op _ o) e)
                                        = do (cs1,t,lval) <- infTarget env tg
                                             (cs2,rval) <- inferSub env t tg
                                             (cs3,e') <- inferSub env t e
                                             w <- newWitness
                                             return (Impl w t (protocol o) : 
                                                     cs1++cs2++cs3, [], assign lval $ eCall (eDot (eVar w) (method o)) [rval,e'])
      where protocol PlusA              = pPlus
            protocol MinusA             = pMinus
            protocol MultA              = pComplex
            protocol PowA               = pComplex
            protocol DivA               = pComplex
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
            assign (Var l (NoQ n)) rhs  = Assign l [PVar NoLoc n Nothing] rhs
            assign (Index l e ix) rhs   = Expr l $ eCall (eDot (eDot e protoKW) setitemKW) [eDot e implKW, ix, rhs]
            assign (Slice l e [sl]) rhs = Expr l $ eCall (eDot (eDot e protoKW) setsliceKW) (eDot e implKW : sliz2args sl ++ [rhs])
            assign tg rhs               = MutAssign l tg rhs
    infEnv env (Assert l e1 e2)         = do (cs1,e1') <- inferBool env e1
                                             (cs2,e2') <- inferSub env tStr e2
                                             return (cs1++cs2, [], Assert l e1' e2')
    infEnv env s@(Pass l)               = return ([], [], s)
    infEnv env (Delete l tg)            = do (cs,t,tg') <- infTarget env tg
                                             return (constr tg' t ++ cs, [], delete tg')
      where constr Var{} t              = [Cast tNone t]
            constr Dot{} t              = [Cast tNone t]
            constr _ t                  = []
            delete (Var _ (NoQ n))      = Assign l [PVar NoLoc n Nothing] eNone
            delete (Index _ e ix)       = Expr l $ eCall (eDot (eDot e protoKW) delitemKW) [eDot e implKW, ix]
            delete (Slice _ e [sl])     = Expr l $ eCall (eDot (eDot e protoKW) delsliceKW) (eDot e implKW : sliz2args sl)
            delete tg                   = MutAssign l tg eNone
            
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
    infEnv env (While l e b els)        = do (cs1,e') <- inferBool env e
                                             (cs2,te1,b') <- infSuiteEnv env b
                                             (cs3,te2,els') <- infSuiteEnv env els
                                             return (cs1++cs2++cs3, [], While l e' b' els')
    infEnv env (For l p e b els)
      | nodup p                         = do (cs1,te,t1,p') <- infEnvT env p
                                             (cs2,e') <- inferSub env (tExist $ pIterable t1) e
                                             (cs3,te1,b') <- infSuiteEnv (define te env) b
                                             (cs4,te2,els') <- infSuiteEnv env els
                                             return (cs1++cs2++cs3++cs4, [], For l p' (eCall (eDot (eDot e' protoKW) iterKW) [eDot e' implKW]) b' els')
    infEnv env (Try l b hs els fin)     = do (cs1,te,b') <- infLiveEnv env b
                                             (cs2,te',els') <- infLiveEnv (maybe id define te $ env) els
                                             (css,tes,hs') <- fmap unzip3 $ mapM (infLiveEnv env) hs
                                             (cs3,te1) <- commonTEnv env $ catMaybes $ (liveCombine te te'):tes
                                             (cs4,te2,fin') <- infSuiteEnv (define te1 env) fin
                                             return (cs1++cs2++cs3++cs4++concat css, te1++te2, Try l b' hs' els' fin')
    infEnv env (With l items b)
      | nodup items                     = do (cs1,te,items') <- infEnv env items
                                             (cs2,te1,b') <- infSuiteEnv (define te env) b
                                             return $ (cs1++cs2, exclude (dom te) te1, With l items' b')

    infEnv env (VarAssign l pats e)
      | nodup pats                      = do (cs1,te,t,pats') <- infEnvT env pats
                                             (cs2,e') <- inferSub env t e
                                             return (cs1++cs2, [ (n,NSVar t) | (n,NVar t) <- te], VarAssign l pats' e')
    
    infEnv env (After l e1 e2)          = do (cs1,e1') <- inferSub env tInt e1
                                             (cs2,t,e2') <- infer env e2
                                             return (cs1++cs2, [], After l e1' e2')
    
    infEnv env d@(Signature _ ns sc@(TSchema _ q t) dec)
      | not $ null redefs               = illegalRedef (head redefs)
      | otherwise                       = return ([], [(n, NSig sc dec) | n <- ns], d)
      where redefs                      = [ n | n <- ns, findName n env /= NReserved ]

    infEnv env (Data l _ _)             = notYet l "data syntax"

    infEnv env (Decl l ds)
      | inDecl env && nodup ds          = do (cs1,te1,ds1) <- infEnv env ds
                                             return (cs1, te1, Decl l ds1)
      | nodup ds                        = do (cs1,te1,ds1) <- infEnv (setInDecl True env) ds
                                             (cs2,ds2) <- checkEnv (define te1 env) False ds1
                                             (cs3,te2,ds3) <- genEnv env (cs1++cs2) te1 ds2
                                             return (cs3, te2, Decl l ds3)


infTarget env (Var l (NoQ n))           = case findName n env of
                                             NReserved ->
                                                 err1 n "Variable not yet assigned"
                                             NSig (TSchema _ [] t') _ ->
                                                 err1 n "Variable not yet assigned"
                                             NVar t ->
                                                 return ([], t, Var l (NoQ n))
                                             NSVar t ->
                                                 return ([], t, Var l (NoQ n))
                                             _ -> 
                                                 err1 n "Variable not assignable:"
infTarget env (Index l e ix)            = do ti <- newTVar
                                             (cs1,ix') <- inferSub env ti ix
                                             t <- newTVar
                                             (cs2,e') <- inferSub env (tExist $ pIndexed ti t) e
                                             return (cs1++cs2, t, Index l e' ix')
infTarget env (Slice l e [sl])          = do (cs1,sl') <- inferSlice env sl
                                             t <- newTVar
                                             (cs2,e') <- inferSub env (tExist $ pSliceable t) e
                                             return (cs1++cs2, t, Slice l e' [sl'])
infTarget env (Slice l e slz)           = notYet l "Multidimensional slicing"
infTarget env (Dot l e n)               = do (cs,t1,e') <- infer env e
                                             t2 <- newTVar
                                             fx <- currFX
                                             st <- newTVar
                                             return (Mut t1 n t2 :
                                                     Cast (fxMut st) fx :
                                                     cs, t2, Dot l e' n)

sliz2args (Sliz _ e1 e2 e3)             = map (maybe eNone id) [e1,e2,e3]

--------------------------------------------------------------------------------------------------------------------------

bindWits eqs                            = [ Assign l0 [PVar l0 n (Just t)] e | (n,t,e) <- eqs ]


matchingDec n sc dec NoDec              = True
matchingDec n sc dec dec'
  | dec == dec'                         = True
  | otherwise                           = decorationMismatch n sc dec


infActorEnv env ss                      = do dsigs <- mapM mkDSig (dvars ss \\ dom sigs)
                                             bsigs <- mapM mkBSig (pvars ss \\ dom (sigs++dsigs))
                                             return (sigs ++ dsigs ++ bsigs)
  where sigs                            = [ (n, NSig sc' dec) | Signature _ ns sc dec <- ss, let sc' = async sc, n <- ns ]
        async (TSchema l q (TFun l' fx p k t))
          | canAsync q fx               = TSchema l q (TFun l' fxAsync p k t)
        async sc                        = sc
        canAsync q (TVar _ tv)          = tv `notElem` tybound q
        canAsync q (TFX _ (FXAct _))    = True
        canAsync q _                    = False
        mkDSig n                        = do p <- newTVarOfKind PRow
                                             k <- newTVarOfKind KRow
                                             t <- newTVar
                                             return (n, NSig (monotype $ tFun fxAsync p k t) NoDec)
        mkBSig n                        = do t <- newTVar
                                             return (n, NSig (monotype t) NoDec)
        dvars ss                        = nub $ concat $ map dvs ss
          where dvs (Decl _ ds)         = [ dname d | d@Def{} <- ds ]
                dvs (If _ bs els)       = foldr intersect (dvars els) [ dvars ss | Branch _ ss <- bs ]
                dvs _                   = []
        pvars ss                        = nub $ concat $ map pvs ss
          where pvs (Assign _ pats _)   = bound pats
                pvs (If _ bs els)       = foldr intersect (pvars els) [ pvars ss | Branch _ ss <- bs ]
                pvs _                   = []

--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Decl where
    infEnv env d@(Def _ n q p k a _ _ _)
      | nodup (p,k)                     = case findName n env of
                                             NSig sc dec | matchingDec n sc dec (deco d) ->
                                                 return ([], [(n, NDef sc dec)], d)
                                             NReserved -> do
                                                 t <- newTVar
                                                 traceM ("\n## infEnv def " ++ prstr (n, NDef (monotype t) (deco d)))
                                                 return ([], [(n, NDef (monotype t) (deco d))], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env d@(Actor _ n q p k b)
      | nodup (p,k)                     = case findName n env of
                                             NReserved -> do
                                                 te <- infActorEnv env b
                                                 prow <- newTVarOfKind PRow
                                                 krow <- newTVarOfKind KRow
                                                 traceM ("\n## infEnv actor " ++ prstr (n, NAct q prow krow te))
                                                 return ([], [(n, NAct q prow krow te)], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env (Class l n q us b)
      | not $ null ps                   = notYet (loc n) "Classes with direct extensions"
      | otherwise                       = case findName n env of
                                             NReserved -> do
                                                 traceM ("\n## infEnv class " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 (cs,te,b') <- infEnv env1 b
                                                 popFX
                                                 (cs1,eq1) <- splitAndSolve env1 (tybound q) te cs         --TODO: add eq1...
                                                 checkNoEscape env (tybound q)
                                                 (nterms,_,_) <- checkAttributes [] te' te
                                                 return (cs1, [(n, NClass q as (map newSig nterms ++ te))], Class l n q us b')
                                             _ -> illegalRedef n
      where env1                        = define (nSigs te') $ reserve (bound b) $ defineSelfOpaque $ defineTVars (stripQual q) env
            (as,ps)                     = mro2 env us
            te'                         = parentTEnv env as
            
            newSig (n, NDef sc dec)     = (n, NSig sc dec)
            newSig (n, NVar t)          = (n, NSig (monotype t) Static)
            newSig (n, i)               = (n,i)

    infEnv env (Protocol l n q us b)    = case findName n env of
                                             NReserved -> do
                                                 traceM ("\n## infEnv protocol " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 (cs,te,b') <- infEnv env1 b
                                                 popFX
                                                 (cs1,eq1) <- splitAndSolve env1 (tybound q) te cs         --TODO: add eq1...
                                                 checkNoEscape env (tybound q)
                                                 (nterms,_,sigs) <- checkAttributes [] te' te
                                                 when (not $ null nterms) $ err2 (dom nterms) "Method/attribute lacks signature"
                                                 when (initKW `elem` sigs) $ err2 (filter (==initKW) sigs) "A protocol cannot define __init__"
                                                 return (cs1, [(n, NProto q ps te)], Protocol l n q us b')
                                             _ -> illegalRedef n
      where env1                        = define (nSigs te') $ reserve (bound b) $ defineSelfOpaque $ defineTVars (stripQual q) env
            ps                          = mro1 env us
            te'                         = parentTEnv env ps

    infEnv env (Extension l n q us b)
      | isActor n env                   = notYet (loc n) "Extension of an actor"
      | isProto n env                   = notYet (loc n) "Extension of a protocol"
--      | length us > 1                   = notYet (loc n) "Extensions with multiple protocols"
      | otherwise                       = do traceM ("\n## infEnv extension " ++ prstr n)
                                             pushFX fxPure tNone
                                             (cs,te,b') <- infEnv env1 b
                                             popFX
                                             (cs1,eq1) <- splitAndSolve env1 (tybound q) te cs             --TODO: add eq1...
                                             checkNoEscape env (tybound q)
                                             (nterms,asigs,sigs) <- checkAttributes final te' te
                                             when (not $ null nterms) $ err2 (dom nterms) "Method/attribute not in listed protocols"
                                             when (not (null asigs || inBuiltin env)) $ err2 asigs "Protocol method/attribute lacks implementation"
                                             when (not $ null sigs) $ err2 sigs "Extension with new methods/attributes not supported"
                                             return (cs1, [(extensionName n us, NExt n q ps te)], Extension l n q us b')
      where env1                        = define (nSigs te') $ reserve (bound b) $ defineSelfOpaque $ defineTVars (stripQual q) env
            ps                          = mro1 env us
            final                       = concat [ conAttrs env pn | (_, TC pn _) <- ps, hasWitness env n pn ]
            te'                         = parentTEnv env ps
            ts                          = map tVar (tybound q)

--------------------------------------------------------------------------------------------------------------------------

checkAttributes final te' te
  | not $ null osigs                    = err2 osigs "Inherited signatures cannot be overridden"
  | not $ null props                    = err2 props "Property attribute cannot have a class-level definition"
  | not $ null nodef                    = err2 nodef "Methods finalized in a previous extension cannot be overridden"
  | otherwise                           = return (nterms, abssigs, dom sigs)
  where (sigs,terms)                    = sigTerms te
        (sigs',terms')                  = sigTerms te'
        (allsigs,allterms)              = (sigs ++ sigs', terms ++ terms')
        nterms                          = exclude (dom allsigs) terms
        abssigs                         = dom allsigs \\ dom allterms
        osigs                           = dom sigs `intersect` dom sigs'
        props                           = dom terms `intersect` dom (propSigs allsigs)
        nodef                           = dom terms `intersect` final

        -- TODO: add Property sigs according to the 'self' assignments in method __init__ (if present)


extensionName                           :: QName -> [TCon] -> Name
extensionName c ps                      = Derived (deriveQ $ tcname $ head ps) (nstr $ deriveQ c)

stripQual q                             = [ Quant v [] | Quant v us <- q ]

--------------------------------------------------------------------------------------------------------------------------

splitAndSolve env [] te cs              = simplify env te cs
splitAndSolve env vs te cs              = do cs <- msubst cs
                                             (cs0,eq0) <- simplify env te cs
                                             let (cs1',cs1) = partition (any (`elem` vs) . tyfree) cs0
                                             eq1 <- solve env cs1'
                                             return (cs1, eq1++eq0)

checkNoEscape env []                    = return ()
checkNoEscape env vs                    = do tvs <- tyfree <$> msubst env
                                             let escaped = vs `intersect` tvs
                                             when (not $ null escaped) $ 
                                                 err2 escaped "Escaping type variable"

addSelf (TFun l x p k t) NoDec          = TFun l x (posRow tSelf p) k t
addSelf t _                             = t

matchActorAssumption env n0 p k te      = do (cs,eq) <- simplify env te [Cast (prowOf p) p0, Cast (krowOf k) k0]
                                             (css,eqs) <- unzip <$> mapM check1 te
                                             return (cs ++ concat css, eq ++ concat eqs)
  where NAct _ p0 k0 te0                = findName n0 env
        check1 (n, NVar t)              = simplify env te [Cast t t0]
          where NSig (TSchema _ _ t0) _ = fromJust $ lookup n te0
        check1 (n, NDef sc _)
          -- Note: defs in actor bodies arent't generalized, so the only possibility sc can have a nonempty scbind 
          -- is that it was copied from a signature (NSig) for n (see infEnv Def above). And then that signature
          -- has to be the sig resulting from the lookup in te0 below. Otherwise (if scbind sc is empty) then n had
          -- no real signature, and the sig in te0 was synthesized (with an empty q) in infActorEnv (mkDSig) above.
          | scbind sc == q              = do (cs,eq) <- splitAndSolve (defineTVars q env) (tybound q) te [asyncast (sctype sc) t0]
                                             checkNoEscape env (tybound q)      
                                             return (cs,eq)
          | otherwise                   = internal (loc n) "checkActAssump"
          where NSig (TSchema _ q t0) _ = fromJust $ lookup n te0

matchAssumption env cl cs def
  | null q1                             = do traceM ("## matchAssumption 1 ")
                                             let t1 = tFun (dfx def) (prowOf $ pos def) (krowOf $ kwd def) (fromJust $ ann def)
                                             (cs2,eq1) <- splitAndSolve env0 (tybound q0) t1 (Cast t1 (if cl then addSelf t0 dec else t0) : cs)
                                             checkNoEscape env (tybound q0)
                                             return (cs2, def{ qbinds = q0, pos = pos0 def, dbody = bindWits eq1 ++ dbody def })
  | otherwise                           = do traceM ("## matchAssumption 2 ")
                                             (cs1, tvs) <- instQBinds env q1
                                             let s = tybound q1 `zip` tvs           -- This cannot just be memoized in the global TypeM substitution,
                                             def <- msubstWith s def{ qbinds = [] } -- since the variables in (tybound q1) aren't necessarily unique
                                             let t1 = tFun (dfx def) (prowOf $ pos def) (krowOf $ kwd def) (fromJust $ ann def)
                                             (cs2,eq1) <- splitAndSolve env0 (tybound q0) t1 (Cast t1 (if cl then addSelf t0 dec else t0) : cs++cs1)
                                             checkNoEscape env (tybound q0)
                                             return (cs2, def{ qbinds = q0, pos = pos0 def, dbody = bindWits eq1 ++ dbody def })
  where NDef (TSchema _ q0 t0) dec      = findName (dname def) env
        q1                              = qbinds def
        env0                            = defineTVars q0 env
        pos0 def | cl && dec /= Static  = PosPar nSelf t' e' $ qualWPar env q0 pos'
                 | otherwise            = qualWPar env q0 (pos def)
          where PosPar nSelf t' e' pos' = pos def
        

--------------------------------------------------------------------------------------------------------------------------

class Check a where
    checkEnv                            :: Env -> Bool -> a -> TypeM (Constraints,a)

instance (Check a) => Check [a] where
    checkEnv env cl []                  = return ([], [])
    checkEnv env cl (d:ds)              = do (cs1,d') <- checkEnv env cl d
                                             (cs2,ds') <- checkEnv env cl ds
                                             return (cs1++cs2, d':ds')

instance Check Decl where
    checkEnv env cl (Def l n q p k a b dec fx)
                                        = do traceM ("## checkEnv def " ++ prstr n ++ ", q: " ++ prstrs q)
                                             t <- maybe newTVar return a
                                             pushFX fx t
                                             (csp,te0,p') <- infEnv env1 p
                                             (csk,te1,k') <- infEnv (define te0 env1) k
                                             (csb,_,b') <- infSuiteEnv (define te1 (define te0 env1)) b
                                             popFX
                                             let cst = if fallsthru b then [Cast tNone t] else []
                                                 csx = [Cast fxPure fx]
                                                 t1 = tFun fx (prowOf p') (krowOf k') t
                                             (cs1,eq1) <- splitAndSolve env1 (tybound q) t1 (cswf++csp++csk++csb++cst++csx)
                                             checkNoEscape env (tybound q)
                                             -- At this point, n has the type given by its def annotations.
                                             -- Now check that this type is no less general than its recursion assumption in env.
                                             matchAssumption env cl cs1 (Def l n q p' k' (Just t) (bindWits eq1 ++ b') dec fx)
      where cswf                        = wellformed env (q,a)
            env1                        = reserve (bound (p,k) ++ bound b) $ defineTVars q env

    checkEnv env cl (Actor l n q p k b) = do traceM ("## checkEnv actor " ++ prstr n)
                                             st <- newTVar
                                             pushFX (fxAct st) tNone
                                             (csp,te0,p') <- infEnv env1 p
                                             (csk,te1,k') <- infEnv (define te0 env1) k
                                             (csb,te,b') <- infSuiteEnv (define te1 $ define te0 env1) b
                                             (cs0,eq0) <- matchActorAssumption env1 n p' k' te
                                             popFX
                                             (cs1,eq1) <- splitAndSolve env1 (tybound q) te (cswf++csp++csk++csb++cs0)
                                             checkNoEscape env (tybound q)
                                             tvs <- tyfree <$> msubst env
                                             when (tvar st `elem` tvs) $ err1 l "Actor state escapes"
                                             return (cs1, Actor l n q p' k' (bindWits (eq1++eq0) ++ b'))
      where cswf                        = wellformed env q
            env1                        = reserve (bound (p,k) ++ bound b) $ defineTVars q $
                                          define [(selfKW, NVar tRef)] $ reserve (statedefs b) $ 
                                          setInDecl False env
                                          -- Don't look up n and include its NAct tenv here. That tenv shows the external
                                          -- actor view, with async def signatures wherever possible. Instead, let a local
                                          -- env build up sequentially inside the actor so that methods can refer to each 
                                          -- other as normal functions (effectuated by "setInDecl False"). Only later, in 
                                          -- matchActorAssumption, is this local env matched against the external one. But 
                                          -- the async method defs are still implied here, though. The actual async wrappers 
                                          -- around methods of the external interface will be applied in the Deactorizer.

    checkEnv env cl (Class l n q us b)  = do traceM ("## checkEnv class " ++ prstr n)
                                             pushFX fxPure tNone
                                             (csb,b') <- checkEnv env1 True b
                                             popFX
                                             (cs1,eq1) <- splitAndSolve env1 (tybound q) te (wellformed env1 (q,us)++csb)
                                             checkNoEscape env (tybound q)
                                             return (cs1, Class l n q us b')        -- TODO: add wits(q) and eq1 to each def in b'
      where env1                        = define te $ defineSelf (NoQ n) q $ defineTVars q env
            NClass _ _ te               = findName n env

    checkEnv env cl (Protocol l n q us b)
                                        = do traceM ("## checkEnv protocol " ++ prstr n ++ render (brackets (commaSep pretty q)))
                                             pushFX fxPure tNone
                                             (csb,b') <- checkEnv env1 True b
                                             popFX
                                             (cs1,eq1) <- splitAndSolve env1 (tybound q) te (wellformed env1 (q,us)++csb)
                                             checkNoEscape env (tybound q)
                                             return (cs1, Protocol l n q us b')     -- TODO: translate into class, add wits(q) to props, eq1 to __init__
      where env1                        = define te $ defineSelf (NoQ n) q $ defineTVars q env
            NProto _ _ te               = findName n env

    checkEnv env cl (Extension l n q us b)
                                        = do traceM ("## checkEnv extension " ++ prstr n)
                                             pushFX fxPure tNone
                                             (csb,b') <- checkEnv env1 True b
                                             popFX
                                             (cs1,eq1) <- splitAndSolve env1 (tybound q) te (wellformed env1 (q,us)++csb)
                                             checkNoEscape env (tybound q)
                                             return (cs1, Extension l n q us b')   -- TODO: translate into class, add wits(q) to props, eq1 to __init__
      where env1                        = define te $ defineSelf n q $ defineTVars q env
            n'                          = extensionName n us
            NExt _ _ _ te               = findName n' env


instance Check Stmt where
    checkEnv env cl (If l bs els)       = do (cs1,bs') <- checkEnv env cl bs
                                             (cs2,els') <- checkEnv env cl els
                                             return (cs1++cs2, If l bs' els')
    checkEnv env cl (Decl l ds)         = do (cs,ds') <- checkEnv env cl ds
                                             return (cs, Decl l ds')
    checkEnv env cl (Signature l ns sc dec)
                                        = do _ <- solve env1 (wellformed env (q,t))
                                             return ([], Signature l ns sc dec)
      where TSchema _ q t               = sc
            env1                        = defineTVars q env
    checkEnv env cl s                   = return ([], s)

instance Check Branch where
    checkEnv env cl (Branch e b)        = do (cs,b') <- checkEnv env cl b
                                             return (cs, Branch e b')


--------------------------------------------------------------------------------------------------------------------------

splitGen                                :: Env -> [TVar] -> Constraints -> TEnv -> Equations 
                                           -> TypeM ([TVar],[TVar], Constraints,Constraints, TEnv, Equations)
splitGen env fvs cs te eqs
  | not $ null noqual_cs                = do traceM ("  #splitting noqual: " ++ prstrs noqual_cs)
                                             traceM ("             ok    : " ++ prstrs cs2)
                                             eq0 <- solve env noqual_cs
                                             splitAgain (fixed_cs++cs2) (eq0++eqs)
  | not $ null ambig_cs                 = do traceM ("  #splitting ambig: " ++ prstrs ambig_cs)
                                             traceM ("             ok   : " ++ prstrs cs3)
                                             eq0 <- solve env ambig_cs
                                             splitAgain (fixed_cs++cs3) (eq0++eqs)
  | otherwise                           = do eqs <- msubst eqs
                                             return (fvs, gvs, fixed_cs, gen_cs, te, eqs)
  where (fixed_cs, cs1)                 = split_fixed fvs cs
        (noqual_cs, cs2)                = split_noqual cs1
        (ambig_cs, cs3)                 = split_ambig safe_vs cs2

        safe_vs                         = if null def_vss then [] else nub $ foldr1 intersect def_vss
        def_vss                         = [ nub $ tyfree sc \\ fvs | (_, NDef sc _) <- te, null $ scbind sc ]
        gvs                             = nub (foldr union [] def_vss ++ tyfree gen_cs)
        gen_cs                          = cs3
        
        splitAgain cs eqs               = do (cs,eq1) <- simplify env te cs
                                             te <- msubst te
                                             fvs <- tyfree <$> msubst (map tVar fvs)
                                             splitGen env fvs cs te (eq1++eqs)

qualify vs cs                           = let (q,wss) = unzip $ map qbind vs in (q, concat wss)
  where qbind v                         = (Quant v (casts ++ impls), wits)
          where casts                   = [ u | Cast (TVar _ v') (TCon _ u) <- cs, v == v' ]
                impls                   = [ p | Impl w (TVar _ v') p <- cs, v == v' ]
                wits                    = [ (w, impl2type t p) | Impl w t@(TVar _ v') p <- cs, v == v' ]

genEnv                                  :: Env -> Constraints -> TEnv -> [Decl] -> TypeM (Constraints,TEnv,[Decl])
genEnv env cs te ds0                    = do te <- msubst te
                                             traceM ("## genEnv 1\n" ++ render (nest 6 $ pretty te))
                                             (cs0,eq0) <- simplify env te cs
                                             te <- msubst te
                                             fvs <- (tyfixed te ++) . tyfree <$> msubst env
                                             (fvs,gvs, fixed_cs,gen_cs, te, eq1) <- splitGen env fvs cs0 te eq0
                                             traceM ("## genEnv 2 [" ++ prstrs gvs ++ "]\n" ++ render (nest 6 $ pretty te))
                                             ds <- msubst ds0
                                             let (q,ws) = qualify gvs gen_cs
                                                 s = tvarSupplyMap (tybound q) (tvarScope env)
                                                 te1 = map (generalize s q) te
                                                 ds1 = map (abstract q ds ws eq1) ds
                                             sequence [ substitute tv t | (tv,t) <- s ]
                                             traceM ("## genEnv 3 [" ++ prstrs (rng s) ++ "]\n" ++ render (nest 6 $ pretty te1))
                                             return (fixed_cs, te1, ds1)
  where
    tyfixed te                          = tyfree $ filter (not . gen) te
      where gen (n, NDef sc _)          = null $ scbind sc
            gen _                       = False

    generalize s q (n, NDef sc d)
      | null $ scbind sc                = (n, NDef (tSchema (subst s q) (subst s (sctype sc))) d)
    generalize s q (n, i)               = (n, i)

    abstract q ds ws eq d@Def{}
      | null $ qbinds d                 = d{ qbinds = q, pos = wit2par ws (pos d), dbody = bindWits eq ++ wsubst ds ws (dbody d) }
    abstract q ds ws eq d@Actor{}       = d{ dbody = bindWits eq ++ wsubst ds ws (dbody d) }
    abstract q ds ws eq d               = d{ dbody = map bindInDef (wsubst ds ws (dbody d)) }
      where bindInDef (Decl l ds')      = Decl l (map bindInDef' ds')
            bindInDef (If l bs els)     = If l [ Branch l (map bindInDef ss) | Branch l ss <- bs ] (map bindInDef els)
            bindInDef stmt              = stmt
            bindInDef' d@Def{}          = d{ dbody = bindWits eq ++ dbody d }
            bindInDef' d                = d{ dbody = map bindInDef (dbody d) }
            
    wsubst ds []                        = id
    wsubst ds ws                        = transform trans
      where
        trans ns (Call l x@(Var _ (NoQ n)) p k)
          | Just _ <- wfind n ns        = Just $ Call l x (wit2arg ws p) k
        trans ns x@(Var _ (NoQ n))
          | Just (p,k,fx) <- wfind n ns = Just $ Lambda NoLoc p k (Call NoLoc x (wit2arg ws (pArg p)) (kArg k)) fx
        trans ns _                      = Nothing
    
        wfind n ns | n `elem` ns        = Nothing
                   | otherwise          = lookup n [ (n,(p,k,fx)) | Def _ n [] p k _ _ _ fx <- ds ]


--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Branch where
    infEnv env (Branch e b)             = do (cs1,e') <- inferBool env e
                                             (cs2,te,b') <- infEnv env b
                                             return (cs1++cs2, te, Branch e' b')

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
                                             return (cs1++cs2, exclude (dom te) te1, Handler ex' b')

instance InfEnv Except where
    infEnv env ex@(ExceptAll l)         = return ([], [], ex)
    infEnv env ex@(Except l x)          = return ([Cast t tException], [], ex)
      where t                           = tCon (TC x [])
    infEnv env ex@(ExceptAs l x n)      = return ([Cast t tException], [(n, NVar t)], ex)
      where t                           = tCon (TC x [])

instance Infer Expr where
    infer env x@(Var _ n)               = case findQName n env of
                                            NVar t -> return ([], t, x)
                                            NSVar t -> do
                                                fx <- currFX
                                                st <- newTVar
                                                return ([Cast (fxMut st) fx], t, x)
                                            NDef sc d -> do 
                                                (cs,t) <- instantiate env sc
                                                return (cs, t, app t x $ witsOf cs)
                                            NClass q _ _ -> do
                                                (cs0,ts) <- instQBinds env q
                                                case findAttr env (TC n ts) initKW of
                                                    Just (_wf,sc,_dec) -> do
                                                        (cs1,t) <- instantiate env sc
                                                        let t0 = tCon $ TC n ts
                                                            t' = subst [(tvSelf,t0)] t{ restype = tSelf }
                                                        return (cs1, t', app t' x $ witsOf (cs0++cs1))
                                            NAct q p k _ -> do
                                                st <- newTVar
                                                (cs,t) <- instantiate env (tSchema q (tFun (fxAct st) p k (tCon0 n q)))
                                                return (cs, t, app t x $ witsOf cs)
                                            NSig _ _ -> nameReserved n
                                            NReserved -> nameReserved n
                                            NBlocked -> nameBlocked n
                                            _ -> nameUnexpected n
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
                                             (cs2,prow,ps') <- infer env ps
                                             (cs3,krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX
                                             w <- newWitness
                                             return (Sub w t (tFun fx prow krow t0) :
                                                     cs1++cs2++cs3, t0, Call l (eCall (eVar w) [e']) ps' ks')
    infer env (Await l e)               = do t0 <- newTVar
                                             (cs1,e') <- inferSub env (tMsg t0) e
                                             st <- newTVar
                                             fx <- currFX
                                             return (Cast (fxAct st) fx :
                                                     cs1, t0, Await l e')
    infer env (Index l e ix)            = do ti <- newTVar
                                             (cs1,ix') <- inferSub env ti ix
                                             t0 <- newTVar
--                                             w <- newWitness
--                                             (cs2,t,e') <- infer env e
--                                             return (Impl w t (pIndexed ti t0) :
--                                                     cs1++cs2, t0, eCall (eDot (eVar w) getitemKW) [e', ix'])
                                             (cs2,e') <- inferSub env (tExist $ pIndexed ti t0) e
                                             return (cs1++cs2, t0, eCall (eDot (eDot e' protoKW) getitemKW) [eDot e' implKW, ix'])
    infer env (Slice l e [sl])          = do (cs1,t,e') <- infer env e
                                             (cs2,sl') <- inferSlice env sl
                                             t0 <- newTVar
                                             w <- newWitness
                                             return (Impl w t (pSliceable t0) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) getsliceKW) (e' : sliz2args sl'))
    infer env (Slice l e slz)           = notYet l "Multidimensional slicing"
    infer env (Cond l e1 e e2)          = do t0 <- newTVar
                                             (cs1,e1') <- inferSub env t0 e1
                                             (cs2,e2') <- inferSub env t0 e2
                                             (cs3,e') <- inferBool env e
                                             return (cs1++cs2++cs3, t0, Cond l e1' e' e2')
    infer env (BinOp l e1 o@(Op _ op) e2)
      | op `elem` [Or,And]              = do (cs1,e1') <- inferBool env e1
                                             (cs2,e2') <- inferBool env e2
                                             return (cs1++cs2, tBool, BinOp l e1' o e2')
      | otherwise                       = do t <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t e2
                                             w <- newWitness
                                             return (Impl w t (protocol op) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Plus               = pPlus
            protocol Minus              = pMinus
            protocol Mult               = pComplex
            protocol Pow                = pComplex
            protocol Div                = pComplex
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
    infer env (UnOp l o@(Op _ op) e)
      | op == Not                       = do (cs,e') <- inferBool env e
                                             return (cs, tBool, UnOp l o e')
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl w t (protocol op) :
                                                     cs, t, eCall (eDot (eVar w) (method op)) [e'])
      where protocol UPlus              = pComplex
            protocol UMinus             = pComplex
            protocol BNot               = pIntegral
            method UPlus                = posKW
            method UMinus               = negKW
            method BNot                 = invertKW
    infer env (CompOp l e1 [OpArg (Op _ op) e2])
      | op `elem` [In,NotIn]            = do t1 <- newTVar
                                             (cs1,e1') <- inferSub env t1 e1
                                             (cs2,e2') <- inferSub env (tExist $ pContainer t1) e2
                                             t2 <- newTVar
                                             w <- newWitness
                                             return (cs1++cs2, tBool, eCall (eDot (eDot e1' protoKW) (method op)) [eDot e1' implKW, e2'])
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

    infer env (Dot l x@(Var _ c) n)
      | NClass q us te <- cinfo         = do (cs0,ts) <- instQBinds env q
                                             case findAttr env (TC c ts) n of
                                                Just (_wf,sc,dec)
                                                  | isProp dec sc -> err l "Property attribute not selectable by class"
                                                  | otherwise -> do
                                                      (cs1,t) <- instantiate env sc
                                                      let t0 = tCon $ TC c ts
                                                          t' = subst [(tvSelf,t0)] $ addSelf t dec
                                                      return (cs0++cs1, t', app2nd dec t' (Dot l x n) $ witsOf (cs0++cs1))
                                                Nothing ->
                                                    case findWitness env c (hasAttr env n) of
                                                        Just wit -> do
                                                            (cs1,p,we) <- instWitness env ts wit
                                                            let Just (wf,sc,dec) = findAttr env p n
                                                            (cs2,t) <- instantiate env sc
                                                            let t0 = tCon $ TC c ts
                                                                t' = subst [(tvSelf,t0)] $ addSelf t dec
                                                            return (cs1++cs2, t', app t' (eDot (wf we) n) $ witsOf cs2)
                                                        Nothing -> err1 l "Attribute not found"
      | NProto q us te <- cinfo         = do (_,ts) <- instQBinds env q
                                             case findAttr env (TC c ts) n of
                                                Just (wf,sc,dec) -> do
                                                    (cs1,t) <- instantiate env sc
                                                    t0 <- newTVar
                                                    let t' = subst [(tvSelf,t0)] $ addSelf t dec
                                                    w <- newWitness
                                                    return (Impl w t0 (TC c ts) :
                                                            cs1, t', app t' (Dot l (wf $ eVar w) n) $ witsOf cs1)
                                                Nothing -> err1 l "Attribute not found"
      where cinfo                       = findQName c env

    infer env (Dot l e n)               = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             t0 <- newTVar
                                             return (Sel w t n t0 :
                                                     cs, t0, eCall (eVar w) [e'])

    infer env (DotI l e i False)        = do (ttup,ti,_) <- tupleTemplate i
                                             (cs,e') <- inferSub env ttup e
                                             return (cs, ti, DotI l e' i False)
    infer env (DotI l e i True)         = do (ttup,_,tl) <- tupleTemplate (i-1)
                                             (cs,e') <- inferSub env ttup e
                                             return (cs, tl, DotI l e' i True)
    infer env (Lambda l p k e fx)
      | nodup (p,k)                     = do pushFX fx tNone
                                             (cs0,te0,p') <- infEnv env1 p
                                             (cs1,te1,k') <- infEnv (define te0 env1) k
                                             (cs2,t,e') <- infer (define te1 (define te0 env1)) e
                                             popFX
                                             return (Cast fxPure fx : 
                                                     cs0++cs1++cs2, tFun fx (prowOf p') (krowOf k') t, Lambda l p' k' e' fx)
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs kargs)     = do (cs1,prow,pargs') <- infer env pargs
                                             (cs2,krow,kargs') <- infer env kargs
                                             return (cs1++cs2, TTuple NoLoc prow krow, Tuple l pargs' kargs')
    infer env (List l es)               = do t0 <- newTVar
                                             (cs,es') <- infElems env es t0
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w t1 (pSequence t0) :
                                                     cs, t1, eCall (eDot (eVar w) fromiterKW) [List l es'])
    infer env (ListComp l e1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e1] t0
                                             let [e1'] = es
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w t1 (pSequence t0) :
                                                     cs1++cs2, t1, eCall (eDot (eVar w) fromiterKW) [ListComp l e1' co'])
    infer env (Set l es)                = do t0 <- newTVar
                                             (cs,es')  <- infElems env es t0
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w t1 (pSet t0) :
                                                     cs, t1, eCall (eDot (eVar w) fromiterKW) [List l es'])
    infer env (SetComp l e1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e1] t0
                                             let [e1'] = es
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w t1 (pSet t0) :
                                                     cs1++cs2, t1, eCall (eDot (eVar w) fromiterKW) [ListComp l e1' co'])
                                             
    infer env (Dict l as)               = do tk <- newTVar
                                             tv <- newTVar
                                             (cs,as') <- infAssocs env as tk tv
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w t1 (pMapping tk tv) :
                                                     cs, t1, eCall (eDot (eVar w) fromiterKW) [List l as'])
    infer env (DictComp l a1 co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             tk <- newTVar
                                             tv <- newTVar
                                             (cs2,as) <- infAssocs (define te env) [a1] tk tv
                                             let [a1'] = as
                                             t1 <- newTVar
                                             w <- newWitness
                                             return (Impl w t1 (pMapping tk tv) :
                                                     cs1++cs2, t1, eCall (eDot (eVar w) fromiterKW) [ListComp l a1' co'])
    infer env (Paren l e)               = do (cs,t,e') <- infer env e
                                             return (cs, t, Paren l e')



tupleTemplate i                         = do ts <- mapM (const newTVar) [0..i]
                                             p <- newTVarOfKind PRow
                                             k <- newTVarOfKind KRow
                                             return (TTuple NoLoc (foldl (flip posRow) p ts) k, head ts, TTuple NoLoc p kwdNil)

isModule env e                          = fmap ModName $ mfilter (isMod env) $ fmap reverse $ dotChain e
  where dotChain (Var _ (NoQ n))        = Just [n]
        dotChain (Dot _ e n)            = fmap (n:) (dotChain e)
        dotChain _                      = Nothing


infElems env [] t0                      = return ([], [])
infElems env (Elem e : es) t0           = do (cs1,e') <- inferSub env t0 e
                                             (cs2,es') <- infElems env es t0
                                             return (cs1++cs2, Elem e' : es')           -- TODO: translate using primitive Iterator
infElems env (Star e : es) t0           = do (cs1,e') <- inferSub env (tExist $ pIterable t0) e
                                             (cs2,es') <- infElems env es t0
                                             return (cs1++cs2, Star e' : es')           -- TODO: translate using primitive Iterator
                                                     

infAssocs env [] tk tv                  = return ([], [])
infAssocs env (Assoc k v : as) tk tv    = do (cs1,k') <- inferSub env tk k
                                             (cs2,v') <- inferSub env tv v
                                             (cs3,as') <- infAssocs env as tv tk
                                             return (cs1++cs2++cs3, Elem (eTuple [k',v']) : as')    -- TODO: translate using primitive Iterator
infAssocs env (StarStar e : as) tk tv   = do (cs1,e') <- inferSub env (tExist $ pIterable $ tTupleP $ posRow tk $ posRow tv posNil) e
                                             (cs2,as') <- infAssocs env as tk tv
                                             return (cs1++cs2, Star e' : as')                       -- TODO: translate using primitive Iterator


inferBool env e                         = do (cs,t,e') <- infer env e
                                             return (cs, eCall (eDot e' boolKW) [])

inferSlice env (Sliz l e1 e2 e3)        = do (cs1,e1') <- inferSub env tInt e1
                                             (cs2,e2') <- inferSub env tInt e2
                                             (cs3,e3') <- inferSub env tInt e3
                                             return (cs1++cs2++cs3, Sliz l e1' e2' e3')

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
    infEnv env (PosPar n a Nothing p)   = do t <- maybe newTVar return a
                                             (cs,te,p') <- infEnv (define [(n, NVar t)] env) p
                                             return (wellformed env a ++ cs, (n, NVar t):te, PosPar n (Just t) Nothing p')
    infEnv env (PosPar n a (Just e) p)  = do t <- maybe newTVar return a
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,p') <- infEnv (define [(n, NVar t)] env) p
                                             return (wellformed env a ++ cs1++cs2, (n, NVar t):te, PosPar n (Just t) (Just e') p')
    infEnv env (PosSTAR n a)            = do t <- maybe newTVar return a
                                             r <- newTVarOfKind PRow
                                             return (Cast t (tTupleP r) :
                                                     wellformed env a, [(n, NVar t)], PosSTAR n (Just $ tTupleP r))
    infEnv env PosNIL                   = return ([], [], PosNIL)

instance InfEnv KwdPar where
    infEnv env (KwdPar n a Nothing k)   = do t <- maybe newTVar return a
                                             (cs,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (wellformed env a ++ cs, (n, NVar t):te, KwdPar n (Just t) Nothing k')
    infEnv env (KwdPar n a (Just e) k)  = do t <- maybe newTVar return a
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (wellformed env a++cs1++cs2, (n, NVar t):te, KwdPar n (Just t) (Just e') k')
    infEnv env (KwdSTAR n a)            = do t <- maybe newTVar return a
                                             r <- newTVarOfKind KRow
                                             return (Cast t (tTupleK r) :
                                                     wellformed env a, [(n, NVar t)], KwdSTAR n (Just $ tTupleK r))
    infEnv env KwdNIL                   = return ([], [], KwdNIL)

---------

instance Infer PosArg where
    infer env (PosArg e p)              = do (cs1,t,e') <- infer env e
                                             (cs2,prow,p') <- infer env p
                                             return (cs1++cs2, posRow t prow, PosArg e' p')
    infer env (PosStar e)               = do (cs,t,e') <- infer env e
                                             prow <- newTVarOfKind PRow
                                             return (Cast t (tTupleP prow) :
                                                     cs, prow, PosStar e')
    infer env PosNil                    = return ([], posNil, PosNil)
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do (cs1,t,e') <- infer env e
                                             (cs2,krow,k') <- infer env k
                                             return (cs1++cs2, kwdRow n t krow, KwdArg n e' k')
    infer env (KwdStar e)               = do (cs,t,e') <- infer env e
                                             krow <- newTVarOfKind KRow
                                             return (Cast t (tTupleK krow) :
                                                     cs, krow, KwdStar e')
    infer env KwdNil                    = return ([], kwdNil, KwdNil)

instance InfEnv Comp where
    infEnv env NoComp                   = return ([], [], NoComp)
    infEnv env (CompIf l e c)           = do (cs1,e') <- inferBool env e
                                             (cs2,te,c') <- infEnv env c
                                             return (cs1++cs2, te, CompIf l e' c')
    infEnv env (CompFor l p e c)        = do (cs1,te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             (cs2,e') <- inferSub env (tExist $ pIterable t1) e
                                             (cs3,te2,c') <- infEnv (define te1 env) c
                                             return (cs1++cs2++cs3, te1++te2, CompFor l p' (eCall (eDot (eDot e' protoKW) iterKW) [eDot e' implKW]) c')

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
    infEnvT env (PVar l n a)            = do t <- maybe newTVar return a
                                             case findName n env of
                                                 NReserved -> do
                                                     return (csa, [(n, NVar t)], t, PVar l n (Just t))
                                                 NSig (TSchema _ [] t') _ ->
                                                     return (Cast t t' : csa, [(n, NVar t')], t, PVar l n (Just t))
                                                 NVar t' ->
                                                     return (Cast t t' : csa, [], t, PVar l n Nothing)
                                                 NSVar t' ->
                                                     return (Cast t t' : csa, [], t, PVar l n Nothing)
                                                 _ -> 
                                                     err1 n "Variable not assignable:"
      where csa                         = wellformed env a
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
                                             return (Cast t1 t2 :
                                                     cs1++cs2, te1++te2, t1, p':ps')


{- Mark's THIH:                                            

myExpl :: Env -> (Name,TSchema,[Alt]) -> TypeM [Constraint]
myExpl env (i, sc, alts)    = do let TSchema vs qs t = sc 
                                     env' = define qs $ define vs $ define (i,sc) $ env
                                
                                 ps <- tiAlts env' alts t
                                 t' <- msubst t
                                 fs <- msubstTV (tyfree env)
                                 ps' <- simplify env' <$> msubst ps
                                
                                 let gs = tyfree t' \\ fs
                                     (ds,rs,xs) = mysplit env fs gs ps'
                                 solve env' xs
                                 when (not $ null $ tyvars rs `intersect` vs) $ fail “context too weak” 

                                 return (ds, quantify rs sc)

myExpl2 :: Env -> (Name,TSchema,[Alt]) -> TypeM [Constraint]
myExpl2 env (i, sc, alts)   = do (qs,t) <- instantiate sc 

                                 ps <- tiAlts env alts t
                                 qs' <- msubst qs
                                 t' <- msubst t
                                 fs <- msubstTV (tyfree env)
                                 ps' <- simplify env <$> msubst ps

                                 let gs = tyfree t' \\ fs
                                     sc' = quantify gs (qs',t')
                                     (ds,rs,xs) <- split fs gs ps'
                                
                                 xs' <- solve env xs
                                 when (sc /= sc') $ fail “signature too general” 
                                 when (not $ null xs') $ fail “context too weak” 

                                 return ds

split fs gs ps          = (ds, rs, xs2)
  where (ds,xs1)        = partition defer ps
        (rs,xs2)        = partition retain xs1
        defer           = all (`elem` fs) . tyfree
        retain          = all (`elem` (fs++gs)) . tyfree



myImpl :: Env -> (Id,[Alt]) -> TypeM ([Constraint],TSchema)
myImpl env (i, alts)        = do t <- newTVar Type
                                 let sc   = TSchema [] t
                                     env' = define (i,sc) env
                                
                                 ps <- tiAlts env' alts t
                                 t' <- msubst t
                                 fs <- msubstTV (tyfree env)
                                 ps' <- simplify env <$> msubst ps
                                
                                 let gs = tyfree t' \\ fs
                                     (ds,rs,xs) = mysplit ce fs gs ps'
                                 solve env' xs

                                 return (ds, quantify gs (rs,t'))

mysplit env fs gs ps        = let (ds,rs0) = partition (all (`elem` fs) . tyfree) ps
                                  (rs,xs) = partition (null . (\\(fs++gs)) . tyfree) rs0
                              in (ds, rs, xs)


tiExpl :: ClassEnv -> [Assump] -> (Id,Scheme,[Alt]) -> TI [Pred]
tiExpl ce as (i, sc, alts)  = do (qs :=> t) <- freshInst sc 
                                 ps <- tiAlts ce as alts t
                                 s <- getSubst
                                 let qs' = apply s qs
                                     t' = apply s t
                                     fs = tv (apply s as)
                                     gs = tv t' \\ fs
                                     sc' = quantify gs (qs' :=> t')
                                     ps' = filter (not . entail ce qs') (apply s ps)
                                 (ds, rs) <- split ce fs gs ps'
                                 if sc /= sc' then
                                    fail “signature too general” 
                                  else if not (null rs) then
                                    fail “context too weak” 
                                  else
                                    return ds

tiImpl :: ClassEnv -> [Assump] -> (Id,[Alt]) -> TI ([Pred],Scheme)
tiImpl ce as (i, alts)  = do t <- newTVar Star
                             let sc = toScheme t
                                 as' = (i :>: sc) : as
                             ps <- tiAlts ce as' alts t
                             s <- getSubst
                             let ps' = apply s ps
                                 t' = apply s t
                                 fs = tv (apply s as)
                                 gs = tv t' \\ fs
                             (ds, rs) <- split ce fs gs ps'
                             if restricted bs then
                                return (ds++rs , quantify (gs\\tv rs) ([] :=> t'))
                              else
                                return (ds, quantify gs (rs :=> t'))

tiImpls :: ClassEnv -> [Assump] -> [Impl] -> TI ([Pred],[Assump])
tiImpls ce as bs   = do ts <- mapM (\_ -> newTVar Star) bs
                        let is = map fst bs
                            scs = map toScheme ts
                            as' = zipWith (:>:) is scs ++ as
                            altss = map snd bs
                        pss <- sequence (zipWith (tiAlts ce as') altss ts)
                        s <- getSubst
                        let ps' = apply s (concat pss)
                            ts' = apply s ts
                            fs = tv (apply s as)
                            vss = map tv ts'
                            gs =  foldr1 union vss \\ fs
                        (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
                        if restricted bs then
                            let gs' = gs \\ tv rs
                                scs' = map (quantify gs' . ([] :=>)) ts
                            in return (ds ++ rs , zipWith (:>:) is scs')
                         else
                            let scs' = map (quantify gs . (rs :=>)) ts'
                            in return (ds, zipWith (:>:) is scs')

split ce fs gs ps       = do ps' <- reduce ce ps
                             let (ds,rs) = partition (all (`elem` fs) . tv) ps'
                             rs' <- defaultedPreds ce (fs ++ gs ) rs
                             return (ds, rs\\rs')

defaultedPreds          = withDefaults (\vps ts -> concat (map snd vps))
defaultSubst            = withDefaults (\vps ts -> zip (map fst vps) ts)
                                        
withDefaults f ce vs ps
  | any null tss        = fail "cannot resolve ambiguity"
  | otherwise           = return (f vps (map head tss))
  where vps             = ambiguities ce vs ps
        tss             = map (candidates ce) vps

ambiguities ce vs ps    = [(v, filter (elem v . tv) ps) | v <- tv ps \\vs]

-}
