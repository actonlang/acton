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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct, showTyFile, prettySigs, typeError, TypeError(..)) where

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
import Acton.Unify
import Acton.Transform
import Acton.Converter
import Acton.TypeM
import Acton.TypeEnv
import qualified InterfaceFiles
import qualified Data.Map
import Data.List (intersperse)

reconstruct                             :: String -> Env0 -> Module -> IO (TEnv, Module, Env0)
reconstruct fname env0 (Module m i ss)  = do --traceM ("#################### original env0 for " ++ prstr m ++ ":")
                                             --traceM (render (pretty env0))
                                             InterfaceFiles.writeFile (fname ++ ".ty") mrefs iface
                                             --traceM ("#################### converted env0:")
                                             --traceM (render (pretty env0'))
                                             return (iface, Module m i ss1T, env0')
                                             
  where ssT                             = if hasTesting i then ss ++ testStmts (emptyDict,emptyDict,emptyDict,emptyDict,emptyDict,emptyDict,emptyDict,emptyDict) else ss
        ss1T                            = if hasTesting i then rmTests ss1 ++ finalStmts env2 (modNameStr m) ss1 else ss1
        env1                            = reserve (assigned ssT) (typeX env0)
        (te,ss1)                        = runTypeM $ infTop env1 ssT
        env2                            = define te (setMod m env0)
        iface                           = unalias env2 te
        mrefs                           = moduleRefs1 env0
        env0'                           = convEnvProtos env0
        hasTesting i                    = Import NoLoc [ModuleItem (ModName [name "testing"]) Nothing] `elem` i
        rmTests (Assign _ [PVar _ n _] _ : ss)
          | nstr n `elem` ["__unit_tests","__sync_actor_tests","__simple_sync_tests","__sync_tests","__async_actor_tests","__async_tests", "__old_env_tests", "__env_tests"]
                                        = rmTests ss
        rmTests (Decl _ [Actor _ n _ _ _ _] : ss)
          | nstr n == "__test_main"     = rmTests ss
        rmTests (s : ss)                = s : rmTests ss
        rmTests []                      = []

        -- Convert the module name (ModName) to a string, e.g. "foo.bar"
        modNameStr (ModName ns) = concat (intersperse "." (map nstr ns))

        -- Inject __name__ variable
        __name__assign = Assign NoLoc [PVar NoLoc (name "__name__") Nothing] (Strings NoLoc [modNameStr m])
        ss1T' = __name__assign : ss1T
         

showTyFile env0 m fname         = do (ms,te) <- InterfaceFiles.readFile fname
                                     putStrLn ("\n#################################### Interface:")
                                     let env1 = foldr addImport env0 ms
                                     putStrLn $ prettySigs env1 m te

prettySigs env m te             = render $ vcat [ text "import" <+> pretty m | m <- moduleRefs1 env ] $++$
                                           vpretty (simp env1 te)
  where env1                    = define te $ setMod m env

nodup x
  | not $ null vs               = err2 vs "Duplicate names:"
  | otherwise                   = True
  where vs                      = duplicates (bound x)

defaultTE                           :: Env -> TEnv -> TypeM TEnv
defaultTE env te                    = do defaultVars (tyfree te \\ tyfree env)
                                         msubst te

defaultVars tvs                     = do tvs' <- tyfree <$> msubst (map tVar tvs)
                                         sequence [ substitute tv (dflt (tvkind tv)) | tv <- tvs' ]
  where dflt KType                  = tNone
        dflt KFX                    = fxPure
        dflt PRow                   = posNil
        dflt KRow                   = kwdNil


addTyping env n s t c                   = c {info = addT n (simp env s) t (info c){errloc = loc n}}
    where addT n s t (DfltInfo l m mbe ts)
                                        = DfltInfo l m mbe ((n,s,t):ts)
          addT _ _ _ info               = info

------------------------------

infTop                                  :: Env -> Suite -> TypeM (TEnv,Suite)
infTop env ss                           = do --traceM ("\n## infEnv top")
                                             pushFX fxPure tNone
                                             (cs,te,ss) <- (if stub env then infEnv else infSuiteEnv) env ss
                                             ss <- msubst ss
                                             popFX
                                             --traceM ("######## solve TOP: " ++ show (length cs))
                                             --traceM (prstrs cs)
                                             eq <- solveAll (posdefine (filter typeDecl te) env) te tNone cs
                                             --traceM ("######## termred TOP")
                                             ss <- termred <$> msubst (pushEqns eq ss)
                                             defaultVars (tyfree ss)
                                             te <- defaultTE env  te
                                             ss <- msubst ss
                                             --traceM ("-------- done TOP")
                                             return (te, ss)

pushEqns eqs ss                         = push eqns0 ss
  where eqns0                           = [ (eq,ns) | eq <- eqs, let ns = free eq `intersect` bound ss ]
        push eqns ss                    = bindWits (map fst eq1) ++ push' eq2 ss
          where (eq1,eq2)               = partition (null . snd) eqns
        push' [] ss                     = ss
        push' eqns (s:ss)
          | null eq1                    = s : push eq2 ss
          | otherwise                   = inject eq1 s : push eq2 ss
          where eq1                     = collect [] (free s) (map fst eqns)
                eq2                     = [ (eq, ns \\ ns') | (eq,ns) <- eqns ]
                ns'                     = bound s
        inject eqs (Decl l ds)          = Decl l (map inj ds)
          where inj (Class l n q us b)  = Class l n q us (map inj' b)
                inj d                   = d{ dbody = bindWits eqs ++ dbody d }
                inj' (Decl l ds)        = Decl l (map inj ds)
                inj' s                  = s
        inject eqs (With l [] ss)       = With l [] (pushEqns eqs ss)
        inject eqs s                    = error ("# Internal error: cyclic witnesses " ++ prstrs eqs ++ "\n# and statement\n" ++ prstr s)
        collect eq0 vs eq
          | null eq1                    = eq0
          | otherwise                   = collect (eq1++eq0) ((free eq1 \\ bound eq1) ++ vs) eq2
          where (eq1,eq2)               = partition (any (`elem` vs) . bound) eq


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
          | length ts == l              = ([ Cast (DfltInfo (loc t) 26 Nothing []) t1 t0 | t1 <- t:ts ], NVar t0)
          where ts                      = [ t | te <- tes, Just (NVar t) <- [lookup n te] ]
        unif n t0 (NSVar t)
          | length ts == l              = ([ Cast (DfltInfo (loc t) 27 Nothing []) t1 t0 | t1 <- t:ts ], NSVar t0)
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

liveCombine te Nothing                  = Nothing
liveCombine Nothing te'                 = Nothing
liveCombine (Just te) (Just te')        = Just $ te++te'

fxUnwrapSc env sc                       = sc{ sctype = fxUnwrap env $ sctype sc }

fxUnwrap env (TFun l fx p k t)          = TFun l (fxUnwrap env fx) p k t
fxUnwrap env (TFX l FXAction)
  | inAct env                           = TFX l FXProc
fxUnwrap env t                          = t

wrap t@TFun{}                           = do tvx <- newTVarOfKind KFX
                                             tvy <- newTVarOfKind KFX
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc t) 28 Nothing []) w tvx (pWrapped (effect t) tvy), t{ effect = tvx })

wrapped l kw env cs ts args             = do tvx <- newTVarOfKind KFX
                                             tvy <- newTVarOfKind KFX
                                             let p = pWrapped tvx tvy
                                                 Just (_, sc, Just Static) = findAttr env p kw
                                             (_,tvs,t0) <- instantiate env sc
                                             fx <- newTVarOfKind KFX
                                             t' <- newTVar
                                             let t1 = subst [(fxSelf,fx)] t0
                                                 t2 = tFun fxPure (foldr posRow posNil ts) kwdNil t'
                                             w <- newWitness
                                             (cs0,_) <- simplify env [] t' [Cast (DfltInfo l 30 Nothing []) t1 t2]
                                             t' <- msubst t'
                                             cs1 <- msubst (Impl (DfltInfo l 29 Nothing []) w fx p : cs)
                                             return (cs0++cs1, t', eCall (tApp (Dot l0 (eVar w) kw) tvs) args)

--------------------------------------------------------------------------------------------------------------------------

instance (InfEnv a) => InfEnv [a] where
    infEnv env []                       = return ([], [], [])
    infEnv env (s : ss)                 = do (cs1,te1,s1) <- infEnv env s
                                             let te1' = if inDecl env then noDefs te1 else te1      -- TODO: also stop class instantiation!
                                                 env' = define te1' env
                                             -- NOTE: constraints in cs1 are *not* seen when solving local constraints of 'ss'! Must fix!!!
                                             (cs2,te2,ss2) <- infEnv env' ss
                                             return (cs1++cs2, te1++te2, s1:ss2)

instance InfEnv Stmt where
    infEnv env (Expr l e)
      | e == eNotImpl                   = return ([], [], Expr l e)
      | otherwise                       = do (cs,_,e') <- infer env e
                                             return (cs, [], Expr l e')

    infEnv env (Assign l pats e)
      | nodup pats, e == eNotImpl       = do (cs1,te,t,pats') <- infEnvT env pats
                                             return (cs1, te, Assign l pats' e)
      | otherwise                       = do (cs1,te,t,pats') <- infEnvT env pats
                                             (cs2,e') <- inferSub env t e
                                             return (cs1++cs2, te, Assign l pats' e')

    infEnv env (Assert l e1 e2)         = do (cs1,_,_,_,e1') <- inferTest env e1
                                             (cs2,e2') <- inferSub env tStr e2
                                             return (cs1++cs2, [], Assert l e1' e2')
    infEnv env s@(Pass l)               = return ([], [], s)

    infEnv env s@(Return l Nothing)     = do t <- currRet
                                             return ([Cast (DfltInfo l 31 Nothing []) tNone t], [], Return l Nothing)
    infEnv env (Return l (Just e))      = do t <- currRet
                                             (cs,e') <- inferSub env t e
                                             return (cs, [], Return l (Just e'))
    infEnv env (Raise l e)              = do (cs,t,e') <- infer env e
                                             return (Cast (DfltInfo (loc e) 32 (Just e) []) t tException : cs, [], Raise l e')
    infEnv env s@(Break _)              = return ([], [], s)
    infEnv env s@(Continue _)           = return ([], [], s)
    infEnv env (If l bs els)            = do (css,tes,bs') <- fmap unzip3 $ mapM (infLiveEnv env) bs
                                             (cs0,te,els') <- infLiveEnv env els
                                             (cs1,te1) <- commonTEnv env $ catMaybes (te:tes)
                                             return (cs0++cs1++concat css, te1, If l bs' els')
    infEnv env (While l e b els)        = do (cs1,env',s,_,e') <- inferTest env e
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
                                             return (Impl (DfltInfo (loc e) 33 (Just e) []) w t2 (pIterable t1) :
                                                     cs1++cs2++cs3++cs4, [], For l p' (eCall (eDot (eVar w) iterKW) [e']) b' els')
    infEnv env (Try l b hs els fin)     = do (cs1,te,b') <- infLiveEnv env b
                                             (cs2,te',els') <- infLiveEnv (maybe id define te $ env) els
                                             (css,tes,hs') <- fmap unzip3 $ mapM (infLiveEnv env) hs
                                             (cs3,te1) <- commonTEnv env $ catMaybes $ (liveCombine te te'):tes
                                             (cs4,te2,fin') <- infSuiteEnv env fin
                                             fx <- currFX
                                             return (--Cast fxProc fx :
                                                     cs1++cs2++cs3++cs4++concat css, te1++te2, Try l b' hs' els' fin')
    infEnv env (With l items b)
      | nodup items                     = do (cs1,te,items') <- infEnv env items
                                             (cs2,te1,b') <- infSuiteEnv (define te env) b
                                             return $ (cs1++cs2, exclude te1 (dom te), With l items' b')

    infEnv env (VarAssign l pats e)
      | nodup pats                      = do (cs1,te,t,pats') <- infEnvT env pats
                                             (cs2,e') <- inferSub env t e
                                             return (cs1++cs2, [ (n,NSVar t) | (n,NVar t) <- te], VarAssign l pats' e')
    
    infEnv env (After l e1 e2)          = do (cs1,e1') <- inferSub env tFloat e1
                                             (cs2,t,e2') <- infer env e2
                                             -- TODO: constrain t
                                             fx <- currFX
                                             return (Cast (DfltInfo l 34 Nothing []) fxProc fx :
                                                     cs1++cs2, [], After l e1' e2')
    
    infEnv env (Signature l ns sc@(TSchema _ q t) dec)
      | not $ null bad                  = illegalSigOverride (head bad)
      | otherwise                       = return ([], [(n, NSig sc dec' Nothing) | n <- ns], Signature l ns sc dec)
      where
        redefs                          = [ (n,i) | n <- ns, let i = findName n env, i /= NReserved ]
        bad                             = [ n | (n,i) <- redefs, not $ ok i ]
        ok (NSig (TSchema _ [] t') d _) = null q && castable env t t' && dec == d
        ok _                            = False
        dec'                            = if inClass env && isProp dec sc then Property else dec

    infEnv env (Data l _ _)             = notYet l "data syntax"

    infEnv env (Decl l ds)
      | inDecl env && nodup ds          = do (cs1,te1,ds1) <- infEnv env ds
                                             return (cs1, te1, Decl l ds1)
      | nodup ds                        = do --traceM ("######## decls: " ++ prstrs (declnames ds))
                                             (cs1,te1,ds1) <- infEnv (setInDecl env) ds
                                             (cs2,ds2) <- checkEnv (define te1 env) ds1
                                             (cs3,te2,eq,ds3) <- genEnv env cs2 te1 ds2
                                             --traceM ("-------- done: " ++ prstrs (declnames ds))
                                             return (cs1++cs3, te2, withLocal (bindWits eq) $ Decl l ds3)

    infEnv env (Delete l targ)          = do (cs0,t0,e0,tg) <- infTarg env targ
                                             (cs1,stmt) <- del t0 e0 tg
                                             return (cs0++cs1, [], stmt)
      where del t0 e0 (TgVar n)         = do return ( Cast (DfltInfo l 35 Nothing []) tNone t0 : [], sAssign (pVar' n) eNone)
            del t0 e0 (TgIndex ix)      = do (cs,ti,ix) <- infer env ix
                                             t <- newTVar
                                             w <- newWitness
                                             return ( Impl (DfltInfo l 36 Nothing []) w t0 (pIndexed ti t) : cs, sExpr $ dotCall w delitemKW [e0, ix] )
            del t0 e0 (TgSlice sl)      = do (cs,sl) <- inferSlice env sl
                                             t <- newTVar
                                             w <- newWitness
                                             return ( Impl (DfltInfo l 37 Nothing []) w t0 (pSliceable t) : cs, sExpr $ dotCall w delsliceKW [e0, sliz2exp sl] )
            del t0 e0 (TgDot n)         = do t <- newTVar
                                             return ( Mut (DfltInfo l 38 Nothing []) t0 n t : Cast (DfltInfo l 39 Nothing []) tNone t : [], sMutAssign (eDot e0 n) eNone )

    infEnv env (MutAssign l targ e)     = do (cs0,t0,e0,tg) <- infTarg env targ
                                             t <- newTVar
                                             (cs1,e) <- inferSub env t e
                                             (cs2,stmt) <- asgn t0 t e0 e tg
                                             return (cs0++cs1++cs2, [], stmt)
      where asgn t0 t e0 e (TgVar n)    = do tryUnify (DfltInfo l 40 Nothing []) t0 t
                                             return ( [], sAssign (pVar' n) e )
            asgn t0 t e0 e (TgIndex ix) = do (cs,ti,ix) <- infer env ix
                                             w <- newWitness
                                             return ( Impl (DfltInfo l 41 Nothing []) w t0 (pIndexed ti t) : cs, sExpr $ dotCall w setitemKW [e0, ix, e] )
            asgn t0 t e0 e (TgSlice sl) = do (cs,sl) <- inferSlice env sl
                                             t' <- newTVar
                                             w <- newWitness
                                             w' <- newWitness
                                             return ( Impl (DfltInfo l 42 Nothing []) w t0 (pSliceable t') :
                                                      Impl (DfltInfo l 43 Nothing []) w' t (pIterable t') : 
                                                      cs, sExpr $ eCall (tApp (eDot (eVar w) setsliceKW) [t]) [e0, eVar w', sliz2exp sl, e] )
            asgn t0 t e0 e (TgDot n)    = do return ( Mut (DfltInfo l 44 Nothing []) t0 n t : [], sMutAssign (eDot e0 n) e )

    infEnv env (AugAssign l targ o e)   = do (cs0,t0,e0,tg) <- infTarg env targ
                                             t1 <- newTVar
                                             (cs1,e) <- inferSub env t1 e
                                             let (proto,kw) = oper t1 o
                                             t <- if o `elem` [MultA,DivA] then newTVar else pure t1
                                             w <- newWitness
                                             (ss,x) <- mkvar t0 e0
                                             (cs2,stmt) <- aug t0 t x (dotCall w kw) e tg
                                             return ( Impl (DfltInfo l 45 Nothing []) w t proto : cs0++cs1++cs2, [], withLocal ss stmt )
      where oper t MultA                = (pTimes t,  imulKW)
            oper t DivA                 = (pDiv t,    itruedivKW)
            oper _ PlusA                = (pPlus,     iaddKW)
            oper _ MinusA               = (pMinus,    isubKW)
            oper _ PowA                 = (pNumber,   ipowKW)
            oper _ ModA                 = (pIntegral, imodKW)
            oper _ EuDivA               = (pIntegral, ifloordivKW)
            oper _ ShiftLA              = (pIntegral, ilshiftKW)
            oper _ ShiftRA              = (pIntegral, irshiftKW)
            oper _ BOrA                 = (pLogical,  iorKW)
            oper _ BAndA                = (pLogical,  iandKW)
            oper _ MMultA               = (pMatrix,   imatmulKW)
            
            aug t0 t x f e (TgVar _)    = do tryUnify (DfltInfo l 46 Nothing []) t0 t
                                             return ( [], sAssign (pVar' x) $ f [eVar x, e] )
            aug t0 t x f e (TgIndex ix) = do (cs,ti,ix) <- infer env ix
                                             w <- newWitness
                                             return ( Impl (DfltInfo l 47 Nothing []) w t0 (pIndexed ti t) :
                                                      cs, sExpr $ dotCall w setitemKW [eVar x, ix, f [dotCall w getitemKW [eVar x, ix], e]])
            aug t0 t x f e (TgSlice sl) = do tryUnify (DfltInfo l 1115 Nothing []) t0 t
                                             (cs,sl) <- inferSlice env sl
                                             t' <- newTVar
                                             w <- newWitness
                                             w' <- newWitness
                                             let e1 = f [dotCall w getsliceKW [eVar x, sliz2exp sl], e]
                                             return ( Impl (DfltInfo l 48 Nothing []) w t (pSliceable t') :
                                                      Impl (DfltInfo l 49 Nothing []) w' t (pIterable t') :
                                                      cs, sExpr $ eCall (tApp (eDot (eVar w) setsliceKW) [t]) [eVar x, eVar w', sliz2exp sl, e1] )
            aug t0 t x f e (TgDot n)    = do return ( Mut (DfltInfo l 50 Nothing []) t0 n t : [], sMutAssign (eDot (eVar x) n) $ f [eDot (eVar x) n, e])


dotCall w kw                            = eCall (eDot (eVar w) kw)

mkvar t (Var _ (NoQ x))                 = return ([], x)
mkvar t e                               = do x <- newTmp
                                             return ([sAssign (pVar x t) e], x)

data Tg                                 = TgVar Name | TgIndex Expr | TgSlice Sliz | TgDot Name

infTarg env e@(Var l (NoQ n))           = case findName n env of
                                             NReserved ->
                                                 err1 n "Variable not yet assigned"
                                             NSig{} ->
                                                 err1 n "Variable not yet assigned"
                                             NVar t ->
                                                 return ([], t, e, TgVar n)
                                             NSVar t -> do
                                                 fx <- currFX
                                                 return ([Cast (DfltInfo l 51 Nothing []) fxProc fx], t, e, TgVar n)
                                             _ -> 
                                                 err1 n "Variable not assignable:"
infTarg env (Index l e ix)              = do (cs,t,e) <- infer env e
                                             fx <- currFX
                                             return (Cast (DfltInfo l 52 Nothing []) fxMut fx : Cast (DfltInfo l 53 (Just e) []) t tObject : cs, t, e, TgIndex ix)
infTarg env (Slice l e sl)              = do (cs,t,e) <- infer env e
                                             fx <- currFX
                                             return (Cast (DfltInfo l 54 Nothing []) fxMut fx : Cast (DfltInfo l 55 (Just e) []) t tObject : cs, t, e, TgSlice sl)
infTarg env (Dot l e n)                 = do (cs,t,e) <- infer env e
                                             fx <- currFX
                                             return (Cast (DfltInfo l 56 Nothing []) fxMut fx : Cast (DfltInfo l 57 (Just e) []) t tObject : cs, t, e, TgDot n)

sliz2exp (Sliz _ e1 e2 e3)              = eCall (eQVar qnSlice) $ map (maybe eNone id) [e1,e2,e3]

withLocal [] s                          = s
withLocal ss s                          = With l0 [] (ss ++ [s])

--------------------------------------------------------------------------------------------------------------------------

matchingDec n sc dec NoDec              = True
matchingDec n sc dec dec'
  | dec == dec'                         = True
  | otherwise                           = decorationMismatch n sc dec

matchDefAssumption env cs def
  | q0 == q1                            = match env cs [] def
  | otherwise                           = do (cs1, tvs) <- instQBinds env q1
                                             let eq0 = witSubst env q1 cs1
                                                 s = qbound q1 `zip` tvs            -- This cannot just be memoized in the global TypeM substitution,
                                             def' <- msubstWith s def{ qbinds = [] } -- since the variables in (qbound q1) aren't necessarily globally unique
                                             match env (cs ++ cs1) eq0 def'
  where NDef (TSchema _ q0 t0) dec _    = findName (dname def) env
        t2 | inClass env                = addSelf t0 (Just dec)
           | otherwise                  = t0
        q1                              = qbinds def
        fx | inAct env                  = dfx def
           | otherwise                  = effect t2
        env0                            = defineTVars q1 $ defineTVars q0 env
        (pos0,kwd0)                     = qual env dec (pos def) (kwd def) (qualWPar env q0)

        match env cs eq0 def            = do --traceM ("## matchDefAssumption " ++ prstr (dname def) ++ ": [" ++ prstrs q1 ++ "] => " ++ prstr (Cast info t1 t2))
                                             (cs2,eq1) <- solveScoped env0 (qbound q0) [] t1 (Cast info t1 t2 : cs)
                                             checkNoEscape (loc def) env (qbound q0)
                                             cs2 <- msubst cs2
                                             return (cs2, def{ qbinds = noqual env q0, pos = pos0, kwd = kwd0,
                                                               dbody = bindWits (eq0++eq1) ++ dbody def, dfx = fx })
           where t1                     = tFun (dfx def) (prowOf $ pos def) (krowOf $ kwd def) (fromJust $ ann def)
                 sc1                    = TSchema NoLoc q1 t1
                 mbl                    = findSigLoc (dname def) env
                 msg                    = "Type incompatibility between signature for and definition of "++Pretty.print (dname def)
                 info                   = maybe (DfltInfo (loc def) 58 Nothing []) (\l -> DeclInfo l (loc def) (dname def) sc1 msg) mbl

qual env dec p k qf | not (inClass env) = (qf p, k)
qual env Static p k qf                  = (qf p, k)
qual env dec PosNIL (KwdPar n t e k) qf = (PosPar n t e (qf PosNIL), k)
qual env dec (PosPar n t e p) k qf      = (PosPar n t e (qf p), k)

--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Decl where
    infEnv env d@(Def _ n q p k a _ _ fx)
      | nodup (p,k)                     = case findName n env of
                                             NSig sc dec _ | t@TFun{} <- sctype sc, matchingDec n sc dec (deco d) -> do
                                                 --traceM ("\n## infEnv (sig) def " ++ prstr (n, NDef sc dec))
                                                 return ([], [(n, NDef (fxUnwrapSc env sc) dec Nothing)], d{deco = dec})
                                             NReserved -> do
                                                 t <- tFun (fxUnwrap env fx) (prowOf p) (krowOf k) <$> maybe newTVar return a
                                                 let sc = tSchema q (if inClass env then dropSelf t (deco d) else t)
                                                 --traceM ("\n## infEnv def " ++ prstr (n, NDef sc (deco d)))
                                                 return ([], [(n, NDef sc (deco d) Nothing)], d)
                                             _ ->
                                                 illegalRedef n
        

    infEnv env d@(Actor _ n q p k b)
      | nodup (p,k)                     = case findName n env of
                                             NReserved -> do
                                                 te <- infActorEnv env b
                                                 let prow = prowOf p
                                                     krow = krowOf k
                                                 --traceM ("\n## infEnv actor " ++ prstr (n, NAct q prow krow te))
                                                 return ([], [(n, NAct q prow krow te Nothing)], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env (Class l n q us b)
      | not $ null ps                   = notYet (loc n) "Classes with direct extensions"
      | otherwise                       = case findName n env of
                                             NReserved -> do
                                                 --traceM ("\n## infEnv class " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 te0 <- infProperties env as' b
                                                 (cs,te,b1) <- infEnv env1 b
                                                 popFX
                                                 (cs1,eq1) <- solveScoped env1 (qbound q) te tNone cs
                                                 checkNoEscape l env (qbound q)
                                                 (nterms,asigs,_) <- checkAttributes [] te' te
                                                 let te1 = if notImplBody b then unSig asigs else []
                                                     te2 = te ++ te1
                                                     b2 = addImpl te1 b1
                                                 return (cs1, [(n, NClass q as' (te0++te2) Nothing)], Class l n q us (bindWits eq1 ++ props te0 ++ b2))
                                             _ -> illegalRedef n
      where env1                        = define (exclude (toSigs te') [initKW]) $ reserve (assigned b) $ defineSelfOpaque $ defineTVars (stripQual q) $ setInClass env
            (as,ps)                     = mro2 env us
            as'                         = if null as && not (inBuiltin env && n == nValue) then leftpath [cValue] else as
            te'                         = parentTEnv env as'
            props te0                   = [ Signature l0 [n] sc Property | (n,NSig sc Property _) <- te0 ]
            
    infEnv env (Protocol l n q us b)    = case findName n env of
                                             NReserved -> do
                                                 --traceM ("\n## infEnv protocol " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 (cs,te,b') <- infEnv env1 b
                                                 popFX
                                                 (cs1,eq1) <- solveScoped env1 (qbound q) te tNone cs
                                                 checkNoEscape l env (qbound q)
                                                 (nterms,_,sigs) <- checkAttributes [] te' te
                                                 let noself = [ n | (n, NSig sc Static _) <- te, tvSelf `notElem` tyfree sc ]
                                                 when (notImplBody b) $ err0 (notImpls b) "A protocol body cannot be NotImplemented"
                                                 when (not $ null nterms) $ err2 (dom nterms) "Method/attribute lacks signature:"
                                                 when (initKW `elem` sigs) $ err2 (filter (==initKW) sigs) "A protocol cannot define __init__"
                                                 when (not $ null noself) $ err2 noself "A static protocol signature must mention Self"
                                                 return (cs1, [(n, NProto q ps te Nothing)], Protocol l n q us (bindWits eq1 ++ b'))
                                             _ -> illegalRedef n
      where env1                        = define (toSigs te') $ reserve (assigned b) $ defineSelfOpaque $ defineTVars (stripQual q) $ setInClass env
            ps                          = mro1 env us
            te'                         = parentTEnv env ps

    infEnv env (Extension l q c us b)
      | length us == 0                  = err (loc n) "Extension lacks a protocol"
--      | length us > 1                   = notYet (loc n) "Extensions with multiple protocols"
      | not $ null witsearch            = err (loc n) ("Extension already exists: " ++ prstr (head witsearch))
      | otherwise                       = do --traceM ("\n## infEnv extension " ++ prstr (extensionName us c))
                                             pushFX fxPure tNone
                                             (cs,te,b1) <- infEnv env1 b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 (qbound q) te tNone cs
                                             checkNoEscape l env (qbound q)
                                             (nterms,asigs,sigs) <- checkAttributes final te' te
                                             when (not $ null nterms) $ err2 (dom nterms) "Method/attribute not in listed protocols:"
                                             when (not $ null sigs) $ err2 sigs "Extension with new methods/attributes not supported"
                                             when (not (null asigs || notImplBody b)) $ err3 l (dom asigs) "Protocol method/attribute lacks implementation:"
                                             let te1 = unSig $ subst s asigs
                                                 te2 = te ++ te1
                                                 b2 = addImpl te1 b1
                                             return (cs1, [(extensionName us c, NExt q c ps te2 Nothing)], Extension l q c us (bindWits eq1 ++ b2))
      where TC n ts                     = c
            env1                        = define (toSigs te') $ reserve (assigned b) $ defineSelfOpaque $ defineTVars (stripQual q) $ setInClass env
            witsearch                   = [ w | w <- witsByPName env (tcname u), matchExactly (tCon c) u w, matching [wtype w] (qbound q) [tCon c] ]
            u                           = head us
            ps                          = subst [(tvSelf,tCon c)] $ mro1 env us -- TODO: check that ps doesn't contradict any previous extension mro for c
            final                       = concat [ conAttrs env (tcname p) | (_,p) <- tail ps, hasWitness env (tCon c) p ]
            te'                         = parentTEnv env ps
            s                           = [(tvSelf, tCon c)]

--------------------------------------------------------------------------------------------------------------------------

checkAttributes final te' te
  | not $ null dupsigs                  = err2 dupsigs "Duplicate signatures for"
  | not $ null props                    = err2 props "Property attributes cannot have class-level definitions:"
  | not $ null nodef                    = err2 nodef "Methods finalized in a previous extension cannot be overridden:"
--  | not $ null nself                    = err0 nself "Negative Self in non-static method signature"
  | otherwise                           = return (nterms, abssigs, dom sigs)
  where (sigs,terms)                    = sigTerms te
        (sigs',terms')                  = sigTerms te'
        (allsigs,allterms)              = (sigs ++ sigs', terms ++ terms')
        dupsigs                         = duplicates (dom sigs)
        nterms                          = exclude terms (dom allsigs)
        abssigs                         = allsigs `exclude` (dom allterms ++ final)
        props                           = dom terms `intersect` dom (propSigs allsigs)
        nodef                           = dom terms `intersect` final
        nself                           = negself te

addImpl [] ss                           = ss
addImpl asigs (s : ss)
  | isNotImpl s                         = fromTEnv (unSig asigs) ++ s : ss
  | otherwise                           = s : addImpl asigs ss

toSigs te                               = map makeSig te
  where makeSig (n, NDef sc dec doc)    = (n, NSig sc dec doc)
        makeSig (n, NVar t)             = (n, NSig (monotype t) Static Nothing)
        makeSig (n, i)                  = (n,i)


--------------------------------------------------------------------------------------------------------------------------

solveAll env te tt []                   = return []
solveAll env te tt cs                   = do --traceM ("\n\n### solveAll " ++ prstrs cs)
                                             (cs,eq) <- simplify env te tt cs
                                             (cs,eq) <- solve env (const True) te tt eq cs
                                             return eq

solveScoped env vs te tt []             = return ([], [])
solveScoped env [] te tt cs             = simplify env te tt cs
solveScoped env vs te tt cs             = do --traceM ("\n\n### solveScoped: " ++ prstrs cs)
                                             (cs,eq) <- simplify env te tt cs
                                             solve env (any (`elem` vs) . tyfree) te tt eq cs

checkNoEscape _ env []                  = return ()
checkNoEscape l env vs                  = do fvs <- tyfree <$> msubst env
                                             let escaped = vs `intersect` fvs
                                             when (not $ null escaped) $ do
                                                 env1 <- msubst env
                                                 traceM ("####### env:\n" ++ prstr env1)
                                                 err l ("Escaping type variables: " ++ prstrs escaped)


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
    checkEnv env x                      = undefined
    checkEnv' env x                     = do (cs,x') <- checkEnv env x
                                             return (cs, [x'])

instance (Check a) => Check [a] where
    checkEnv env []                     = return ([], [])
    checkEnv env (d:ds)                 = do (cs1,d') <- checkEnv' env d
                                             (cs2,ds') <- checkEnv env ds
                                             return (cs1++cs2, d'++ds')

------------------

infActorEnv env ss                      = do dsigs <- mapM mkNDef dvars                                 -- exposed defs without sigs
                                             bsigs <- mapM mkNVar pvars                                 -- exposed assigns without sigs
                                             return (abssigs ++ unSig concsigs ++ dsigs ++ bsigs)       -- abstract sigs ++ exposed sigs + the above
  where sigs                            = [ (n, NSig sc dec Nothing) | Signature _ ns sc dec <- ss, n <- ns, not $ isHidden n ]
        (concsigs, abssigs)             = partition ((`elem`(dvars++pvars)) . fst) sigs
        dvars                           = notHidden $ methods ss \\ dom sigs
        mkNDef n                        = do t <- newTVar
                                             return (n, NDef (monotype $ t) NoDec Nothing)
        svars                           = statevars ss
        pvars                           = pvarsF ss \\ dom (sigs) \\ dvars
        pvarsF ss                       = nub $ concat $ map pvs ss
          where pvs (Assign _ pats _)   = notHidden $ bound pats \\ svars   -- svars only excluded until we move stateful actor cmds to __init__
                pvs (If _ bs els)       = foldr intersect (pvarsF els) [ pvarsF ss | Branch _ ss <- bs ]
                pvs _                   = []
        mkNVar n                        = do t <- newTVar
                                             return (n, NVar t)

matchActorAssumption env n0 p k te      = do --traceM ("## matchActorAssumption " ++ prstrs te)
                                             (css,eqs) <- unzip <$> mapM check1 te0
                                             let cs = [Cast (DfltInfo (ploc p) 60 Nothing []) (tTuple p0 k0) (tTuple (prowOf p) (krowOf k)),
                                                       Seal (DfltInfo (ploc p) 112 Nothing []) p0, Seal (DfltInfo (kloc k) 113 Nothing []) k0]
                                             (cs,eq) <- simplify env obs tNone (cs ++ concat css)
                                             return (cs, eq ++ concat eqs)
  where NAct _ p0 k0 te0 _              = findName n0 env
        ns                              = dom te0
        obs                             = te0 ++ te
        te1                             = nTerms $ te `restrict` ns
        check1 (n, NSig _ _ _)          = return ([], [])
        check1 (n, NVar t0)             = do --traceM ("## matchActorAssumption for attribute " ++ prstr n)
                                             return ([Cast (DfltInfo (loc n) 62 Nothing []) t t0, Seal (DfltInfo (loc n) 114 Nothing []) t0],[])
          where Just (NVar t)           = lookup n te1
        check1 (n, NDef sc0 _ _)        = do (cs0,_,t) <- instantiate env sc
                                             (c0,t') <- wrap t
                                             let c1 = Cast (DfltInfo (loc n) 63 Nothing []) t' (sctype sc0)
                                                 cs1 = map (Seal (DfltInfo  (loc n) 115 Nothing [])) (leaves sc0)
                                                 q = scbind sc
                                             --traceM ("## matchActorAssumption for method " ++ prstr n ++ ": " ++ prstr c1)
                                             (cs2,eq) <- solveScoped (defineTVars q env) (qbound q) obs tNone (c0:c1:cs0++cs1)
                                             checkNoEscape (loc n) env (qbound q)
                                             return (cs2, eq)
          where Just (NDef sc _ _)      = lookup n te1
        check1 (n, i)                   = return ([], [])
        ploc (PosPar n _ _ _)           = loc n
        ploc (PosSTAR n _)              = loc n
        ploc _                          = NoLoc
        kloc (KwdPar n _ _ _)           = loc n
        kloc (KwdSTAR n _)              = loc n
        kloc _                          = NoLoc

infProperties env as b
  | Just (self,ss) <- inits             = infProps self ss
  | otherwise                           = return []
  where inherited                       = concat $ map (conAttrs env . tcname . snd) as
        explicit                        = concat [ ns | Signature _ ns sc dec <- b, isProp dec sc ]
        inits                           = listToMaybe [ (x, dbody d) | Decl _ ds <- b, d <- ds, dname d == initKW, Just x <- [selfPar d] ]
        infProps self (MutAssign _ (Dot _ (Var _ (NoQ x)) n) _ : b)
          | x /= self                   = return []
          | n `notElem` inherited,
            n `notElem` explicit        = do t <- newTVar
                                             te <- infProps self b
                                             return ((n,NSig (monotype t) Property Nothing) : te)
          | otherwise                   = infProps self b 
        infProps self (Expr _ (Call _ (Dot _ (Var _ c) n) _ _) : b)
          | isClass env c, n == initKW  = infProps self b
        infProps self _                 = return []
        selfPar Def{pos=PosPar x _ _ _} = Just x
        selfPar Def{kwd=KwdPar x _ _ _} = Just x
        selfPar _                       = Nothing

infDefBody env n (PosPar x _ _ _) k b
  | inClass env && n == initKW          = infInitEnv (setInDef env) x b
infDefBody env n p (KwdPar x _ _ _) b
  | inClass env && n == initKW          = infInitEnv (setInDef env) x b
infDefBody env _ _ _ b                  = infSuiteEnv (setInDef env) b

infInitEnv env self (MutAssign l (Dot l' e1@(Var _ (NoQ x)) n) e2 : b)
  | x == self                           = do (cs1,t1,e1') <- infer env e1
                                             t2 <- newTVar
                                             (cs2,e2') <- inferSub env t2 e2
                                             (cs3,te,b') <- infInitEnv env self b
                                             return (Mut (DfltInfo l 64 Nothing []) t1 n t2 :
                                                     cs1++cs2++cs3, te, MutAssign l (Dot l' e1' n) e2' : b')
infInitEnv env self (Expr l e : b)
  | Call{fun=Dot _ (Var _ c) n} <- e,
    isClass env c, n == initKW          = do (cs1,_,e') <- infer env e
                                             (cs2,te,b') <- infInitEnv env self b
                                             return (cs1++cs2, te, Expr l e' : b')
infInitEnv env self b                   = infSuiteEnv env b

abstractDefs env q eq b                 = map absDef b
  where absDef (Decl l ds)               = Decl l (map absDef' ds)
        absDef (If l bs els)            = If l [ Branch e (map absDef ss) | Branch e ss <- bs ] (map absDef els)
        absDef stmt                     = stmt
        absDef' d@Def{}                 = d{ pos = pos1, dbody = bindWits eq ++ dbody d }
          where pos1                    = case pos d of
                                            PosPar nSelf t e p | deco d /= Static ->
                                                PosPar nSelf t e $ qualWPar env q p
                                            p -> qualWPar env q p

instance Check Decl where
    checkEnv env (Def l n q p k a b dec fx)
                                        = do --traceM ("## checkEnv def " ++ prstr n ++ " FX " ++ prstr fx')
                                             t <- maybe newTVar return a
                                             pushFX fx' t
                                             st <- newTVar
                                             wellformed env1 q
                                             wellformed env1 a
                                             when (inClass env) $ 
                                              --   tryUnify (DfltInfo l 600 Nothing []) tSelf $ selfType p k dec
                                                 tryUnify (Simple l "Type of first parameter of class method does not unify with Self") tSelf $ selfType p k dec
                                             (csp,te0,p') <- infEnv env1 p
                                             (csk,te1,k') <- infEnv (define te0 env1) k
                                             (csb,_,b') <- infDefBody (define te1 (define te0 env1)) n p' k' b
                                             popFX
                                             let cst = if fallsthru b then [Cast (DfltInfo l 65 Nothing []) tNone t] else []
                                                 t1 = tFun fx' (prowOf p') (krowOf k') t
                                             (cs1,eq1) <- solveScoped env1 tvs [] t1 (csp++csk++csb++cst)
                                             checkNoEscape l env tvs
                                             -- At this point, n has the type given by its def annotations.
                                             -- Now check that this type is no less general than its recursion assumption in env.
                                             let body = bindWits eq1 ++ defaultsP p' ++ defaultsK k' ++ b'
                                             (cs,def) <- matchDefAssumption env cs1 (Def l n q p' k' (Just t) body dec fx')
                                             return (cs, def{ pos = noDefaultsP (pos def), kwd = noDefaultsK (kwd def) })
      where env1                        = reserve (bound (p,k) ++ assigned b \\ stateScope env) $ defineTVars q env
            tvs                         = qbound q
            fx'                         = fxUnwrap env fx

    checkEnv env (Actor l n q p k b)    = do --traceM ("## checkEnv actor " ++ prstr n)
                                             pushFX fxProc tNone
                                             wellformed env1 q
                                             (csp,te1,p') <- infEnv env1 p
                                             (csk,te2,k') <- infEnv (define te1 env1) k
                                             (csb,te,b') <- (if stub env then infEnv else infSuiteEnv) (define te2 $ define te1 env1) b
                                             (cs0,eq0) <- matchActorAssumption env1 n p' k' te
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 (tvSelf:tvs) te tNone (csp++csk++csb++cs0)
                                             checkNoEscape l env (tvSelf:tvs)
                                             fvs <- tyfree <$> msubst env
                                             let body = bindWits (eq1++eq0) ++ defaultsP p' ++ defaultsK k' ++ b'
                                                 act = Actor l n (noqual env q) (qualWPar env q p') k' body
                                             return (cs1, act{ pos = noDefaultsP (pos act), kwd = noDefaultsK (kwd act) })
      where env1                        = reserve (bound (p,k) ++ assigned b) $ defineTVars q $
                                          define [(selfKW, NVar t0)] $ setInAct env
            t0                          = tCon $ TC (NoQ n) (map tVar tvs)
            tvs                         = qbound q
            NAct _ _ _ te0 _            = findName n env

    checkEnv' env (Class l n q us b)    = do --traceM ("## checkEnv class " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             wellformed env1 us
                                             (csb,b') <- checkEnv (define (subst s te) env1) b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 (tvSelf:tvs) te tNone csb
                                             checkNoEscape l env (tvSelf:tvs)
                                             return (cs1, [Class l n (noqual env q) (map snd as) (abstractDefs env q eq1 b')])
      where env1                        = defineSelf (NoQ n) q $ defineTVars q $ setInClass env
            tvs                         = tvSelf : qbound q
            NClass _ as te _            = findName n env
            s                           = [(tvSelf, tCon (TC (NoQ n) (map tVar $ qbound q)))]

    checkEnv' env (Protocol l n q us b) = do --traceM ("## checkEnv protocol " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             (csu,wmap) <- wellformedProtos env1 us
                                             (csb,b') <- checkEnv (define te env1) b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 (tvSelf:tvs) te tNone (csu++csb)
                                             checkNoEscape l env (tvSelf:tvs)
                                             b' <- msubst b'
                                             return (cs1, convProtocol env n q ps eq1 wmap b')
      where env1                        = defineSelf (NoQ n) q $ defineTVars q $ setInClass env
            tvs                         = tvSelf : qbound q
            NProto _ ps te _            = findName n env

    checkEnv' env (Extension l q c us b)
      | isActor env n                   = notYet (loc n) "Extension of an actor"
      | isProto env n                   = notYet (loc n) "Extension of a protocol"
      | otherwise                       = do --traceM ("## checkEnv extension " ++ prstr n ++ "(" ++ prstrs us ++ ")")
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             (csu,wmap) <- wellformedProtos env1 us
                                             (csb,b') <- checkEnv (define (subst s te) env1) b
                                             popFX
                                             (cs1,eq1) <- solveScoped env1 (tvSelf:tvs) te tNone (csu++csb)
                                             checkNoEscape l env (tvSelf:tvs)
                                             b' <- msubst b'
                                             return (cs1, convExtension env n' c q ps eq1 wmap b')
      where env1                        = defineInst c ps thisKW' $ defineSelf n q $ defineTVars q $ setInClass env
            tvs                         = tvSelf : qbound q
            n                           = tcname c
            n'                          = extensionName us c
            NExt _ _ ps te _            = findName n' env
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
                                             return ([], Signature l ns sc' dec')
      where TSchema l q t               = sc
            sc' | null q                = sc
                | otherwise             = let TFun l' x p k t' = t in TSchema l (noqual env q) (TFun l' x (qualWRow env q p) k t')
            dec'                        = if inClass env && isProp dec sc then Property else dec
            env1                        = defineTVars q env
    checkEnv env s                      = return ([], s)

instance Check Branch where
    checkEnv env (Branch e b)           = do (cs,b') <- checkEnv env b
                                             return (cs, Branch e b')

--------------------------------------------------------------------------------------------------------------------------

refine                                  :: Env -> Constraints -> TEnv -> Equations -> TypeM (Constraints, [TVar], Constraints, TEnv, Equations)
refine env cs te eq
  | not $ null solve_cs                 = do --traceM ("  #solving: " ++ prstrs solve_cs)
                                             (cs',eq') <- solve env doSolve te tNone eq cs
                                             refineAgain cs' eq'
  | not $ null ambig_vs                 = do --traceM ("  #defaulting: " ++ prstrs ambig_vs)
                                             (cs',eq') <- solve env doDefault te tNone eq cs
                                             refineAgain cs' eq'
  | not $ null tail_vs                  = do sequence [ tryUnify (Simple NoLoc "internal") (tVar v) (tNil $ tvkind v) | v <- tail_vs ]
                                             refineAgain cs eq
  | otherwise                           = do eq <- msubst eq
                                             let (fix_cs, gen_cs) = partition fixed cs
                                             return (fix_cs, gen_vs, gen_cs, te, eq)
  where fix_vs                          = closeDepVars (tyfree env ++ fxfree te ++ fxfree cs) cs
        ambig_vs                        = tyfree cs \\ closeDepVars (fix_vs++safe_vs) cs
        tail_vs                         = gen_vs `intersect` (tailvars te ++ tailvars cs)

        safe_vs                         = if null def_vss then [] else nub $ foldr1 intersect def_vss
        def_vss                         = [ nub $ filter canGen $ tyfree sc | (_, NDef sc _ _) <- te, null $ scbind sc ]
        gen_vs                          = nub (foldr union (tyfree cs) def_vss) \\ fix_vs

        solve_cs                        = [ c | c <- cs, doSolve c ]

        fixed c                         = all (`elem` fix_vs) $ tyfree c

        doSolve c                       = not (canQual c) && not (fixed c)
        doDefault c                     = isAmbig c && not (fixed c)

        canQual (Impl _ _ (TVar _ v) p) = univar v
        canQual c                       = False

        canGen tv                       = tvkind tv /= KFX

        isAmbig c                       = any (`elem` ambig_vs) (tyfree c)

        refineAgain cs eq               = do (cs1,eq1) <- simplify env te tNone cs
                                             te <- msubst te
                                             env <- msubst env
                                             refine env cs1 te (eq1++eq)

fxfree te                               = [ tv | tv <- tyfree te, tvkind tv == KFX ]

genEnv                                  :: Env -> Constraints -> TEnv -> [Decl] -> TypeM (Constraints,TEnv,Equations,[Decl])
genEnv env cs te ds
  | any typeDecl te                     = do te <- msubst te
                                             --traceM ("## genEnv types 1\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("## genEnv types cs:\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                             (cs,eq) <- simplify (posdefine te env) te tNone cs
                                             te <- msubst te
                                             --traceM ("## genEnv types 2\n" ++ render (nest 6 $ pretty te))
                                             return (cs, te, eq, ds)
  | onTop env                           = do te <- msubst te
                                             --traceM ("## genEnv defs 1\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("## genEnv defs cs:\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                             (cs,eq) <- simplify env te tNone cs
                                             te <- msubst te
                                             env <- msubst env
                                             (fix_cs, gen_vs, gen_cs, te, eq) <- refine env cs te eq
                                             --traceM ("## genEnv defs 2 [" ++ prstrs gen_vs ++ "]\n" ++ render (nest 6 $ pretty te))
                                             let (q,ws) = qualify gen_vs gen_cs
                                                 te1 = map (generalize gen_vs q) te
                                                 (eq1,eq2) = splitEqs (dom ws) eq
                                                 ds1 = map (abstract q ds ws eq1) ds
                                             --traceM ("## genEnv defs 3 [" ++ prstrs gen_vs ++ "]\n" ++ render (nest 6 $ pretty te1))
                                             return (fix_cs, te1, eq2, ds1)
  | otherwise                           = do --traceM ("## genEnv local\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("## genEnv local cs:\n" ++ render (nest 4 $ vcat $ map pretty cs))
                                             return (cs, te, [], ds)
  where
    qualify vs cs                       = let (q,wss) = unzip $ map qbind vs in (q, concat wss)
      where qbind v                     = (Quant v (casts ++ impls), wits)
              where casts               = [ u | Cast _ (TVar _ v') (TCon _ u) <- cs, v == v' ]
                    impls               = [ p | Impl _ w (TVar _ v') p <- cs, v == v' ]
                    wits                = [ (w, impl2type t p) | Impl _ w t@(TVar _ v') p <- cs, v == v' ]

    generalize vs q (n, NDef sc d doc)
      | null $ scbind sc                = (n, NDef (tSchema q1 (sctype sc)) d doc)
      where q1                          = [ qv | qv@(Quant v _) <- q, v `elem` vs ]
    generalize vs q (n, i)              = (n, i)


    abstract q ds ws eq d@Def{}
      | null $ qbinds d                 = d{ qbinds = noqual env q, 
                                             pos = wit2par ws (pos d),
                                             dbody = bindWits eq ++ wsubst ds q ws (dbody d) }
      | otherwise                       = d{ dbody = bindWits eq ++ wsubst ds q ws (dbody d) }

    wsubst ds [] []                     = id
    wsubst ds q ws                      = termsubst s
      where s                           = [ (n, Lambda l0 p k (Call l0 (tApp (eVar n) tvs) (wit2arg ws (pArg p)) (kArg k)) fx) 
                                            | Def _ n [] p k _ _ _ fx <- ds ]
            tvs                         = map tVar $ qbound q

    splitEqs ws eq
      | null eq1                        = ([], eq)
      | otherwise                       = (eq1++eq1', eq2')
      where (eq1,eq2)                   = partition (any (`elem` ws) . free) eq
            (eq1',eq2')                 = splitEqs (bound eq1 ++ ws) eq2


defaultsP (PosPar n (Just t) (Just e) p)
  | e /= eNone                          = set : defaultsP p
  where test                            = eCall (tApp (eQVar primISNONE) [t]) [eVar n]
        set                             = sAssign (pVar n t) (eCond e test (eVar n))
defaultsP (PosPar n _ _ p)              = defaultsP p
defaultsP _                             = []

noDefaultsP (PosPar n (Just t) (Just e) p)
                                        = PosPar n (Just $ tOpt t) Nothing (noDefaultsP p)
noDefaultsP (PosPar n t e p)            = PosPar n t e (noDefaultsP p)
noDefaultsP k                           = k

defaultsK (KwdPar n (Just t) (Just e) k)
  | e /= eNone                          = set : defaultsK k
  where test                            = eCall (tApp (eQVar primISNONE) [t]) [eVar n]
        set                             = sAssign (pVar n t) (eCond e test (eVar n))
defaultsK (KwdPar n _ _ k)              = defaultsK k
defaultsK _                             = []

noDefaultsK (KwdPar n (Just t) (Just e) k)
                                        = KwdPar n (Just $ tOpt t) Nothing (noDefaultsK k)
noDefaultsK (KwdPar n t e k)            = KwdPar n t e (noDefaultsK k)
noDefaultsK k                           = k


--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Branch where
    infEnv env (Branch e b)             = do (cs1,env',s,_,e') <- inferTest env e
                                             (cs2,te,b') <- infEnv env' b
                                             return (cs1++cs2, te, Branch e' (termsubst s b'))

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 66 (Just e) []) w t pContextManager :
                                                     cs, [], WithItem e' Nothing)           -- TODO: translate using w
    infEnv env (WithItem e (Just p))    = do (cs1,t1,e') <- infer env e
                                             (cs2,te,t2,p') <- infEnvT env p
                                             w <- newWitness
                                             return (Cast (DfltInfo (loc e) 67 (Just e) []) t1 t2 :
                                                     Impl (DfltInfo (loc e) 68 (Just e) []) w t1 pContextManager :
                                                     cs1++cs2, te, WithItem e' (Just p'))         -- TODO: translate using w

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do (cs1,te,ex') <- infEnv env ex
                                             (cs2,te1,b') <- infEnv (define te env) b
                                             return (cs1++cs2, exclude te1 (dom te), Handler ex' b')

instance InfEnv Except where
    infEnv env (ExceptAll l)            = return ([], [], ExceptAll l)
    infEnv env (Except l x)             = return ([Cast (DfltInfo l 69 Nothing []) t tException], [], Except l x)
      where t                           = tCon (TC (unalias env x) [])
    infEnv env (ExceptAs l x n)         = return ([Cast (DfltInfo l 70 Nothing []) t tException], [(n, NVar t)], ExceptAs l x n)
      where t                           = tCon (TC (unalias env x) [])

instance Infer Expr where
    infer env x@(Var l n)               = case findQName n env of
                                            NVar t -> return ([], t, x)
                                            NSVar t -> do
                                                fx <- currFX
                                                return ([Cast (Simple l ("State variable may only be accessed in a proc")) fxProc fx], t, x)
                                            NDef sc d _ -> do
                                                (cs,tvs,t) <- instantiate env sc
                                                let e = app t (tApp x tvs) $ witsOf cs
                                                    cs1 = map (addTyping env n sc t) cs
                                                --traceM ("## type of " ++ prstr n ++ " = " ++ prstr t ++ ", cs = " ++ render(commaList cs))
                                                if actorSelf env
                                                    then wrapped l attrWrap env cs1 [tActor,t] [eVar selfKW,e]
                                                    else return (cs1, t, e)
                                            NClass q _ _ _ -> do
                                                (cs0,ts) <- instQBinds env q
                                                --traceM ("## Instantiating " ++ prstr n)
                                                let ns = abstractAttrs env n
                                                when (not $ null ns) (err3 (loc n) ns "Abstract attributes prevent instantiation:")
                                                case findAttr env (TC n ts) initKW of
                                                    Just (_,sc,_) -> do
                                                        (cs1,tvs,t) <- instantiate env sc
                                                        let t0 = tCon $ TC (unalias env n) ts
                                                            t' = subst [(tvSelf,t0)] t{ restype = tSelf }
                                                        return (cs0++cs1, t', app t' (tApp x (ts++tvs)) $ witsOf (cs0++cs1))
                                            NAct q p k _ _ -> do
--                                                when (abstractActor env n) (err1 n "Abstract actor cannot be instantiated:")
                                                (cs,tvs,t) <- instantiate env (tSchema q (tFun fxProc p k (tCon0 (unalias env n) q)))
                                                return (cs, t, app t (tApp x tvs) $ witsOf cs)
                                            NSig _ _ _ -> nameReserved n
                                            NReserved -> nameReserved n
                                            _ -> nameUnexpected n

    infer env e@(Int _ val s)           = do t <- newTVar
                                             w <- newWitness
                                             return ([Impl (DfltInfo (loc e) 72 (Just e) []) w t pNumber], t, eCall (eDot (eVar w) fromatomKW) [e])
    infer env e@(Float _ val s)         = do t <- newTVar
                                             w <- newWitness
                                             return ([Impl (DfltInfo (loc e) 73 (Just e) []) w t pRealFloat], t, eCall (eDot (eVar w) fromatomKW) [e])
    infer env e@Imaginary{}             = notYetExpr e
    infer env e@(Bool _ val)            = return ([], tBool, e)
    infer env e@(None _)                = return ([], tNone, e)
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env e@(Strings _ ss)          = return ([], tStr, e)
    infer env e@(BStrings _ ss)         = return ([], tBytes, e)
    infer env (Call l e ps ks)          = inferCall env True l e ps ks
    infer env (TApp l e ts)             = internal l "Unexpected TApp in infer"
    infer env (Async l e)               = do (cs,t,e) <- infer env e                        -- expect an action returning t'
                                             prow <- newTVarOfKind PRow
                                             krow <- newTVarOfKind KRow
                                             t' <- newTVar
                                             let tf fx = tFun fx prow krow
                                             return (Cast (DfltInfo (loc e) 74 (Just e) []) t (tf fxAction t') :
                                                     cs, tf fxProc (tMsg t'), Async l e)    -- produce a proc returning Msg[t']
    infer env (Await l e)               = do t0 <- newTVar
                                             (cs1,e') <- inferSub env (tMsg t0) e
                                             fx <- currFX
                                             return (Cast (DfltInfo (loc e) 75 (Just e) []) fxProc fx :
                                                     cs1, t0, Await l e')
    infer env (Index l e ix)            = do ti <- newTVar
                                             (cs1,ix') <- inferSub env ti ix
                                             t0 <- newTVar
                                             w <- newWitness
                                             (cs2,t,e') <- infer env e
                                             return (Impl (DfltInfo (loc e) 76 (Just e) []) w t (pIndexed ti t0) :
                                                     cs1++cs2, t0, eCall (eDot (eVar w) getitemKW) [e', ix'])
    infer env (Slice l e sl)            = do (cs1,sl') <- inferSlice env sl
                                             (cs2,t,e') <- infer env e
                                             t0 <- newTVar
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 77 (Just e) []) w t (pSliceable t0) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) getsliceKW) [e', sliz2exp sl'])
    infer env (NDSlice l e slz)         = do (css,es) <- fmap unzip $ mapM (inferNDSlice env) slz
                                             t <- newTVar
                                             (cs,e') <- inferSub env (tNDArray t) e
                                             return (concat css++cs, (tNDArray t), eCall (eDot e' ndgetsliceKW) [List l (map Elem es)])
    infer env (Cond l e1 e e2)          = do t0 <- newTVar
                                             (cs0,env',s,_,e') <- inferTest env e
                                             (cs1,e1') <- inferSub env' t0 e1
                                             (cs2,e2') <- inferSub env t0 e2
                                             return (cs0++cs1++cs2, t0, Cond l (termsubst s e1') e' e2')
    infer env (IsInstance l e c)        = case findQName c env of
                                             NClass q _ _ _ -> do
                                                (cs,t,e') <- infer env e
                                                ts <- newTVars [ tvkind v | v <- qbound q ]
                                                return (cs, tBool, IsInstance l e' c)
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
    infer env e@(BinOp l e1 op e2)
      | op `elem` [Or,And]              = do (cs,_,_,t,e') <- inferTest env e
                                             return (cs, t, e')
      | op == Mult                      = do t <- newTVar
                                             t' <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t' e2
                                             w <- newWitness
                                             return (Impl (DfltInfo l 79 (Just e) []) w t (pTimes t') :
                                                     cs1++cs2, t, eCall (eDot (eVar w) mulKW) [e1',e2'])
      | op == Div                       = do t <- newTVar
                                             t' <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t e2
                                             w <- newWitness
                                             return (Impl (DfltInfo l 80 (Just e) []) w t (pDiv t') :
                                                     cs1++cs2, t', eCall (eDot (eVar w) truedivKW) [e1',e2'])
      | otherwise                       = do t <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env (rtype op t) e2
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 81 (Just e) []) w t (protocol op) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Plus               = pPlus
            protocol Minus              = pMinus
            protocol Pow                = pNumber
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
            method Pow                  = powKW
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
      | op == Not                       = do (cs,_,_,_,e') <- inferTest env e
                                             return (cs, tBool, UnOp l op e')
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 82 (Just e) []) w t (protocol op) :
                                                     cs, t, eCall (eDot (eVar w) (method op)) [e'])
      where protocol UPlus              = pNumber
            protocol UMinus             = pNumber
            protocol BNot               = pIntegral
            method UPlus                = posKW
            method UMinus               = negKW
            method BNot                 = invertKW
    infer env e@(CompOp l e1 [OpArg op e2])
      | op `elem` [In,NotIn]            = do t1 <- newTVar
                                             (cs1,e1') <- inferSub env t1 e1
                                             t2 <- newTVar
                                             (cs2,e2') <- inferSub env t2 e2
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e1) 83 (Just e) []) w t2 (pContainer t1) :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w) (method op)) [e2', e1'])
      | otherwise                       = do t <- newTVar
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t e2
                                             w <- newWitness
                                             return (Impl (DfltInfo l 84 (Just e) []) w t (protocol op) :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w) (method op)) [e1',e2'])
                                             -- TODO: This gives misleading error msg; it says that "e1 op e2 must implement protocol op"
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

    infer env (Dot l x@(Var _ c) n)
      | NClass q us te _ <- cinfo       = do (cs0,ts) <- instQBinds env q
                                             let tc = TC c' ts
                                             case findAttr env tc n of
                                                Just (_,sc,dec)
                                                  | dec == Just Property -> err l "Property attribute not selectable by class"
                                                  | abstractAttr env tc n -> err l "Abstract attribute not selectable by class"
                                                  | otherwise -> do
                                                      (cs1,tvs,t) <- instantiate env sc
                                                      let t' = subst [(tvSelf,tCon tc)] $ addSelf t dec
                                                      return (cs0++cs1, t', app2nd dec t' (tApp (Dot l x n) (ts++tvs)) $ witsOf (cs0++cs1))
                                                Nothing ->
                                                    case findProtoByAttr env c' n of
                                                        Just p -> do
                                                            p <- instwildcon env p
                                                            we <- eVar <$> newWitness
                                                            let Just (wf,sc,dec) = findAttr env p n
                                                            (cs2,tvs,t) <- instantiate env sc
                                                            let t' = subst [(tvSelf,tCon tc)] $ addSelf t dec
                                                            return (cs2, t', app t' (tApp (eDot (wf we) n) tvs) $ witsOf cs2)
                                                        Nothing -> err1 l "Attribute not found"
      | NProto q us te _ <- cinfo       = do (_,ts) <- instQBinds env q
                                             let tc = TC c' ts
                                             case findAttr env tc n of
                                                Just (wf,sc,dec) -> do
                                                    (cs1,tvs,t) <- instantiate env sc
                                                    t0 <- newTVar
                                                    let t' = subst [(tvSelf,t0)] $ addSelf t dec
                                                    w <- newWitness
                                                    return (Impl (DfltInfo l 85 Nothing []) w t0 tc :
                                                            cs1, t', app t' (tApp (Dot l (wf $ eVar w) n) tvs) $ witsOf cs1)
                                                Nothing -> err1 l "Attribute not found"
      where c'                          = unalias env c
            cinfo                       = findQName c' env

    infer env (Dot l e n)
      | n == initKW                     = err1 n "__init__ cannot be selected by instance"
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             t0 <- newTVar
                                             let con = case t of
                                                          TOpt _ _ -> Sel (Simple l (Pretty.print t ++ " does not have an attribute "++ Pretty.print n ++
                                                                           "\nHint: you may need to test if " ++ Pretty.print e ++ " is not None")) w t n t0
                                                          _ -> Sel (DfltInfo l 86 (Just e) []) w t n t0
                                             return  (con : cs, t0, eCall (eVar w) [e'])

    infer env e@(Rest _ _ _)            = notYetExpr e
--    infer env (Rest l e n)              = do p <- newTVarOfKind PRow
--                                             k <- newTVarOfKind KRow
--                                             t0 <- newTVar
--                                             (cs,e') <- inferSub env (tTuple p (kwdRow n t0 k)) e
--                                             return (cs, tTuple p k, Rest l e' n)

    infer env (DotI l e i)              = do (tup,ti,_) <- tupleTemplate i
                                             (cs,e') <- inferSub env tup e
                                             return (cs, ti, DotI l e' i)

    infer env e@(RestI _ _ _)           = notYetExpr e
--    infer env (RestI l e i)             = do (tup,_,rest) <- tupleTemplate i
--                                             (cs,e') <- inferSub env tup e
--                                             return (cs, rest, RestI l e' i)

    infer env (Lambda l p k e fx)
      | nodup (p,k)                     = do pushFX fx tNone
                                             (cs0,te0,p') <- infEnv env1 p
                                             (cs1,te1,k') <- infEnv (define te0 env1) k
                                             let env2 = define te1 $ define te0 env1
                                             (cs2,t,e') <- case e of
                                                             Call l' e' ps ks -> inferCall env2 False l' e' ps ks
                                                             _ -> infer env2 e
                                             popFX
                                             return (cs0++cs1++cs2, tFun fx (prowOf p') (krowOf k') t, Lambda l (noDefaultsP p') (noDefaultsK k') e' fx)
                                                     -- TODO: replace defaulted params with Conds
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs kargs)     = do (cs1,prow,pargs') <- infer env pargs
                                             (cs2,krow,kargs') <- infer env kargs
                                             return (cs1++cs2, TTuple l prow krow, Tuple l pargs' kargs')
    infer env (List l es)               = do t0 <- newTVar
                                             (cs,es') <- infElems env es t0
                                             return (cs, tList t0, List l es')
    infer env (ListComp l e co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e] t0
                                             let [e'] = es
                                             return (cs1++cs2, tList t0, ListComp l e' co')
    infer env (Set l es)                = do t0 <- newTVar
                                             (cs,es')  <- infElems env es t0
                                             w <- newWitness
                                             return (Impl (DfltInfo l 87 Nothing []) w t0 pHashable : cs, tSet t0, eCall (tApp (eQVar primMkSet) [t0]) [eVar w,Set l es'])
    infer env (SetComp l e co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             t0 <- newTVar
                                             (cs2,es) <- infElems (define te env) [e] t0
                                             w <- newWitness
                                             let Elem v = head es
                                                 e' = Elem (annot (tHashableW t0) (eVar w) t0 v)
                                             return (Impl (DfltInfo l 89 Nothing []) w t0 pHashable : cs1++cs2, tSet t0, SetComp l e' co')
    infer env (Dict l as)               = do tk <- newTVar
                                             tv <- newTVar
                                             (cs,as') <- infAssocs env as tk tv
                                             w <- newWitness 
                                             return (Impl (DfltInfo l 88 Nothing []) w tk pHashable : cs, tDict tk tv, eCall (tApp (eQVar primMkDict) [tk, tv]) [eVar w,Dict l as'])
    infer env (DictComp l a co)
      | nodup co                        = do (cs1,te,co') <- infEnv env co
                                             tk <- newTVar
                                             tv <- newTVar
                                             (cs2,as) <- infAssocs (define te env) [a] tk tv
                                             w <- newWitness
                                             let Assoc k v = head as
                                                 a' = Assoc (annot (tHashableW tk) (eVar w) tk k) v
                                             return (Impl (DfltInfo l 90 Nothing []) w tk pHashable : cs1++cs2, tDict tk tv, DictComp l a' co')

    infer env (Paren l e)               = do (cs,t,e') <- infer env e
                                             return (cs, t, Paren l e')

inferCall env unwrap l e ps ks          = do (cs1,t,e') <- infer env e{eloc = l}
                                             (cs1,t,e') <- if unwrap && actorSelf env then wrapped l attrUnwrap env cs1 [t] [e'] else pure (cs1,t,e')
                                             (cs2,prow,ps') <- infer env ps
                                             (cs3,krow,ks') <- infer env ks
                                             t0 <- newTVar
                                             fx <- currFX
                                             w <- newWitness
                                             let i = case e of
                                                        Var _ n@(NoQ n')
                                                          | NDef sc _ _ <- findQName n env,
                                                            Just l2 <- findDefLoc n' env ->
                                                                   DeclInfo l l2 n' sc ("Type incompatibility between definition of and call of "++Pretty.print n')
                                                        _ -> DfltInfo l 837 (Just (Call l e ps ks)) []
                                             return (Sub i w t (tFun fx prow krow t0)  :
                                            -- return (Sub (DfltInfo l 837 (Just (Call l e ps ks)) []) w t (tFun fx prow krow t0) :
                                                     cs1++cs2++cs3, t0, Call l (eCall (eVar w) [e']) ps' ks')



tupleTemplate i                         = do ts <- mapM (const newTVar) [0..i]              -- Handle DotI or RestI...
                                             p <- newTVarOfKind PRow
                                             k <- newTVarOfKind KRow
                                             let p0 = foldl (flip posRow) p ts
                                                 p1 = foldl (flip posRow) p (tail ts)
                                             return (TTuple NoLoc p0 k, head ts, TTuple NoLoc p1 k)


infElems env [] t0                      = return ([], [])
infElems env (Elem e : es) t0           = do (cs1,e') <- inferSub env t0 e
                                             (cs2,es') <- infElems env es t0
                                             return (cs1++cs2, Elem e' : es')
infElems env (Star e : es) t0           = do t1 <- newTVar
                                             (cs1,e') <- inferSub env t1 e
                                             (cs2,es') <- infElems env es t0
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 89 (Just e) []) w t1 (pIterable t0) :
                                                     cs1++cs2, Star e' : es')
                                                     

infAssocs env [] tk tv                  = return ([], [])
infAssocs env (Assoc k v : as) tk tv    = do (cs1,k') <- inferSub env tk k
                                             (cs2,v') <- inferSub env tv v
                                             (cs3,as') <- infAssocs env as tk tv
--                                             return (cs1++cs2++cs3, Elem (eTuple [k',v']) : as')
                                             return (cs1++cs2++cs3, Assoc k' v' : as')
infAssocs env (StarStar e : as) tk tv   = do t1 <- newTVar
                                             (cs1,e') <- inferSub env t1 e
                                             (cs2,as') <- infAssocs env as tk tv
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 90 (Just e) []) w t1 (pIterable $ tTupleP $ posRow tk $ posRow tv posNil) :
--                                                     cs1++cs2, Star e' : as')
                                                     cs1++cs2, StarStar e' : as')


inferTest env (BinOp l e1 And e2)       = do (cs1,env1,s1,t1,e1') <- inferTest env e1
                                             (cs2,env2,s2,t2,e2') <- inferTest env1 e2
                                             t <- newTVar
                                             w1 <- newWitness
                                             w2 <- newWitness
                                             return (Sub (DfltInfo l 91 (Just e1) []) w1 t1 t : Sub (DfltInfo l 92 (Just e2) []) w2 t2 t :
                                                     cs1++cs2, env2, s1++s2, t, BinOp l (eCall (eVar w1) [e1']) And (eCall (eVar w2) [termsubst s1 e2']))
inferTest env (BinOp l e1 Or e2)        = do (cs1,_,_,t1,e1') <- inferTest env e1
                                             (cs2,_,_,t2,e2') <- inferTest env e2
                                             t <- newTVar
                                             w1 <- newWitness
                                             w2 <- newWitness
                                             return (Sub (DfltInfo (loc e1) 93 (Just e1) []) w1 t1 (tOpt t) : Sub (DfltInfo (loc e1) 94 (Just e2) []) w2 t2 t :
                                                     cs1++cs2, env, [], t, BinOp l (eCall (eVar w1) [e1']) Or (eCall (eVar w2) [e2']))
inferTest env (UnOp l Not e)            = do (cs,_,_,_,e') <- inferTest env e
                                             return (cs, env, [], tBool, UnOp l Not e')
inferTest env (CompOp l e1 [OpArg op e2])
  | Just n <- isNotNone e1 op e2        = do t <- newTVar
                                             (cs1,e') <- inferSub env (tOpt t) (eVar n)
                                             return (cs1, define [(n,NVar t)] env, sCast n (tOpt t) t, tBool, eCall (tApp (eQVar primISNOTNONE) [t]) [e'])
  | Just n <- isNone e1 op e2           = do t <- newTVar
                                             (cs1,e') <- inferSub env (tOpt t) (eVar n)
                                             return (cs1, env, [], tBool, eCall (tApp (eQVar primISNONE) [t]) [e'])
inferTest env (IsInstance l e@(Var _ (NoQ n)) c)
                                        = case findQName c env of
                                             NClass q _ _ _ -> do
                                                (cs,t,e') <- infer env e
                                                ts <- newTVars [ tvkind v | v <- qbound q ]
                                                let tc = tCon (TC c ts)
                                                return (cs, define [(n,NVar tc)] env, sCast n t tc, tBool, IsInstance l e' c)
                                             _ -> nameUnexpected c
inferTest env (Paren l e)               = do (cs,env',s,t,e') <- inferTest env e
                                             return (cs, env', s, t, Paren l e')
inferTest env e                         = do (cs,t,e') <- infer env e
                                             return (cs, env, [], t, e')


isNotNone (Var _ (NoQ n)) IsNot None{}  = Just n
isNotNone (Var _ (NoQ n)) NEq None{}    = Just n
isNotNone None{} IsNot (Var _ (NoQ n))  = Just n
isNotNone None{} NEq (Var _ (NoQ n))    = Just n
isNotNone e op e'                       = Nothing

isNone (Var _ (NoQ n)) Is None{}        = Just n
isNone (Var _ (NoQ n)) Eq None{}        = Just n
isNone None{} Is (Var _ (NoQ n))        = Just n
isNone None{} Eq (Var _ (NoQ n))        = Just n
isNone e op e'                          = Nothing


sCast n t t'                            = [(n, eCAST t t' (eVar n))]

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
                                             return (Sub (DfltInfo (loc e) 96 (Just e) []) w t' t : cs, eCall (eVar w) [e'])

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
                                             wellformed env t
                                             let t' = t -- {tloc = loc n}
                                             (cs,te,p') <- infEnv (define [(n, NVar t')] env) p
                                             return (cs, (n, NVar t'):te, PosPar n (Just t') Nothing p')
    infEnv env (PosPar n a (Just e) p)  = do t <- maybe newTVar return a
                                             wellformed env t
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,p') <- infEnv (define [(n, NVar t)] env) p
                                             return (cs1++cs2, (n, NVar t):te, PosPar n (Just t) (Just e') p')
    infEnv env (PosSTAR n a)            = do t <- maybe newTVar return a
                                             wellformed env t
                                             r <- newTVarOfKind PRow
                                             return ([Cast (DfltInfo (loc n) 97 Nothing []) t (tTupleP r)], [(n, NVar t)], PosSTAR n (Just $ tTupleP r))
    infEnv env PosNIL                   = return ([], [], PosNIL)

instance InfEnv KwdPar where
    infEnv env (KwdPar n a Nothing k)   = do t <- maybe newTVar return a
                                             wellformed env t
                                             (cs,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (cs, (n, NVar t):te, KwdPar n (Just t) Nothing k')
    infEnv env (KwdPar n a (Just e) k)  = do t <- maybe newTVar return a
                                             wellformed env t
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (cs1++cs2, (n, NVar t):te, KwdPar n (Just t) (Just e') k')
    infEnv env (KwdSTAR n a)            = do t <- maybe newTVar return a
                                             wellformed env t
                                             r <- newTVarOfKind KRow
                                             return ([Cast (DfltInfo (loc n) 98 Nothing []) t (tTupleK r)], [(n, NVar t)], KwdSTAR n (Just $ tTupleK r))
    infEnv env KwdNIL                   = return ([], [], KwdNIL)

---------

instance Infer PosArg where
    infer env (PosArg e p)              = do (cs1,t,e') <- infer env e
                                             (cs2,prow,p') <- infer env p
                                             return (cs1++cs2, posRow t prow, PosArg e' p')
    infer env (PosStar e)               = do prow <- newTVarOfKind PRow
                                             (cs,e') <- inferSub env (tTupleP prow) e
                                             return (cs, posStar prow, PosStar e')
    infer env PosNil                    = return ([], posNil, PosNil)
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do (cs1,t,e') <- infer env e
                                             (cs2,krow,k') <- infer env k
                                             return (cs1++cs2, kwdRow n t krow, KwdArg n e' k')
    infer env (KwdStar e)               = do krow <- newTVarOfKind KRow
                                             (cs,e') <- inferSub env (tTupleK krow) e
                                             return (cs, kwdStar krow, KwdStar e')
    infer env KwdNil                    = return ([], kwdNil, KwdNil)

instance InfEnv Comp where
    infEnv env NoComp                   = return ([], [], NoComp)
    infEnv env (CompIf l e c)           = do (cs1,env',s,_,e') <- inferTest env e
                                             (cs2,te,c') <- infEnv env' c
                                             return (cs1++cs2, te, CompIf l e' (termsubst s c'))
    infEnv env (CompFor l p e c)        = do (cs1,te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             t2 <- newTVar
                                             (cs2,e') <- inferSub env t2 e
                                             (cs3,te2,c') <- infEnv (define te1 env) c
                                             w <- newWitness
                                             return (Impl (DfltInfo (loc e) 101 (Just e) []) w t2 (pIterable t1) :
                                                     cs1++cs2++cs3, te1++te2, CompFor l p' (eCall (eDot (eVar w) iterKW) [e']) c')

instance InfEnvT PosPat where
    infEnvT env (PosPat p ps)           = do (cs1,te1,t,p') <- infEnvT env p
                                             (cs2,te2,r,ps') <- infEnvT env ps
                                             return (cs1++cs2, te1++te2, posRow t r, PosPat p' ps')
    infEnvT env (PosPatStar p)          = do (cs,te,t,p') <- infEnvT env p
                                             r <- newTVarOfKind PRow
                                             return (Cast (DfltInfo (loc p) 102 Nothing []) t (tTupleP r) :
                                                     cs, te, posStar r, PosPatStar p')
    infEnvT env PosPatNil               = return ([], [], posNil, PosPatNil)


instance InfEnvT KwdPat where
    infEnvT env (KwdPat n p ps)         = do (cs1,te1,t,p') <- infEnvT env p
                                             (cs2,te2,r,ps') <- infEnvT env ps
                                             return (cs1++cs2, te1++te2, kwdRow n t r, KwdPat n p' ps')
    infEnvT env (KwdPatStar p)          = do (cs,te,t,p') <- infEnvT env p
                                             r <- newTVarOfKind KRow
                                             return (Cast (DfltInfo (loc p) 103 Nothing []) t (tTupleK r) :
                                                     cs, te, kwdStar r, KwdPatStar p')
    infEnvT env KwdPatNil               = return ([], [], kwdNil, KwdPatNil)



instance InfEnvT Pattern where
    infEnvT env (PWild l a)             = do t <- maybe newTVar return a
                                             wellformed env t
                                             return ([], [], t, PWild l (Just t))
    infEnvT env (PVar l n a)            = do t <- maybe newTVar return a
                                             wellformed env t
                                             case findName n env of
                                                 NReserved -> do
                                                     --traceM ("## infEnvT " ++ prstr n ++ " : " ++ prstr t)
                                                     return ([], [(n, NVar t)], t, PVar l n (Just t))
                                                 NSig (TSchema _ [] t') _ _
                                                   | TFun{} <- t' -> notYet l "Pattern variable with previous function signature"
                                                   | otherwise -> do
                                                     --traceM ("## infEnvT (sig) " ++ prstr n ++ " : " ++ prstr t ++ " < " ++ prstr t')
                                                     return ([Cast (DfltInfo l 104 Nothing []) t t'], [(n, NVar t')], t, PVar l n (Just t))
                                                 NVar t'
                                                   | isJust a -> do
                                                     return ([], [], t, PVar l n (Just t))
                                                   | otherwise ->
                                                     return ([], [], t', PVar l n Nothing)
                                                 NSVar t' -> do
                                                     fx <- currFX
                                                     return (Cast (DfltInfo l 106 Nothing []) fxProc fx :
                                                             Cast (DfltInfo l 107 Nothing []) t t' : 
                                                             [], [], t, PVar l n Nothing)
                                                 _ -> 
                                                     err1 n "Variable not assignable:"
    infEnvT env (PTuple l ps ks)        = do (cs1,te1,prow,ps') <- infEnvT env ps
                                             (cs2,te2,krow,ks') <- infEnvT env ks
                                             return (cs1++cs2, te1++te2, TTuple NoLoc prow krow, PTuple l ps' ks')
    infEnvT env (PList l ps p)          = do (cs1,te1,t1,ps') <- infEnvT env ps
                                             (cs2,te2,t2,p') <- infEnvT (define te1 env) p
                                             w <- newWitness
                                             return (Impl (DfltInfo l 108 Nothing []) w t2 (pSequence t1) :
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
                                             tryUnify (DfltInfo (loc p) 109 Nothing []) t1 t2
                                             return (cs1++cs2, te1++te2, t1, p':ps')



-- Test discovery --------------------------------------------------------------

tEnv                                    = tCon (TC (gname [name "__builtin__"] (name "Env")) []) 
emptyDict                               = Dict NoLoc []

testStmts (uts, sats, ssts, sts, aats, ats, oets, ets) = [dictAssign "__unit_tests" "UnitTest" uts,
                                           dictAssign "__sync_actor_tests" "SyncActorTest" sats,
                                           dictAssign "__simple_sync_tests" "SimpleSyncTest" ssts,
                                           dictAssign "__sync_tests" "SyncTest" sts,
                                           dictAssign "__async_actor_tests" "AsyncActorTest" aats,
                                           dictAssign "__async_tests" "AsyncTest" ats,
                                           dictAssign "__old_env_tests" "OldEnvTest" oets,
                                           dictAssign "__env_tests" "EnvTest" ets,
                                           testActor]

finalStmts env m ss =
    let (uts, sats, ssts, sts, aats, ats, oets, ets) = testFuns env m ss
    in testStmts (mkDict "UnitTest" uts, mkDict "SyncActorTest" sats, mkDict "SimpleSyncTest" ssts, mkDict "SyncTest" sts, mkDict "AsyncActorTest" aats, mkDict "AsyncTest" ats, mkDict "OldEnvTest" oets, mkDict "EnvTest" ets)

gname ns n                              = GName (ModName ns) n
dername a b                             = Derived (name a) (name b)

dictAssign dictname cl dict             = sAssign (pVar (name dictname) (tDict tStr (testing cl))) dict
 
testing tstr                            = tCon (TC (gname [name "testing"] (name tstr)) [])

mkDict cl as                            = eCall (tApp (eQVar primMkDict) [tStr, testing cl]) [w,Dict NoLoc as]
    where w                             = eCall (eQVar (gname [name "__builtin__"] (dername "Hashable" "str"))) []
          
testActor                               = sDecl [Actor NoLoc (name "__test_main") []
                                                 PosNIL (KwdPar (name "env")  (Just tEnv) Nothing KwdNIL)
                                             [sExpr (eCall (eQVar (gname [name "testing"] (name "test_runner")))
                                                           (map (eVar . name) ["env","__unit_tests","__sync_actor_tests","__simple_sync_tests","__sync_tests","__async_actor_tests","__async_tests","__old_env_tests","__env_tests"]))]]

row2list (TRow _ _ _ t r)               = t : row2list r
row2list (TNil _ _)                     = []

mkAssoc d testType modName =
    Assoc (Strings NoLoc [nstr (dname d)])
          (eCall (eQVar (gname [name "testing"] testType))
                 [ eVar (dname d)
                 , Strings NoLoc [drop 6 (nstr (dname d))]
                 , comment (dbody d)
                 , Strings NoLoc [modName]
                 ])
  where comment (Expr _ s@(Strings _ ss) : _) = s
        comment _ = Strings NoLoc [""]

testFuns :: Env0 -> String -> Suite -> ([Assoc],[Assoc],[Assoc],[Assoc],[Assoc],[Assoc],[Assoc],[Assoc])
testFuns env modName ss = tF ss [] [] [] [] [] [] [] []
  where
    tF (With _ _ ss' : ss) uts sats ssts sts aats ats oets ets = tF (ss' ++ ss) uts sats ssts sts aats ats oets ets
    tF (Decl l (d@Def{}:ds) : ss) uts sats ssts sts aats ats oets ets
      | isTestName (dname d) =
          case testType (findQName (NoQ (dname d)) env) of
            Just UnitType ->
              tF (Decl l ds : ss) (mkAssoc d (name "UnitTest") modName : uts) sats ssts sts aats ats oets ets
            Just SyncType ->
              tF (Decl l ds : ss) uts (mkAssoc d (name "SyncActorTest") modName : sats) ssts sts aats ats oets ets
            Just SimpleSyncTest ->
              tF (Decl l ds : ss) uts sats (mkAssoc d (name "SimpleSyncTest") modName : ssts) sts aats ats oets ets
            Just SyncTest ->
              tF (Decl l ds : ss) uts sats ssts (mkAssoc d (name "SyncTest") modName : sts) aats ats oets ets
            Just AsyncType ->
              tF (Decl l ds : ss) uts sats ssts sts (mkAssoc d (name "AsyncActorTest") modName : aats) ats oets ets
            Just AsyncTest ->
              tF (Decl l ds : ss) uts sats ssts sts aats (mkAssoc d (name "AsyncTest") modName : ats) oets ets
            Just OldEnvType ->
              tF (Decl l ds : ss) uts sats ssts sts aats ats (mkAssoc d (name "OldEnvTest") modName : oets) ets
            Just EnvTest ->
              tF (Decl l ds : ss) uts sats ssts sts aats ats oets (mkAssoc d (name "EnvTest") modName : ets)
            Nothing -> tF (Decl l ds : ss) uts sats ssts sts aats ats oets ets
    tF (Decl l (_:ds) : ss) uts sats ssts sts aats ats oets ets = tF (Decl l ds : ss) uts sats ssts sts aats ats oets ets
    tF (Decl _ [] : ss) uts sats ssts sts aats ats oets ets = tF ss uts sats ssts sts aats ats oets ets
    tF (_ : ss) uts sats ssts sts aats ats oets ets = tF ss uts sats ssts sts aats ats oets ets
    tF [] uts sats ssts sts aats ats oets ets = (reverse uts, reverse sats, reverse aats, reverse ssts, reverse sts, reverse ats, reverse oets, reverse ets)

isTestName n                             = take 6 (nstr n) == "_test_"

data TestType = UnitType | SyncType | SimpleSyncTest | SyncTest | AsyncType | AsyncTest | OldEnvType | EnvTest
                deriving (Eq,Show,Read)

testType (NDef (TSchema _ []  (TFun _ fx (TNil _ PRow) k res)) _ _)
              | res /= tNone && res /= tStr = Nothing
              | otherwise    = case row2list k of
                                []         -> if fx == fxPure || fx == fxMut then Just UnitType else if fx == fxProc then Just SimpleSyncTest else Nothing
                                [t]        -> if t == logging_handler then Just SyncType else if t == syncT then Just SyncTest else if t == asyncT then Just AsyncTest else if t == envT then Just EnvTest else Nothing
                                [t1,t2]    -> if t2 == logging_handler && isGoodAction t1 then Just AsyncType else Nothing
                                [t1,t2,t3] -> if t3 == logging_handler && isGoodAction t1 && t2 == tEnv then Just OldEnvType else Nothing
                                _          -> Nothing
    where logging_handler      =  tCon (TC (gname [name "logging"] (name "Handler")) [])
          syncT                =  tCon (TC (gname [name "testing"] (name "SyncT")) [])
          asyncT               =  tCon (TC (gname [name "testing"] (name "AsyncT")) [])
          envT                 =  tCon (TC (gname [name "testing"] (name "EnvT")) [])
          isGoodAction t@(TFun _ fx p (TNil _ KRow) res)
             | fx == fxAction
               && res == tNone  = case row2list p of
                                    [t1,t2] -> (t1 == tOpt tBool || t1 == tBool || t1 == tNone) && (t2 == tOpt tException || t2 == tException || t2 == tNone)
                                    _       -> False
          isGoodAction t        = False
testType _                      = Nothing
