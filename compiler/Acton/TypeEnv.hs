{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module Acton.TypeEnv where

import Pretty
import Utils
import Acton.Syntax
import Acton.Env
import Acton.TypeM
import Acton.Printer
import Acton.Names
import Acton.Subst

data TypeX                      = TypeX {
                                    actorstate :: Maybe Type,
                                    context    :: EnvCtx,
                                    indecl     :: Bool }

type Env                        = EnvF TypeX

data EnvCtx                     = CtxTop | CtxAct | CtxClass deriving (Eq,Show)

typeX env0                      = setX env0 TypeX{ actorstate = Nothing, context = CtxTop, indecl = False }

instance Pretty TypeX where
    pretty _                    = empty

instance Subst TypeX where
    msubst x                    = do as <- msubst (actorstate x)
                                     return x{ actorstate = as }
    tyfree x                    = tyfree (actorstate x)


setActorFX st env               = modX env $ \x -> x{ actorstate = Just st }

maybeSetActorFX st env          = maybe (setActorFX st env) (const env) (actorstate $ envX env)       -- Only set if not already present

setInAct env                    = modX env $ \x -> x{ context = CtxAct }

setInClass env                  = modX env $ \x -> x{ context = CtxClass }

setInDecl env                   = modX env $ \x -> x{ indecl = True }

actorFX env l                   = case actorstate (envX env) of
                                    Just st -> fxAct st
                                    Nothing -> err l "Actor scope expected"

onTop env                       = context (envX env) == CtxTop

inAct env                       = context (envX env) == CtxAct

inClass env                     = context (envX env) == CtxClass

inDecl env                      = indecl $ envX env



-- Well-formed tycon applications -------------------------------------------------------------------------------------------------

class WellFormed a where
    wf                      :: EnvF x -> a -> Constraints

instance (WellFormed a) => WellFormed (Maybe a) where
    wf env                  = maybe [] (wf env)

instance (WellFormed a) => WellFormed [a] where
    wf env                  = concatMap (wf env)

instance WellFormed TCon where
    wf env (TC n ts)        = wf env ts ++ subst s [ constr u (tVar v) | Quant v us <- q, u <- us ]
      where q               = case findQName n env of
                                NAct q p k te  -> q
                                NClass q us te -> q
                                NProto q us te -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wf: Class or protocol name expected, got " ++ show i)
            s               = tybound q `zip` ts
            constr u t      = if isProto env (tcname u) then Impl (name "_") t u else Cast t (tCon u)

wfProto                     :: EnvF x -> TCon -> TypeM (Constraints, Constraints)
wfProto env (TC n ts)       = do cs <- instQuals env q ts
                                 return (wf env ts, cs)
  where q                   = case findQName n env of
                                NProto q us te -> q
                                NReserved -> nameReserved n
                                i -> err1 n ("wfProto: Protocol name expected, got " ++ show i)
            
instance WellFormed Type where
    wf env (TCon _ tc)      = wf env tc
    wf env (TFun _ x p k t) = wf env x ++ wf env p ++ wf env p ++ wf env k ++ wf env t
    wf env (TTuple _ p k)   = wf env p ++ wf env k
    wf env (TOpt _ t)       = wf env t
    wf env (TRow _ _ _ t r) = wf env t ++ wf env r
    wf env _                = []


instance WellFormed QBind where
    wf env (Quant v us)     = wf env us



-- Instantiation -------------------------------------------------------------------------------------------------------------------

instantiate                 :: EnvF x -> TSchema -> TypeM (Constraints, [Type], Type)
instantiate env (TSchema _ q t)
                            = do (cs, tvs) <- instQBinds env q
                                 let s = tybound q `zip` tvs
                                 return (cs, tvs, subst s t)

instQBinds                  :: EnvF x -> QBinds -> TypeM (Constraints, [Type])
instQBinds env q            = do ts <- newTVars [ tvkind v | Quant v _ <- q ]
                                 cs <- instQuals env q ts
                                 return (cs, ts)

instWitness                 :: EnvF x -> [Type] -> Witness -> TypeM (Constraints,TCon,Expr)        -- witnesses of cs already applied in e!
instWitness env ts wit      = case wit of
                                 WClass q p w ws -> do
                                    cs <- instQuals env q ts
                                    return (cs, subst (tybound q `zip` ts) p, wexpr ws (eCall (tApp (eQVar w) ts) $ wvars cs))
                                 WInst p w ws ->
                                    return ([], p, wexpr ws (eQVar w))

instQuals                   :: EnvF x -> QBinds -> [Type] -> TypeM Constraints
instQuals env q ts          = do let s = tybound q `zip` ts
                                 sequence [ constr (subst s (tVar v)) (subst s u) | Quant v us <- q, u <- us ]
  where constr t u@(TC n _)
          | isProto env n   = do w <- newWitness; return $ Impl w t u
          | otherwise       = return $ Cast t (tCon u)

wvars                       :: Constraints -> [Expr]
wvars cs                    = [ eVar v | Impl v _ _ <- cs ]


-- Misc. ---------------------------------------------------------------------------------------------------------------------------

bindWits eqs                            = [ Assign l0 [PVar l0 n (Just t)] e | (n,t,e) <- eqs ]

impl2type t (TC n ts)                   = tCon $ TC n (t:ts)

wit2row ws                              = \p -> foldr f p ws
  where f (w,t)                         = TRow NoLoc PRow w t

wit2arg ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosArg (eVar w)

wit2par ws                              = \p -> foldr f p ws
  where f (w,t)                         = PosPar w (Just t) Nothing

var2arg xs                              = \p -> foldr f p xs
  where f x                             = PosArg (eVar x)

exp2arg es                              = \p -> foldr PosArg p es

witsOf cs                               = [ eVar w | Impl w t p <- cs ]

qualWPar env q                          = wit2par (qualWits env q)

qualWRow env q                          = wit2row (qualWits env q)

qualWits env q                          = [ (tvarWit tv p, impl2type (tVar tv) p) | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

witSubst env q cs                       = [ (w0,t,eVar w) | ((w,t),w0) <- ws `zip` ws0 ]
  where ws                              = [ (w, impl2type t p) | Impl w t p <- cs ]
        ws0                             = [ tvarWit tv p | Quant tv ps <- q, p <- ps, isProto env (tcname p) ]

