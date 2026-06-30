{-# LANGUAGE FlexibleInstances #-}
module Acton.Boxing where

import Acton.Syntax
import Acton.Names
import Acton.NameInfo
import Acton.Env
import Acton.QuickType
import Acton.Prim
import Acton.Builtin
import Acton.Subst
import Pretty
import Utils
import Debug.Trace
import qualified Data.HashSet as HashSet
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.HashMap.Strict as M

doBoxing                           :: Acton.Env.Env0 -> Module -> IO Module
doBoxing env m                     = do (_,ss) <- boxing (boxEnv env) (mbody m)
                                        return m{mbody = ss}   

type BoxEnv                        = EnvF BoxX


data StaticWitness                  = StaticWitness { swBaseClass :: TCon, swObjectName :: QName, swObjectPath :: [Name] }

data BoxX                          = BoxX { inClassX :: Maybe (TCon, TEnv)
                                          , inActionX :: Bool
                                          , returnTypeX :: Maybe Type
                                          , staticWitnessesX :: M.HashMap Name StaticWitness
                                          }


boxEnv                             :: Env0 -> BoxEnv
boxEnv env0                        = setX env0 BoxX{ inClassX = Nothing
                                                   , inActionX = False
                                                   , returnTypeX = Nothing
                                                   , staticWitnessesX = M.empty
                                                   }


setInClass mtc env                  = modX env $ \x -> x{ inClassX = mtc }

getInClass env                      = inClassX $ envX env

setInAction b env                   = modX env $ \x -> x{ inActionX = b }

getInAction env                     = inActionX $ envX env

setReturnType t env                 = modX env $ \x -> x{ returnTypeX = t }

getReturnType env                   = returnTypeX $ envX env

-- Static witness calls ---------------------------------------------------------------

setStaticWitness n mbw env          = modX env $ \x -> x{ staticWitnessesX = upd (staticWitnessesX x) }
  where upd                         = maybe (M.delete n) (M.insert n) mbw

lookupStaticWitness env n           = M.lookup n (staticWitnessesX $ envX env)

-- Recognize witness expressions whose runtime object has a static C name.  A
-- dot path records selection of embedded super-witness fields from that root.
staticWitnessOf                     :: BoxEnv -> Expr -> Maybe StaticWitness
staticWitnessOf env (Dot _ e n)     = do w <- staticWitnessOf env e
                                         return w{ swObjectPath = swObjectPath w ++ [n] }
staticWitnessOf env (Call _ f p KwdNil)
                                    = staticWitnessRoot env f p
  where staticWitnessRoot env (TApp _ f ts) p
                                    = staticWitnessRoot' env f ts p
        staticWitnessRoot env f p   = staticWitnessRoot' env f [] p

        staticWitnessRoot' env (Var _ qn) ts p
                                    = do qn' <- builtinWitnessClass env qn
                                         obj <- staticWitnessObject env qn' ts p
                                         return StaticWitness{ swBaseClass = TC qn' ts, swObjectName = obj, swObjectPath = [] }
        staticWitnessRoot' _ _ _ _  = Nothing

        builtinWitnessClass env qn  = case unalias env qn of
                                        qn'@(GName m _)
                                          | m == mBuiltin,
                                            isClass env qn'
                                              -> Just qn'
                                        _ -> Nothing

        staticWitnessObject env qn@(GName m n) ts p
          | Just key <- specialStaticKey env n ts
                                    = Just (GName m (Derived n key))
          | p == PosNil             = Just qn
        staticWitnessObject _ _ _ _ = Nothing

        specialStaticKey env (Derived n1 n2) [TCon _ (TC qn []), _]
          | n1 == nMapping,
            n2 == nDict             = builtinStaticKey env qn
          | n1 == nSetP,
            n2 == nSetT             = builtinStaticKey env qn
        specialStaticKey _ _ _      = Nothing

        builtinStaticKey env qn     = case unalias env qn of
                                        GName m key
                                          | m == mBuiltin,
                                            key `elem` [nInt, nU64, nStr, nBytes]
                                              -> Just key
                                        _ -> Nothing
staticWitnessOf _ _                 = Nothing

-- Follow a super-witness path to the witness class whose method table is used.
staticWitnessMethodClass env w attr
                                    = do tc <- followWitnessPath env (swBaseClass w) (swObjectPath w)
                                         if hasAttr env tc attr then Just tc else Nothing
  where followWitnessPath _ tc []   = Just tc
        followWitnessPath env (TC qn ts) (w:ws)
                                    = do p <- witnessPathProtocol w
                                         qn' <- builtinWitnessPathClass env qn p
                                         followWitnessPath env (TC qn' ts) ws

        witnessPathProtocol (Internal Witness s _)
                                    = Just (name s)
        witnessPathProtocol _       = Nothing

        builtinWitnessPathClass env qn p
                                    = case unalias env qn of
                                        GName m n
                                          | m == mBuiltin ->
                                              let qn' = GName mBuiltin (Derived p n)
                                              in if isClass env qn' then Just qn' else Nothing
                                        _ -> Nothing

staticWitnessExpr w                 = foldl (Dot NoLoc) (eCallP root PosNil) (swObjectPath w)
  where root
          | swObjectName w == tcname (swBaseClass w)
                                    = tApp (eQVar (swObjectName w)) (tcargs (swBaseClass w))
          | otherwise               = eQVar (swObjectName w)

methodQName (TC (GName m c) _) n    = GName m (Derived c n)
methodQName (TC (QName m c) _) n    = GName m (Derived c n)
methodQName (TC (NoQ c) _) n        = NoQ (Derived c n)

generatedMethodName (GName m (Derived c n))
  | m == mBuiltin                  = Just (GName m c, n)
generatedMethodName (QName m (Derived c n))
  | m == mBuiltin                  = Just (GName m c, n)
generatedMethodName _              = Nothing

generatedMethodType env qn ts       = do (tc, n) <- generatedMethodClass env qn ts
                                         return (rtypeOf env tc n)

generatedCallableType env (TApp _ (Var _ n) ts)
                                    = generatedMethodType env n ts
generatedCallableType env (TApp _ f _)
                                    = generatedCallableType env f
generatedCallableType env (Var _ n)
                                    = generatedMethodType env n []
generatedCallableType _ _           = Nothing

generatedMethodClass env qn ts      = do (c, n) <- generatedMethodName qn
                                         generatedClass env c n ts

generatedClass env qn n ts
  | isClass env qn,
    hasAttr env tc n                = Just (tc, n)
  | otherwise                       = Nothing
  where tc                          = TC qn ts

directMethodImpl env qn n           = case findQName qn env of
                                        NClass _ _ te _ -> direct te
                                        NProto _ _ te _ -> direct te
                                        _               -> False
  where direct te                   = case findAttrInfoIn n te of
                                        Just NDef{} -> True
                                        _           -> False

-- Unboxing helpers -------------------------------------

-- returns the uninstantiated type of method n in class c, i.e. the type of the corresponding method in oldest superclass.
-- n may be a method for which findAttrSchemas has no info, e.g. __init__, __str__, etc, in which case Nothing is returned.
generalTypeC                       :: EnvF x -> QName -> Name -> Maybe Type
generalTypeC env c n                =  case lookup n (findAttrSchemas env c) of
                                           Just (NDef sc _ _) -> Just (sctype sc)
                                           Just (NSig sc _ _) -> Just (sctype sc)
                                           Just  _ -> Nothing
                                           Nothing -> Nothing
                                           
-- generalType env n returns the same info as generalTypeC if n is a method of a class, but expects the TEnv of the class to be accessed via the env;
-- for use while traversing the class.
-- If n is a toplevel function it returns just the type in the NameInfo from the env.
-- n may be a method for which findAttrSchemas has no info, e.g. __init__, __str__, etc, in which case Nothing is returned.
generalType                        :: EnvF x -> Name -> Maybe Type
generalType env n                  = case getNI n of
                                           Just (NDef sc _ _) -> Just $ sctype sc
                                           Just (NSig sc _ _) -> Just $ sctype sc
                                           ni -> Nothing 
    where getNI n                  = case contextIs env CtxClass of
                                           True -> Nothing
                                           False -> Just (findQName (NoQ n) env)
 
-- matchTypes expects to be called with both arguments a function type; the specific one returned by typeOf for a name f and
-- the general one returned by findAttrSchemas (if applicable) for the same name.
-- matchTypes t t' tags as TUnboxed those type components in t which can be unboxed taking the uninstantiated type t'
-- (type of the oldest corresponding method in a superclass) into account. 
matchTypes t@TCon{} t'@TCon{}
   | t == t'                        = if isUnboxable t then TUnboxed NoLoc t else t
matchTypes (TFun _ fx p _ r) (TFun _ fx' p' _ r')
                                    = tFun fx (matchTypes p p') kwdNil (matchTypes r r')
matchTypes (TRow _ _ _ t r) (TRow _ _ _ t' r')
                                    = posRow (matchTypes t t') (matchTypes r r')
matchTypes TNil{} TNil{}            = posNil                                    
matchTypes t (TUnboxed _ t')        = matchTypes t t'
matchTypes t _                      = t    -- ignore possibilities for unboxing 


-- returns the representation type of method f selected from tc.
-- If generalTypeC cannot find an uninstantiated type to match against, the instantiated type is matched against itself (so, unboxable types will be annotated with TUnboxed).
rtypeOf                             :: EnvF x -> TCon -> Name -> Type
rtypeOf env tc f
  | f == initKW                      = matchTypes (sctype sc) (sctype sc)
  | otherwise                        = case generalTypeC env (tcname tc) f of
                                           Just t -> matchTypes (sctype sc) t
                                           Nothing -> matchTypes (sctype sc) (sctype sc)
    where (sc,_)                     = findAttr' env tc f

rtypeOf' env w f                     = rtypeOf env tc f
    where NVar (TCon _ tc)           = findQName w env

-- Auxiliaries ---------------------------------------------------------------------------------------------------

-- unboxing

integralTypes                      = [tBigint, tInt, tI32, tI16, tI8, tU64, tU32, tU16, tU8, tU1]
numericTypes                       = integralTypes ++ [tFloat]
unboxableTypes                     = tBool : tail numericTypes

isUnboxable t                      = t `elem` unboxableTypes

isUnboxedRep TUnboxed{}            = True
isUnboxedRep t                     = isUnboxable t

asUnboxedRep t
  | isUnboxable t                  = TUnboxed NoLoc t
  | otherwise                      = t

boxedRepType (TUnboxed _ t)         = t
boxedRepType t                      = t

unboxedRepType (TUnboxed _ t)       = Just t
unboxedRepType t
  | isUnboxable t                   = Just t
  | otherwise                       = Nothing

rawRepType (TUnboxed _ t)           = Just t
rawRepType _                        = Nothing

forceUnbox env t (Box _ e)
  | boxedResultExpr env e            = UnBox t e
  | otherwise                        = e
forceUnbox env t e                   = UnBox t e

boxedResultExpr env (Paren _ e)      = boxedResultExpr env e
boxedResultExpr env DotI{}           = True
boxedResultExpr env c@(Call _ f _ KwdNil)
  | rawClassConstructor env c f       = False
  | callReturnsMsg env f              = False
  | otherwise                         = callReturnsBoxed env f
boxedResultExpr env _                = False

rawClassConstructor env c f           = callIsClass env f && isUnboxable (boxedRepType (typeOf env c))

exposeMsg fx t
  | fx == fxAction                    = tMsg t
  | otherwise                         = t

callReturnsMsg env f                  = case rtypeOfFun env f of
                                          TFun _ fx _ _ _ -> fx == fxAction
                                          _               -> False

callReturnsBoxed env f@(TApp _ f0 _)
  | TApp _ (Var _ n) ts <- f,
    Just _ <- generatedMethodType env n ts
                                      = callableReturnsBoxed env f
callReturnsBoxed env f@(TApp _ f0 _)
  | callIsClass env f0               = True
  | otherwise                        = callableReturnsBoxed env f
callReturnsBoxed env f@(Var _ n)
  | Just _ <- generatedMethodType env n []
                                      = callableReturnsBoxed env f
callReturnsBoxed env f@(Var _ n)
  | NClass{} <- findQName n env      = True
  | otherwise                        = callableReturnsBoxed env f
callReturnsBoxed env f               = callableReturnsBoxed env f

callIsClass env (TApp _ (Var _ n) ts)
  | Just _ <- generatedMethodType env n ts
                                      = False
callIsClass env (TApp _ f _)         = callIsClass env f
callIsClass env (Var _ n)
  | Just _ <- generatedMethodType env n []
                                      = False
callIsClass env (Var _ n)            = case findQName n env of
                                         NClass{} -> True
                                         _        -> False
callIsClass env _                    = False

callableReturnsBoxed env f           = case rtypeOfFun env f of
                                         TFun _ _ _ _ t -> isUnboxable t
                                         _              -> False

callableRawRep env f                  = case rtypeOfFun env f of
                                         TFun _ _ _ _ (TUnboxed _ t) -> Just t
                                         _                          -> Nothing

generatedCallableRawRep env f         = case generatedCallableType env f of
                                         Just (TFun _ _ _ _ (TUnboxed _ t)) -> Just t
                                         _                                  -> Nothing

generatedExprType env (Paren _ e)     = generatedExprType env e
generatedExprType env (Let _ ss e)    = generatedExprType (define (envOf ss) env) e
generatedExprType env (TApp _ e _)    = generatedExprType env e
generatedExprType env (Async _ e)     = generatedExprType env e
generatedExprType env (Box t _)       = Just t
generatedExprType env (UnBox t _)     = Just t
generatedExprType env (Call _ f _ KwdNil)
                                      = case generatedCallableType env f of
                                          Just (TFun _ fx _ _ t) -> Just (exposeMsg fx t)
                                          _                     -> Nothing
generatedExprType env (Dot _ e n)     = do t <- generatedExprType env e
                                           generatedDotType env t n
generatedExprType _ _                 = Nothing

generatedDotType env (TCon _ tc) n     = Just $ sctype $ fst $ findAttr' env (snd $ splitTC env tc) n
generatedDotType env (TVar _ tv) n     = Just $ sctype $ fst $ findAttr' env (snd $ splitTC env (findTVBound env tv)) n
generatedDotType _ _ _                 = Nothing

unboxedFieldType env e@(Var _ n) attr
  | isTypeRef (findQName n env)     = Nothing
  | otherwise                       = unboxedFieldType' env e attr
unboxedFieldType env e n            = unboxedFieldType' env e n

unboxedFieldType' env e n           = case typeOf env e of
                                        TCon _ tc -> fieldType tc
                                        TVar _ tv -> fieldType (findTVBound env tv)
                                        _         -> Nothing
  where fieldType tc                = let (_, tc') = splitTC env tc
                                          t        = sctype $ fst $ findAttr' env tc' n
                                      in if isUnboxedRep t then Just (boxedType t) else Nothing
        boxedType (TUnboxed _ t)     = t
        boxedType t                  = t

-- True when an assignment target physically stores an unboxed (raw) value, so
-- an in-place raw augmented assignment ('+=' etc.) is sound.  A field of a
-- generic cell such as $Cell.cell keeps the boxed representation even when its
-- element type is unboxable (the field type stays a type variable), so a raw
-- in-place update there would corrupt the stored boxed pointer.  This reuses
-- unboxedFieldType, the same field-representation decision the read side relies
-- on, so reads and writes agree on whether a slot is raw or boxed.  Every target
-- that actually reaches the augmented-assignment rewrite is a Dot (a volatile
-- cell slot or a class/actor field); the catch-all keeps the prior raw behavior.
unboxedAugTarget env (Dot _ e n)    = case unboxedFieldType env e n of
                                        Just _  -> True
                                        Nothing -> False
unboxedAugTarget env _              = True

isTypeRef NAct{}                    = True
isTypeRef NClass{}                  = True
isTypeRef NProto{}                  = True
isTypeRef NExt{}                    = True
isTypeRef _                         = False

varType (NVar t)                    = Just t
varType (NSVar t)                   = Just t
varType _                           = Nothing

qvarType env n                      = tryQName n env >>= varType

envVarType n te                     = case lookup n te of
                                        Just (NVar t)  -> Just t
                                        Just (NSVar t) -> Just t
                                        _              -> Nothing

assignTargetType env s n e          = maybe (maybe (typeOf env e) id (qvarType env (NoQ n))) id (envVarType n (envOf s))

targetType env (Dot _ e n)          = sctype sc
  where t0                          = typeOf env e
        (_,c0)                      = case t0 of
                                         TCon _ tc -> splitTC env tc
                                         TVar _ tv -> splitTC env (findTVBound env tv)
        (sc, _)                     = findAttr' env c0 n
targetType env (Var _ n)
  | Just t <- qvarType env n
                                    = t
targetType env e                    = typeOf env e


prims = [primISINSTANCE, primISNOTNONE, primISNONE]

unboxedPrim p
  | p == primISINSTANCE            = primISINSTANCE0
  | p == primISNOTNONE             = primISNOTNONE0
  | p == primISNONE                = primISNONE0

 
unbox _ (Box _ e)               = e
unbox t e                       = UnBox t e

tryUnbox, tryBox :: Type -> Expr -> Expr
tryUnbox (TUnboxed _ t) e       = unbox t e
tryUnbox _ e                    = e
tryBox (TUnboxed _ _) (UnBox _ e)= e
tryBox (TUnboxed _ t) e          = Box t e
tryBox _ e                      = e

literalUnboxedRep (Int _ val _)
  | val < (-9223372036854775808) = Nothing
  | val > 18446744073709551615   = Nothing
  | val > 9223372036854775807    = Just tU64
  | otherwise                    = Just tInt
literalUnboxedRep (Float _ _ _)  = Just tFloat
literalUnboxedRep _              = Nothing

exprUnboxedRep env e@Int{}       = literalUnboxedRep e
exprUnboxedRep env e@Float{}     = literalUnboxedRep e
exprUnboxedRep env e@DotI{}      = Nothing
-- After boxing, an And/Or expression has boxed operands and yields a boxed
-- value (its result IS one of the operands), so it must not be re-boxed by a
-- use-site fallback, even though its static type is unboxable.
exprUnboxedRep env (BinOp _ _ op _)           -- MZ
  | op `elem` [And, Or]           = Nothing   -- MZ
exprUnboxedRep env c@(Call _ f _ KwdNil)
  | Just t <- generatedCallableRawRep env f
                                  = Just t
  | Just _ <- generatedCallableType env f
                                  = Nothing
  | callReturnsMsg env f          = Nothing
  | rawClassConstructor env c f    = unboxedRepType (typeOf env c)
  | Just t <- callableRawRep env f = Just t
exprUnboxedRep env e
  | Just t <- generatedExprType env e
                                  = unboxedRepType t
exprUnboxedRep env e             = unboxedRepType (typeOf env e)

fixreturn env rt e e1
  | getInAction env                = e1
  | Just t <- rawRepType rt        = forceUnbox env t e1
  | Box{} <- e1                    = e1
  | Just t <- exprUnboxedRep env e = Box t e1
  | otherwise                      = e1

fixreturnUnknown env e e1
  | getInAction env                = e1
  | Just t <- exprUnboxedRep env e = forceUnbox env t e1
  | otherwise                      = e1

fixassign env (TOpt _ t) e
  | Box{} <- e                     = e
  | isUnboxable t,
    exprUnboxedRep env e == Just t = Box t e
fixassign env t e
  | Just t' <- unboxedRepType t    = forceUnbox env t' e
  | Box{} <- e                     = e          -- MZ
  | Just rt <- exprUnboxedRep env e = Box rt e  -- MZ
  | otherwise                      = e

fixarg env (TUnboxed _ t) e     = forceUnbox env t e
fixarg env (TOpt _ t) e@Box{}   = e
fixarg env (TOpt _ t) e
  | isUnboxable t && exprUnboxedRep env e == Just t
                                  = Box t e
fixarg env t e@Box{}             = e
fixarg env t e
  | isUnboxable t && isRawArg e   = Box t e
  where isRawArg UnBox{}          = True
        isRawArg e                = case typeOf env e of
                                      TUnboxed{} -> True
                                      _          -> False
fixarg env t e
  | Nothing <- unboxedRepType t,
    Just rt <- exprUnboxedRep env e
                                  = Box rt e
fixarg env _ e                   = e

fixargs                         :: BoxEnv -> PosArg -> Type -> PosArg
fixargs env (PosArg e p) r@TRow{}= PosArg (fixarg env (rtype r) e) (fixargs env p (rtail r))
-- fixargs(PosStar e) r       = PosStar (tryUnbox t e) 
--    where t                      = rtype r
fixargs env p@PosArg{} _          = p
fixargs env PosNil _               = PosNil

fixpars (PosPar n (Just t) mbe p) r
                                = PosPar n (Just t') mbe (fixpars p (rtail r))
     where t'                   = rtype r
fixpars PosNIL _            = PosNIL

rtypeOfFun env f@(TApp _ (Var _ n) _)
  | Just rt <- generatedMethodType env n (targs f)
                                    = rt
rtypeOfFun env f@(TApp _ (Var _ n) _)
  | boxedCPrim n                    = typeOf env f
rtypeOfFun env (TApp _ f0 [])       = rtypeOfFun env f0
rtypeOfFun env f@(TApp _ f0 ts)     = matchTypes (typeOf env f) (typeInstOf env (map (const tWild) ts) f0)
rtypeOfFun env (Async _ f)          = rtypeOfFun env f
rtypeOfFun env f@(Dot _ (Var _ x) n)
  | NClass{} <- findQName x env      = case generalTypeC env x n of
                                         Just t  -> matchTypes (typeOf env f) t
                                         Nothing -> matchTypes (typeOf env f) (typeOf env f)
rtypeOfFun env f@(Dot _ e n)        = case typeOf env e of
                                        TCon _ tc -> rtypeOf env tc n
                                        TVar _ tv -> rtypeOf env (findTVBound env tv) n
                                        _         -> typeOf env f
rtypeOfFun env f@(Var _ n)
  | Just rt <- generatedMethodType env n []
                                    = rt
rtypeOfFun env f@(Var _ n)
  | boxedCPrim n                    = typeOf env f
  | otherwise                       = case findQName n env of
                                        NDef sc _ _
                                          | isInternalQName n -> matchTypes (typeOf env f) (typeOf env f)
                                          | otherwise         -> matchTypes (typeOf env f) (sctype sc)
                                        NSig sc _ _  -> matchTypes (typeOf env f) (sctype sc)
                                        NClass q _ _ _
                                          | null q      -> matchTypes (typeOf env f) (typeOf env f)
                                        _            -> typeOf env f
rtypeOfFun env f                    = typeOf env f

boxedCPrim n                        = n `elem` [primAFTER, primAFTERc, primAFTERf]

isInternalQName (NoQ n)             = isInternal n
isInternalQName (GName _ n)         = isInternal n
isInternalQName (QName _ n)         = isInternal n

-- Walking the syntax tree to place Box/UnBox annotations and sometimes restructure code to mimic C code on unboxed values ---------------
-- Return list of used witnesses and restructured code

class Boxing a where
    boxing :: BoxEnv -> a -> IO (HashSet.HashSet Name,a)

boxingTarget env (Dot l e n)       = do (ws1,e1) <- boxing env e
                                        return (ws1, Dot l e1 n)
boxingTarget env (DotI l e i)      = do (ws1,e1) <- boxing env e
                                        return (ws1, DotI l e1 i)
boxingTarget env (Rest l e n)      = do (ws1,e1) <- boxing env e
                                        return (ws1, Rest l e1 n)
boxingTarget env (RestI l e i)     = do (ws1,e1) <- boxing env e
                                        return (ws1, RestI l e1 i)
boxingTarget env e                 = return (HashSet.empty, e)

instance Boxing a => Boxing (Maybe a) where
    boxing env (Just x)           = do (ws1, x1) <- boxing env x
                                       return (ws1, Just x1)
    boxing env Nothing            = return (HashSet.empty, Nothing)

instance (Boxing a) => Boxing ([a]) where
    boxing env []                   = return (HashSet.empty,[])
    boxing env (x : xs)             = do (ws1,x1)  <- boxing env x
                                         (ws2,xs2) <- boxing env xs
                                         return (HashSet.union ws1 ws2, x1:xs2)

instance {-# OVERLAPS #-} Boxing ([Stmt]) where
    boxing env []                     = return (HashSet.empty,[])
    boxing env (x@(Assign _ [PVar _ w _] rhs) : xs)
    --  if witness n is not used in xs, delete statement x defining n
       | isWitness w                  = do (ws1,x') <- boxing env x      
                                           (ws2,xs') <- boxing env1 xs
                                           return $ if HashSet.member w  ws2 then (HashSet.union ws1 (HashSet.delete w ws2),x':xs') else (ws2,xs')
      where te                        = envOf x
            env1                      = setStaticWitness w (staticWitnessOf env rhs) (define te env)

    boxing env (x : xs)              = do (ws1,x') <- boxing env x
                                          let env1 = define (envOf x') env
                                          (ws2,xs') <- boxing env1 xs
                                          return (HashSet.union ws1 ws2, x' : xs')

 
-- After boxing, each Expr of unboxable type is boxed. Operands in BinOp/CompOp/UnOp expressions
-- are first boxed as part of their own boxing, then unboxed when used to form the BinOp/CompOp/UnOp term
instance Boxing Expr where
    boxing env e@(Var _ (NoQ n))
       | isWitness n                = return (HashSet.singleton n, e)
       | isUnboxable t              = return (HashSet.empty, Box t e)
       where t                      = typeOf env e
    -- Qualified imported constants such as math.pi need the same boxed read
    -- as local unboxable variables, but typeOf is not safe for all Vars here.
    boxing env v@(Var _ n)
       | Just t <- varType (findQName n env), isUnboxable t
                                    = return (HashSet.empty, Box t v)
    boxing env (Call _ (Dot _ e@(Var _ w@(NoQ n)) attr) p KwdNil)
      | isWitness n                 = do (ws1,p1) <- boxing env p
                                         (ws2,e1) <- boxingWitness env w attr p1 pr rest
                                         return (HashSet.union ws1 ws2,e1)
     where
      rt@(TFun _ _ pr _ rest)       = rtypeOf' env w attr
      boxingWitness                 :: BoxEnv -> QName -> Name -> PosArg -> Type -> Type -> IO (HashSet.HashSet Name,Expr)
      boxingWitness env w attr p pr rest = case findQName w env of
                                        NVar (TCon _ (TC _ ts))
                                  --         | any (not . vFree) ts    -> return ([n], eCallP (eDot (eQVar w) attr) p)
                                           | attr == fromatomKW      -> boxingFromAtom w es ts rt pr rest
                                           | attr `elem` binopKWs    -> boxingBinop w attr es ts rt pr rest  -- rest indicates "result type", not any form of remainder
                                           | attr `elem` unopKWs     -> boxingUnop w attr es ts rt pr rest
                                           | attr `elem` eqordKWs    -> boxingCompop w attr es ts rt pr rest
                                        _                            -> boxingDirectOrDynamic w attr p rt pr rest
       where es                     = posargs p
--             vFree (TCon _ (TC _ _))= True
--             vFree _                = False

      boxingDirectOrDynamic w attr p rt pr rest
                                    = case lookupStaticWitness env n >>= \sw -> staticWitnessMethodClass env sw attr >>= \tc -> return (sw, tc) of
                                        Just (sw, tc)
                                          | directMethodImpl env (tcname tc) attr
                                            -> return (HashSet.empty, tryBox rest $ staticWitnessCall sw tc attr (fixargs env p pr))
                                        Just _        -> do let c = eCallP (eDot (eQVar w) attr) (fixargs env p pr)
                                                            return (HashSet.singleton n, tryBox rest c)
                                        Nothing       -> do let c = eCallP (eDot (eQVar w) attr) (fixargs env p pr)
                                                            return (HashSet.singleton n, tryBox rest c)

      staticWitnessCall sw tc attr p= eCallP (tApp (eQVar (methodQName tc attr)) (tcargs tc)) (PosArg (staticWitnessExpr sw) p)

      boxingFromAtom w [i@Int{}] ts _ _ _
        | t `elem` numericTypes,
          t /= tBigint              = return (HashSet.empty, Box (last ts) (unbox t i))
        where t                     = head ts
      boxingFromAtom w [x@Float{}] ts _ _ _
                                    = return (HashSet.empty, Box (last ts) (unbox (head ts) x))
      boxingFromAtom w es ts rt pr rest
                                    = boxingDirectOrDynamic w fromatomKW (posarg es) rt pr rest
      boxingBinop w attr es@[x1, x2] ts _ _ _
        | isUnboxable t            =  return (HashSet.empty, Box (last ts) $ Paren NoLoc $ BinOp NoLoc (unbox t x1) op (unbox t x2))
        where t                     = head ts
              op                    = bin2Binary attr
      boxingBinop w attr es _ rt pr rest
                                    = boxingDirectOrDynamic w attr (posarg es) rt pr rest
      boxingUnop w attr es@[x1] ts _ _ _
        | isUnboxable t             =  return (HashSet.empty, Box (last ts) $ Paren NoLoc $ UnOp NoLoc op (unbox t x1))
        where t                     = head ts
              op                    = un2Unary attr
      boxingUnop w attr es _ rt pr rest
                                    = boxingDirectOrDynamic w attr (posarg es) rt pr rest
      boxingCompop w attr es@[x1, x2] ts _ _ _
        | isUnboxable t             = return (HashSet.empty, Box tBool $ Paren NoLoc $ CompOp NoLoc (unbox t x1) [OpArg op (unbox t x2)])
        where t = head ts
              op = cmp2Comparison attr
      boxingCompop w attr es _ rt pr rest
                                    = boxingDirectOrDynamic w attr (posarg es) rt pr rest
    boxing env (Call l e@(TApp _ (Var _ f) ts) p KwdNil)
      | f `elem` prims              = do (ws1,p1) <- boxing env p
                                         return (ws1, Box tBool $ eCallP e' (fixargs env p1 r))
      | otherwise                   = do (ws1,p1) <- boxing env p
                                         return (ws1, tryBox (exposeMsg fx t) $ eCallP e (fixargs env p1 r))
       where e'                     = tApp (eQVar (unboxedPrim f)) ts
             TFun _ fx r _ t        = rtypeOfFun env e
    boxing env (Call l f@Async{} p KwdNil)
                                    = do (ws1,f1) <- boxing env f
                                         (ws2,p1) <- boxing env p
                                         return (HashSet.union ws1 ws2, eCallP f1 (fixargs env p1 r))
        where  TFun _ _ r _ _       = rtypeOfFun env f
    boxing env (Call l (Dot _ e n) PosNil KwdNil)
      | n == boolKW,
        Just rt <- unboxedRepType (typeOf env e)
                                    = do (ws1,e1) <- boxing env e
                                         let z = UnBox rt (Int l 0 "0")
                                         return (ws1, Box tBool $ CompOp l (forceUnbox env rt e1) [OpArg NEq z])
    boxing env (Call l f p KwdNil)  = do (ws1,f1) <- boxing env f
                                         (ws2,p1) <- boxing env p
                                         let c = eCallP f1 (fixargs env p1 r)
                                         return (HashSet.union ws1 ws2, tryBox (exposeMsg fx t) c)
        where  TFun _ fx r _ t      = rtypeOfFun env f
    boxing env (TApp l f ts)        = do (ws1,f1) <- boxing env f
                                         return (ws1, TApp l f1 ts)
    boxing env (Let l ss e)         = do (ws1, ss') <- boxing env ss
                                         (ws2, e') <- boxing env e
                                         return (HashSet.union ws1 ws2, Let l ss' e')
    boxing env (Async l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Async l e1)
    boxing env (Await l e)          = do (ws1,e1) <- boxing env e
                                         return (ws1, Await l e1)
    boxing env e@(Cond l e1 e2 e3)  = do (ws1,e1') <- boxing env e1
                                         (ws2,e2') <- boxing env e2
                                         (ws3,e3') <- boxing env e3
                                         case unboxedRepType (typeOf env e) of
                                             Just t -> return (HashSet.unions [ws1, ws2, ws3], Box t $ Cond l (forceUnbox env t e1') e2' (forceUnbox env t e3'))
                                    --         Nothing -> return (HashSet.unions [ws1, ws2, ws3], Cond l e1' e2' e3')
                                             Nothing -> return (HashSet.unions [ws1, ws2, ws3], Cond l (boxValueExpr env e1 e1') e2' (boxValueExpr env e3 e3'))  -- MZ
    boxing env (IsInstance l e qn)  = do (ws1,e1) <- boxing env e
                                    --     return (ws1, IsInstance l e1 qn)
                                         return (ws1, IsInstance l (boxValueExpr env e e1) qn) -- MZ
--    boxing env e@(BinOp l e1 op e2)
--      | op `notElem` [And, Or],
--        Just rt <- unboxedRepType t
--                                    = do (ws1,e1') <- boxing env e1
--                                         (ws2,e2') <- boxing env e2
--                                         return (HashSet.union ws1 ws2, Box rt $ Paren NoLoc $ BinOp l (unboxArg e1 e1') op (unboxArg e2 e2'))
--      where t                       = typeOf env e
--            unboxArg e0 e'          = maybe e' (\t -> forceUnbox env t e') (unboxedRepType (typeOf env e0))
    boxing env e@(BinOp l e1 op e2)                                         -- MZ
      | op `elem` [And, Or]         = do (ws1,e1') <- boxing env e1         -- MZ
                                         (ws2,e2') <- boxing env e2         -- MZ
                                         return (HashSet.union ws1 ws2, if boxedRepType t == tBool then Box tBool (BinOp l (forceUnbox env tBool e1') op (forceUnbox env tBool e2')) else BinOp l (boxValueExpr env e1 e1') op (boxValueExpr env e2 e2'))  -- MZ
        where t                       = typeOf env e1
--    boxing env e@(BinOp l e1 op e2) = do (ws1,e1') <- boxing env e1
--                                         (ws2,e2') <- boxing env e2
--                                         return (HashSet.union ws1 ws2, BinOp l e1' op e2')
    boxing env (CompOp l e os)      = do (ws1,e1) <- boxing env e
                                         (ws2,e2) <- boxing env os
                                         return (HashSet.union ws1 ws2, CompOp l e1 e2)
    boxing env (UnOp l Not e)       = do (ws1,e1) <- boxing env e
                                         return (ws1, if boxedRepType t == tBool then Box tBool (UnOp l Not (forceUnbox env tBool e1)) else UnOp l Not (boxValueExpr env e e1))
       where t                      = typeOf env e
    boxing env (UnOp l uop e)
      | Just rt <- unboxedRepType (typeOf env e)
                                    = do (ws1,e') <- boxing env e
                                         return (ws1, Box rt $ Paren NoLoc $ UnOp l uop (unbox rt e'))
    boxing env (UnOp l uop e)       = do (ws1,e') <- boxing env e
                                         return (ws1, UnOp l uop e')
    boxing env d@(Dot _ (Var _ n) _)
      | isTypeRef (findQName n env) = return (HashSet.empty, d)
    boxing env d@(Dot l e n)        = do (ws1,e1) <- boxing env e
                                         let d' = Dot l e1 n
                                         return (ws1, maybe d' (\t -> Box t d') (unboxedFieldType env e n))
    boxing env (DotI l e i)         = do (ws1,e1) <- boxing env e
                                         return (ws1, DotI l e1 i)
    boxing env (Rest l e n)         = do (ws1,e1) <- boxing env e
                                         return (ws1, Rest l e1 n)
    boxing env (RestI l e i)        = do (ws1,e1) <- boxing env e
                                         return (ws1, RestI l e1 i)
    boxing env (Tuple l p k)        = do (ws1,p1) <- boxingTupleArgs env p
                                         return (ws1,Tuple l p1 k)
    boxing env (List l es)          = do (ws1,es1) <- boxing env es
                                         return (ws1, List l es1)
    boxing env (Set l es)           = do (ws1,es1) <- boxing env es
                                         return (ws1, Set l es1)
    boxing env (Dict l es)          = do (ws1,es1) <- boxing env es
                                         return (ws1, Dict l es1)
    boxing env e                    = return (HashSet.empty,e)

boxingTupleArgs env (PosArg e p)    = do (ws1,e1) <- boxing env e
                                         (ws2,p1) <- boxingTupleArgs env p
                                         return (HashSet.union ws1 ws2, PosArg (boxValueExpr env e e1) p1)
boxingTupleArgs env (PosStar e)     = do (ws1,e1) <- boxing env e
                                         return (ws1, PosStar e1)
boxingTupleArgs _ PosNil            = return (HashSet.empty, PosNil)

boxValueExpr env e e1
  | Box{} <- e1                      = e1
  | boxedResultExpr env e            = e1
boxValueExpr env (Call _ f _ KwdNil) e1
  | callReturnsMsg env f              = e1
boxValueExpr env (Paren _ e) e1       = boxValueExpr env e e1
boxValueExpr env e e1
  | Just rt <- exprUnboxedRep env e
                                      = Box rt e1
  | otherwise                        = e1

instance Boxing OpArg where
    boxing env (OpArg op e)         = do (ws1,e1) <- boxing env e
                                         return (ws1, OpArg op e1)

instance Boxing Stmt where
    boxing env (Expr l e)           = do (ws1,e1) <- boxing env e
                                         return (ws1, Expr l e1)
    boxing env (Assign l [pt@(PVar _ _ Nothing)] e@(Call _ (Dot _ (Var _ (NoQ w)) attr) p KwdNil))  -- (re)introduce augmented arithmetic operator for unboxable types; Assign case
      | isWitness w && attr `elem` augopKWs && isUnboxable t
                                    = do (ws, p') <- boxing env p
                                         let [x1,x2] = posargs p'
                                         return (ws, AugAssign l (unbox t x1) op (unbox t x2))
          where t                      = typeOf env e
                op                     = bin2Aug attr

    boxing env (Assign l [p@(PVar _ n (Just t))] e)
                                    = do (ws1,p') <- boxing env p
                                         (ws2, e') <- boxing env e
                                         return (HashSet.union ws1 ws2, Assign l [pvar p'] (fixassign env (asUnboxedRep t) e'))
      where pvar (PVar l n _)       = PVar l n (Just (asUnboxedRep t))
            pvar p                  = p
    boxing env s@(Assign l [pt@(PVar _ n Nothing)] e)
                                   = do (ws1,pt1) <- boxing env pt
                                        (ws2,e2) <- boxing env e
                                        return (HashSet.union ws1 ws2,Assign l [pt1] (fixassign env t e2))
             where t               = assignTargetType env s n e


    boxing env (MutAssign l tg e@(Call _ (Dot _ (Var _ (NoQ w)) attr) p KwdNil))  -- (re)introduce augmented arithmetic operator for unboxable types; MutAssign case
      | isWitness w && attr `elem` augopKWs  && isUnboxable t
                                    = do let [x1,x2] = posargs p
                                         (ws, x2') <- boxing env x2
                                         if unboxedAugTarget env tg
                                           -- target physically stores an unboxed value: do a raw
                                           -- in-place augmented assignment.
                                           then return (ws, AugAssign l x1 op (unbox t x2'))
                                           -- target physically stores a boxed value (e.g. a generic
                                           -- $Cell cell holding an unboxable element): keep the
                                           -- arithmetic unboxed but box the result for the store,
                                           -- rather than doing raw arithmetic on the boxed slot.
                                           else return (ws, MutAssign l tg (Box t (Paren NoLoc (BinOp NoLoc (unbox t x1) binop (unbox t x2')))))
          where t                   = typeOf env e
                op                  = bin2Aug attr
                binop               = bin2Binary (incr2bin attr)
    boxing env (MutAssign l tg e)   = do (ws0,tg1) <- boxingTarget env tg
                                         (ws1,e1) <- boxing env e
                                         return (HashSet.union ws0 ws1, MutAssign l tg1 (fixassign env t e1))
          where t                   = targetType env tg
    boxing env (Pass l)             = return (HashSet.empty, Pass l)
    boxing env (Delete l t)         = return (HashSet.empty, Delete l t)
    boxing env (Return l (Just e))  = do (ws1,e1) <- boxing env e
                                         case getReturnType env of
                                           Just rt -> return (ws1, Return l (Just (fixreturn env rt e e1)))
                                           Nothing -> return (ws1, Return l (Just (fixreturnUnknown env e e1)))
    boxing env (Return l Nothing)   = return (HashSet.empty, Return l Nothing)
    boxing env (Break l)            = return (HashSet.empty, Break l)
    boxing env (Continue l)         = return (HashSet.empty, Continue l)
    boxing env (If l bs ss)         = do (ws1,bs1) <- boxing env bs
                                         (ws2,ss1) <- boxing env ss
                                         return (HashSet.union ws1 ws2,If l bs1 ss1)
    boxing env (While l e ss els)   = do (ws1,e1) <- boxing env e
                                         (ws2,ss1) <- boxing env ss
                                         (ws3,els1) <- boxing env els
                                         return (HashSet.unions [ws1, ws2, ws3], While l e1 ss1 els1)
    boxing env s@(Signature l ns sc d) = return (HashSet.empty, s)
    boxing env g@(Decl l ds)        = do (ws1, ds1) <- boxing env1 ds
                                         return (ws1, Decl l ds1)
      where te                      = envOf g
            env1                    = define te env
    boxing env s                    = error ("Unhandled boxing " ++show s)
 
instance Boxing Decl where
    boxing env (Class l n q cs ss ddoc)
                                    = do (ws1, ss1) <- boxing env2 ss'
                                         return (ws1, Class l n q cs ss1 ddoc)
        where ss'                   = vsubst [(tvSelf, tCon c)] ss
              c                     = TC (NoQ n) (map tVar $ qbound q)
              env1                  = defineTVars q env
              env2                  = Acton.Boxing.setInClass (Just(c,findAttrSchemas env1 (NoQ n))) env1
    boxing env f@(Def l n q p KwdNIL t ss dec fx ddoc)
                                    = do (ws2,ss') <- boxing env1 ss
                                         return (ws2, Def l n q pFinal KwdNIL (Just t') ss' dec fx ddoc)
      where ft                      = tFun fx (prowOf p) kwdNil (fromJust t)
            (pFinal, t0@(TFun _ _ _ _ t'))
                                    = case getInClass env of
                                          Just(tc,_)
                                            | dec == Static -> (fixpars p r, mt)
                                            | otherwise     -> (keepSelf p r, mt)
                                             where mt@(TFun _ _ r _ _) = rtypeOf env tc n
                                          Nothing -> (fixpars p r, mt)
                                             where mt@(TFun _ _ r _ _) = maybe (matchTypes ft ft) (matchTypes ft) (generalType env n)
            keepSelf (PosPar a b c p') r
                                    = PosPar a b c (fixpars p' r)
            keepSelf p r            = fixpars p r
            te                      = envOf pFinal
            env1                    = setReturnType (Just t') $ setInAction (fx == fxAction) $ define te env
  
instance Boxing Branch where
    boxing env (Branch e ss)       = do (ws1,e') <- boxing env e
                                        (ws2,ss') <- boxing env ss
                                        return (HashSet.union ws1 ws2, Branch e' ss')

instance Boxing PosPar where
    boxing env (PosPar n (Just t) e p)
                                   = do (ws1, e1) <- boxing env e
                                        (ws2, p2) <- boxing env p
                                        return (HashSet.union ws1 ws2, PosPar n (Just t) e1 p2) -- keep boxed if has default value (if isUnboxable t then maybe Nothing (Just . unbox t) e1 else e1)  p2)
    boxing env (PosSTAR n t)       = return (HashSet.empty, PosSTAR n t)
    boxing env PosNIL              = return (HashSet.empty,PosNIL)


instance Boxing PosArg where
    boxing env (PosArg e p)        = do (ws1,e1) <- boxing env e
                                        (ws2,p1) <- boxing env p
                                        return (HashSet.union ws1 ws2, PosArg e1 p1)
      where t                      = typeOf env e
    boxing env (PosStar e)         = do (ws1,e1) <- boxing env e
                                        return (ws1, PosStar e1)
    boxing env PosNil              = return (HashSet.empty,PosNil)

instance Boxing Elem where
    boxing env (Elem e)            = do (ws1,e1) <- boxing env e
                                        return (ws1, Elem (boxValueExpr env e e1))
    boxing env (Star e)            = do (ws1,e1) <- boxing env e
                                        return (ws1, Star e1)

instance Boxing Assoc where
    boxing env (Assoc k v)         = do (ws1,k1) <- boxing env k
                                        (ws2,v1) <- boxing env v
                                        return (HashSet.union ws1 ws2, Assoc (boxValueExpr env k k1) (boxValueExpr env v v1))
    boxing env (StarStar e)        = do (ws1,e1) <- boxing env e
                                        return (ws1, StarStar e1)

instance Boxing Pattern where
    boxing env v@(PVar l n mbt)    = return (HashSet.empty,v)
    
bin2Binary kw
   | kw == addKW                   = Plus
   | kw == subKW                   = Minus
   | kw == mulKW                   = Mult
   | kw == powKW                   = Pow
   | kw == truedivKW               = Div
   | kw == modKW                   = Mod
   | kw == floordivKW              = EuDiv
   | kw == lshiftKW                = ShiftL
   | kw == rshiftKW                = ShiftR
   | kw == orKW                    = BOr
   | kw == xorKW                   = BXor
   | kw == andKW                   = BAnd

un2Unary kw
   | kw == posKW                   = UPlus
   | kw == negKW                   = UMinus
   | kw == invertKW                = BNot

cmp2Comparison kw
   | kw == eqKW                    = Eq
   | kw == neKW                    = NEq
   | kw == ltKW                    = Lt
   | kw == leKW                    = LE
   | kw == gtKW                    = Gt
   | kw == geKW                    = GE
   | kw == isKW                    = Is
   | kw == isnotKW                 = IsNot


bin2Aug kw
   | kw == iaddKW                  = PlusA
   | kw == isubKW                  = MinusA
   | kw == imulKW                  = MultA
   | kw == ipowKW                  = PowA
   | kw == itruedivKW              = DivA
   | kw == imodKW                  = ModA
   | kw == ifloordivKW             = EuDivA
   | kw == ilshiftKW               = ShiftLA
   | kw == irshiftKW               = ShiftRA
   | kw == iorKW                   = BOrA
   | kw == ixorKW                  = BXorA
   | kw == iandKW                  = BAndA

-- Map an augmented-assignment witness keyword to its plain binary counterpart.
-- imatmulKW is deliberately omitted: it has no unboxable operand type (matmul is
-- matrix-only), so it never reaches the only caller (which is guarded by
-- isUnboxable), and bin2Binary has no matmul case either. Keeping the range a
-- subset of bin2Binary's domain avoids a non-exhaustive crash on future changes.
incr2bin kw
   | kw == iaddKW                  = addKW
   | kw == isubKW                  = subKW
   | kw == imulKW                  = mulKW
   | kw == ipowKW                  = powKW
   | kw == itruedivKW              = truedivKW
   | kw == imodKW                  = modKW
   | kw == ifloordivKW             = floordivKW
   | kw == ilshiftKW               = lshiftKW
   | kw == irshiftKW               = rshiftKW
   | kw == iorKW                   = orKW
   | kw == ixorKW                  = xorKW
   | kw == iandKW                  = andKW
