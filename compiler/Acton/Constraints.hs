{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Constraints where

import Debug.Trace
import Data.Maybe (maybeToList, fromJust)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Control.Exception
import Data.Typeable
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Env


s1 @@ s2                                = s1 ++ [ (l,subst s1 t) | (l,t) <- s2 ]


unify t1 t2                                 = do -- traceM ("unify " ++ prstr (CEqu l0 0 t1 t2))
                                                 unify' t1 t2

unify' (TVar tv1) (TVar tv2)
  | tv1 == tv2                              = return ()
unify' (TVar tv) t2                         = do s <- substitution
                                                 case Map.lookup tv s of
                                                   Just t1 -> unify' t1 t2
                                                   Nothing -> do t2' <- mapsubst t2
                                                                 when (tv `elem` tyvars t2') (throwError $ InfiniteType tv)
                                                                 substitute tv t2'
unify' t1 (TVar tv)                         = do s <- substitution
                                                 case Map.lookup tv s of
                                                   Just t2 -> unify' t1 t2
                                                   Nothing -> do t1' <- mapsubst t1
                                                                 when (tv `elem` tyvars t1') (throwError $ InfiniteType tv)
                                                                 substitute tv t1'

--       as declared      as called
unify' (TFun a1 r1 t1) (TFun a2 r2 t2)      = do unify a1 a2
                                                 unify r2 r1            -- contra-variant
                                                 unify t1 t2
unify' (TRecord r1) (TRecord r2)            = unify r1 r2
unify' (TDict k1 v1) (TDict k2 v2)          = do unify k1 k2
                                                 unify v1 v2
unify' (TTuple r1) (TTuple r2)              = unify r1 r2
unify' (TList t1) (TList t2)                = unify t1 t2
unify' (TSet t1) (TSet t2)                  = unify t1 t2
unify' (TMsg t1) (TMsg t2)                  = unify t1 t2
unify' TStr TStr                            = return ()
unify' TInt TInt                            = return ()
unify' TFloat TFloat                        = return ()
unify' TBool TBool                          = return ()
unify' TNone TNone                          = return ()
unify' _     TNone                          = return ()         -- temporary, until the opt type...
unify' TNone _                              = return ()         -- temporary, until the opt type...


unify' (RPos t1 r1) r2                      = do (t2,r2') <- findFst r2 (rowTail r1)
                                                 unify t1 t2
                                                 unify r1 r2'
  where findFst r tl                        = do r' <- mapsubst r
                                                 tl' <- mapsubst tl
                                                 findFst' r' tl'
        findFst' (RPos t r2) tl             = return (t, r2)
        findFst' (RStar1 t RNil) tl         = do r3 <- newTVar
                                                 unify t (TTuple r3)
                                                 (t',r3') <- findFst r3 tl
                                                 return (t', r3')
        findFst' r2@RStar1{} tl             = internal r1 r2
        findFst' (RKwd n t r2) tl           = return (t, r2)
        findFst' (RStar2 t RNil) tl         = do r3 <- newTVar
                                                 a2 <- newTVar
                                                 unify t (TRecord r3)
                                                 (t',r3') <- findFst r3 tl
                                                 return (t', r3')
        findFst' r2@RStar2{} tl             = internal r1 r2
        findFst' RNil tl                    = throwError EmptyRow
        findFst' r2@(TVar tv) tl
          | r2 == tl                        = throwError (ConflictingRow tv)
          | otherwise                       = do t <- newTVar
                                                 r <- newTVar
                                                 substitute tv (RPos t r)
                                                 return (t, r)
        findFst' r tl                       = error ("##### findFst' " ++ prstr r)

unify' (RStar1 t r1) r2                     = do r1' <- mapsubst r1
                                                 case r1' of
                                                    RNil -> do
                                                        r <- newTVar
                                                        unify t (TTuple r)
                                                        unify r r2
                                                    _ -> throwError Defer

unify' r1 (RStar1 t r2)                     = do r2' <- mapsubst r2
                                                 case r2' of
                                                    RNil -> do
                                                        r <- newTVar
                                                        unify (TTuple r) t
                                                        unify r1 r
                                                    _ -> throwError Defer

unify' (RKwd n t1 r1) r2                    = do (t2,r2') <- findKwd RNil n r2 (rowTail r1)
                                                 unify t1 t2
                                                 unify (del n r1) (del n r2')
  where findKwd r0 n r tl                   = do r0' <- mapsubst r0
                                                 r'  <- mapsubst r
                                                 tl' <- mapsubst tl
                                                 findKwd' r0' n r' tl'
        findKwd' r0 n (RKwd n1 t r2) tl
          | n == n1                         = return (t, revApp r0 r2)
          | otherwise                       = findKwd' (RKwd n1 t r0) n r2 tl
        findKwd' r0 n (RStar2 t RNil) tl    = do r3 <- newTVar
                                                 a2 <- newTVar
                                                 unify t (TRecord r3)
                                                 (t',r3') <- findKwd RNil n r3 tl
                                                 return (t', revApp r0 r3')
        findKwd' r0 n r2@RStar2{} tl        = internal r1 r2
        findKwd' r0 n (RPos t r2) tl        = findKwd' (RPos t r0) n r2 tl
        findKwd' r0 n (RStar1 t RNil) tl    = do r3 <- newTVar
                                                 unify t (TTuple r3)
                                                 findKwd r0 n r3 tl
        findKwd' r0 n r2@RStar1{} tl        = internal r1 r2
        findKwd' r0 n RNil tl               = throwError (KwdNotFound n)
        findKwd' r0 n r2@(TVar tv) tl
          | r2 == tl                        = throwError (ConflictingRow tv)
          | otherwise                       = do t <- newTVar
                                                 r <- newTVar
                                                 substitute tv (RKwd n t r)
                                                 return (t, revApp r0 r)

unify' (RStar2 t r1) r2                     = do r <- newTVar
                                                 unify t (TRecord r)
                                                 r' <- mapsubst r
                                                 r1' <- mapsubst r1
                                                 unify (catRow r' r1') r2

unify' r1 (RStar2 t r2)                     = do r <- newTVar
                                                 unify (TRecord r) t
                                                 r' <- mapsubst r
                                                 r2' <- mapsubst r2
                                                 unify r1 (catRow r' r2')

unify' RNil RNil                            = return ()

unify' (TSchema vs1 cs1 t1) (TSchema vs2 cs2 t2)
  | vs1==vs1 && cs1==cs2 && t1==t2          = return ()

-- Will go away when Rank-N records become nominal
unify' (TSchema vs1 [] t1) t2               = do ts <- mapM (const newTVar) vs1
                                                 let s = vs1 `zip` ts
                                                 unify' (subst s t1) t2

unify' (TList t1) (TDict k2 v2)             = do unify t1 k2                  -- Temporary HACK...
                                                 unify TInt v2


unify' t1 t2                                = throwError (NoUnify (simplify t1) (simplify t2))



catRow (RPos t r1) r2                       = RPos t (catRow r1 r2)
catRow (RKwd n t r1) r2                     = RKwd n t (catRow r1 r2)
catRow (RStar1 t r1) r2                     = RStar1 t (catRow r1 r2)
catRow (RStar2 t r1) r2                     = RStar2 t (catRow r1 r2)
catRow RNil r2                              = r2
catRow r1 RNil                              = r1
catRow r1 r2                                = internal r1 r2

rowTail (RPos _ r)                      = rowTail r
rowTail (RStar1 _ r)                    = rowTail r
rowTail (RKwd _ _ r)                    = rowTail r
rowTail (RStar2 _ r)                    = rowTail r
rowTail r                               = r             -- TVar v or RNil

revApp (RPos t r1) r2                   = revApp r1 (RPos t r2)
revApp (RStar1 t r1) r2                 = revApp r1 (RStar1 t r2)
revApp (RKwd n t r1) r2                 = revApp r1 (RKwd n t r2)
revApp (RStar2 t r1) r2                 = revApp r1 (RStar2 t r2)
revApp RNil r2                          = r2

del n (RKwd m t r)
  | n == m                              = del n r
  | otherwise                           = RKwd m t (del n r)
del n (RStar2 (TRecord r1) r)           = RStar2 (TRecord (del n r1)) (del n r)
del n (RStar2 t r)                      = RStar2 t (del n r)
del n (RPos t r)                        = RPos t (del n r)
del n (RStar1 t r)                      = RStar1 t (del n r)
del n r                                 = r

internal r1 r2                          = error ("Internal: Cannot unify: " ++ prstr r1 ++ " = " ++ prstr r2)



type TVarMap                            = Map TVar Type

type Deferred                           = [Constraint]

type EffectStack                        = [Type]

type Unique                             = Int

type SolveState                         = (Unique, [Constraint], EffectStack, Deferred, SrcInfo, TVarMap)

type TypeM a                            = ExceptT SolveErr (State SolveState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = case evalState (runExceptT m) (1,[],[],[],[],Map.empty) of
                                            Right x  -> x
                                            Left err -> error "Unhandled constraint-solver exception"

unique                                  = lift $ state $ \(u,c,f,d,i,s) -> (u, (u+1,c,f,d,i,s))

constrain cs                            = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,cs++c,f,d,i,s))

pushFX t                                = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,t:f,d,i,s))

currentFX                               :: TypeM Type
currentFX                               = lift $ state $ \(u,c,t:f,d,i,s) -> (t, (u,c,t:f,d,i,s))

effect l fx                             = do fx0 <- currentFX
                                             constrain [CEqu l 36 fx fx0]

defer cs                                = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,cs:d,i,s))

substitute tv t                         = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,d,i,Map.insert tv t s))

dump info1                              = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,d,reverse info1 ++ i,s))

constraints                             :: TypeM [Constraint]
constraints                             = (lift $ state $ \(u,c,f,d,i,s) -> (c, (u,[],f,d,i,s))) >>= mapsubst

popFX                                   :: TypeM ()
popFX                                   = (lift $ state $ \(u,c,t:f,d,i,s) -> ((), (u,c,f,d,i,s)))

deferred                                :: TypeM Deferred
deferred                                = lift $ state $ \(u,c,f,d,i,s) -> (d, (u,c,f,[],i,s))

dumped                                  :: TypeM SrcInfo
dumped                                  = (lift $ state $ \(u,c,f,d,i,s) -> (reverse i, (u,c,f,d,[],s))) >>= mapsubst

substitution                            = lift $ state $ \(u,c,f,d,i,s) -> (s, (u,c,f,d,i,s))

resetsubst False                        = return ()
resetsubst True                         = do (u,c,f,d,i,s) <- lift get
                                             c <- mapsubst c
                                             f <- mapsubst f
                                             i <- mapsubst i
                                             lift $ put (u,c,f,d,i,Map.empty)

realsubst                               = do s <- substitution
                                             mapsubst (Map.toList s)

class MapSubst a where
    mapsubst                            :: a -> TypeM a

instance MapSubst InfoTag where
    mapsubst (GEN l t)                  = GEN l <$> mapsubst t
    mapsubst (INS l t)                  = INS l <$> mapsubst t

instance MapSubst a => MapSubst [a] where
    mapsubst                            = mapM mapsubst

instance MapSubst a => MapSubst (Maybe a) where
    mapsubst                            = mapM mapsubst

instance MapSubst a => MapSubst (Name,a) where
    mapsubst (v, t)                     = mapsubst t >>= \t' -> return (v,t')
    
instance MapSubst (TVar,Type) where
    mapsubst (tv, t)                    = mapsubst t >>= \t' -> return (tv,t')

instance MapSubst Type where
    mapsubst (TVar l)                   = do s <- substitution
                                             case Map.lookup l s of
                                                 Just t  -> mapsubst t
                                                 Nothing -> return (TVar l)
    mapsubst (TFun act row t)           = TFun <$> mapsubst act <*> mapsubst row <*> mapsubst t
    mapsubst (TRecord row)              = TRecord <$> mapsubst row
    mapsubst (TDict t1 t2)              = TDict <$> mapsubst t1 <*> mapsubst t2
    mapsubst (TTuple pos)               = TTuple <$> mapsubst pos
    mapsubst (TList t)                  = TList <$> mapsubst t
    mapsubst (TSet t)                   = TSet <$> mapsubst t
    mapsubst (RPos t r)                 = RPos <$> mapsubst t <*> mapsubst r
    mapsubst (RStar1 t r)               = RStar1 <$> mapsubst t <*> mapsubst r
    mapsubst (RKwd n t r)               = RKwd n <$> mapsubst t <*> mapsubst r
    mapsubst (RStar2 t r)               = RStar2 <$> mapsubst t <*> mapsubst r
    mapsubst (TSchema vs cs t)          = TSchema vs <$> mapsubst cs <*> mapsubst t     -- vs are always disjoint from dom(subst)
    mapsubst t                          = return t

instance MapSubst Constraint where
    mapsubst (CEqu l v t1 t2)           = CEqu l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (CIn l v t1 t2)            = CIn l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (CDot l v t1 n t2)         = CDot l v <$> mapsubst t1 <*> return n <*> mapsubst t2
    mapsubst (CIx l v t1 t2 t3)         = CIx l v <$> mapsubst t1 <*> mapsubst t2 <*> mapsubst t3
    mapsubst (CMod l v t1 t2)           = CMod l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (CPlus l v t1)             = CPlus l v <$> mapsubst t1
    mapsubst (CNum l v t1)              = CNum l v <$> mapsubst t1
    mapsubst (CBool l v t1)             = CBool l v <$> mapsubst t1


newTVar                                 = intTVar <$> unique

newTVars n                              = mapM (const newTVar) [1..n]

newTEnv vs                              = (vs `zip`) <$> mapM (const newTVar) (nub vs)

instantiate l t0@(TSchema vs cs t)      = do tvs <- newTVars (length vs)
                                             let s  = vs `zip` tvs
                                                 cs = [ subst s c{cloc=l} | c <- cs ]
                                                 t1 = subst s t
                                             constrain cs
                                             dump [INS l t1]
                                             return t1
instantiate l t                         = do dump [INS l t]
                                             return t


data SolveErr                           = Defer
                                        | EmptyRow
                                        | KwdNotFound Name
                                        | ConflictingRow TVar
                                        | InfiniteType TVar
                                        | NoUnify Type Type
                                        | NoSelect Name
                                        | NoOverload Constraint
                                        | NoSolve Constraint SolveErr
                                        | NotYet SrcLoc Doc
                                        deriving (Eq,Show,Typeable)

instance Subst SolveErr where
    tyvars (ConflictingRow tv)          = [tv]
    tyvars (InfiniteType tv)            = [tv]
    tyvars (NoUnify t1 t2)              = (tyvars t1 ++ tyvars t2) \\ [[]]
    tyvars (NoSolve c err)              = (tyvars c ++ tyvars err) \\ [[]]
    tyvars _                            = []

    subst s (ConflictingRow tv)         = case lookup tv s of
                                            Just (TVar tv') -> ConflictingRow tv'
                                            _               -> ConflictingRow tv
    subst s (InfiniteType tv)           = case lookup tv s of
                                            Just (TVar tv') -> InfiniteType tv'
                                            _               -> InfiniteType tv
    subst s (NoUnify t1 t2)             = NoUnify (subst s t1) (subst s t2)
    subst s (NoSolve c err)             = NoSolve (subst s c) (subst s err)
    subst s err                         = err

instance Control.Exception.Exception SolveErr

solveError err                          = (loc err,render (expl err))
  where expl EmptyRow                   = text "Positional elements are missing"
        expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
        expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
        expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
        expl (NoSelect n)               = text "Attribute" <+> pretty n <+> text "is not found"
        expl (NoOverload c)             = explain c
        expl (NoUnify t1 t2)
          | isR t1 || isR t2            = text "Actual row" <+> pretty t1 <+> 
                                          text "and expected row" <+> pretty t2 <+> 
                                          text "do not match"
          | otherwise                   = text "Actual type" <+> pretty t1 <+> 
                                          text "and expected type" <+> pretty t2 <+> 
                                          text "do not unify"
          where isR RNil                = True
                isR (RPos _ _)          = True
                isR (RKwd _ _ _)        = True
                isR (RStar1 _ _)        = True
                isR (RStar2 _ _)        = True
                isR _                   = False
        expl (NoSolve c err)            = parens (text "version" <+> int (vn c)) $$ 
                                          explain c $$ 
                                          nonEmpty (text "because:" <+>) expl err
        expl (NotYet _ doc)             = text "Not yet supported by type inference:" <+> doc
        loc (NoSolve c _)               = cloc c
        loc (NoOverload c)              = cloc c
        loc (NotYet l _)                = l
        loc _                           = l0

explain (CEqu _ v t1 t2)                = text txt1 <+> pretty t1 $$
                                          text txt2 <+> pretty t2 $$
                                          if null txt3 then empty else text txt3
  where (txt1,txt2,txt3)                = equTxt v
explain (CIn _ v t1 t2)                 = text "Type" <+> pretty t2 <+> 
                                          text "does not contain values of type" <+> pretty t1
explain (CDot _ v t1 n t2)              = text "Cannot select attribute" <+> quotes (pretty n) <+> 
                                          text "of type" <+> pretty t2 <+>
                                          text "from value of type" <+> pretty t1
explain (CIx _ v t1 t2 t3)              = text "Cannot select an item of type" <+> pretty t3 <+> 
                                          text "with an index of type" <+> pretty t2 <+>
                                          text "from value of type" <+> pretty t1
explain (CNum _ v t1)                   = text "Type" <+> pretty t1 <+> 
                                          text "is not numeric"
explain (CPlus _ v t1)                  = text "Type" <+> pretty t1 <+> 
                                          text "does not support the '+' operator"
explain (CBool _ v t1)                  = text "Type" <+> pretty t1 <+> 
                                          text "does not convert to a truth value"

equTxt 36   = ("Top-level effect", "does not match the expected empty effect", "")                          -- scanTop
equTxt 0    = ("Instance type", "does not match", "")                                                       -- Var inst dump

equTxt 1    = ("Target type", "does not match right-hand side expression type", "")                         -- Assign
equTxt 2    = ("Target type", "does not match right-hand side expression type", "")                         -- AugAssign
equTxt 4    = ("Type", "does not match expected return type", "")                                           -- Return

equTxt 5    = ("Function type", "does not match the expected type", "of the defined function identifier")   -- Def n

equTxt 6    = ("The type of 'self'", "does not match the expected instance type", "")                       -- Class self
equTxt 7    = ("The type of '__init__'", "does not match the expected function type", "")                   -- Class __init__
equTxt 8    = ("Class type", "does not match the expected type", "of the defined class identifier")         -- Class n
equTxt 9    = ("Method type", "does not match the expected type", "")                                       -- Class Def n

equTxt 10   = ("The type of 'self'", "does not match the expected instance type", "")                       -- Actor self
equTxt 11   = ("Actor type", "does not match the expected type", "of the defined actor identifier")         -- Actor n

equTxt 13   = ("Data tree type", "does not match the type", "that is expected of the target")               -- Data
equTxt 14   = ("Decorator type", "does not match the type", "determined by the decorated item")             -- Decorator
equTxt 17   = ("Var target type", "does not match right-hand side expression type", "")                     -- VarAssign

equTxt 12   = ("Method type", "does not match the expected type", "")                                       -- Actor Def n

equTxt 15   = ("Exception type", "does not match the expected type", "of its alias")                        -- ExceptAs
equTxt 16   = ("Item type", "does not match the expected type", "of its alias")                             -- WithItem
equTxt 30   = ("Slice element type", "does not match the expected type", "")                                -- Slice

equTxt 18   = ("Called function's type", "does not match the expected type", "")                            -- Call
equTxt 37   = ("Awaited message's type", "does not match the expected type", "")                            -- Await
equTxt 38   = ("Indexed expression's type", "does not match the expected type", "")                         -- Ix Index
equTxt 39   = ("Sliced expression's type", "does not match the expected type", "")                          -- Ix Slice

equTxt 19   = ("The 'true' branch type", "does not match the expected type", "of the conditional expression")   -- Cond true
equTxt 20   = ("The 'false' branch type", "does not match the expected type", "of the conditional expression")  -- Cond false
equTxt 21   = ("The parameter type", "does not match the expected type", "of the unary operator")           -- UnOp
equTxt 22   = ("Referenced expression type", "does not match the expected tuple type", "")                  -- DotI
equTxt 29   = ("Lambda type", "does not match", "")                                                         -- Lambda inst dump

equTxt 23   = ("List element type", "does not match the expected type", "")                                 -- List
equTxt 24   = ("Unpacking expression type", "does not match the expected list type", "")                    -- List
equTxt 25   = ("Dictionary element type", "does not match the expected type", "")                           -- Dict
equTxt 26   = ("Set element type", "does not match the expected type", "")                                  -- Set
equTxt 27   = ("Unpacking expression type", "does not match the expected set type", "")                     -- Set

equTxt 35   = ("Parenthesized expression type", "does not match", "")                                       -- Paren

equTxt 28   = ("Type of default value", "does not match the expected type", "of the function parameter")    -- Param
equTxt 33   = ("Parameter type", "does not match the type", "implied by the * operator")                    -- StarPar
equTxt 34   = ("Parameter type", "does not match the type", "implied by the ** operator")                   -- StarPar

equTxt 32   = ("Superclass type", "does not match the expected callable type", "")                          -- Arg
equTxt 31   = ("Left comparison operand type", "does not match type", "of the right operand")               -- OpArg

equTxt 101  = ("Union effect", "does not match", "(impossible)")                                            -- Def union fx
equTxt 102  = ("Union effect", "does not match", "(impossible)")                                            -- Class union fx
equTxt 103  = ("Union effect", "does not match", "(impossible)")                                            -- Actor union fx
equTxt 104  = ("Union effect", "does not match", "(impossible)")                                            -- Actor Def union fx
equTxt 105  = ("Union effect", "does not match", "(impossible)")                                            -- Lambda union fx

equTxt 201  = ("Generic type", "does not match", "(impossible)")                                            -- Var gen dump
equTxt 202  = ("Generic record type", "does not match", "(impossible)")                                     -- CDot Record gen dump

equTxt n    = ("Types", "and", "do not unify (unknown tag version " ++ show n ++ ")")



simplify x                              = subst s x
  where s                               = [ (tv, wildTVar) | tv <- nub (tyvars x) ]

solveAll cs                                 = do -- traceM ("##### SolveAll: ")
                                                 -- traceM (prstr cs)
                                                 -- traceM ("##### Resolving "++ show (length cs))
                                                 -- resetsubst True
                                                 resolveAll False cs
                                                 s <- realsubst
                                                 -- traceM ("##### Result:\n" ++ prstr s)
                                                 return ()

reduce cs                                   = do red False cs
                                                 cs0 <- deferred
                                                 cs1 <- mapsubst cs0
                                                 if cs0 == cs1 then return cs1 else reduce cs1

resolve cs                                  = do red True cs
                                                 cs0 <- deferred
                                                 cs1 <- mapsubst cs0
                                                 if cs1 == [] then return () else resolve cs1
                                                 
    
resolveAll f cs                             = do red f cs
                                                 cs0 <- deferred
                                                 if cs0 == [] then return () else do
                                                     cs1 <- mapsubst cs0
                                                     let useForce = cs1 == cs0
                                                     -- traceM ("##### Reduced: "++show useForce++" "++show (length cs1))
                                                     -- when useForce (traceM (prstr cs0))
                                                     resolveAll useForce cs1

red f []                                    = return ()
red f (c : cs)                              = do c' <- mapsubst c
                                                 red1 f c' `catchError` handler c
                                                 red f cs
  where handler c err                       = do c' <- mapsubst c
                                                 case err of
                                                     Defer -> defer c'
                                                     _     -> throwError $ NoSolve (simplify c') err

red1 f (CEqu _ _ t1 t2)                     = unify' t1 t2

red1 f (CIn _ _ t (TList u))                = unify' t u
red1 f (CIn _ _ t (TDict k v))              = unify' t k
red1 f (CIn _ _ t TStr)                     = unify' t TStr
red1 f (CIn _ _ t (TRecord r))              = do t1 <- newTVar; unify' t (TTuple (RPos TStr (RPos t1 RNil)))    -- ...hack...

red1 f (CDot l _ (TRecord r) n t)           = case lookupRow n r of
                                                Right t0 -> do
                                                    dump [GEN l t0]
                                                    t1 <- instantiate l $ openFX t0
                                                    cs1 <- constraints
                                                    unify' t1 t
                                                    red f cs1
                                                Left RNil ->
                                                    throwError $ NoSelect n
                                                Left r1  -> do
                                                    r2 <- newTVar
                                                    unify' r1 (RKwd n t r2)
  where lookupRow n (RKwd n' t r)
          | n == n'                         = Right t
          | otherwise                       = lookupRow n r
        lookupRow n (RPos t r)              = lookupRow n r
        lookupRow n r                       = Left r
red1 f (CDot l _ (TList t) n u)
  | nstr n == "append"                      = builtinFun l (TFun RNil (RPos t RNil) TNone) u
  | nstr n == "clear"                       = builtinFun l (TFun RNil RNil TNone) u
  | nstr n == "copy"                        = builtinFun l (TFun RNil RNil (TList t)) u
  | nstr n == "extend"                      = builtinFun l (TFun RNil (RPos (TList t) RNil) TNone) u
  | nstr n == "insert"                      = builtinFun l (TFun RNil (RPos TInt (RPos (TList t) RNil)) TNone) u
  | nstr n == "pop"                         = builtinFun l (TFun RNil (RPos TInt RNil) t) u
  | nstr n == "remove"                      = builtinFun l (TFun RNil (RPos t RNil) TNone) u
  | nstr n == "reverse"                     = builtinFun l (TFun RNil RNil TNone) u
  | nstr n == "sort"                        = builtinFun l (TFun RNil RNil TNone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (CDot l _ (TDict k v) n u)
  | nstr n == "items"                       = builtinFun l (TFun RNil RNil (TList (TTuple (RPos k (RPos v RNil))))) u
  | nstr n == "keys"                        = builtinFun l (TFun RNil RNil (TList k)) u
  | nstr n == "values"                      = builtinFun l (TFun RNil RNil (TList v)) u
  | nstr n == "update"                      = builtinFun l (TFun RNil (RPos (TDict k v) RNil) TNone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (CDot l _ (TSet t) n u)
  | nstr n == "add"                         = builtinFun l (TFun RNil (RPos t RNil) TNone) u
  | nstr n == "clear"                       = builtinFun l (TFun RNil RNil TNone) u
  | nstr n == "pop"                         = builtinFun l (TFun RNil RNil t) u
  | nstr n == "remove"                      = builtinFun l (TFun RNil (RPos t RNil) TNone) u
  | nstr n == "discard"                     = builtinFun l (TFun RNil (RPos t RNil) TNone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (CDot l _ TStr n u)
  | nstr n == "capitalize"                  = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "casefold"                    = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "center"                      = builtinFun l (TFun RNil (RPos TInt RNil) TStr) u
  | nstr n == "count"                       = builtinFun l (TFun RNil (RPos TStr RNil) TInt) u
  | nstr n == "encode"                      = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "endswith"                    = builtinFun l (TFun RNil (RPos TStr RNil) TBool) u
  | nstr n == "expandtabs"                  = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "format"                      = do r <- newTVar; builtinFun l (TFun RNil r TStr) u
  | nstr n == "format_map"                  = builtinFun l (TFun RNil (RPos (TDict TStr TStr) RNil) TStr) u
  | nstr n == "find"                        = builtinFun l (TFun RNil (RPos TStr RNil) TInt) u
--  | nstr n == "index"                       = builtinFun l (TFun RNil (RPos TStr RNil) TInt) u
  | nstr n == "isalnum"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isalpha"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isdecimal"                   = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isdigit"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isidentifier"                = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "islower"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isnumeric"                   = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isprintable"                 = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isspace"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "istitle"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "isupper"                     = builtinFun l (TFun RNil RNil TBool) u
  | nstr n == "join"                        = builtinFun l (TFun RNil (RPos (TList TStr) RNil) TStr) u
  | nstr n == "ljust"                       = builtinFun l (TFun RNil (RPos TInt RNil) TStr) u
  | nstr n == "lower"                       = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "lstrip"                      = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "partition"                   = builtinFun l (TFun RNil (RPos TStr RNil) (TTuple (RPos TStr (RPos TStr (RPos TStr RNil))))) u
  | nstr n == "replace"                     = builtinFun l (TFun RNil (RPos TStr (RPos TStr RNil)) TStr) u
  | nstr n == "rfind"                       = builtinFun l (TFun RNil (RPos TStr RNil) TInt) u
  | nstr n == "rindex"                      = builtinFun l (TFun RNil (RPos TStr RNil) TInt) u
  | nstr n == "rjust"                       = builtinFun l (TFun RNil (RPos TInt RNil) TStr) u
  | nstr n == "rpartition"                  = builtinFun l (TFun RNil (RPos TStr RNil) (TTuple (RPos TStr (RPos TStr (RPos TStr RNil))))) u
  | nstr n == "rsplit"                      = do v <- newTVar; builtinFun l (TFun RNil (RPos TStr v) (TList TStr)) u
  | nstr n == "rstrip"                      = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "split"                       = do v <- newTVar; builtinFun l (TFun RNil (RPos TStr v) (TList TStr)) u
  | nstr n == "splitlines"                  = builtinFun l (TFun RNil RNil (TList TStr)) u
  | nstr n == "startswith"                  = builtinFun l (TFun RNil (RPos TStr RNil) TBool) u
  | nstr n == "strip"                       = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "swapcase"                    = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "title"                       = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "translate"                   = builtinFun l (TFun RNil (RPos (TDict TInt TStr) RNil) TStr) u
  | nstr n == "upper"                       = builtinFun l (TFun RNil RNil TStr) u
  | nstr n == "zfill"                       = builtinFun l (TFun RNil (RPos TInt RNil) TStr) u
  | otherwise                               = throwError $ NoSelect n

red1 f (CIx _ _ (TList t) i u)              = do unify' TInt i; unify t u
red1 f (CIx _ _ (TDict k v) i u)            = do unify' k i; unify v u
red1 f (CIx _ _ TStr i u)                   = do unify' TInt i; unify TStr u

red1 f (CMod _ _ TStr TRecord{})            = return ()
red1 f (CMod _ _ TStr TTuple{})             = return ()
red1 f (CMod _ _ TStr TList{})              = return ()
red1 f (CMod _ _ TStr TSet{})               = return ()
red1 f (CMod _ _ TStr TStr)                 = return ()
red1 f (CMod _ _ TStr TInt)                 = return ()
red1 f (CMod _ _ TStr TFloat)               = return ()
red1 f (CMod _ _ TStr TBool)                = return ()
red1 f (CMod _ _ TInt t)                    = unify' TInt t
red1 f (CMod _ _ TFloat t)                  = unify' TFloat t

red1 f (CPlus _ _ TList{})                  = return ()
red1 f (CPlus _ _ TSet{})                   = return ()
red1 f (CPlus _ _ TStr)                     = return ()
red1 f (CPlus _ _ TInt)                     = return ()
red1 f (CPlus _ _ TFloat)                   = return ()

red1 f (CNum _ _ TInt)                      = return ()
red1 f (CNum _ _ TFloat)                    = return ()

red1 f (CBool _ _ TRecord{})                = return ()
red1 f (CBool _ _ TDict{})                  = return ()
red1 f (CBool _ _ TTuple{})                 = return ()
red1 f (CBool _ _ TList{})                  = return ()
red1 f (CBool _ _ TSet{})                   = return ()
red1 f (CBool _ _ TMsg{})                   = return ()
red1 f (CBool _ _ TStr)                     = return ()
red1 f (CBool _ _ TInt)                     = return ()
red1 f (CBool _ _ TFloat)                   = return ()
red1 f (CBool _ _ TBool)                    = return ()
red1 f (CBool _ _ TNone)                    = return ()

red1 False (CIn _ _ _ TVar{})               = throwError Defer
red1 False (CDot _ _ TVar{} _ _)            = throwError Defer
red1 False (CIx _ _ TVar{} _ _)             = throwError Defer
red1 False (CMod _ _ TVar{} _)              = throwError Defer
red1 False (CPlus _ _ TVar{})               = throwError Defer
red1 False (CNum _ _ TVar{})                = throwError Defer
red1 False (CBool _ _ TVar{})               = throwError Defer
red1 False c                                = throwError $ NoOverload c

red1 True (CIn l _ t u)                     = do v <- newTVar; unify' (TDict t v) u
red1 True c@(CDot _ _ t n u)
  | nstr n `elem` listmeths                 = do v <- newTVar; unify' t (TList v);                          -- temp. fix until backtracking
                                                 c' <- mapsubst c; red1 True c'
  | nstr n `elem` dictmeths                 = do k <- newTVar; v <- newTVar; unify' t (TDict k v);          -- temp. fix until backtracking
                                                 c' <- mapsubst c; red1 True c'
  | nstr n `elem` strmeths                  = do unify' t TStr; c' <- mapsubst c; red1 True c'              -- temp. fix until backtracking
  | otherwise                               = do r <- newTVar; unify' t (TRecord (RKwd n u r))
  where listmeths                           = ["append","clear","copy","extend","insert","pop","remove","reverse","sort"]
        dictmeths                           = ["keys","values","items"]
        strmeths                            = ["capitalize","casefold","center",{-"count",-}"encode","endswith","expandtabs","format",
                                               "format_map","find",{-"index",-}"isalnum","isalpha","isdecimal","isdigit","isidentifier",
                                               "islower","isnumeric","isprintable","isspace","istitle","isupper","join","ljust","lower",
                                               "lstrip","partition","replace","rfind","rindex","rjust","rpartition","rsplit","rstrip",
                                               "split","splitlines","startswith","strip","swapcase",{-"title",-}"translate","upper","zfill"]
red1 True (CIx l _ t TInt u)                = unify' t (TList u)
red1 True (CIx l _ t i u)                   = unify' t (TDict i u)
red1 True (CMod _ _ t u)                    = do unify' t TInt; unify' u TInt
red1 True (CPlus _ _ t)                     = unify' t TInt
red1 True (CNum _ _ t)                      = unify' t TInt
red1 True (CBool _ _ t)                     = unify' t TStr

builtinFun l t u                            = do dump [GEN l t0]
                                                 t1 <- instantiate l $ openFX t0
                                                 unify' t1 u
  where t0                                  = TSchema [] [] t
