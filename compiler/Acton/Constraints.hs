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


unify t1 t2                                 = do -- traceM ("unify " ++ prstr (QEqu l0 0 t1 t2))
                                                 unify' t1 t2

unify' (OVar tv1) (OVar tv2)
  | tv1 == tv2                              = return ()
unify' (OVar tv) t2                         = do s <- substitution
                                                 case Map.lookup tv s of
                                                   Just t1 -> unify' t1 t2
                                                   Nothing -> do t2' <- mapsubst t2
                                                                 when (tv `elem` tyvars t2') (throwError $ InfiniteType tv)
                                                                 substitute tv t2'
unify' t1 (OVar tv)                         = do s <- substitution
                                                 case Map.lookup tv s of
                                                   Just t2 -> unify' t1 t2
                                                   Nothing -> do t1' <- mapsubst t1
                                                                 when (tv `elem` tyvars t1') (throwError $ InfiniteType tv)
                                                                 substitute tv t1'

--       as declared      as called
unify' (OFun a1 r1 t1) (OFun a2 r2 t2)      = do unify a1 a2
                                                 unify r2 r1            -- contra-variant
                                                 unify t1 t2
unify' (ORecord r1) (ORecord r2)            = unify r1 r2
unify' (ODict k1 v1) (ODict k2 v2)          = do unify k1 k2
                                                 unify v1 v2
unify' (OTuple r1) (OTuple r2)              = unify r1 r2
unify' (OList t1) (OList t2)                = unify t1 t2
unify' (OSet t1) (OSet t2)                  = unify t1 t2
unify' (OMsg t1) (OMsg t2)                  = unify t1 t2
unify' OStr OStr                            = return ()
unify' OInt OInt                            = return ()
unify' OFloat OFloat                        = return ()
unify' OBool OBool                          = return ()
unify' ONone ONone                          = return ()
unify' _     ONone                          = return ()         -- temporary, until the opt type...
unify' ONone _                              = return ()         -- temporary, until the opt type...


unify' (OPos t1 r1) r2                      = do (t2,r2') <- findFst r2 (rowTail r1)
                                                 unify t1 t2
                                                 unify r1 r2'
  where findFst r tl                        = do r' <- mapsubst r
                                                 tl' <- mapsubst tl
                                                 findFst' r' tl'
        findFst' (OPos t r2) tl             = return (t, r2)
        findFst' (OStar1 t ONil) tl         = do r3 <- newOVar
                                                 unify t (OTuple r3)
                                                 (t',r3') <- findFst r3 tl
                                                 return (t', r3')
        findFst' r2@OStar1{} tl             = internal r1 r2
        findFst' (OKwd n t r2) tl           = return (t, r2)
        findFst' (OStar2 t ONil) tl         = do r3 <- newOVar
                                                 a2 <- newOVar
                                                 unify t (ORecord r3)
                                                 (t',r3') <- findFst r3 tl
                                                 return (t', r3')
        findFst' r2@OStar2{} tl             = internal r1 r2
        findFst' ONil tl                    = throwError EmptyRow
        findFst' r2@(OVar tv) tl
          | r2 == tl                        = throwError (ConflictingRow tv)
          | otherwise                       = do t <- newOVar
                                                 r <- newOVar
                                                 substitute tv (OPos t r)
                                                 return (t, r)
        findFst' r tl                       = error ("##### findFst' " ++ prstr r)

unify' (OStar1 t r1) r2                     = do r1' <- mapsubst r1
                                                 case r1' of
                                                    ONil -> do
                                                        r <- newOVar
                                                        unify t (OTuple r)
                                                        unify r r2
                                                    _ -> throwError Defer

unify' r1 (OStar1 t r2)                     = do r2' <- mapsubst r2
                                                 case r2' of
                                                    ONil -> do
                                                        r <- newOVar
                                                        unify (OTuple r) t
                                                        unify r1 r
                                                    _ -> throwError Defer

unify' (OKwd n t1 r1) r2                    = do (t2,r2') <- findKwd ONil n r2 (rowTail r1)
                                                 unify t1 t2
                                                 unify (del n r1) (del n r2')
  where findKwd r0 n r tl                   = do r0' <- mapsubst r0
                                                 r'  <- mapsubst r
                                                 tl' <- mapsubst tl
                                                 findKwd' r0' n r' tl'
        findKwd' r0 n (OKwd n1 t r2) tl
          | n == n1                         = return (t, revApp r0 r2)
          | otherwise                       = findKwd' (OKwd n1 t r0) n r2 tl
        findKwd' r0 n (OStar2 t ONil) tl    = do r3 <- newOVar
                                                 a2 <- newOVar
                                                 unify t (ORecord r3)
                                                 (t',r3') <- findKwd ONil n r3 tl
                                                 return (t', revApp r0 r3')
        findKwd' r0 n r2@OStar2{} tl        = internal r1 r2
        findKwd' r0 n (OPos t r2) tl        = findKwd' (OPos t r0) n r2 tl
        findKwd' r0 n (OStar1 t ONil) tl    = do r3 <- newOVar
                                                 unify t (OTuple r3)
                                                 findKwd r0 n r3 tl
        findKwd' r0 n r2@OStar1{} tl        = internal r1 r2
        findKwd' r0 n ONil tl               = throwError (KwdNotFound n)
        findKwd' r0 n r2@(OVar tv) tl
          | r2 == tl                        = throwError (ConflictingRow tv)
          | otherwise                       = do t <- newOVar
                                                 r <- newOVar
                                                 substitute tv (OKwd n t r)
                                                 return (t, revApp r0 r)

unify' (OStar2 t r1) r2                     = do r <- newOVar
                                                 unify t (ORecord r)
                                                 r' <- mapsubst r
                                                 r1' <- mapsubst r1
                                                 unify (catRow r' r1') r2

unify' r1 (OStar2 t r2)                     = do r <- newOVar
                                                 unify (ORecord r) t
                                                 r' <- mapsubst r
                                                 r2' <- mapsubst r2
                                                 unify r1 (catRow r' r2')

unify' ONil ONil                            = return ()

unify' (OSchema vs1 cs1 t1) (OSchema vs2 cs2 t2)
  | vs1==vs1 && cs1==cs2 && t1==t2          = return ()

-- Will go away when Rank-N records become nominal
unify' (OSchema vs1 [] t1) t2               = do ts <- mapM (const newOVar) vs1
                                                 let s = vs1 `zip` ts
                                                 unify' (subst s t1) t2

unify' (OList t1) (ODict k2 v2)             = do unify t1 k2                  -- Temporary HACK...
                                                 unify OInt v2


unify' t1 t2                                = throwError (NoUnify (simplify t1) (simplify t2))



catRow (OPos t r1) r2                       = OPos t (catRow r1 r2)
catRow (OKwd n t r1) r2                     = OKwd n t (catRow r1 r2)
catRow (OStar1 t r1) r2                     = OStar1 t (catRow r1 r2)
catRow (OStar2 t r1) r2                     = OStar2 t (catRow r1 r2)
catRow ONil r2                              = r2
catRow r1 ONil                              = r1
catRow r1 r2                                = internal r1 r2

rowTail (OPos _ r)                      = rowTail r
rowTail (OStar1 _ r)                    = rowTail r
rowTail (OKwd _ _ r)                    = rowTail r
rowTail (OStar2 _ r)                    = rowTail r
rowTail r                               = r             -- OVar v or ONil

revApp (OPos t r1) r2                   = revApp r1 (OPos t r2)
revApp (OStar1 t r1) r2                 = revApp r1 (OStar1 t r2)
revApp (OKwd n t r1) r2                 = revApp r1 (OKwd n t r2)
revApp (OStar2 t r1) r2                 = revApp r1 (OStar2 t r2)
revApp ONil r2                          = r2

del n (OKwd m t r)
  | n == m                              = del n r
  | otherwise                           = OKwd m t (del n r)
del n (OStar2 (ORecord r1) r)           = OStar2 (ORecord (del n r1)) (del n r)
del n (OStar2 t r)                      = OStar2 t (del n r)
del n (OPos t r)                        = OPos t (del n r)
del n (OStar1 t r)                      = OStar1 t (del n r)
del n r                                 = r

internal r1 r2                          = error ("Internal: Cannot unify: " ++ prstr r1 ++ " = " ++ prstr r2)



type OVarMap                            = Map OVar OType

type Deferred                           = [Qonstraint]

type EffectStack                        = [OType]

type Unique                             = Int

type SolveState                         = (Unique, [Qonstraint], EffectStack, Deferred, SrcInfo, OVarMap)

type TypeM a                            = ExceptT SolveErr (State SolveState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = case evalState (runExceptT m) (1,[],[],[],[],Map.empty) of
                                            Right x  -> x
                                            Left err -> error "Unhandled constraint-solver exception"

unique                                  = lift $ state $ \(u,c,f,d,i,s) -> (u, (u+1,c,f,d,i,s))

constrain cs                            = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,cs++c,f,d,i,s))

pushFX t                                = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,t:f,d,i,s))

currentFX                               :: TypeM OType
currentFX                               = lift $ state $ \(u,c,t:f,d,i,s) -> (t, (u,c,t:f,d,i,s))

effect l fx                             = do fx0 <- currentFX
                                             constrain [QEqu l 36 fx fx0]

defer cs                                = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,cs:d,i,s))

substitute tv t                         = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,d,i,Map.insert tv t s))

dump info1                              = lift $ state $ \(u,c,f,d,i,s) -> ((), (u,c,f,d,reverse info1 ++ i,s))

constraints                             :: TypeM [Qonstraint]
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
    
instance MapSubst (OVar,OType) where
    mapsubst (tv, t)                    = mapsubst t >>= \t' -> return (tv,t')

instance MapSubst OType where
    mapsubst (OVar l)                   = do s <- substitution
                                             case Map.lookup l s of
                                                 Just t  -> mapsubst t
                                                 Nothing -> return (OVar l)
    mapsubst (OFun act row t)           = OFun <$> mapsubst act <*> mapsubst row <*> mapsubst t
    mapsubst (ORecord row)              = ORecord <$> mapsubst row
    mapsubst (ODict t1 t2)              = ODict <$> mapsubst t1 <*> mapsubst t2
    mapsubst (OTuple pos)               = OTuple <$> mapsubst pos
    mapsubst (OList t)                  = OList <$> mapsubst t
    mapsubst (OSet t)                   = OSet <$> mapsubst t
    mapsubst (OPos t r)                 = OPos <$> mapsubst t <*> mapsubst r
    mapsubst (OStar1 t r)               = OStar1 <$> mapsubst t <*> mapsubst r
    mapsubst (OKwd n t r)               = OKwd n <$> mapsubst t <*> mapsubst r
    mapsubst (OStar2 t r)               = OStar2 <$> mapsubst t <*> mapsubst r
    mapsubst (OSchema vs cs t)          = OSchema vs <$> mapsubst cs <*> mapsubst t     -- vs are always disjoint from dom(subst)
    mapsubst t                          = return t

instance MapSubst Qonstraint where
    mapsubst (QEqu l v t1 t2)           = QEqu l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (QIn l v t1 t2)            = QIn l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (QDot l v t1 n t2)         = QDot l v <$> mapsubst t1 <*> return n <*> mapsubst t2
    mapsubst (QIx l v t1 t2 t3)         = QIx l v <$> mapsubst t1 <*> mapsubst t2 <*> mapsubst t3
    mapsubst (QMod l v t1 t2)           = QMod l v <$> mapsubst t1 <*> mapsubst t2
    mapsubst (QPlus l v t1)             = QPlus l v <$> mapsubst t1
    mapsubst (QNum l v t1)              = QNum l v <$> mapsubst t1
    mapsubst (QBool l v t1)             = QBool l v <$> mapsubst t1


newOVar                                 = intOVar <$> unique

newOVars n                              = mapM (const newOVar) [1..n]

newTEnv vs                              = (vs `zip`) <$> mapM (const newOVar) (nub vs)

instantiate l t0@(OSchema vs cs t)      = do tvs <- newOVars (length vs)
                                             let s   = vs `zip` tvs
                                                 cs1 = [ subst s c{cloc=l} | c <- cs ]
                                                 t1  = subst s t
                                             constrain cs1
                                             dump [INS l t1]
                                             return t1
instantiate l t                         = do dump [INS l t]
                                             return t


data SolveErr                           = Defer
                                        | EmptyRow
                                        | KwdNotFound Name
                                        | ConflictingRow OVar
                                        | InfiniteType OVar
                                        | NoUnify OType OType
                                        | NoSelect Name
                                        | NoOverload Qonstraint
                                        | NoSolve Qonstraint SolveErr
                                        | NotYet SrcLoc Doc
                                        deriving (Eq,Show,Typeable)

instance OSubst SolveErr where
    tyvars (ConflictingRow tv)          = [tv]
    tyvars (InfiniteType tv)            = [tv]
    tyvars (NoUnify t1 t2)              = (tyvars t1 ++ tyvars t2) \\ [[]]
    tyvars (NoSolve c err)              = (tyvars c ++ tyvars err) \\ [[]]
    tyvars _                            = []

    subst s (ConflictingRow tv)         = case lookup tv s of
                                            Just (OVar tv') -> ConflictingRow tv'
                                            _               -> ConflictingRow tv
    subst s (InfiniteType tv)           = case lookup tv s of
                                            Just (OVar tv') -> InfiniteType tv'
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
          where isR ONil                = True
                isR (OPos _ _)          = True
                isR (OKwd _ _ _)        = True
                isR (OStar1 _ _)        = True
                isR (OStar2 _ _)        = True
                isR _                   = False
        expl (NoSolve c err)            = parens (text "version" <+> int (vn c)) $$ 
                                          explain c $$ 
                                          nonEmpty (text "because:" <+>) expl err
        expl (NotYet _ doc)             = text "Not yet supported by type inference:" <+> doc
        loc (NoSolve c _)               = cloc c
        loc (NoOverload c)              = cloc c
        loc (NotYet l _)                = l
        loc _                           = l0

explain (QEqu _ v t1 t2)                = text txt1 <+> pretty t1 $$
                                          text txt2 <+> pretty t2 $$
                                          if null txt3 then empty else text txt3
  where (txt1,txt2,txt3)                = equTxt v
explain (QIn _ v t1 t2)                 = text "Type" <+> pretty t2 <+> 
                                          text "does not contain values of type" <+> pretty t1
explain (QDot _ v t1 n t2)              = text "Cannot select attribute" <+> quotes (pretty n) <+> 
                                          text "of type" <+> pretty t2 <+>
                                          text "from value of type" <+> pretty t1
explain (QIx _ v t1 t2 t3)              = text "Cannot select an item of type" <+> pretty t3 <+> 
                                          text "with an index of type" <+> pretty t2 <+>
                                          text "from value of type" <+> pretty t1
explain (QNum _ v t1)                   = text "Type" <+> pretty t1 <+> 
                                          text "is not numeric"
explain (QPlus _ v t1)                  = text "Type" <+> pretty t1 <+> 
                                          text "does not support the '+' operator"
explain (QBool _ v t1)                  = text "Type" <+> pretty t1 <+> 
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
equTxt 202  = ("Generic record type", "does not match", "(impossible)")                                     -- QDot Record gen dump

equTxt n    = ("Types", "and", "do not unify (unknown tag version " ++ show n ++ ")")



simplify x                              = subst s x
  where s                               = [ (tv, wildOVar) | tv <- nub (tyvars x) ]

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

red1 f (QEqu _ _ t1 t2)                     = unify' t1 t2

red1 f (QIn _ _ t (OList u))                = unify' t u
red1 f (QIn _ _ t (ODict k v))              = unify' t k
red1 f (QIn _ _ t OStr)                     = unify' t OStr
red1 f (QIn _ _ t (ORecord r))              = do t1 <- newOVar; unify' t (OTuple (OPos OStr (OPos t1 ONil)))    -- ...hack...

red1 f (QDot l _ (ORecord r) n t)           = case lookupRow n r of
                                                Right t0 -> do
                                                    dump [GEN l t0]
                                                    t1 <- instantiate l $ openFX t0
                                                    cs1 <- constraints
                                                    unify' t1 t
                                                    red f cs1
                                                Left ONil ->
                                                    throwError $ NoSelect n
                                                Left r1  -> do
                                                    r2 <- newOVar
                                                    unify' r1 (OKwd n t r2)
  where lookupRow n (OKwd n' t r)
          | n == n'                         = Right t
          | otherwise                       = lookupRow n r
        lookupRow n (OPos t r)              = lookupRow n r
        lookupRow n r                       = Left r
red1 f (QDot l _ (OList t) n u)
  | nstr n == "append"                      = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "clear"                       = builtinFun l (OFun ONil ONil ONone) u
  | nstr n == "copy"                        = builtinFun l (OFun ONil ONil (OList t)) u
  | nstr n == "extend"                      = builtinFun l (OFun ONil (OPos (OList t) ONil) ONone) u
  | nstr n == "insert"                      = builtinFun l (OFun ONil (OPos OInt (OPos (OList t) ONil)) ONone) u
  | nstr n == "pop"                         = builtinFun l (OFun ONil (OPos OInt ONil) t) u
  | nstr n == "remove"                      = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "reverse"                     = builtinFun l (OFun ONil ONil ONone) u
  | nstr n == "sort"                        = builtinFun l (OFun ONil ONil ONone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (QDot l _ (ODict k v) n u)
  | nstr n == "items"                       = builtinFun l (OFun ONil ONil (OList (OTuple (OPos k (OPos v ONil))))) u
  | nstr n == "keys"                        = builtinFun l (OFun ONil ONil (OList k)) u
  | nstr n == "values"                      = builtinFun l (OFun ONil ONil (OList v)) u
  | nstr n == "update"                      = builtinFun l (OFun ONil (OPos (ODict k v) ONil) ONone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (QDot l _ (OSet t) n u)
  | nstr n == "add"                         = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "clear"                       = builtinFun l (OFun ONil ONil ONone) u
  | nstr n == "pop"                         = builtinFun l (OFun ONil ONil t) u
  | nstr n == "remove"                      = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | nstr n == "discard"                     = builtinFun l (OFun ONil (OPos t ONil) ONone) u
  | otherwise                               = throwError $ NoSelect n
red1 f (QDot l _ OStr n u)
  | nstr n == "capitalize"                  = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "casefold"                    = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "center"                      = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | nstr n == "count"                       = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "encode"                      = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "endswith"                    = builtinFun l (OFun ONil (OPos OStr ONil) OBool) u
  | nstr n == "expandtabs"                  = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "format"                      = do r <- newOVar; builtinFun l (OFun ONil r OStr) u
  | nstr n == "format_map"                  = builtinFun l (OFun ONil (OPos (ODict OStr OStr) ONil) OStr) u
  | nstr n == "find"                        = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
--  | nstr n == "index"                       = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "isalnum"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isalpha"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isdecimal"                   = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isdigit"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isidentifier"                = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "islower"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isnumeric"                   = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isprintable"                 = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isspace"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "istitle"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "isupper"                     = builtinFun l (OFun ONil ONil OBool) u
  | nstr n == "join"                        = builtinFun l (OFun ONil (OPos (OList OStr) ONil) OStr) u
  | nstr n == "ljust"                       = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | nstr n == "lower"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "lstrip"                      = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "partition"                   = builtinFun l (OFun ONil (OPos OStr ONil) (OTuple (OPos OStr (OPos OStr (OPos OStr ONil))))) u
  | nstr n == "replace"                     = builtinFun l (OFun ONil (OPos OStr (OPos OStr ONil)) OStr) u
  | nstr n == "rfind"                       = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "rindex"                      = builtinFun l (OFun ONil (OPos OStr ONil) OInt) u
  | nstr n == "rjust"                       = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | nstr n == "rpartition"                  = builtinFun l (OFun ONil (OPos OStr ONil) (OTuple (OPos OStr (OPos OStr (OPos OStr ONil))))) u
  | nstr n == "rsplit"                      = do v <- newOVar; builtinFun l (OFun ONil (OPos OStr v) (OList OStr)) u
  | nstr n == "rstrip"                      = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "split"                       = do v <- newOVar; builtinFun l (OFun ONil (OPos OStr v) (OList OStr)) u
  | nstr n == "splitlines"                  = builtinFun l (OFun ONil ONil (OList OStr)) u
  | nstr n == "startswith"                  = builtinFun l (OFun ONil (OPos OStr ONil) OBool) u
  | nstr n == "strip"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "swapcase"                    = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "title"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "translate"                   = builtinFun l (OFun ONil (OPos (ODict OInt OStr) ONil) OStr) u
  | nstr n == "upper"                       = builtinFun l (OFun ONil ONil OStr) u
  | nstr n == "zfill"                       = builtinFun l (OFun ONil (OPos OInt ONil) OStr) u
  | otherwise                               = throwError $ NoSelect n

red1 f (QIx _ _ (OList t) i u)              = do unify' OInt i; unify t u
red1 f (QIx _ _ (ODict k v) i u)            = do unify' k i; unify v u
red1 f (QIx _ _ OStr i u)                   = do unify' OInt i; unify OStr u

red1 f (QMod _ _ OStr ORecord{})            = return ()
red1 f (QMod _ _ OStr OTuple{})             = return ()
red1 f (QMod _ _ OStr OList{})              = return ()
red1 f (QMod _ _ OStr OSet{})               = return ()
red1 f (QMod _ _ OStr OStr)                 = return ()
red1 f (QMod _ _ OStr OInt)                 = return ()
red1 f (QMod _ _ OStr OFloat)               = return ()
red1 f (QMod _ _ OStr OBool)                = return ()
red1 f (QMod _ _ OInt t)                    = unify' OInt t
red1 f (QMod _ _ OFloat t)                  = unify' OFloat t

red1 f (QPlus _ _ OList{})                  = return ()
red1 f (QPlus _ _ OSet{})                   = return ()
red1 f (QPlus _ _ OStr)                     = return ()
red1 f (QPlus _ _ OInt)                     = return ()
red1 f (QPlus _ _ OFloat)                   = return ()

red1 f (QNum _ _ OInt)                      = return ()
red1 f (QNum _ _ OFloat)                    = return ()

red1 f (QBool _ _ ORecord{})                = return ()
red1 f (QBool _ _ ODict{})                  = return ()
red1 f (QBool _ _ OTuple{})                 = return ()
red1 f (QBool _ _ OList{})                  = return ()
red1 f (QBool _ _ OSet{})                   = return ()
red1 f (QBool _ _ OMsg{})                   = return ()
red1 f (QBool _ _ OStr)                     = return ()
red1 f (QBool _ _ OInt)                     = return ()
red1 f (QBool _ _ OFloat)                   = return ()
red1 f (QBool _ _ OBool)                    = return ()
red1 f (QBool _ _ ONone)                    = return ()

red1 False (QIn _ _ _ OVar{})               = throwError Defer
red1 False (QDot _ _ OVar{} _ _)            = throwError Defer
red1 False (QIx _ _ OVar{} _ _)             = throwError Defer
red1 False (QMod _ _ OVar{} _)              = throwError Defer
red1 False (QPlus _ _ OVar{})               = throwError Defer
red1 False (QNum _ _ OVar{})                = throwError Defer
red1 False (QBool _ _ OVar{})               = throwError Defer
red1 False c                                = throwError $ NoOverload c

red1 True (QIn l _ t u)                     = do v <- newOVar; unify' (ODict t v) u
red1 True c@(QDot _ _ t n u)
  | nstr n `elem` listmeths                 = do v <- newOVar; unify' t (OList v);                          -- temp. fix until backtracking
                                                 c' <- mapsubst c; red1 True c'
  | nstr n `elem` dictmeths                 = do k <- newOVar; v <- newOVar; unify' t (ODict k v);          -- temp. fix until backtracking
                                                 c' <- mapsubst c; red1 True c'
  | nstr n `elem` strmeths                  = do unify' t OStr; c' <- mapsubst c; red1 True c'              -- temp. fix until backtracking
  | otherwise                               = do r <- newOVar; unify' t (ORecord (OKwd n u r))
  where listmeths                           = ["append","clear","copy","extend","insert","pop","remove","reverse","sort"]
        dictmeths                           = ["keys","values","items"]
        strmeths                            = ["capitalize","casefold","center",{-"count",-}"encode","endswith","expandtabs","format",
                                               "format_map","find",{-"index",-}"isalnum","isalpha","isdecimal","isdigit","isidentifier",
                                               "islower","isnumeric","isprintable","isspace","istitle","isupper","join","ljust","lower",
                                               "lstrip","partition","replace","rfind","rindex","rjust","rpartition","rsplit","rstrip",
                                               "split","splitlines","startswith","strip","swapcase",{-"title",-}"translate","upper","zfill"]
red1 True (QIx l _ t OInt u)                = unify' t (OList u)
red1 True (QIx l _ t i u)                   = unify' t (ODict i u)
red1 True (QMod _ _ t u)                    = do unify' t OInt; unify' u OInt
red1 True (QPlus _ _ t)                     = unify' t OInt
red1 True (QNum _ _ t)                      = unify' t OInt
red1 True (QBool _ _ t)                     = unify' t OStr

builtinFun l t u                            = do dump [GEN l t0]
                                                 t1 <- instantiate l $ openFX t0
                                                 unify' t1 u
  where t0                                  = OSchema [] [] t
