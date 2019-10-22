{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct,reconstruct2,typeError,env2type) where

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
import Acton.Constraints
import qualified InterfaceFiles

reconstruct2                            :: String -> Env -> Module -> IO (TEnv, SrcInfo)
reconstruct2 outname env modul          = let (te,inf) = runTypeM $ (,) <$> infTop env1 suite <*> getDump
                                              te' = [ (n,t) | (n,t) <- te, notemp n ]
                                          in do
                                              InterfaceFiles.writeFile (outname ++ ".ty") te'
                                              return (te', inf)
  where Module _ _ suite                = modul
        env1                            = block (bound suite) env


typeError                               = solveError


chkRedef ss                             = True -- TBD

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


-- Infer -------------------------------

infTop env ss                           = do pushFX fxNil
                                             te <- infEnv env ss
                                             popFX
                                             cs <- collectConstraints
                                             solve cs
                                             msubst te

class Infer a where
    infer                               :: Env -> a -> TypeM Type

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM TEnv

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (TEnv,Type)

-- class InfData a where
--     infData                             :: Env -> a -> TypeM TEnv


splitGen                                :: [TVar] -> TEnv -> Constraints -> TypeM (Constraints, TEnv)
splitGen tvs te cs
  | null ambig_cs                       = return (fixed_cs, mapSnd generalize te)
  | otherwise                           = do solve ambig_cs
                                             cs1 <- simplify (fixed_cs++gen_cs)
                                             te1 <- msubst te
                                             tvs1 <- fmap tyfree $ mapM msubst $ map tVar tvs
                                             splitGen tvs1 te1 cs1
  where 
    (fixed_cs, cs')                     = partition (null . (\\tvs) . tyfree) cs
    (ambig_cs, gen_cs)                  = partition (ambig te . tyfree) cs'
    ambig te vs                         = or [ not $ null (vs \\ tyfree t) | (n, NVar t _) <- te ]
    q_new                               = mkBind gen_cs
    generalize (NVar (TSchema l q t) d) = NVar (closeFX $ TSchema l (subst s (q_new++q)) (subst s t)) d
      where s                           = tybound q_new `zip` map tVar (tvarSupply \\ tvs \\ tybound q)
    mkGen i                             = i
    mkBind                              :: Constraints -> [TBind]
    mkBind cs                           = undefined         -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

genTEnv                                 :: Env -> TEnv -> TypeM TEnv
genTEnv env te                          = do cs <- collectConstraints
                                             cs1 <- simplify cs
                                             te1 <- msubst te
                                             tvs <- fmap tyfree $ mapM msubst $ map tVar $ tyfree env
                                             (cs2, te2) <- splitGen tvs te1 cs1
                                             constrain cs2
                                             dump [ INS (loc v) t | (v, NVar t _) <- te1 ]
                                             dump [ GEN (loc v) t | (v, NVar t _) <- te2 ]
                                             return te2

commonTEnv env tenvs                    = unifyTEnv env tenvs (foldr intersect [] $ map dom tenvs)

-- unionTEnv env tenvs                     = do te1 <- commonTEnv env tenvs
--                                              te2 <- unifyTEnv env tenvs (concatMap dom tenvs \\ dom te1)
--                                              return (te1++te2)                      -- TODO: o_constrain opt te2...


infLiveEnv env x
  | fallsthru x                         = Just <$> infEnv env x
  | otherwise                           = const Nothing <$> infEnv env x

instance InfEnv [Stmt] where
    infEnv env []                       = return []
    infEnv env (s : ss)                 = do te1 <- infEnv env s
                                             te2 <- infEnv (define te1 env) ss
                                             return (te1++te2)

instance InfEnv Stmt where
    infEnv env (Expr _ e)               = do _ <- infer env e
                                             return []
    infEnv env (Assign _ pats e)
      | nodup pats                      = do (te, t1) <- infEnvT env pats
                                             t2 <- infer env e
                                             constrain [Equ t1 t2]                  -- eq of pats and rhs
                                             return te
    infEnv env (AugAssign l pat o e)    = do t1 <- infer env pat                    -- TODO: add op constraint
                                             t2 <- infer env e
                                             constrain [Equ t1 t2]                  -- equality of target and rhs
                                             return []
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
                                             _ <- infEnv env b
                                             _ <- infEnv env els
                                             return []
    infEnv env (For l p e b els)
      | nodup p                         = do (te, t1) <- infEnvT env p
                                             t2 <- infer env e
                                             _ <- infEnv (define te env) b
                                             _ <- infEnv env els
                                             constrain [Impl t2 (cIterable t1)]             -- containment in for loop
                                             return []
    infEnv env (Try _ b hs els fin)     = do te <- infLiveEnv env (b ++ els)
                                             tes <- mapM (infLiveEnv env) hs
                                             te1 <- commonTEnv env $ catMaybes $ te:tes
                                             te2 <- infEnv (define te1 env) fin
                                             return (te1++te2)
    infEnv env (With _ items b)
      | nodup items                     = do te <- infEnv env items
                                             te1 <- infEnv (define te env) b
                                             return $ prune (dom te) te1

    infEnv env (Data l _ _)             = notYet l (text "data syntax")
--    infEnv env (Data l Nothing b)       = do te <- infData env1 b
--                                             let te1 = filter (notemp . fst) te1
--                                             o_constrain [QEqu l 4 (ORecord $ env2row ONil te1) t]    -- assumption on return value
--                                             return []
--      where t                           = o_getReturn env
--            env1                        = o_reserve (filter istemp $ bound b) env

--    infEnv env (Data l (Just p) b)
--      | nodup p                         = do (te0, t) <- infEnvT env p
--                                             (te1,te2) <- partition (istemp . fst) <$> infData env b
--                                             o_constrain [QEqu l 13 (ORecord $ env2row ONil te2) t]   -- eq of target and rhs..
--                                             return (te0 ++ te1)
            
    infEnv env (VarAssign _ pats e)
      | nodup pats                      = do (te, t1) <- infEnvT env pats
                                             t2 <- infer env e
                                             constrain [Equ t1 t2]                      -- eq of pats and rhs
                                             return te
    
    infEnv env (Decl _ ds)
      | not $ null redefs               = err2 redefs "Illegal redefinition:"
      | nodup vs && chkCycles ds        = do te <- newTEnv vs
                                             let env1 = define te env
                                             te1 <- infEnv env1 ds
--                                             constrain [ Equ t (findname v env1) | (v,t) <- te1 ]  -- eq of name and rhs
                                             te2 <- genTEnv env te1
                                             return te2
      where vs                          = bound ds
            redefs                      = filter (not . blocked env) vs


-- instance InfData [Stmt] where
--     infData env []                      = return []
--     infData env (s : ss)                = do te1 <- infData env s
--                                              te2 <- infData (o_define (filter (istemp . fst) te1) env) ss
--                                              unionTEnv env [te1,te2]

-- instance InfData Stmt where
--     infData env (While _ e b els)       = do inferBool env e
--                                              te1 <- infEnv env b
--                                              te2 <- infEnv env els
--                                              unionTEnv env [[], te1, te2]
--     infData env (For _ p e b els)       = undefined
--     infData env (If _ bs els)           = do tes <- mapM (infData env) bs
--                                              te <- infData env els
--                                              unionTEnv env (te:tes)
--     infData env s                       = infEnv env s

-- instance InfData Branch where
--     infData env (Branch e b)            = do inferBool env e
--                                              infData env b

instance InfEnv [Decl] where
    infEnv env ds                       = concat <$> mapM (infEnv env) ds

instance InfEnv Decl where
    infEnv env (Actor l n q p ann b)        -- TODO: schema [q] => (p) -> ann
      | nodup p && noshadow svars p && 
        chkRedef b                      = do pushFX (fxAct tWild)
                                             let env0 = block (bound p ++ bound b ++ svars ++ oldstate) $ define envActorSelf env
                                             (prow, krow, te0) <- infParams env0 p
                                             let env1 = define te0 env0
                                             te1 <- infEnv env1 b
                                             te2 <- genTEnv env te1
                                             fx <- fxAct <$> newTVar
                                             return [(n, nVar (tFun fx prow krow (tRecord $ env2row tNil te2)))]
      where svars                       = statedefs b
            oldstate                    = statescope env

    infEnv env (Def l n q p ann b (Sync _)) -- TODO: schema [q] => (p) -> ann
      | nodup p && noshadow svars p     = do t <- newTVar
                                             pushFX (fxRet t tWild)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             let env1 = block (bound p ++ bound b) env
                                             (prow, krow, te) <- infParams env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- fxSync <$> newTVar
                                             return [(n, nVar (tFun fx prow krow t))]
      where svars                       = statescope env

    infEnv env (Def l n q p ann b Async)    -- TODO: schema [q] => (p) -> ann
      | nodup p && noshadow svars p     = do t <- newTVar
                                             pushFX (fxRet t tWild)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             let env1 = block (bound p ++ bound b) env
                                             (prow, krow, te) <- infParams env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- fxAsync <$> newTVar
                                             return [(n, nVar (tFun fx prow krow ({-tMsg-}t)))]
      where svars                       = statescope env

    infEnv env (Def l n q p ann b modif)    -- TODO: schema [q] => (p) -> ann
      | nodup p                         = do t <- newTVar
                                             fx <- newTVar
                                             pushFX (fxRet t fx)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             let env1 = block (bound p ++ bound b ++ svars) env
                                             (prow, krow, te) <- infParams env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             return [(n, nVar (tFun fx prow krow t))]     -- TODO: modif/NoDecoration
      where svars                       = statescope env

    infEnv env (Class l n q cs b)
      | nodup cs && chkRedef b          = do t0 <- newOVar
                                             inherited <- return ONil --inferSuper env cs
                                             o_pushFX ONil
                                             te <- infEnv env0 b                     -- visible bindings in b???
                                             (l1, t1, te1) <- getInit l <$> mapM (wrap t0) te
                                             r0 <- newOVar
                                             o_popFX
                                             o_constrain [QEqu l 6 t0 (ORecord (env2row inherited te1))]
                                             o_constrain [QEqu l1 7 t1 (OFun ONil r0 ONone)]                 -- assumption on "__init__"
                                             return (OFun ONil r0 t0)
      where env0                        = block (bound b ++ svars) env
            svars                       = statescope env
            methnames                   = [ v | Decl _ ds <- b, Def{dname=v} <- ds ]
            wrap t0 (n,t)
              | n `elem` methnames      = do r':t':fx':_ <- newOVars 3
                                             let internal = OFun fx' (OPos t0 r') t'
                                                 external = OFun fx' r' t'
                                             o_constrain [QEqu (nloc n) 9 t internal]
                                             return (n,external)                        -- assumption on method n
              | otherwise               = return (n,t)
                                        
    infEnv env (Protocol l n q cs b)    = return []       -- undefined
    infEnv env (Extension l n q cs b)   = return []       -- undefined
    infEnv env (Signature l ns t dec)   = return []       -- undefined.....


inferPure env e                         = do pushFX tNil
                                             t <- infer env e
                                             popFX
                                             return t

env2row                                 = foldl (\r (n,NVar t _) -> kwdRow n t r)           -- TODO: stabilize this...

env2type tenv                           = ORecord (env2row ONil tenv)

instance InfEnv Branch where
    infEnv env (Branch e b)             = do inferBool env e
                                             infEnv env b

instance InfEnv [WithItem] where
    infEnv env []                       = return []
    infEnv env (item:items)             = do te1 <- infEnv env item
                                             te2 <- infEnv (o_define te1 env) items
                                             return (te1++te2)

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do _ <- infer env e                   -- TODO: Mgr o_constraint on t
                                             return []
    infEnv env (WithItem e (Just p))    = do t1 <- infer env e                  -- TODO: Mgr o_constraint on t1
                                             (te, t2) <- infEnvT env p
                                             o_constrain [QEqu (loc p) 16 t1 t2]  -- Eq of item and alias 
                                             return te

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do te <- infEnv env ex
                                             te1 <- infEnv (o_define te env) b
                                             return $ o_prune (dom te) te1

instance InfEnv Except where
    infEnv env (ExceptAll _)            = return []
    infEnv env (Except l x)             = do _ <- infer env (Var (nloc x) x)               -- TODO: Constraint on exception type...
                                             return []
    infEnv env (ExceptAs l x n)         = do t:r:fx:_ <- newOVars 3
                                             t1 <- infer env (Var (nloc x) x)              -- TODO: Constraint on exception type...
                                             o_constrain [QEqu l 15 t1 (OFun fx r t)]   -- eq of excpt-value and alias
                                             return [(n,t)]


inferBool env e                         = do t <- infer env e
                                             let cs0 = []                          -- Will become QBool o_constraint....
                                             return ()

instance Infer Index where
    infer env (Index l e)               = infer env e
    infer env (Slice l e1 e2 e3)        = do ts <- mapM (infer env) es
                                             o_constrain [ QEqu (eloc e) 30 t OInt | (t,e) <- ts `zip` es ]
                                             return OInt                          -- TODO: think this through...
      where es                          = concat $ map maybeToList (e1:e1:maybeToList e3)

instance Infer Expr where
    infer env (Var l n)                 = do o_dump [GEN l t0]
                                             o_instantiate l $ o_openFX t0
      where t0                          = o_findVar n env
    infer env (Int _ val s)             = return OInt
    infer env (Float _ val s)           = return OFloat
    infer env e@Imaginary{}             = notYetExpr e
    infer env (Bool _ val)              = return OBool
    infer env (None _)                  = newOVar           -- TODO: replace with open union incluing None
    infer env (NotImplemented _)        = newOVar
    infer env (Ellipsis _)              = newOVar
    infer env (Strings _ ss)            = return OStr
    infer env (BStrings _ ss)           = return OStr
    infer env (UStrings _ ss)           = return OStr
    infer env (Call l e args)           = do t <- infer env e
                                             o_dump [INS (loc e) t]
                                             row <- infer env args
                                             t0:fx:_ <- newOVars 2
                                             o_effect l fx
                                             o_constrain [QEqu l 18 t (OFun fx row t0)]                  -- callability of e
                                             return t0
    infer env (Await l e)               = do t <- infer env e
                                             t0 <- newOVar
                                             fx <- syncFX <$> newOVar
                                             o_effect l fx
                                             o_constrain [QEqu l 37 t t0]
                                             return t0
    infer env (Ix l e [i@Index{}])      = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newOVar
                                             o_constrain [QIx l 38 t ti t0]                               -- indexability of e
                                             return t0
    infer env (Ix l e [s@Slice{}])      = do t <- infer env e
                                             ti <- infer env s
                                             t0 <- newOVar
                                             o_constrain [QIx l 39 t ti t0]                               -- subscriptability of e
                                             return t
    infer env (Cond l e1 e e2)          = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             inferBool env e
                                             t0 <- newOVar
                                             o_constrain [QEqu l 19 t1 t0, QEqu l 20 t2 t0]               -- eq of l/r branch and join
                                             return t0
    infer env (BinOp l e1 (Op _ op) e2)
      | op `elem` logical               = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QBool l 1 t1, QBool l 2 t2]                     -- truth value of e1/e2
                                             return OBool
      | op `elem` bitwise               = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QEqu l 19 t1 OInt, QEqu l 1 t2 OInt]            -- applicability of op
                                             return OInt
      | op == Div                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QEqu l 19 t1 t2, QNum l 1 t1]                   -- eq of args, applicability of '/'
                                             return OFloat
      | op == EuDiv                     = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QEqu l 19 t1 t2, QNum l 1 t1]                   -- eq of args, applicability of '//'
                                             return OInt
      | op == Mod                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QMod l 1 t1 t2]                                 -- applicability of '%'
                                             return t1
      | op == Plus                      = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QEqu l 19 t1 t2, QPlus l 1 t1]                  -- eq of args, applicability of '+'
                                             return t1
      | otherwise                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             o_constrain [QEqu l 19 t1 t2, QNum l 1 t1]                   -- eq of args, applicability of op
                                             return t1
      where logical                     = [Or,And]
            bitwise                     = [BOr,BXor,BAnd,ShiftL,ShiftR]
    infer env (UnOp l (Op _ Not) e)     = do t <- infer env e
                                             o_constrain [QBool l 3 t]                                    -- applicability of 'not'
                                             return OBool
    infer env (UnOp l (Op _ BNot) e)    = do t <- infer env e
                                             o_constrain [QEqu l 21 t OInt]                               -- applicability of '~'
                                             return OInt
    infer env (UnOp l o e)              = do t <- infer env e
                                             o_constrain [QNum l 2 t]                                     -- applicability of o
                                             return t
    infer env (CompOp l e ops)          = do t1 <- infer env e
                                             scanCompOps env t1 ops
                                             return OBool
    infer env (Dot l e n)               = do t <- infer env e
                                             t0 <- newOVar
                                             o_constrain [QDot l 1 t n t0]                                -- n-selectability of e
                                             return t0
    infer env (DotI l e i)              = do t <- infer env e
                                             t0:tvs <- newOVars (i+2)
                                             o_constrain [QEqu l 22 t (OTuple (row t0 tvs i))]            -- index-selectability of e
                                             return t0
      where row t0 (t1:tvs) 0           = OPos t0 t1
            row t0 (t1:tvs) n           = OPos t1 (row t0 tvs (n-1))
    infer env (Lambda l par e)
      | nodup par                       = do t0:fx:_ <- newOVars 2
                                             o_pushFX fx
                                             (te, row) <- infEnvT env par
                                             t <- infer (o_define te env) e
                                             o_popFX
                                             o_dump [INS l $ OFun fx row t]
                                             return (OFun fx row t)
    infer env (Tuple l es)              = do r <- infer env es
                                             return (OTuple r)
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Generator l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (o_define te env) e
                                             return (OList t)                                  -- !! Should be CGenerator...
    infer env (List l es)               = do r <- infer env es
                                             t0 <- newOVar
                                             o_constrain (fold t0 (map loc es) r)
                                             return (OList t0)
      where fold t0 [] ONil             = []
            fold t0 (l:ls) (OPos t r)   = QEqu l 23 t t0 : fold t0 ls r
            fold t0 (l:ls) (OStar1 t r) = QEqu l 24 t (OList t0) : fold t0 ls r
    infer env (ListComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (o_define te env) e
                                             return (OList t)
    infer env (Dict l as)               = do ts <- mapM (infer env) as
                                             t1 <- ODict <$> newOVar <*> newOVar
                                             o_constrain [ QEqu (loc a) 25 t t1 | (t,a) <- ts `zip` as]   -- eq of dict elements
                                             return t1
    infer env (DictComp l e co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (o_define te env) e
                                             return t
    infer env (Set l es)                = do r <- infer env es
                                             t0 <- newOVar
                                             o_constrain (fold t0 (map loc es) r)
                                             return (OSet t0)
      where fold t0 [] ONil             = []
            fold t0 (l:ls) (OPos t r)   = QEqu l 26 t t0 : fold t0 ls r
            fold t0 (l:ls) (OStar1 t r) = QEqu l 27 t (OSet t0) : fold t0 ls r
    infer env (SetComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (o_define te env) e
                                             return (OSet t)
    infer env (Record l fs)             = do te <- infer env fs
                                             return (ORecord te)
    infer env (RecordComp l n e co)     = do te <- infEnv env co
                                             let env1 = o_define te env
                                             _ <- infer env1 (Var (nloc n) n)
                                             _ <- infer env1 e
                                             r <- newOVar
                                             return (ORecord r)                 -- !! Big over-generalization, for now
    infer env (Paren l e)               = infer env e


infParams                               :: Env -> Params -> TypeM (PosRow, KwdRow, TEnv)
infParams env (Params pos NoStar [] kX)
  | not (null pos)                      = infParams env (Params [] NoStar pos kX)
infParams env (Params pos pX kwd kX)    = do (r1, te1) <- infPos env pos pX
                                             (r2, te2) <- infKwd (define te1 env) kwd kX
                                             return (r1, r2, te1++te2)

infPos env [] NoStar                    = (posNil, [])
infPos env [] (StarPar _ n _)           = do r <- newTVar
                                             return (r, [(n, nVar $ tTuple r)])
infPos env (Param n a Nothing : ps) s   = do t <- newTVar
                                             (r, te) <- infPos (define [(n,nVar t)] env) ps s
                                             return (posRow t r, (n,nVar t):te)
infPos env (Param n a (Just e) : ps) s  = do t <- newTVar
                                             t' <- infer env e
                                             constrain [Sub t' t]
                                             (r, te) <- infPos (define [(n,nVar t)] env) ps s
                                             return (posRow t r, (n,nVar t):te)

infKwd env [] NoStar                    = (kwdNil, [])
infKwd env [] (StarPar _ n _)           = do r <- newTVar
                                             return (r, [(n, nVar $ tRecord r)])
infKwd env (Param n a Nothing : ps) s   = do t <- newTVar
                                             (r, te) <- infKwd (define [(n,nVar t)] env) ps s
                                             return (kwdRow n t r, (n,nVar t):te)
infKwd env (Param n a (Just e) : ps) s  = do t <- newTVar
                                             t' <- infer env e
                                             constrain [Sub t' t]
                                             (r, te) <- infKwd (define [(n,nVar t)] env) ps s
                                             return (kwdRow n t r, (n,nVar t):te)


instance Infer Assoc where
    infer env (Assoc k v)               = ODict <$> infer env k <*> infer env v
    infer env (StarStarAssoc e)         = infer env e

instance Infer [Elem Expr] where
    infer env (Elem e : elems)          = OPos <$> infer env e <*> infer env elems
    infer env (Star e : elems)          = OStar1 <$> infer env e <*> infer env elems
    infer env []                        = return ONil

instance Infer [Field] where
    infer env (Field n e : fs)          = (OKwd n) <$> infer env e <*> infer env fs
    infer env (StarStarField e : fs)    = OStar2 <$> infer env e <*> infer env fs
    infer env []                        = return ONil

instance InfEnv Comp where
    infEnv env NoComp                   = return []
    infEnv env (CompIf l e c)           = do inferBool env e
                                             infEnv env c
    infEnv env (CompFor l p e c)        = do (te1, t1) <- infEnvT env p
                                             t2 <- infer env e
                                             o_constrain [QIn l 2 t1 t2]                        -- containment in 'for' comprehension
                                             te2 <- infEnv (o_define te1 env) c
                                             return (te1++te2)

instance Infer Exception where
    infer env (Exception e1 Nothing)    = infer env e1
    infer env (Exception e1 (Just e2))  = do t1 <- infer env e1
                                             _ <- infer env e2                                -- TODO: Constraint on exception type...
                                             return t1

instance InfEnvT [Pattern] where
    infEnvT env [p]                     = infEnvT env p
    infEnvT env (p:ps)                  = do (te1, t1) <- infEnvT env p
                                             (te2, t2) <- infEnvT (o_define te1 env) ps
                                             o_constrain [QEqu (loc p) 1 t1 t2]
                                             return (te1++te2, t1)

instance InfEnvT [Elem Pattern] where
    infEnvT env (Elem p : elems)        = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT (o_define te1 env) elems
                                             return (te1++te2, OPos t r)
    infEnvT env (Star p : elems)        = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT env elems
                                             return (te1++te2, OStar1 t r)
    infEnvT env []                      = return ([], ONil)


instance InfEnvT Pattern where
    infEnvT env (PVar _ n Nothing)
      | o_reserved env n                = do t <- newOVar
                                             return ([(n,t)], t)
      | monotype t                      = return ([], t)
      | otherwise                       = err1 n "Illegal reassignment:"
      where t                           = o_findVar n env
    infEnvT env (PIx l e [i@Index{}])   = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newOVar
                                             o_constrain [QIx l 1 t ti t0]
                                             return ([], t0)                                    -- indexability of e
    infEnvT env (PDot l e n)            = do t <- infer env e
                                             t0 <- newOVar
                                             o_constrain [QDot l 1 t n t0]
                                             return ([], t0)                                    -- n-selectability of e
    infEnvT env (PTuple _ ps)           = do (te, r) <- infEnvT env ps
                                             return (te, OTuple r)
    infEnvT env (PList _ ps)            = do (te, r) <- infEnvT env ps
                                             t0 <- newOVar
                                             o_constrain (fold t0 (map loc ps) r)
                                             return (te, OList t0)
      where fold t0 [] ONil             = []
            fold t0 (l:ls) (OPos t r)   = QEqu l 23 t t0 : fold t0 ls r
            fold t0 (l:ls) (OStar1 t r) = QEqu l 24 t (OList t0) : fold t0 ls r
    infEnvT env (PParen l p)            = infEnvT env p
    infEnvT env (PData l n es)          = do t0 <- newOVar
                                             t <- inferIxs env t0 es
                                             return ([(n,t0)], t)

inferIxs env t0 []                      = return t0
inferIxs env t0 (i:is)                  = do t1 <- newOVar
                                             ti <- infer env i
                                             t2 <- inferIxs env t1 is 
                                             o_constrain [QIx (loc i) 1 t0 ti t1]
                                             return t2

instance Infer Pattern where
    infer env p                         = noenv <$> infEnvT env p
      where noenv ([], t)               = t
            noenv (te, _)               = nameNotFound (head (dom te))
                                             

instance Infer [Arg] where
    infer env (Arg e : args)            = OPos <$> infer env e <*> infer env args
    infer env (StarArg e : args)        = OStar1 <$> infer env e <*> infer env args
    infer env (KwArg n e : args)        = (OKwd n) <$> infer env e <*> infer env args
    infer env (StarStarArg e : args)    = OStar2 <$> infer env e <*> infer env args
    infer env []                        = return ONil

inferSuper env (Arg e : args)           = do t <- inferPure env e
                                             r <- inferSuper env args
                                             r':t':_ <- newOVars 2
                                             o_constrain [QEqu (eloc e) 32 t (OFun ONil r' t')]    -- assumpt on base class
                                             return (OStar2 t' r)
inferSuper env (StarArg e : args)       = do t <- infer env e
                                             r <- inferSuper env args
                                             return (OStar1 t r)                -- Handle at all?
inferSuper env (KwArg n e : args)       = inferSuper env (Arg e : args)
inferSuper env (StarStarArg e:args)     = inferSuper env (StarArg e : args)
inferSuper env []                       = return ONil

scanCompOps env t0 []                   = return ()
scanCompOps env t0 (oparg : opargs)     = do t1 <- inferCompOp env t0 oparg
                                             scanCompOps env t1 opargs
                                             return ()

inferCompOp env t0 (OpArg (Op l o) e)
  | o `elem` [In, NotIn]                = do t1 <- infer env e
                                             o_constrain [QIn l 3 t0 t1]                          -- containment in test op
                                             return t1
  | otherwise                           = do t1 <- infer env e
                                             o_constrain [QEqu l 31 t0 t1]                        -- eq of compared values
                                             return t1

getInit l tenv                          = case partition ((=="__init__") . nstr . fst) tenv of
                                            ([],_) -> notYet l (text "Class declaration without an '__init__' method")
                                            ([(n,t)],tenv1) -> (nloc n, t, tenv1)

