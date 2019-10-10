{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct,typeError,env2type) where

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
import Acton.Env
import Acton.Constraints
import qualified InterfaceFiles
        
reconstruct                             :: String -> TEnv -> Module -> IO (TEnv, SrcInfo)
reconstruct outname ienv modul
  | chkRedef suite                      = let (te,inf) = runTypeM $ (,) <$> infTop env1 suite <*> dumped
                                              tenv     = [ (n,t) | (n,t) <- te, notemp n ]
                                              t        = env2type tenv
                                          in do
                                              InterfaceFiles.writeFile (outname ++ ".ty") t
                                              return (tenv, inf)
  where Module _ _ suite                = modul
        env1                            = reserve (bound suite) $ define ienv $ define builtins emptyEnv        

typeError                               = solveError


chkRedef ss                             = True -- TBD

chkCycles (Class _ n q cs b : ds)
                                        = noforward cs n ds && all chkClass b && chkCycles ds
  where chkClass s@Decl{}               = chkCycles (decls s ++ ds)
        chkClass s                      = noforward s n ds
chkCycles (Decorator _ qn args d : ds)  = noforward qn n ds && noforward args n ds && chkCycles (d:ds)
  where n                               = declname d
chkCycles (d : ds)                      = chkCycles ds
chkCycles []                            = True

noforward x n y
  | not $ null vs                       = err2 vs "Illegal forward reference:"
  | otherwise                           = True
  where vs                              = free x `intersect` (n : bound y)

nodup x
  | not $ null vs                       = err2 vs "Duplicate names:"
  | otherwise                           = True
  where vs                              = duplicates (bound x)

noshadow svs x
  | not $ null vs                       = err2 vs "Illegal state shadowing:"
  | otherwise                           = True
  where vs                              = intersect (bound x) svs

singleext b
  | length xs > 1                       = err2 xs "Multiple actor extensions"
  | otherwise                           = True
  where xs                              = [ c | c@Extends{} <- b ]


-- Infer -------------------------------

infTop env ss                           = do pushFX RNil
                                             te <- infEnv env ss
                                             popFX
                                             cs <- constraints
                                             solveAll cs
                                             mapsubst te

class Infer a where
    infer                               :: Env Type -> a -> TypeM Type

class InfEnv a where
    infEnv                              :: Env Type -> a -> TypeM TEnv

class InfEnvT a where
    infEnvT                             :: Env Type -> a -> TypeM (TEnv,Type)

class InfData a where
    infData                             :: Env Type -> a -> TypeM TEnv


generalize tvs cs t                     = TSchema (unTVar $ rng s) (subst s cs) (subst s t)
  where s                               = (tyvars t \\ tvs) `zip` schemaTVars

genTEnv env te                          = do cs <- constraints
                                             traceM ("#### Reducing " ++ prstrs (dom te))
                                             traceM (prstr cs)
                                             cs1 <- reduce cs
                                             traceM ("#### Residue " ++ prstrs (dom te))
                                             traceM (prstr cs1)
                                             te1 <- mapsubst te
                                             dump [ INS (loc v) t | (v,t) <- te1 ]
                                             tvs <- fmap tyvars $ mapM mapsubst $ map TVar $ tyvars env
                                             let (cs2, cs3) = partition (null . (\\tvs) . tyvars) cs1
                                                 te2 = mapSnd (closeFX . generalize tvs cs3) te1
                                             traceM ("#### Generalized " ++ prstrs (dom te))
                                             traceM (prstr te2)
                                             traceM ("#### Retaining " ++ prstrs (dom te))
                                             traceM (prstr cs2)
                                             constrain cs2
                                             dump [ GEN (loc v) t | (v,t) <- te2 ]
                                             return te2

commonTEnv env tenvs                    = mergeTEnv env tenvs (foldr intersect [] $ map dom tenvs)

unionTEnv env tenvs                     = do te1 <- commonTEnv env tenvs
                                             te2 <- mergeTEnv env tenvs (concatMap dom tenvs \\ dom te1)
                                             return (te1++te2)                      -- TODO: constrain opt te2...

mergeTEnv env tenvs vs                  = mapM merge (nub vs)
  where merge v
          | length ts == 1              = return (v, head ts)
          | all monotype ts             = do t <- unifyAll v ts
                                             return (v,t)
          | all polytype ts             = do ts1 <- mapM (instantiate (loc v) . openFX) ts
                                             t <- unifyAll v ts1
                                             head <$> genTEnv env [(v,t)]
          | otherwise                   = err2 vs "Name both defined and assigned:"
          where ts                      = [ t | Just t <- map (lookup v) tenvs ]
                vs                      = [ w | te <- tenvs, (w,_) <- te, w == v ]
        unifyAll v (t:ts)               = do constrain [ CEqu (loc v) 300 t t' | t' <- ts ]     -- eq of merged
                                             return t

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
    infEnv env (TypeSig _ _ _)          = do return []
    infEnv env (Assign _ pats e)
      | nodup pats                      = do (te, t1) <- infEnvT env pats
                                             t2 <- infer env e
                                             constrain [CEqu (loc e) 1 t1 t2]       -- eq of pats and rhs
                                             return te
    infEnv env (AugAssign l pat o e)    = do t1 <- infer env pat                    -- TODO: add op constraint
                                             t2 <- infer env e
                                             constrain [CEqu l 2 t1 t2]             -- equality of target and rhs
                                             return []
    infEnv env (Assert _ es)            = do mapM (inferBool env) es
                                             return []
    infEnv env (Pass _)                 = return []
    infEnv env (Delete _ pat)
      | nodup pat                       = do _ <- infer env pat                     -- TODO: constrain pat targets to opt type
                                             return []
    infEnv env (Return _ Nothing)       = return []
    infEnv env (Return l (Just e))      = do t <- infer env e
                                             constrain [CEqu l 4 t t0]              -- assumption on return value
                                             return []
      where t0                          = getReturn env
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
                                             constrain [CIn l 1 t1 t2]                              -- containment in for loop
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

    infEnv env (Data l Nothing b)       = do te <- infData env1 b
                                             let te1 = filter (notemp . fst) te1
                                             constrain [CEqu l 4 (TStruct $ env2row RNil te1) t]    -- assumption on return value
                                             return []
      where t                           = getReturn env
            env1                        = reserve (filter istemp $ bound b) env

    infEnv env (Data l (Just p) b)
      | nodup p                         = do (te0, t) <- infEnvT env p
                                             (te1,te2) <- partition (istemp . fst) <$> infData env b
                                             constrain [CEqu l 13 (TStruct $ env2row RNil te2) t]   -- eq of target and rhs..
                                             return (te0 ++ te1)
            
    infEnv env (VarAssign _ pats e)
      | nodup pats                      = do (te, t1) <- infEnvT env pats
                                             t2 <- infer env e
                                             constrain [CEqu (loc e) 17 t1 t2]                      -- eq of pats and rhs
                                             return te
    
    infEnv env (Decl _ ds)
      | not $ null redefs               = err2 redefs "Illegal redefinition:"
      | nodup vs && chkCycles ds        = do te <- newTEnv vs
                                             let env1 = define te env
                                             ts <- mapM (infer env1) ds
                                             let te1 = vs `zip` ts
                                             constrain [ CEqu (loc v) 5 t (findVar v env1) | (v,t) <- te1 ]  -- eq of name and rhs
                                             te2 <- genTEnv env te1
                                             return te2
      where vs                          = bound ds
            redefs                      = filter (not . reserved env) vs


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


instance Infer Decl where
    infer env (Def l n q p ann b NoMod)
      | nodup p                         = do fx <- newTVar
                                             pushFX fx                              -- TODO: constrain opt result if fallsthru b
                                             t <- newTVar
                                             let env1 = reserve (bound p ++ bound b ++ svars) $ setReturn t env
                                             (te, row) <- infEnvT env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             return (TFun fx row t)
      where svars                       = stvars env

    infer env (Def l n q p ann b Async)
      | nodup p && noshadow svars p     = do fx0 <- newTVar
                                             pushFX fx0                              -- TODO: constrain opt result if fallsthru b
                                             t <- newTVar
                                             let env1 = reserve (bound p ++ bound b \\ svars) $ setReturn t env
                                             (te, row) <- infEnvT env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- asyncFX <$> newTVar
                                             return (TFun fx row ({-TMsg-}t))
      where svars                       = stvars env

    infer env (Def l n q p ann b (Sync _))
      | nodup p && noshadow svars p     = do fx0 <- newTVar
                                             pushFX fx0                              -- TODO: constrain opt result if fallsthru b
                                             t <- newTVar
                                             let env1 = reserve (bound p ++ bound b \\ svars) $ setReturn t env
                                             (te, row) <- infEnvT env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- syncFX <$> newTVar
                                             return (TFun fx row t)
      where svars                       = stvars env

    infer env (Class l n q cs b)
      | nodup cs && chkRedef b          = do t0 <- newTVar
                                             inherited <- inferSuper env cs
                                             pushFX RNil
                                             te <- infEnv env0 b                     -- visible bindings in b???
                                             (l1, t1, te1) <- getInit l <$> mapM (wrap t0) te
                                             r0 <- newTVar
                                             popFX
                                             constrain [CEqu l 6 t0 (TStruct (env2row inherited te1))]
                                             constrain [CEqu l1 7 t1 (TFun RNil r0 TNone)]                 -- assumption on "__init__"
                                             return (TFun RNil r0 t0)
      where env0                        = reserve (bound b) $ newstate [] env
            methnames                   = [ v | Decl _ ds <- b, Def{dname=v} <- ds ]
            wrap t0 (n,t)
              | n `elem` methnames      = do r':t':fx':_ <- newTVars 3
                                             let internal = TFun fx' (RPos t0 r') t'
                                                 external = TFun fx' r' t'
                                             constrain [CEqu (nloc n) 9 t internal]
                                             return (n,external)                        -- assumption on method n
              | otherwise               = return (n,t)
                                        
    infer env (Actor l n q p ann b)
      | nodup p && noshadow svars p && 
        chkRedef b && singleext b       = do fx <- actFX <$> newTVar
                                             pushFX fx
                                             t0 <- newTVar
                                             let env0 = reserve (bound p ++ bound b) $ newstate svars $ define [(self,t0)] env
                                             (te0, row) <- infEnvT env0 p
                                             let env1 = define te0 env0
                                             sup <- mapM (infer env1) exts
                                             te1 <- infEnv env1 b
                                             constrain [CEqu l 10 t0 (TStruct $ env2row (inherited sup) te1)]
                                             return (TFun fx row t0)
      where svars                       = statevars b
            exts                        = [ Call l (qname2expr n) args | Extends l n args <- b ]
            inherited sup               = foldl (flip RStar2) RNil (sup :: [Type])

    infer env (Decorator l qn args d)
      | nodup args                      = do t1 <- inferPure env deco
                                             t2 <- infer env d
                                             t0 <- newTVar
                                             constrain [CEqu l 14 t1 (TFun RNil (RPos t2 RNil) t0)]    -- assumption on qn
                                             return t0
      where deco                        = if null args then qname2expr qn else Call l (qname2expr qn) args

inferPure env e                         = do pushFX RNil
                                             t <- infer env e
                                             popFX
                                             return t

env2row                                 = foldl (\r (n,t) -> RKwd n t r)

env2type tenv                           = TStruct (env2row RNil tenv)

instance InfEnv Branch where
    infEnv env (Branch e b)             = do inferBool env e
                                             infEnv env b

instance InfEnv [WithItem] where
    infEnv env []                       = return []
    infEnv env (item:items)             = do te1 <- infEnv env item
                                             te2 <- infEnv (define te1 env) items
                                             return (te1++te2)

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do _ <- infer env e                   -- TODO: Mgr constraint on t
                                             return []
    infEnv env (WithItem e (Just p))    = do t1 <- infer env e                  -- TODO: Mgr constraint on t1
                                             (te, t2) <- infEnvT env p
                                             constrain [CEqu (loc p) 16 t1 t2]  -- Eq of item and alias 
                                             return te

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do te <- infEnv env ex
                                             te1 <- infEnv (define te env) b
                                             return $ prune (dom te) te1

instance InfEnv Except where
    infEnv env (ExceptAll _)            = return []
    infEnv env (Except l e)             = do _ <- infer env e                   -- TODO: Constraint on exception type...
                                             return []
    infEnv env (ExceptAs l e n)         = do t:r:fx:_ <- newTVars 3
                                             t1 <- infer env e                  -- TODO: Constraint on exception type...
                                             constrain [CEqu l 15 t1 (TFun fx r t)]   -- eq of excpt-value and alias
                                             return [(n,t)]


inferBool env e                         = do t <- infer env e
                                             let cs0 = []                          -- Will become CBool constraint....
                                             return ()

instance Infer Index where
    infer env (Index l e)               = infer env e
    infer env (Slice l e1 e2 e3)        = do ts <- mapM (infer env) es
                                             constrain [ CEqu (eloc e) 30 t TInt | (t,e) <- ts `zip` es ]
                                             return TInt                          -- TODO: think this through...
      where es                          = concat $ map maybeToList (e1:e1:maybeToList e3)

instance Infer Expr where
    infer env (Var l n)                 = do dump [GEN l t0]
                                             instantiate l $ openFX t0
      where t0                          = findVar n env
    infer env (Int _ val s)             = return TInt
    infer env (Float _ val s)           = return TFloat
    infer env e@Imaginary{}             = notYetExpr e
    infer env (Bool _ val)              = return TBool
    infer env (None _)                  = newTVar           -- TODO: replace with open union incluing None
    infer env (NotImplemented _)        = newTVar
    infer env (Ellipsis _)              = newTVar
    infer env (Strings _ ss)            = return TStr
    infer env (BStrings _ ss)           = return TStr
    infer env (UStrings _ ss)           = return TStr
    infer env (Call l e args)           = do t <- infer env e
                                             dump [INS (loc e) t]
                                             row <- infer env args
                                             t0:fx:_ <- newTVars 2
                                             effect l fx
                                             constrain [CEqu l 18 t (TFun fx row t0)]                  -- callability of e
                                             return t0
    infer env (Await l e)               = do t <- infer env e
                                             t0 <- newTVar
                                             fx <- syncFX <$> newTVar
                                             effect l fx
                                             constrain [CEqu l 37 t t0]
                                             return t0
    infer env (Ix l e [i@Index{}])      = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newTVar
                                             constrain [CIx l 38 t ti t0]                               -- indexability of e
                                             return t0
    infer env (Ix l e [s@Slice{}])      = do t <- infer env e
                                             ti <- infer env s
                                             t0 <- newTVar
                                             constrain [CIx l 39 t ti t0]                               -- subscriptability of e
                                             return t
    infer env (Cond l e1 e e2)          = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             inferBool env e
                                             t0 <- newTVar
                                             constrain [CEqu l 19 t1 t0, CEqu l 20 t2 t0]               -- eq of l/r branch and join
                                             return t0
    infer env (BinOp l e1 (Op _ op) e2)
      | op `elem` logical               = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CBool l 1 t1, CBool l 2 t2]                     -- truth value of e1/e2
                                             return TBool
      | op `elem` bitwise               = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CEqu l 19 t1 TInt, CEqu l 1 t2 TInt]            -- applicability of op
                                             return TInt
      | op == Div                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CEqu l 19 t1 t2, CNum l 1 t1]                   -- eq of args, applicability of '/'
                                             return TFloat
      | op == EuDiv                     = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CEqu l 19 t1 t2, CNum l 1 t1]                   -- eq of args, applicability of '//'
                                             return TInt
      | op == Mod                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CMod l 1 t1 t2]                                 -- applicability of '%'
                                             return t1
      | op == Plus                      = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CEqu l 19 t1 t2, CPlus l 1 t1]                  -- eq of args, applicability of '+'
                                             return t1
      | otherwise                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [CEqu l 19 t1 t2, CNum l 1 t1]                   -- eq of args, applicability of op
                                             return t1
      where logical                     = [Or,And]
            bitwise                     = [BOr,BXor,BAnd,ShiftL,ShiftR]
    infer env (UnOp l (Op _ Not) e)     = do t <- infer env e
                                             constrain [CBool l 3 t]                                    -- applicability of 'not'
                                             return TBool
    infer env (UnOp l (Op _ BNot) e)    = do t <- infer env e
                                             constrain [CEqu l 21 t TInt]                               -- applicability of '~'
                                             return TInt
    infer env (UnOp l o e)              = do t <- infer env e
                                             constrain [CNum l 2 t]                                     -- applicability of o
                                             return t
    infer env (CompOp l e ops)          = do t1 <- infer env e
                                             scanCompOps env t1 ops
                                             return TBool
    infer env (Dot l e n)               = do t <- infer env e
                                             t0 <- newTVar
                                             constrain [CDot l 1 t n t0]                                -- n-selectability of e
                                             return t0
    infer env (DotI l e i)              = do t <- infer env e
                                             t0:tvs <- newTVars (i+2)
                                             constrain [CEqu l 22 t (TTuple (row t0 tvs i))]            -- index-selectability of e
                                             return t0
      where row t0 (t1:tvs) 0           = RPos t0 t1
            row t0 (t1:tvs) n           = RPos t1 (row t0 tvs (n-1))
    infer env (Lambda l par e)
      | nodup par                       = do t0:fx:_ <- newTVars 2
                                             pushFX fx
                                             (te, row) <- infEnvT env par
                                             t <- infer (define te env) e
                                             popFX
                                             dump [INS l $ TFun fx row t]
                                             return (TFun fx row t)
    infer env (Tuple l es)              = do r <- infer env es
                                             return (TTuple r)
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Generator l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return (TList t)                                  -- !! Should be CGenerator...
    infer env (List l es)               = do r <- infer env es
                                             t0 <- newTVar
                                             constrain (fold t0 (map loc es) r)
                                             return (TList t0)
      where fold t0 [] RNil             = []
            fold t0 (l:ls) (RPos t r)   = CEqu l 23 t t0 : fold t0 ls r
            fold t0 (l:ls) (RStar1 t r) = CEqu l 24 t (TList t0) : fold t0 ls r
    infer env (ListComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return (TList t)
    infer env (Dict l as)               = do ts <- mapM (infer env) as
                                             t1 <- TDict <$> newTVar <*> newTVar
                                             constrain [ CEqu (loc a) 25 t t1 | (t,a) <- ts `zip` as]   -- eq of dict elements
                                             return t1
    infer env (DictComp l e co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return t
    infer env (Set l es)                = do r <- infer env es
                                             t0 <- newTVar
                                             constrain (fold t0 (map loc es) r)
                                             return (TSet t0)
      where fold t0 [] RNil             = []
            fold t0 (l:ls) (RPos t r)   = CEqu l 26 t t0 : fold t0 ls r
            fold t0 (l:ls) (RStar1 t r) = CEqu l 27 t (TSet t0) : fold t0 ls r
    infer env (SetComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return (TSet t)
    infer env (Struct l fs)             = do te <- infer env fs
                                             return (TStruct te)
    infer env (StructComp l n e co)     = do te <- infEnv env co
                                             let env1 = define te env
                                             _ <- infer env1 (Var (nloc n) n)
                                             _ <- infer env1 e
                                             r <- newTVar
                                             return (TStruct r)                 -- !! Big over-generalization, for now
    infer env (Paren l e)               = infer env e

instance InfEnvT Params where
    infEnvT env (Params pos NoStar [] kX)
      | not (null pos)                  = infEnvT env (Params [] NoStar pos kX)
    infEnvT env (Params pos pX kwd kX)  = do (te0, r0) <- inferPar env (const RPos) pos
                                             (te1, r1) <- starPar RStar1 TTuple pX
                                             (te2, r2) <- inferPar (define te1 $ define te0 env) RKwd kwd
                                             (te3, r3) <- starPar RStar2 TStruct kX
                                             return (te0++te1++te2++te3, (r0 . r1 . r2 . r3) RNil)
                                             
inferPar env f []                       = return ([], id)
inferPar env f (Param n a Nothing : ps) = do t0 <- newTVar
                                             let te0 = [(n,t0)]
                                             (te1, r1) <- inferPar (define te0 env) f ps
                                             return (te0++te1, f n t0 . r1)
inferPar env f (Param n a (Just e): ps) = do t <- infer env e
                                             t0 <- newTVar
                                             let te0 = [(n,t0)]
                                             constrain [CEqu (eloc e) 28 t t0]              -- eq of n and default value
                                             (te1, r1) <- inferPar (define te0 env) f ps
                                             return (te0++te1, f n t0 . r1)

starPar rc tc NoStar                    = return ([], id)
starPar rc tc (StarPar _ n _)           = do t1 <- newTVar
                                             r1 <- newTVar
                                             constrain [CEqu (nloc n) 33 t1 (tc r1)]        -- *param is a tuple/struct
                                             return ([(n,t1)], rc t1)               

instance Infer Assoc where
    infer env (Assoc k v)               = TDict <$> infer env k <*> infer env v
    infer env (StarStarAssoc e)         = infer env e

instance Infer [Elem Expr] where
    infer env (Elem e : elems)          = RPos <$> infer env e <*> infer env elems
    infer env (Star e : elems)          = RStar1 <$> infer env e <*> infer env elems
    infer env []                        = return RNil

instance Infer [Field] where
    infer env (Field n e : fs)          = (RKwd n) <$> infer env e <*> infer env fs
    infer env (StarStarField e : fs)    = RStar2 <$> infer env e <*> infer env fs
    infer env []                        = return RNil

instance InfEnv Comp where
    infEnv env NoComp                   = return []
    infEnv env (CompIf l e c)           = do inferBool env e
                                             infEnv env c
    infEnv env (CompFor l p e c)        = do (te1, t1) <- infEnvT env p
                                             t2 <- infer env e
                                             constrain [CIn l 2 t1 t2]                        -- containment in 'for' comprehension
                                             te2 <- infEnv (define te1 env) c
                                             return (te1++te2)

instance Infer Exception where
    infer env (Exception e1 Nothing)    = infer env e1
    infer env (Exception e1 (Just e2))  = do t1 <- infer env e1
                                             _ <- infer env e2                                -- TODO: Constraint on exception type...
                                             return t1

instance InfEnvT [Pattern] where
    infEnvT env [p]                     = infEnvT env p
    infEnvT env (p:ps)                  = do (te1, t1) <- infEnvT env p
                                             (te2, t2) <- infEnvT (define te1 env) ps
                                             constrain [CEqu (loc p) 1 t1 t2]
                                             return (te1++te2, t1)

instance InfEnvT [Elem Pattern] where
    infEnvT env (Elem p : elems)        = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT (define te1 env) elems
                                             return (te1++te2, RPos t r)
    infEnvT env (Star p : elems)        = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT env elems
                                             return (te1++te2, RStar1 t r)
    infEnvT env []                      = return ([], RNil)


instance InfEnvT Pattern where
    infEnvT env (PVar _ n Nothing)
      | reserved env n                  = do t <- newTVar
                                             return ([(n,t)], t)
      | monotype t                      = return ([], t)
      | otherwise                       = err1 n "Illegal reassignment:"
      where t                           = findVar n env
    infEnvT env (PIx l e [i@Index{}])   = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newTVar
                                             constrain [CIx l 1 t ti t0]
                                             return ([], t0)                                    -- indexability of e
    infEnvT env (PDot l e n)            = do t <- infer env e
                                             t0 <- newTVar
                                             constrain [CDot l 1 t n t0]
                                             return ([], t0)                                    -- n-selectability of e
    infEnvT env (PTuple _ ps)           = do (te, r) <- infEnvT env ps
                                             return (te, TTuple r)
    infEnvT env (PList _ ps)            = do (te, r) <- infEnvT env ps
                                             t0 <- newTVar
                                             constrain (fold t0 (map loc ps) r)
                                             return (te, TList t0)
      where fold t0 [] RNil             = []
            fold t0 (l:ls) (RPos t r)   = CEqu l 23 t t0 : fold t0 ls r
            fold t0 (l:ls) (RStar1 t r) = CEqu l 24 t (TList t0) : fold t0 ls r
    infEnvT env (PParen l p)            = infEnvT env p
    infEnvT env (PData l n es)          = do t0 <- newTVar
                                             t <- inferIxs env t0 es
                                             return ([(n,t0)], t)

inferIxs env t0 []                      = return t0
inferIxs env t0 (i:is)                  = do t1 <- newTVar
                                             ti <- infer env i
                                             t2 <- inferIxs env t1 is 
                                             constrain [CIx (loc i) 1 t0 ti t1]
                                             return t2

instance Infer Pattern where
    infer env p                         = noenv <$> infEnvT env p
      where noenv ([], t)               = t
            noenv (te, _)               = nameNotFound (head (dom te))
                                             

instance Infer [Arg] where
    infer env (Arg e : args)            = RPos <$> infer env e <*> infer env args
    infer env (StarArg e : args)        = RStar1 <$> infer env e <*> infer env args
    infer env (KwArg n e : args)        = (RKwd n) <$> infer env e <*> infer env args
    infer env (StarStarArg e : args)    = RStar2 <$> infer env e <*> infer env args
    infer env []                        = return RNil

inferSuper env (Arg e : args)           = do t <- inferPure env e
                                             r <- inferSuper env args
                                             r':t':_ <- newTVars 2
                                             constrain [CEqu (eloc e) 32 t (TFun RNil r' t')]    -- assumpt on base class
                                             return (RStar2 t' r)
inferSuper env (StarArg e : args)       = do t <- infer env e
                                             r <- inferSuper env args
                                             return (RStar1 t r)                -- Handle at all?
inferSuper env (KwArg n e : args)       = inferSuper env (Arg e : args)
inferSuper env (StarStarArg e:args)     = inferSuper env (StarArg e : args)
inferSuper env []                       = return RNil

scanCompOps env t0 []                   = return ()
scanCompOps env t0 (oparg : opargs)     = do t1 <- inferCompOp env t0 oparg
                                             scanCompOps env t1 opargs
                                             return ()

inferCompOp env t0 (OpArg (Op l o) e)
  | o `elem` [In, NotIn]                = do t1 <- infer env e
                                             constrain [CIn l 3 t0 t1]                          -- containment in test op
                                             return t1
  | otherwise                           = do t1 <- infer env e
                                             constrain [CEqu l 31 t0 t1]                        -- eq of compared values
                                             return t1

notYetExpr e                            = notYet (loc e) (pretty e)

notYet loc doc                          = Control.Exception.throw $ NotYet loc doc

getInit l tenv                          = case partition ((=="__init__") . nstr . fst) tenv of
                                            ([],_) -> notYet l (text "Class declaration without an '__init__' method")
                                            ([(n,t)],tenv1) -> (nloc n, t, tenv1)

