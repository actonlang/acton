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
        
reconstruct                             :: String -> OTEnv -> Module -> IO (OTEnv, SrcInfo)
reconstruct outname ienv modul
  | chkRedef suite                      = let (te,inf) = runTypeM $ (,) <$> infTop env1 suite <*> dumped
                                              tenv     = [ (n,t) | (n,t) <- te, notemp n ]
                                              t        = env2type tenv
                                          in do
                                              InterfaceFiles.writeFile (outname ++ ".ty") t
                                              return (tenv, inf)
  where Module _ _ suite                = modul
        env1                            = reserve (bound suite) $ define ienv $ define old_builtins emptyEnv

typeError                               = solveError


chkRedef ss                             = True -- TBD

chkCycles (d@Class{} : ds)              = noforward (qual d) d ds && all (chkDecl d ds) (dbody d) && chkCycles ds
chkCycles (d@Protocol{} : ds)           = noforward (qual d) d ds && all (chkDecl d ds) (dbody d) && chkCycles ds
chkCycles (d@Decorator{} : ds)          = noforward (dqname d) d ds && noforward (dargs d) d ds && chkCycles (d:ds)
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

infTop env ss                           = do pushFX ONil
                                             te <- infEnv env ss
                                             popFX
                                             cs <- constraints
                                             solveAll cs
                                             mapsubst te

class Infer a where
    infer                               :: OEnv -> a -> TypeM OType

class InfEnv a where
    infEnv                              :: OEnv -> a -> TypeM OTEnv

class InfEnvT a where
    infEnvT                             :: OEnv -> a -> TypeM (OTEnv,OType)

class InfData a where
    infData                             :: OEnv -> a -> TypeM OTEnv


generalize tvs cs t                     = OSchema (unOVar $ rng s) (subst s cs) (subst s t)
  where s                               = (tyvars t \\ tvs) `zip` schemaOVars

genTEnv env te                          = do cs <- constraints
                                             --traceM ("#### Reducing " ++ prstrs (dom te))
                                             --traceM (prstr cs)
                                             cs1 <- reduce cs
                                             --traceM ("#### Residue " ++ prstrs (dom te))
                                             --traceM (prstr cs1)
                                             te1 <- mapsubst te
                                             dump [ INS (loc v) t | (v,t) <- te1 ]
                                             tvs <- fmap tyvars $ mapM mapsubst $ map OVar $ tyvars env
                                             let (cs2, cs3) = partition (null . (\\tvs) . tyvars) cs1
                                                 te2 = mapSnd (closeFX . generalize tvs cs3) te1
                                             --traceM ("#### Generalized " ++ prstrs (dom te))
                                             --traceM (prstr te2)
                                             --traceM ("#### Retaining " ++ prstrs (dom te))
                                             --traceM (prstr cs2)
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
        unifyAll v (t:ts)               = do constrain [ QEqu (loc v) 300 t t' | t' <- ts ]     -- eq of merged
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
                                             constrain [QEqu (loc e) 1 t1 t2]       -- eq of pats and rhs
                                             return te
    infEnv env (AugAssign l pat o e)    = do t1 <- infer env pat                    -- TODO: add op constraint
                                             t2 <- infer env e
                                             constrain [QEqu l 2 t1 t2]             -- equality of target and rhs
                                             return []
    infEnv env (Assert _ es)            = do mapM (inferBool env) es
                                             return []
    infEnv env (Pass _)                 = return []
    infEnv env (Delete _ pat)
      | nodup pat                       = do _ <- infer env pat                     -- TODO: constrain pat targets to opt type
                                             return []
    infEnv env (Return _ Nothing)       = return []
    infEnv env (Return l (Just e))      = do t <- infer env e
                                             constrain [QEqu l 4 t t0]              -- assumption on return value
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
                                             constrain [QIn l 1 t1 t2]                              -- containment in for loop
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
                                             constrain [QEqu l 4 (ORecord $ env2row ONil te1) t]    -- assumption on return value
                                             return []
      where t                           = getReturn env
            env1                        = reserve (filter istemp $ bound b) env

    infEnv env (Data l (Just p) b)
      | nodup p                         = do (te0, t) <- infEnvT env p
                                             (te1,te2) <- partition (istemp . fst) <$> infData env b
                                             constrain [QEqu l 13 (ORecord $ env2row ONil te2) t]   -- eq of target and rhs..
                                             return (te0 ++ te1)
            
    infEnv env (VarAssign _ pats e)
      | nodup pats                      = do (te, t1) <- infEnvT env pats
                                             t2 <- infer env e
                                             constrain [QEqu (loc e) 17 t1 t2]                      -- eq of pats and rhs
                                             return te
    
    infEnv env (Decl _ ds)
      | not $ null redefs               = err2 redefs "Illegal redefinition:"
      | nodup vs && chkCycles ds        = do te <- newTEnv vs
                                             let env1 = define te env
                                             ts <- mapM (infer env1) ds
                                             let te1 = vs `zip` ts
                                             constrain [ QEqu (loc v) 5 t (findVar v env1) | (v,t) <- te1 ]  -- eq of name and rhs
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
      | nodup p                         = do fx <- newOVar
                                             pushFX fx                              -- TODO: constrain opt result if fallsthru b
                                             t <- newOVar
                                             let env1 = reserve (bound p ++ bound b ++ svars) $ setReturn t env
                                             (te, row) <- infEnvT env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             return (OFun fx row t)
      where svars                       = stvars env

    infer env (Def l n q p ann b Async)
      | nodup p && noshadow svars p     = do fx0 <- newOVar
                                             pushFX fx0                              -- TODO: constrain opt result if fallsthru b
                                             t <- newOVar
                                             let env1 = reserve (bound p ++ bound b \\ svars) $ setReturn t env
                                             (te, row) <- infEnvT env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- asyncFX <$> newOVar
                                             return (OFun fx row ({-OMsg-}t))
      where svars                       = stvars env

    infer env (Def l n q p ann b (Sync _))
      | nodup p && noshadow svars p     = do fx0 <- newOVar
                                             pushFX fx0                              -- TODO: constrain opt result if fallsthru b
                                             t <- newOVar
                                             let env1 = reserve (bound p ++ bound b \\ svars) $ setReturn t env
                                             (te, row) <- infEnvT env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- syncFX <$> newOVar
                                             return (OFun fx row t)
      where svars                       = stvars env

    infer env (Class l n q cs b)
      | nodup cs && chkRedef b          = do t0 <- newOVar
                                             inherited <- return ONil --inferSuper env cs
                                             pushFX ONil
                                             te <- infEnv env0 b                     -- visible bindings in b???
                                             (l1, t1, te1) <- getInit l <$> mapM (wrap t0) te
                                             r0 <- newOVar
                                             popFX
                                             constrain [QEqu l 6 t0 (ORecord (env2row inherited te1))]
                                             constrain [QEqu l1 7 t1 (OFun ONil r0 ONone)]                 -- assumption on "__init__"
                                             return (OFun ONil r0 t0)
      where env0                        = reserve (bound b) $ newstate [] env
            methnames                   = [ v | Decl _ ds <- b, Def{dname=v} <- ds ]
            wrap t0 (n,t)
              | n `elem` methnames      = do r':t':fx':_ <- newOVars 3
                                             let internal = OFun fx' (OPos t0 r') t'
                                                 external = OFun fx' r' t'
                                             constrain [QEqu (nloc n) 9 t internal]
                                             return (n,external)                        -- assumption on method n
              | otherwise               = return (n,t)
                                        
    infer env (Protocol l n q cs b)     = newOVar       -- undefined
    infer env (Extension l n q cs b)    = newOVar       -- undefined

    infer env (Actor l n q p ann b)
      | nodup p && noshadow svars p && 
        chkRedef b                      = do fx <- actFX <$> newOVar
                                             pushFX fx
                                             t0 <- newOVar
                                             let env0 = reserve (bound p ++ bound b) $ newstate svars $ define [(self,t0)] env
                                             (te0, row) <- infEnvT env0 p
                                             let env1 = define te0 env0
                                             te1 <- infEnv env1 b
                                             constrain [QEqu l 10 t0 (ORecord $ env2row ONil te1)]
                                             return (OFun fx row t0)
      where svars                       = statevars b

    infer env (Decorator l qn args d)
      | nodup args                      = do t1 <- inferPure env deco
                                             t2 <- infer env d
                                             t0 <- newOVar
                                             constrain [QEqu l 14 t1 (OFun ONil (OPos t2 ONil) t0)]    -- assumption on qn
                                             return t0
      where deco                        = if null args then qname2expr qn else Call l (qname2expr qn) args

inferPure env e                         = do pushFX ONil
                                             t <- infer env e
                                             popFX
                                             return t

env2row                                 = foldl (\r (n,t) -> OKwd n t r)

env2type tenv                           = ORecord (env2row ONil tenv)

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
                                             constrain [QEqu (loc p) 16 t1 t2]  -- Eq of item and alias 
                                             return te

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do te <- infEnv env ex
                                             te1 <- infEnv (define te env) b
                                             return $ prune (dom te) te1

instance InfEnv Except where
    infEnv env (ExceptAll _)            = return []
    infEnv env (Except l e)             = do _ <- infer env e                   -- TODO: Constraint on exception type...
                                             return []
    infEnv env (ExceptAs l e n)         = do t:r:fx:_ <- newOVars 3
                                             t1 <- infer env e                  -- TODO: Constraint on exception type...
                                             constrain [QEqu l 15 t1 (OFun fx r t)]   -- eq of excpt-value and alias
                                             return [(n,t)]


inferBool env e                         = do t <- infer env e
                                             let cs0 = []                          -- Will become QBool constraint....
                                             return ()

instance Infer Index where
    infer env (Index l e)               = infer env e
    infer env (Slice l e1 e2 e3)        = do ts <- mapM (infer env) es
                                             constrain [ QEqu (eloc e) 30 t OInt | (t,e) <- ts `zip` es ]
                                             return OInt                          -- TODO: think this through...
      where es                          = concat $ map maybeToList (e1:e1:maybeToList e3)

instance Infer Expr where
    infer env (Var l n)                 = do dump [GEN l t0]
                                             instantiate l $ openFX t0
      where t0                          = findVar n env
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
                                             dump [INS (loc e) t]
                                             row <- infer env args
                                             t0:fx:_ <- newOVars 2
                                             effect l fx
                                             constrain [QEqu l 18 t (OFun fx row t0)]                  -- callability of e
                                             return t0
    infer env (Await l e)               = do t <- infer env e
                                             t0 <- newOVar
                                             fx <- syncFX <$> newOVar
                                             effect l fx
                                             constrain [QEqu l 37 t t0]
                                             return t0
    infer env (Ix l e [i@Index{}])      = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newOVar
                                             constrain [QIx l 38 t ti t0]                               -- indexability of e
                                             return t0
    infer env (Ix l e [s@Slice{}])      = do t <- infer env e
                                             ti <- infer env s
                                             t0 <- newOVar
                                             constrain [QIx l 39 t ti t0]                               -- subscriptability of e
                                             return t
    infer env (Cond l e1 e e2)          = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             inferBool env e
                                             t0 <- newOVar
                                             constrain [QEqu l 19 t1 t0, QEqu l 20 t2 t0]               -- eq of l/r branch and join
                                             return t0
    infer env (BinOp l e1 (Op _ op) e2)
      | op `elem` logical               = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QBool l 1 t1, QBool l 2 t2]                     -- truth value of e1/e2
                                             return OBool
      | op `elem` bitwise               = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QEqu l 19 t1 OInt, QEqu l 1 t2 OInt]            -- applicability of op
                                             return OInt
      | op == Div                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QEqu l 19 t1 t2, QNum l 1 t1]                   -- eq of args, applicability of '/'
                                             return OFloat
      | op == EuDiv                     = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QEqu l 19 t1 t2, QNum l 1 t1]                   -- eq of args, applicability of '//'
                                             return OInt
      | op == Mod                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QMod l 1 t1 t2]                                 -- applicability of '%'
                                             return t1
      | op == Plus                      = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QEqu l 19 t1 t2, QPlus l 1 t1]                  -- eq of args, applicability of '+'
                                             return t1
      | otherwise                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [QEqu l 19 t1 t2, QNum l 1 t1]                   -- eq of args, applicability of op
                                             return t1
      where logical                     = [Or,And]
            bitwise                     = [BOr,BXor,BAnd,ShiftL,ShiftR]
    infer env (UnOp l (Op _ Not) e)     = do t <- infer env e
                                             constrain [QBool l 3 t]                                    -- applicability of 'not'
                                             return OBool
    infer env (UnOp l (Op _ BNot) e)    = do t <- infer env e
                                             constrain [QEqu l 21 t OInt]                               -- applicability of '~'
                                             return OInt
    infer env (UnOp l o e)              = do t <- infer env e
                                             constrain [QNum l 2 t]                                     -- applicability of o
                                             return t
    infer env (CompOp l e ops)          = do t1 <- infer env e
                                             scanCompOps env t1 ops
                                             return OBool
    infer env (Dot l e n)               = do t <- infer env e
                                             t0 <- newOVar
                                             constrain [QDot l 1 t n t0]                                -- n-selectability of e
                                             return t0
    infer env (DotI l e i)              = do t <- infer env e
                                             t0:tvs <- newOVars (i+2)
                                             constrain [QEqu l 22 t (OTuple (row t0 tvs i))]            -- index-selectability of e
                                             return t0
      where row t0 (t1:tvs) 0           = OPos t0 t1
            row t0 (t1:tvs) n           = OPos t1 (row t0 tvs (n-1))
    infer env (Lambda l par e)
      | nodup par                       = do t0:fx:_ <- newOVars 2
                                             pushFX fx
                                             (te, row) <- infEnvT env par
                                             t <- infer (define te env) e
                                             popFX
                                             dump [INS l $ OFun fx row t]
                                             return (OFun fx row t)
    infer env (Tuple l es)              = do r <- infer env es
                                             return (OTuple r)
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Generator l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return (OList t)                                  -- !! Should be CGenerator...
    infer env (List l es)               = do r <- infer env es
                                             t0 <- newOVar
                                             constrain (fold t0 (map loc es) r)
                                             return (OList t0)
      where fold t0 [] ONil             = []
            fold t0 (l:ls) (OPos t r)   = QEqu l 23 t t0 : fold t0 ls r
            fold t0 (l:ls) (OStar1 t r) = QEqu l 24 t (OList t0) : fold t0 ls r
    infer env (ListComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return (OList t)
    infer env (Dict l as)               = do ts <- mapM (infer env) as
                                             t1 <- ODict <$> newOVar <*> newOVar
                                             constrain [ QEqu (loc a) 25 t t1 | (t,a) <- ts `zip` as]   -- eq of dict elements
                                             return t1
    infer env (DictComp l e co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return t
    infer env (Set l es)                = do r <- infer env es
                                             t0 <- newOVar
                                             constrain (fold t0 (map loc es) r)
                                             return (OSet t0)
      where fold t0 [] ONil             = []
            fold t0 (l:ls) (OPos t r)   = QEqu l 26 t t0 : fold t0 ls r
            fold t0 (l:ls) (OStar1 t r) = QEqu l 27 t (OSet t0) : fold t0 ls r
    infer env (SetComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (define te env) e
                                             return (OSet t)
    infer env (Record l fs)             = do te <- infer env fs
                                             return (ORecord te)
    infer env (RecordComp l n e co)     = do te <- infEnv env co
                                             let env1 = define te env
                                             _ <- infer env1 (Var (nloc n) n)
                                             _ <- infer env1 e
                                             r <- newOVar
                                             return (ORecord r)                 -- !! Big over-generalization, for now
    infer env (Paren l e)               = infer env e

instance InfEnvT Params where
    infEnvT env (Params pos NoStar [] kX)
      | not (null pos)                  = infEnvT env (Params [] NoStar pos kX)
    infEnvT env (Params pos pX kwd kX)  = do (te0, r0) <- inferPar env (const OPos) pos
                                             (te1, r1) <- starPar OStar1 OTuple pX
                                             (te2, r2) <- inferPar (define te1 $ define te0 env) OKwd kwd
                                             (te3, r3) <- starPar OStar2 ORecord kX
                                             return (te0++te1++te2++te3, (r0 . r1 . r2 . r3) ONil)
                                             
inferPar env f []                       = return ([], id)
inferPar env f (Param n a Nothing : ps) = do t0 <- newOVar
                                             let te0 = [(n,t0)]
                                             (te1, r1) <- inferPar (define te0 env) f ps
                                             return (te0++te1, f n t0 . r1)
inferPar env f (Param n a (Just e): ps) = do t <- infer env e
                                             t0 <- newOVar
                                             let te0 = [(n,t0)]
                                             constrain [QEqu (eloc e) 28 t t0]              -- eq of n and default value
                                             (te1, r1) <- inferPar (define te0 env) f ps
                                             return (te0++te1, f n t0 . r1)

starPar rc tc NoStar                    = return ([], id)
starPar rc tc (StarPar _ n _)           = do t1 <- newOVar
                                             r1 <- newOVar
                                             constrain [QEqu (nloc n) 33 t1 (tc r1)]        -- *param is a tuple/record
                                             return ([(n,t1)], rc t1)               

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
                                             constrain [QIn l 2 t1 t2]                        -- containment in 'for' comprehension
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
                                             constrain [QEqu (loc p) 1 t1 t2]
                                             return (te1++te2, t1)

instance InfEnvT [Elem Pattern] where
    infEnvT env (Elem p : elems)        = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT (define te1 env) elems
                                             return (te1++te2, OPos t r)
    infEnvT env (Star p : elems)        = do (te1, t) <- infEnvT env p
                                             (te2, r) <- infEnvT env elems
                                             return (te1++te2, OStar1 t r)
    infEnvT env []                      = return ([], ONil)


instance InfEnvT Pattern where
    infEnvT env (PVar _ n Nothing)
      | reserved env n                  = do t <- newOVar
                                             return ([(n,t)], t)
      | monotype t                      = return ([], t)
      | otherwise                       = err1 n "Illegal reassignment:"
      where t                           = findVar n env
    infEnvT env (PIx l e [i@Index{}])   = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newOVar
                                             constrain [QIx l 1 t ti t0]
                                             return ([], t0)                                    -- indexability of e
    infEnvT env (PDot l e n)            = do t <- infer env e
                                             t0 <- newOVar
                                             constrain [QDot l 1 t n t0]
                                             return ([], t0)                                    -- n-selectability of e
    infEnvT env (PTuple _ ps)           = do (te, r) <- infEnvT env ps
                                             return (te, OTuple r)
    infEnvT env (PList _ ps)            = do (te, r) <- infEnvT env ps
                                             t0 <- newOVar
                                             constrain (fold t0 (map loc ps) r)
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
                                             constrain [QIx (loc i) 1 t0 ti t1]
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
                                             constrain [QEqu (eloc e) 32 t (OFun ONil r' t')]    -- assumpt on base class
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
                                             constrain [QIn l 3 t0 t1]                          -- containment in test op
                                             return t1
  | otherwise                           = do t1 <- infer env e
                                             constrain [QEqu l 31 t0 t1]                        -- eq of compared values
                                             return t1

notYetExpr e                            = notYet (loc e) (pretty e)

notYet loc doc                          = Control.Exception.throw $ NotYet loc doc

getInit l tenv                          = case partition ((=="__init__") . nstr . fst) tenv of
                                            ([],_) -> notYet l (text "Class declaration without an '__init__' method")
                                            ([(n,t)],tenv1) -> (nloc n, t, tenv1)

