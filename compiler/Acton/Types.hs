{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct,reconstruct2,typeError) where

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

reconstruct                             :: String -> Env -> Module -> IO (TEnv, SrcInfo)
reconstruct outname env modul           = do InterfaceFiles.writeFile (outname ++ ".ty") (unalias env te)
                                             return (te, info)
  where Module m _ suite                = modul
        env1                            = block (bound suite) env{ defaultmod = m }
        (te,info)                       = runTypeM $ (,) <$> infTop env1 suite <*> getDump

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
                                             dump [ INS (loc v) t | (v, NVar (TSchema _ [] t) _) <- te1 ]
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
      | nodup pats                      = do t1 <- infer env e
                                             (te, t2) <- infEnvT env pats
                                             constrain [Sub t1 t2]
                                             return te
    infEnv env (AugAssign l pat (Op _ op) e)
                                        = do t1 <- infer env e
                                             t2 <- infer env pat
                                             constrain [Sub t1 t2, Impl t2 (protocol op)]
                                             return []
      where protocol PlusA              = cPlus
            protocol MinusA             = cMinus
            protocol MultA              = cNumber
            protocol PowA               = cNumber
            protocol DivA               = cNumber
            protocol ModA               = cReal
            protocol EuDivA             = cReal
            protocol ShiftLA            = cIntegral
            protocol ShiftRA            = cIntegral
            protocol BOrA               = cLogical
            protocol BXorA              = cLogical
            protocol BAndA              = cLogical
            protocol MMultA             = cMatrix
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
                                             constrain [Impl t2 (cIterable t1)]
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
--                                             o_constrain [QEqu l 4 (ORecord $ env2row ONil te1) t]
--                                             return []
--      where t                           = o_getReturn env
--            env1                        = o_reserve (filter istemp $ bound b) env

--    infEnv env (Data l (Just p) b)
--      | nodup p                         = do (te0, t) <- infEnvT env p
--                                             (te1,te2) <- partition (istemp . fst) <$> infData env b
--                                             o_constrain [QEqu l 13 (ORecord $ env2row ONil te2) t]
--                                             return (te0 ++ te1)
            
    infEnv env (VarAssign _ pats e)
      | nodup pats                      = do t1 <- infer env e
                                             (te, t2) <- infEnvT env pats
                                             constrain [Sub t1 t2]
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
                                             (prow, krow, te0) <- infParams env1 p
                                             te1 <- infEnv (define te0 env1) b
                                             te2 <- genTEnv env te1
                                             fx <- fxAct <$> newTVar
                                             return [(n, nVar (tFun fx prow krow (tRecord $ env2row tNil te2)))]
      where svars                       = statedefs b
            oldstate                    = statescope env
            env1                        = block (bound p ++ bound b ++ svars ++ oldstate) $ define envActorSelf env
            
    infEnv env (Def l n q p ann b (Sync _)) -- TODO: schema [q] => (p) -> ann
      | nodup p && noshadow svars p     = do t <- newTVar
                                             pushFX (fxRet t tWild)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             (prow, krow, te) <- infParams env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- fxSync <$> newTVar
                                             return [(n, nVar (tFun fx prow krow t))]
      where svars                       = statescope env
            env1                        = block (bound p ++ bound b) env

    infEnv env (Def l n q p ann b Async)    -- TODO: schema [q] => (p) -> ann
      | nodup p && noshadow svars p     = do t <- newTVar
                                             pushFX (fxRet t tWild)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             (prow, krow, te) <- infParams env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             fx <- fxAsync <$> newTVar
                                             return [(n, nVar (tFun fx prow krow ({-tMsg-}t)))]
      where svars                       = statescope env
            env1                        = block (bound p ++ bound b) env

    infEnv env (Def l n q p ann b modif)    -- TODO: schema [q] => (p) -> ann
      | nodup p                         = do t <- newTVar
                                             fx <- newTVar
                                             pushFX (fxRet t fx)
                                             when (fallsthru b) (subFX (fxRet tNone tWild))
                                             (prow, krow, te) <- infParams env1 p
                                             _ <- infEnv (define te env1) b
                                             popFX
                                             return [(n, nVar (tFun fx prow krow t))]     -- TODO: modif/NoDecoration
      where svars                       = statescope env
            env1                        = block (bound p ++ bound b ++ svars) env
{-
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
-}                                        
    infEnv env (Protocol l n q cs b)    = return []       -- undefined
    infEnv env (Extension l n q cs b)   = return []       -- undefined
    infEnv env (Signature l ns t dec)   = return []       -- undefined.....


inferPure env e                         = do pushFX tNil
                                             t <- infer env e
                                             popFX
                                             return t

env2row                                 = foldl (\r (n,NVar t _) -> kwdRow n t r)           -- TODO: stabilize this...

instance InfEnv Branch where
    infEnv env (Branch e b)             = do inferBool env e
                                             infEnv env b

instance InfEnv [WithItem] where
    infEnv env []                       = return []
    infEnv env (item:items)             = do te1 <- infEnv env item
                                             te2 <- infEnv (define te1 env) items
                                             return (te1++te2)

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do t <- infer env e
                                             constrain [Impl t cContextManager]
                                             return []
    infEnv env (WithItem e (Just p))    = do t1 <- infer env e
                                             (te, t2) <- infEnvT env p
                                             constrain [Equ t1 t2, Impl t1 cContextManager]
                                             return te

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do te <- infEnv env ex
                                             te1 <- infEnv (define te env) b
                                             return $ prune (dom te) te1

instance InfEnv Except where
    infEnv env (ExceptAll _)            = return []
    infEnv env (Except l x)             = case findqname x env of
                                            NClass q us _
                                              | cException `elem` us -> return []
                                            _ -> err1 x "Not a class deriving from Exception"
    infEnv env (ExceptAs l x n)         = case findqname x env of
                                            NClass q us _
                                              | cException `elem` us -> do
                                                 ts <- newTVars (length q)
                                                 return [(n, nVar (tCon (TC x ts)))] 
                                            _ -> err1 x "Not a class deriving from Exception"


instance Infer Expr where
    infer env (Var l n)                 = case findname n env of
                                            NVar sc dec -> do 
                                                dump [GEN l sc]
                                                instantiate env $ openFX sc
                                            NSVar t ->
                                                return t
                                            NClass q _ _ -> do
                                                ts <- newTVars (length q)
                                                return (tAt (TC (NoQual n) ts))
    infer env (Int _ val s)             = return tInt
    infer env (Float _ val s)           = return tFloat
    infer env e@Imaginary{}             = notYetExpr e
    infer env (Bool _ val)              = return tBool
    infer env (None _)                  = return tNone
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env (Strings _ ss)            = return tStr
    infer env (BStrings _ ss)           = return tStr
    infer env (UStrings _ ss)           = return tStr
    infer env (Call l e ps ks)          = do t <- infer env e
                                             dump [INS (loc e) t]
                                             prow <- infer env ps
                                             krow <- infer env ks
--                                             (prow, krow) <- inferArgs env args
                                             t0 <- newTVar
                                             fx <- currFX
                                             constrain [Sub t (tFun fx prow krow t0)]
                                             return t0
    infer env (Await l e)               = do t <- infer env e
                                             t0 <- newTVar
                                             fx <- fxSync <$> newTVar
                                             equFX fx
                                             constrain [Sub t (tMsg t0)]
                                             return t0
    infer env (Index l e [i])           = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newTVar
                                             constrain [Impl t (cIndexed ti t0)]
                                             return t0
    infer env (Slice l e [s])           = do t <- infer env e
                                             inferSlice env s
                                             constrain [Impl t cSliceable]
                                             return t
    infer env (Cond l e1 e e2)          = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             inferBool env e
                                             t0 <- newTVar
                                             constrain [Sub t1 t0, Sub t2 t0]
                                             return t0
    infer env (BinOp l e1 (Op _ op) e2)
      | op `elem` [Or,And]              = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             constrain [Impl t1 cTruth, Impl t2 cTruth]
                                             return tBool
      | otherwise                       = do t1 <- infer env e1
                                             t2 <- infer env e2
                                             t <- newTVar
                                             constrain [Sub t1 t, Sub t2 t, Impl t (protocol op)]
                                             return t
      where protocol Plus               = cPlus
            protocol Minus              = cMinus
            protocol Mult               = cNumber
            protocol Pow                = cNumber
            protocol Div                = cNumber
            protocol Mod                = cReal
            protocol EuDiv              = cReal
            protocol ShiftL             = cIntegral
            protocol ShiftR             = cIntegral
            protocol BOr                = cLogical
            protocol BXor               = cLogical
            protocol BAnd               = cLogical
            protocol MMult              = cMatrix
    infer env (UnOp l (Op _ op) e)
      | op == Not                       = do t <- infer env e
                                             constrain [Impl t cTruth]
                                             return tBool
      | otherwise                       = do t <- infer env e
                                             constrain [Impl t (protocol op)]
                                             return t
      where protocol UPlus              = cNumber
            protocol UMinus             = cNumber
            protocol BNot               = cIntegral
    infer env (CompOp l e ops)          = do t1 <- infer env e
                                             walk t1 ops
                                             return tBool
      where walk t0 []                     = return ()
            walk t0 (OpArg (Op l o) e:ops)
              | o `elem` [In,NotIn]     = do t1 <- infer env e
                                             constrain [Impl t1 (cContainer t0)]
                                             walk t1 ops
                                             return ()
              | otherwise               = do t1 <- infer env e
                                             t <- newTVar
                                             constrain [Sub t0 t, Sub t1 t, Impl t (protocol o)]
                                             walk t ops
            protocol Eq                 = cEq
            protocol NEq                = cEq
            protocol LtGt               = cEq
            protocol Lt                 = cOrd
            protocol Gt                 = cOrd
            protocol LE                 = cOrd
            protocol GE                 = cOrd
            protocol Is                 = cIdentity
            protocol IsNot              = cIdentity
    infer env (Dot l e n)               = do t <- infer env e
                                             t0 <- newTVar
                                             constrain [Sel t n t0]
                                             return t0
    infer env (DotI l e i)              = do t <- infer env e
                                             t0:tvs <- newTVars (i+2)
                                             constrain [Equ t (tTuple (row t0 tvs i))]
                                             return t0
      where row t0 [r] 0                = posRow t0 r
            row t0 (t1:tvs) n           = posRow t1 (row t0 tvs (n-1))
    infer env (Lambda l par e)
      | nodup par                       = do fx <- newTVar
                                             pushFX fx
                                             (prow, krow, te) <- infParams env1 par
                                             t <- infer (define te env1) e
                                             popFX
                                             dump [INS l $ tFun fx prow krow t]
                                             return (tFun fx prow krow t)
      where env1                        = block (bound par) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l es)              = do ts <- mapM (infer env) es
                                             return (OTuple r)
    infer env (TupleComp l (Elem e) co)
      | nodup co                        = do te <- infEnv env co
                                             t <- infer (o_define te env) e
                                             return (OList t)                   -- !! Big over-generalization, for now
    infer env (Record l fs)             = do te <- infer env fs
                                             return (ORecord te)
    infer env (RecordComp l n e co)     = do te <- infEnv env co
                                             let env1 = o_define te env
                                             _ <- infer env1 (Var (nloc n) n)
                                             _ <- infer env1 e
                                             r <- newOVar
                                             return (ORecord r)                 -- !! Big over-generalization, for now
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
    infer env (Paren l e)               = infer env e


inferBool env e                         = do t <- infer env e
                                             constrain [Impl t cTruth]
                                             return ()

inferSlice env (Sliz l e1 e2 e3)        = do ts <- mapM (infer env) es
                                             constrain [ Equ t tInt | (t,e) <- ts `zip` es ]
                                             return ()
  where es                              = concat $ map maybeToList (e1:e1:maybeToList e3)


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

instance Infer PosArg where
    infer env (PosArg e p)              = do t <- infer env e
                                             prow <- infer env p
                                             return (posRow t prow)
    infer env (PosStar e)               = do t <- infer env t
                                             prow <- newTVar
                                             constrain [Equ t (tTuple prow)]
                                             return prow
    infer env PosNil                    = return posNil
    
instance Infer KwdArg where
    infer env (KwdArg n e k)            = do t <- infer env e
                                             krow <- infer env k
                                             return (kwdRow n t krow)
    infer env (KwdStar e)               = do t <- infer env t
                                             krow <- newTVar
                                             constrain [Equ t (tRecord krow)]
                                             return krow
    infer env KwdNil                    = return kwdNil
    
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
    infEnvT env (PIndex l e [i])        = do t <- infer env e
                                             ti <- infer env i
                                             t0 <- newOVar
                                             o_constrain [QIx l 1 t ti t0]
                                             return ([], t0)                                    -- indexability of e
    infEnvT env (PSlice l e [i])        = do t <- infer env e
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
                                             
{-
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

getInit l tenv                          = case partition ((=="__init__") . nstr . fst) tenv of
                                            ([],_) -> notYet l (text "Class declaration without an '__init__' method")
                                            ([(n,t)],tenv1) -> (nloc n, t, tenv1)

-}