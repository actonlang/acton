{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.CPS(convert) where

import Debug.Trace
import Control.Monad.State.Strict
import Control.Monad.Writer
import Utils
import Pretty
import Acton.Syntax
import Acton.Printer
import Acton.Names
import Acton.Builtin
import Acton.Prim

convert                                 :: TInfo -> Module -> IO Module
convert inf (Module qn imps stmts)      = return $ Module qn imps $ runCpsM $ cps (env0 inf) stmts

type CpsM a                             = State CpsState a

type CpsState                           = ([Int], [Stmt])

runCpsM                                 :: CpsM a -> a
runCpsM m                               = evalState m ([1..], [])

newName                                 :: String -> CpsM Name
newName s                               = state (\(uniq:supply, stmts) -> (Internal s uniq CPSPass, (supply, stmts)))

newNames                                = mapM newName

prefix                                  :: [Stmt] -> CpsM ()
prefix ss                               = state (\(supply, stmts) -> ((), (supply, reverse ss ++ stmts)))

swapPrefixes                            :: [Stmt] -> CpsM [Stmt]
swapPrefixes ss                         = state (\(supply, stmts) -> (stmts, (supply, ss)))

withPrefixes                            :: CpsM a -> CpsM (a,[Stmt])
withPrefixes m                          = do ss0 <- swapPrefixes []
                                             r <- m
                                             ss1 <- swapPrefixes ss0
                                             return (r, reverse ss1)


nType                                   = name "type"                       -- TODO: de-pythonize these references
nTraceback                              = name "__traceback__"

data Frame                              = Act
                                        | Meth   Name                       -- contParam
                                        | Seq    Name                       -- nextCont
                                        | Loop   Name                       -- loopEntry
                                        | Wrap   Name                       -- finalizer
                                        | Unwrap Name Name                  -- levelParam contParam
                                        | Pop
                                        deriving (Eq,Show)

data Env                                = Env { ctxt :: [Frame], defd :: [Name], tinfo :: TInfo }

type TInfo                              = Substitution

env0 inf                                = Env { ctxt = [], defd = [], tinfo = inf }

infixr +:
frame +: env                            = env { ctxt = frame : ctxt env }

emptyCtxt env                           = env{ ctxt = [] }

define x env                            = env { defd = bound x ++ defd env }


eCallCont c args                        = eCallV primCONT (eVar c : args)

pushH h                                 = sExpr (eCallV primPUSH [h])

format (Int _ 0 _, cont)                = [sReturn cont]
format (lvl, cont)                      = [sExpr (eCallV primPOP [lvl]), sReturn cont]

wrapC c f env                           = eCallCont c [level, eLambda [] cont]
  where (level, cont)                   = f 0 env

unwrapL 0 lvl                           = eVar lvl
unwrapL n lvl                           = eBinOp (eInt n) Plus (unwrapL 0 lvl)

seqcont n (Pop : env)                   = seqcont (n+1) env
seqcont n (Seq c : _)                   = (eInt n, eCallCont c [eNone])
seqcont n (Loop c : _)                  = (eInt n, eCallCont c [eNone])
seqcont n (Meth c : _)                  = (eInt n, eCallCont c [eNone])
seqcont n (Wrap c : env)                = (eInt n, wrapC c seqcont env)
seqcont n (Unwrap lvl cnt : _)          = (unwrapL n lvl, eCallCont cnt [])

cntcont n (Pop : env)                   = cntcont (n+1) env
cntcont n (Seq c : env)                 = cntcont n env
cntcont n (Loop c : _)                  = (eInt n, eCallCont c [eNone])
cntcont n (Wrap c : env)                = (eInt n, wrapC c cntcont env)
cntcont n (Unwrap lvl cnt : env)        = cntcont n env

brkcont n (Pop : env)                   = brkcont (n+1) env
brkcont n (Seq c : env)                 = brkcont n env
brkcont n (Loop c : env)                = seqcont n env
brkcont n (Wrap c : env)                = (eInt n, wrapC c brkcont env)
brkcont n (Unwrap lvl cnt : env)        = brkcont n env

retcont e n (Pop : env)                 = retcont e (n+1) env
retcont e n (Seq c : env)               = retcont e n env
retcont e n (Loop c : env)              = retcont e n env
retcont e n (Meth c : _)                = (eInt n, eCallCont c [e])
retcont e n (Wrap c : env)              = (eInt n, wrapC c (retcont e) env)
retcont e n (Unwrap lvl cnt : env)      = retcont e n env

quicknext (Seq c : _)                   = Just (eVar c)
quicknext (Loop c : _)                  = Just (eVar c)
quicknext (Meth c : _)                  = Just (eVar c)
quicknext (Wrap c : env)                = Just (wrapC c seqcont env)
quicknext _                             = Nothing


cpsSuite env ss                         = do (ss',prefixes) <- withPrefixes $ pre env ss
                                             cps env (prefixes ++ ss')

class CPS a where
    cps :: Env -> a -> CpsM a

instance CPS [Stmt] where
    cps env []
      | inCont env                      = return $ format $ seqcont 0 (ctxt env)
    cps env (Continue _ : _)
      | inCont env                      = return $ format $ cntcont 0 (ctxt env)
    cps env (Break _ : _)
      | inCont env                      = return $ format $ brkcont 0 (ctxt env)
    cps env (Return _ Nothing : _)
      | inCont env                      = return $ format $ retcont eNone 0 (ctxt env)
    cps env (Return _ (Just e) : _)
      | contCall env e,
        Just c <- quicknext (ctxt env)  = return $ sReturn (addContArg e c) : []
      | inCont env                      = return $ format $ retcont e 0 (ctxt env)

    cps env (Assign _ [PVar _ n _] e : 
             Return _ (Just e') : _)
      | contCall env e, e' == eVar n,
        Just c <- quicknext (ctxt env)  = return $ sReturn (addContArg e c) : []

    cps env [Expr _ e]
      | contCall env e,
        Just c <- quicknext (ctxt env)  = return $ sReturn (addContArg e c) : []

    cps env (Expr _ e : ss)
      | contCall env e                  = do ns <- newNames ["cont","res"]
                                             let [k,x] = ns
                                             ss' <- cps env ss
                                             return $ sDef k (pospar [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (Assign _ [PVar _ x _] e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             ss' <- cps env ss
                                             return $ sDef k (pospar [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []
    cps env (Assign _ ps e : ss)
      | contCall env e                  = do ns <- newNames ["cont","res"]
                                             let [k,x] = ns
                                             ss' <- cps env (sAssign ps (eVar x) : ss)
                                             return $ sDef k (pospar [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (Update _ ts e : ss)
      | contCall env e                  = do ns <- newNames ["cont","res"]
                                             let [k,x] = ns
                                             ss' <- cps env (sUpdate ts (eVar x) : ss)
                                             return $ sDef k (pospar [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (IUpdate _ t op e : ss)
      | contCall env e                  = do ns <- newNames ["cont","res"]
                                             let [k,x] = ns
                                             ss' <- cps env (sIUpdate t op (eVar x) : ss)
                                             return $ sDef k (pospar [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (Decl l ds : ss)            = do ds' <- mapM (cps env1) ds
                                             ss' <- cps env1 ss
                                             return $ sDecl ds' : ss'
      where env1                        = define ds env
    
    cps env (s : ss)
      | not (needCont env s)            = do ss' <- cps (define s env) ss
                                             return $ s : ss'
    
    cps env [If _ bs els]               = do bs' <- mapM (cps env) bs
                                             els' <- cpsSuite env els
                                             return $ sIf bs' els' : []
    
    cps env [While _ e b els]           = do ns <- newNames ["loop","res"]
                                             let [k,x] = ns
                                             b' <- cpsSuite (Loop k +: env) b
                                             els' <- cpsSuite env els
                                             let body = sIf1 e b' els' : []
                                             return $ sDef k (pospar [x]) body : 
                                                      jump k

    cps env [For _ p e b els]           = do ns <- newNames ["loop","res","iter"]
                                             let [k,x,ivar] = ns
                                             b' <- cpsSuite (Loop k +: env) b
                                             els' <- cpsSuite env els
                                             let advance = sAssign [p] (eCall (eDot (eVar ivar) nextKW) [])
                                                 body = sTry (advance : b') [handler qnStopIteration els'] [] [] : []
                                             return $ sAssign [pVar ivar Nothing] (eCallVar iterKW [e]) :
                                                      sDef k (pospar [x]) body :
                                                      jump k

    cps env [Try _ b [] [] fin]         = do ns <- newNames ["finalizer","x","level","cont"]
                                             let [fcnt,x,lvl,cnt] = ns
                                             fin' <- cpsSuite (Unwrap lvl cnt +: env) fin
                                             b' <- cpsSuite (Pop +: Wrap fcnt +: env) b
                                             return $ sDef fcnt (pospar [lvl, cnt]) fin' :
                                                      pushH (finalH x fcnt) :
                                                      b'

    cps env [Try _ b hs [] []]          = do ns <- newNames ["handler","x"]
                                             let [hcnt,x] = ns
                                             body <- hbody env x hs
                                             b' <- cpsSuite (Pop +: env) b
                                             return $ sDef hcnt (pospar [x]) body :
                                                      pushH (eVar hcnt) :
                                                      b'

    cps env [Try _ b hs els []]         = do ns <- newNames ["handler","else","x"]
                                             let [hcnt,ecnt,x] = ns
                                             body <- hbody env x hs
                                             els' <- cpsSuite env els
                                             b' <- cpsSuite (Pop +: Seq ecnt +: env) b
                                             return $ sDef hcnt (pospar [x]) body :
                                                      sDef ecnt (pospar [x]) els' :
                                                      pushH (eVar hcnt) :
                                                      b'

    cps env [Try _ b hs [] fin]         = do ns <- newNames ["finalizer","handler","x","level","cont"]
                                             let [fcnt,hcnt,x,lvl,cnt] = ns
                                             fin' <- cpsSuite (Unwrap lvl cnt +: env) fin
                                             body <- hbody (Pop +: Wrap fcnt +: env) x hs
                                             b' <- cpsSuite (Pop +: Wrap fcnt +: env) b
                                             return $ sDef fcnt (pospar [lvl, cnt]) fin' :
                                                      sDef hcnt (pospar [x]) (pushH (finalH x fcnt) : body) :
                                                      pushH (eVar hcnt) :
                                                      b'
                                          
    cps env [Try _ b hs els fin]        = do ns <- newNames ["finalizer","else","handler","x","level","cont"]
                                             let [fcnt,ecnt,hcnt,x,lvl,cnt] = ns
                                             fin' <- cpsSuite (Unwrap lvl cnt +: env) fin
                                             body <- hbody (Pop +: Wrap fcnt +: env) x hs
                                             els' <- cpsSuite (Pop +: Wrap fcnt +: env) els
                                             b' <- cpsSuite (Pop +: Seq ecnt +: Wrap fcnt +: env) b
                                             return $ sDef fcnt (pospar [lvl, cnt]) fin' :
                                                      sDef hcnt (pospar [x]) (pushH (finalH x fcnt) : body) :
                                                      sDef ecnt (pospar [x]) (pushH (finalH x fcnt) : els') :
                                                      pushH (eVar hcnt) :
                                                      b'

    cps env [With _ [] b]               = cpsSuite env b
    
    cps env [With l (item:items) b]     = do ns <- newNames ["mgr","finalizer","handler","x","level","cont"]
                                             let [m,fcnt,hcnt,x,lvl,cnt] = ns
                                             b' <- cpsSuite (Pop +: Wrap fcnt +: env) [With l items b]
                                             return $ enter m item ++
                                                      sDef fcnt (pospar [lvl, cnt]) (exit m) :
                                                      sDef hcnt (pospar [x]) (hbody m x) :
                                                      pushH (eVar hcnt) :
                                                      b'
      where hbody m x                   = sIf1 (ifexit m x) [sReturn (eBool True)] [] : []
            enter m (WithItem e mbp)
              | Just p <- mbp           = [sAssign [pVar m Nothing] e, sAssign [p] (eCall (eDot (eVar m) enterKW) [])]
              | otherwise               = [sAssign [pVar m Nothing] e]
            ifexit m x                  = eCall (eDot (eVar m) exitKW) [type_x,val_x,tb_x]      -- TODO: de-pythonize this test
              where type_x              = eCall (eVar nType) [val_x]
                    val_x               = eVar x
                    tb_x                = eDot val_x nTraceback
            exit m                      = [sExpr (eCall (eDot (eVar m) exitKW) [eNone,eNone,eNone])]
            
    cps env (s : ss)                    = do ns <- newNames ["cont","res"]
                                             let [k,x] = ns
                                             ss' <- cps env ss
                                             ss1 <- cps (Seq k +: env) [s]
                                             return $ sDef k (pospar [x]) ss' :
                                                      ss1

    cps env []                          = return []

instance CPS Decl where
    cps env (Class l n q cs b)          = Class l n q cs <$> cpsSuite env1 b
      where env1                        = emptyCtxt env

    cps env (Protocol l n q cs b)       = Protocol l n q cs <$> cpsSuite env1 b
      where env1                        = emptyCtxt env

    cps env (Extension l n q cs b)      = Extension l n q cs <$> cpsSuite env1 b
      where env1                        = emptyCtxt env

    cps env (Actor l n q p k a b)       = Actor l n q (addContParam p) k a <$> cpsSuite env1 b
      where env1                        = Meth contKW +: env { ctxt = [Act] }

    cps env (Def l n q p k a b m)
      | contDef (tinfo env) l n m       = Def l n q (addContParam p) k a <$> cpsSuite env1 b <*> return m
      where env1                        = Meth contKW +: env { defd = bound p }

    cps env d                           = return d
    

instance CPS Branch where
    cps env (Branch e ss)               = Branch e <$> cpsSuite env ss

    
instance CPS Handler where
    cps env (Handler ex ss)             = Handler ex <$> cpsSuite env ss


jump k                                  = sReturn (eCall (eVar k) [eNone]) : []


addContArg (Call l e pos KwdNil) c      = Call NoLoc e (add pos c) KwdNil
  where add PosNil c                    = PosArg c PosNil
        add (PosArg e p) c              = PosArg e (add p c)

addContParam PosNIL                     = PosPar contKW Nothing Nothing PosNIL
addContParam (PosPar n t e p)           = PosPar n t e (addContParam p)



finalH x f                              = eLambda [x] (eCallVar f [eInt 0, raiseH])
  where raiseH                          = eLambda [] (eCallV primRAISE [eVar x])

hbody env x hs                          = do hs' <- mapM (cps env) hs
                                             return $ sTry [sRaise (eVar x)] hs' [] [] :
                                                      []


contCall env (Call l (Var _ n) p k)
  | n == primAWAIT                      = True
  | isPrim n                            = False
  | n `elem` ns0                        = False
  where ns0                             = [qnStr,qnInt,qnLen,qnPrint]
        isPrim (QName m _)              = m == mPrim
        isPrim _                        = False
contCall env (Call l (Dot _ _ n) p k)
  | n `elem` ns0                        = False
  where ns0                             = attrKWs
contCall env (Call l e p k)             = True                      -- TODO: utilize type...
contCall env _                          = False

contDef env l n m
--  | m == Async                          = True
  | impure l n                          = True
  | otherwise                           = False
  where impure l n                      = True                      -- TODO: utilize type...

inCont env                              = length (ctxt env) > 0


class NeedCont a where
    needCont                            :: Env -> a -> Bool

instance NeedCont a => NeedCont [a] where
    needCont env xs                     = any (needCont env) xs

instance NeedCont Branch where
    needCont env (Branch _ ss)          = needCont env ss

instance NeedCont Handler where
    needCont env (Handler _ ss)         = needCont env ss

instance NeedCont Stmt where
    needCont env (Expr _ e)             = contCall env e
    needCont env (Assign _ _ e)         = contCall env e
    needCont env (Update _ _ e)         = contCall env e
    needCont env (IUpdate _ _ _ e)      = contCall env e
    needCont env (If _ bs els)          = needCont env bs || needCont env els
    needCont env (While _ _ b els)      = needCont env b || needCont env els
    needCont env (For _ _ _ b els)      = needCont env b || needCont env els
    needCont env (Try _ b hs els fin)   = needCont env b || needCont env hs || needCont env fin
    needCont env (With _ _ b)           = needCont env b
    needCont env _                      = False



------------------------------------------------
------------------------------------------------
------------------------------------------------


class PreCPS a where
    pre                                 :: Env -> a -> CpsM a
    preTop                              :: Env -> a -> CpsM a
    preTop env                          = pre env

instance PreCPS a => PreCPS [a] where
    pre env                             = mapM (pre env)

instance PreCPS a => PreCPS (Maybe a) where
    pre env Nothing                     = return Nothing
    pre env (Just a)                    = fmap Just (pre env a)
    
    preTop env Nothing                  = return Nothing
    preTop env (Just a)                 = fmap Just (preTop env a)

instance PreCPS Stmt where
    pre env (Expr l e)                  = Expr l <$> preTop env e
    pre env (Assign l ps e)             = Assign l <$> pre env ps <*> preTop env e
    pre env (Update l ts e)             = Update l <$> pre env ts <*> preTop env e
    pre env (IUpdate l t op e)          = IUpdate l <$> pre env t <*> return op <*> preTop env e
    pre env (Assert l e mbe)            = Assert l <$> pre env e <*> pre env mbe
    pre env (Delete l t)                = Delete l <$> pre env t
    pre env (Return l e)                = Return l <$> preTop env e
    pre env (Raise l e)                 = Raise l <$> pre env e
    pre env (If l bs els)               = If l <$> pre env bs <*> return els
    pre env (While l e b els)           = While l <$> pre env e <*> return b <*> return els
    pre env (For l p e b els)           = For l <$> pre env p <*> pre env e <*> return b <*> return els
    pre env (Try l b hs els fin)        = return $ Try l b hs els fin
    pre env (With l [item] b)           = With l <$> pre env [item] <*> return b
    pre env (With l (item:items) b)     = pre env (With l [item] [With l items b])
    pre env (Data l p b)                = Data l <$> pre env p <*> pre env b
    pre env (VarAssign l p e)           = VarAssign l <$> pre env p <*> pre env e
    pre env s                           = return s

instance PreCPS Decl where
    pre env (Actor l n q ps ks ann b)   = Actor l n q <$> pre env ps <*> pre env ks <*> return ann <*> return b
    pre env (Def l n q ps ks ann b m)   = Def l n q <$> pre env ps <*> pre env ks <*> return ann <*> return b <*> return m
    pre env d                           = return d
    
instance PreCPS Branch where
    pre env (Branch e ss)               = Branch  <$> pre env e <*> return ss

instance PreCPS Exception where
    pre env (Exception e1 e2)           = Exception <$> pre env e1 <*> pre env e2

instance PreCPS WithItem where
    pre env (WithItem e p)              = WithItem <$> pre env e <*> pre env p

instance PreCPS PosArg where
    pre env (PosArg e p)                = PosArg <$> pre env e <*> pre env p
    pre env (PosStar e)                 = PosStar <$> pre env e
    pre env PosNil                      = return PosNil

instance PreCPS KwdArg where
    pre env (KwdArg n e p)              = KwdArg n <$> pre env e <*>pre env p
    pre env (KwdStar e)                 = KwdStar <$> pre env e
    pre env KwdNil                      = return KwdNil

instance PreCPS Expr where
    pre env e0@(Call l e ps ks)
      | contCall env e0                 = do ps1 <- pre env ps                                          -- TODO: utilize type of e
                                             ks1 <- pre env ks
                                             v <- newName "sync"
                                             prefix [sAssign [pVar v Nothing] (Call l e ps1 ks1)]
                                             return (eVar v)
    pre env (Call l e ps ks)            = Call l <$> pre env e <*> pre env ps <*> pre env ks
    pre env (Index l e ix)              = Index l <$> pre env e <*> pre env ix
    pre env (Slice l e sl)              = Slice l <$> pre env e <*> pre env sl
    pre env (Cond l e1 e e2)            = Cond l <$> pre env e1 <*> pre env e <*> pre env e2
    pre env (BinOp l e1 op e2)          = BinOp l <$> pre env e1 <*> return op <*> pre env e2
    pre env (CompOp l e ops)            = CompOp l <$> pre env e <*> pre env ops
    pre env (UnOp l o e)                = UnOp l o <$> pre env e
    pre env (Dot l e n)                 = Dot l <$> pre env e <*> return n
    pre env (DotI l e i t)              = DotI l <$> pre env e <*> return i <*> return t
    pre env (Lambda l ps ks e)          = do (e1,stmts) <- withPrefixes $ pre env e
                                             case stmts of                                              -- TODO: utilize type of e (+below)
                                                 [] ->
                                                    liftM3 (Lambda l) (pre env ps) (pre env ks)(return e1)
                                                 _  -> do 
                                                    ps1 <- pre env ps
                                                    ks1 <- pre env ks
                                                    f <- newName "lambda"
                                                    prefix [sDecl [Def l f [] ps1 ks1 Nothing (stmts ++ [sReturn e1]) NoDec]]
                                                    return (Var l0 (NoQual f))
    pre env (Yield l e)                 = Yield l <$> pre env e
    pre env (YieldFrom l e)             = YieldFrom l <$> pre env e
    pre env (Tuple l es ks)             = Tuple l <$> pre env es <*> pre env ks
    pre env (List l es)                 = List l <$> pre env es
    pre env (ListComp l (Elem e) c)     = do (e1,stmts) <- withPrefixes $ liftM2 (ListComp l) (fmap Elem $ pre env e) (pre env c)
                                             case stmts of
                                                 [] -> return e1
                                                 _  -> do acc <- newName "acc"
                                                          preComp env (s0 acc) (s1 acc) c >> return (eVar acc)
      where s0 acc                      = sAssign [pVar acc Nothing] (eCallV qnList [])
            s1 acc                      = sExpr (eCall (eDot (eVar acc) (name "append")) [e])
    pre env (Dict l es)                 = Dict l <$> pre env es
    pre env (DictComp l (Assoc k v) c)  = do (e1,stmts) <- withPrefixes $ liftM2 (DictComp l) (liftM2 Assoc (pre env k) (pre env v)) (pre env c)
                                             case stmts of
                                                 [] -> return e1
                                                 _  -> do acc <- newName "acc"
                                                          preComp env (s0 acc) (s1 acc) c >> return (eVar acc)
      where s0 acc                      = sAssign [pVar acc Nothing] (eCallV qnDict [])
            s1 acc                      = undefined -- sAssign [tIndex (eVar acc) k] v
    pre env (Set l es)                  = Set l <$> pre env es
    pre env (SetComp l (Elem e) c)      = do (e1,stmts) <- withPrefixes $ liftM2 (SetComp l) (fmap Elem $ pre env e) (pre env c)
                                             case stmts of
                                                [] -> return e1
                                                _  -> do acc <- newName "acc"
                                                         preComp env (s0 acc) (s1 acc) c >> return (eVar acc)
      where s0 acc                      = sAssign [pVar acc Nothing] (eCallV qnSetT [])
            s1 acc                      = sExpr (eCall (eDot (eVar acc) (name "add")) [e])
    pre env (Paren l e)                 = Paren l <$> pre env e
    pre env e                           = return e

    preTop env e0@(Call l e ps ks)
      | contCall env e0                 = Call l e <$> pre env ps <*> pre env ks
    preTop env e                        = pre env e


preComp env s0 s1 c                     = pre env (s0 : mkLoop c) >>= prefix
  where mkLoop NoComp                   = [s1]
        mkLoop (CompIf l e c)           = [If l [Branch e (mkLoop c)] []]
        mkLoop (CompFor l t e c)        = [For l t e (mkLoop c) []]

instance PreCPS Sliz where
    pre env (Sliz l a b c)              = Sliz l <$> pre env a <*> pre env b <*> pre env c

instance PreCPS OpArg where
    pre env (OpArg op e)                = OpArg op <$> pre env e

instance PreCPS Elem where
    pre env (Elem e)                    = Elem <$> pre env e
    pre env (Star e)                    = Star <$> pre env e

instance PreCPS Assoc where
    pre env (Assoc k v)                 = Assoc <$> pre env k <*> pre env v
    pre env (StarStar e)                = StarStar <$> pre env e

instance PreCPS PosPar where
    pre env (PosPar n t e p)            = do (e1,stmts) <- withPrefixes $ pre env e
                                             case stmts of
                                                 [] -> PosPar n t e1 <$> pre env p
                                                 _  -> notYet (loc e) "Continuations inside a default value"
    pre env (PosSTAR n t)               = return $ PosSTAR n t
    pre env PosNIL                      = return PosNIL

instance PreCPS KwdPar where
    pre env (KwdPar n t e p)            = do (e1,stmts) <- withPrefixes $ pre env e
                                             case stmts of                                              -- TODO: utilize type of e
                                                 [] -> KwdPar n t e1 <$> pre env p
                                                 _  -> notYet (loc e) "Continuations inside a default value"
    pre env (KwdSTAR n t)               = return $ KwdSTAR n t
    pre env KwdNIL                      = return KwdNIL

instance PreCPS PosPat where
    pre env (PosPat p ps)               = PosPat <$> pre env p <*> pre env ps
    pre env (PosPatStar p)              = PosPatStar <$> pre env p
    pre env PosPatNil                   = return PosPatNil
    
instance PreCPS KwdPat where
    pre env (KwdPat n p ps)             = KwdPat n <$> pre env p <*> pre env ps
    pre env (KwdPatStar p)              = KwdPatStar <$> pre env p
    pre env KwdPatNil                   = return KwdPatNil

instance PreCPS Comp where
    pre env (CompFor l t e c)           = CompFor l <$> pre env t <*> pre env e <*> pre env c
    pre env (CompIf l e c)              = CompIf l <$> pre env e <*> pre env c
    pre env NoComp                      = return NoComp

instance PreCPS Pattern where
    pre env (PVar l n a)                = return (PVar l n a)
    pre env (PTuple l ps ks)            = PTuple l <$> pre env ps <*> pre env ks
    pre env (PList l ps p)              = PList l <$> pre env ps <*> pre env p
    pre env (PParen l p)                = PParen l <$> pre env p
    pre env (PData l n ixs)             = PData l n <$> pre env ixs

instance PreCPS Target where
    pre env (TaVar l n)                 = return (TaVar l n)
    pre env (TaIndex l e ix)            = TaIndex l <$> pre env e <*> pre env ix
    pre env (TaSlice l e sl)            = TaSlice l <$> pre env e <*> pre env sl
    pre env (TaDot l e n)               = TaDot l <$> pre env e <*> return n
    pre env (TaDotI l e i tl)           = TaDotI l <$> pre env e <*> return  i <*> return tl
    pre env (TaTuple l ps)              = TaTuple l <$> pre env ps
    pre env (TaParen l p)               = TaParen l <$> pre env p
