{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.CPS(convert) where

import Debug.Trace
import Control.Monad.State
import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin


convert s (Module qn imps stmts)        = return $ Module qn imps $ runCpM $ cps (env0 s) stmts

type CpM a                              = State [Int] a

runCpM                                  :: CpM a -> a
runCpM m                                = evalState m [1..] 

newName                                 :: String -> CpM Name
newName s                               = state (\(uniq:supply) -> (Internal s uniq CPSPass, supply))

newNames                                = mapM newName

rtsREF s                                = eDot (eVar nRTS) (name s)

rtsPUSH                                 = rtsREF "PUSH"
rtsPOP                                  = rtsREF "POP"
rtsRAISE                                = rtsREF "RAISE"

nRTS                                    = name "___RTS___"
contKW                                  = name "___cont___"

nType                                   = name "type"                   -- TODO: de-pythonize these references
nTraceback                              = name "__traceback__"

data Frame                              = Act
                                        | Meth   Name                       -- contParam
                                        | Seq    Name                       -- nextCont
                                        | Loop   Name                       -- loopEntry
                                        | Wrap   Name                       -- finalizer
                                        | Unwrap Name Name                  -- levelParam contParam
                                        | Pop
                                        deriving (Eq,Show)

data Env                                = Env { ctxt :: [Frame], defd :: [Name], tinfo :: Substitution }

env0 s                                  = Env { ctxt = [], defd = [], tinfo = s }

infixr +:
frame +: env                            = env { ctxt = frame : ctxt env }

define x env                            = env { defd = bound x ++ defd env }


sDef n p b                              = sDecl [Def NoLoc n [] p KwdNIL Nothing b NoMod]

sReturn e                               = Return NoLoc (Just e)
sAssign ps e                            = Assign NoLoc ps e
sAugAssign p o e                        = AugAssign NoLoc p o e
sRaise e                                = Raise NoLoc (Just (Exception e Nothing))
sExpr e                                 = Expr NoLoc e
sDecl ds                                = Decl NoLoc ds
sTry b hs els fin                       = Try NoLoc b hs els fin
sIf bs els                              = If NoLoc bs els
sIf1 e b els                            = sIf [Branch e b] els

handler qn b                            = Handler (Except NoLoc qn) b

eCall e es                              = Call NoLoc e (foldl (flip PosArg) PosNil es) KwdNil
eCallVar c es                           = eCall (eVar c) es
eVar n                                  = Var NoLoc n
eDot e n                                = Dot NoLoc e n
eNone                                   = None NoLoc
eInt n                                  = Int NoLoc n (show n)
eBool b                                 = Bool NoLoc b
eBinOp e o e'                           = BinOp NoLoc e (Op NoLoc o) e'
eLambda ns e                            = Lambda NoLoc (param ns) KwdNIL e

param xs                                = foldr (\n p -> PosPar n Nothing Nothing p) PosNIL xs

pVar n t                                = PVar NoLoc n t

pushH h                                 = sExpr (eCall rtsPUSH [eVar selfKW, h])            -- TODO: ensure selfKW is in scope

format (Int _ 0 _, cont)                = [sReturn cont]
format (lvl, cont)                      = [sExpr (eCall rtsPOP [self,lvl]), sReturn cont]
  where self                            = eVar selfKW                                       -- TODO: ensure selfKW is in scope

wrapC c f env                           = eCallVar c [level, eLambda [] cont]
  where (level, cont)                   = f 0 env

unwrapL 0 lvl                           = eVar lvl
unwrapL n lvl                           = eBinOp (eInt n) Plus (unwrapL 0 lvl)

seqcont n (Pop : env)                   = seqcont (n+1) env
seqcont n (Seq c : _)                   = (eInt n, eCallVar c [eNone])
seqcont n (Loop c : _)                  = (eInt n, eCallVar c [eNone])
seqcont n (Meth c : _)                  = (eInt n, eCallVar c [eNone])
seqcont n (Wrap c : env)                = (eInt n, wrapC c seqcont env)
seqcont n (Unwrap lvl cnt : _)          = (unwrapL n lvl, eCallVar cnt [])

cntcont n (Pop : env)                   = cntcont (n+1) env
cntcont n (Seq c : env)                 = cntcont n env
cntcont n (Loop c : _)                  = (eInt n, eCallVar c [eNone])
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
retcont e n (Meth c : _)                = (eInt n, eCallVar c [e])
retcont e n (Wrap c : env)              = (eInt n, wrapC c (retcont e) env)
retcont e n (Unwrap lvl cnt : env)      = retcont e n env

quicknext (Seq c : _)                   = Just (eVar c)
quicknext (Loop c : _)                  = Just (eVar c)
quicknext (Meth c : _)                  = Just (eVar c)
quicknext (Wrap c : env)                = Just (wrapC c seqcont env)
quicknext _                             = Nothing
  

class CPS a where
    cps :: Env -> [a] -> CpM [a]

instance CPS Stmt where
    cps env []
      | inCont env                      = return $ format $ seqcont 0 (ctxt env)
    cps env (Continue _ : _)
      | inCont env                      = return $ format $ cntcont 0 (ctxt env)
    cps env (Break _ : _)
      | inCont env                      = return $ format $ brkcont 0 (ctxt env)
    cps env (Return _ Nothing : _)
      | inCont env                      = return $ format $ retcont eNone 0 (ctxt env)
    cps env (Return _ (Just e) : _)
      | inCont env                      = return $ format $ retcont e 0 (ctxt env)

    cps env [Expr _ e]
      | contCall env e,
        Just c <- quicknext (ctxt env)  = return $ sReturn (addContArg e c) : []

    cps env (Expr _ e : ss)
      | contCall env e                  = do [k,x] <- newNames ["cont","res"]
                                             ss' <- cps' env ss
                                             return $ sDef k (param [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (Assign _ ps e : ss)
      | contCall env e                  = do [k,x] <- newNames ["cont","res"]
                                             ss' <- cps' env (sAssign ps (eVar x) : ss)
                                             return $ sDef k (param [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (AugAssign _ p op e : ss)
      | contCall env e                  = do [k,x] <- newNames ["cont","res"]
                                             ss' <- cps' env (sAugAssign p op (eVar x) : ss)
                                             return $ sDef k (param [x]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []

    cps env (Decl _ ds : ss)            = do ds' <- cps env1 ds
                                             ss' <- cps env1 ss
                                             return $ sDecl ds' : ss'
      where env1                        = define ds env
    
    cps env (s : ss)
      | not (mayBlock env s)            = do ss' <- cps (define s env) ss
                                             return $ s : ss'
    
    cps env [If _ bs els]               = do bs' <- cps env bs
                                             els' <- cps env els
                                             return $ sIf bs' els' : []
    
    cps env [While _ e b els]           = do [k,x] <- newNames ["loop","res"]
                                             b' <- cps (Loop k +: env) b
                                             els' <- cps env els
                                             let body = sIf1 e b' els' : []
                                             return $ sDef k (param [x]) (nonlocals env body) : 
                                                      jump k

    cps env [For _ p e b els]           = do [k,x,ivar] <- newNames ["loop","res","iter"]
                                             b' <- cps (Loop k +: env) b
                                             els' <- cps' env els
                                             let advance = sAssign [p] (eCall (eDot (eVar ivar) nextKW) [])
                                                 body = sTry (advance : b') [handler qnStopIteration els'] [] [] : []
                                             return $ sAssign [pVar ivar Nothing] (eCallVar iterKW [e]) :
                                                      sDef k (param [x]) (nonlocals env body) :
                                                      jump k

    cps env [Try _ b [] [] fin]         = do [fcnt,x,lvl,cnt] <- newNames ["finalizer","x","level","cont"]
                                             fin' <- cps' (Unwrap lvl cnt +: env) fin
                                             b' <- cps (Pop +: Wrap fcnt +: env) b
                                             return $ sDef fcnt (param [lvl, cnt]) fin' :
                                                      pushH (finalH x fcnt) :
                                                      b'

    cps env [Try _ b hs [] []]          = do [hcnt,x] <- newNames ["handler","x"]
                                             body <- hbody env x hs
                                             b' <- cps (Pop +: env) b
                                             return $ sDef hcnt (param [x]) body :
                                                      pushH (eVar hcnt) :
                                                      b'

    cps env [Try _ b hs els []]         = do [hcnt,ecnt,x] <- newNames ["handler","else","x"]
                                             body <- hbody env x hs
                                             els' <- cps' env els
                                             b' <- cps (Pop +: Seq ecnt +: env) b
                                             return $ sDef hcnt (param [x]) body :
                                                      sDef ecnt (param [x]) els' :
                                                      pushH (eVar hcnt) :
                                                      b'

    cps env [Try _ b hs [] fin]         = do [fcnt,hcnt,x,lvl,cnt] <- newNames ["finalizer","handler","x","level","cont"]
                                             fin' <- cps' (Unwrap lvl cnt +: env) fin
                                             body <- hbody (Pop +: Wrap fcnt +: env) x hs
                                             b' <- cps (Pop +: Wrap fcnt +: env) b
                                             return $ sDef fcnt (param [lvl, cnt]) fin' :
                                                      sDef hcnt (param [x]) (pushH (finalH x fcnt) : body) :
                                                      pushH (eVar hcnt) :
                                                      b'
                                          
    cps env [Try _ b hs els fin]        = do [fcnt,ecnt,hcnt,x,lvl,cnt] <- newNames ["finalizer","else","handler","x","level","cont"]
                                             fin' <- cps' (Unwrap lvl cnt +: env) fin
                                             body <- hbody (Pop +: Wrap fcnt +: env) x hs
                                             els' <- cps' (Pop +: Wrap fcnt +: env) els
                                             b' <- cps (Pop +: Seq ecnt +: Wrap fcnt +: env) b
                                             return $ sDef fcnt (param [lvl, cnt]) fin' :
                                                      sDef hcnt (param [x]) (pushH (finalH x fcnt) : body) :
                                                      sDef ecnt (param [x]) (pushH (finalH x fcnt) : els') :
                                                      pushH (eVar hcnt) :
                                                      b'

    cps env [With _ [] b]               = cps env b
    
    cps env [With l (item:items) b]     = do [m,fcnt,hcnt,x,lvl,cnt] <- newNames ["mgr","finalizer","handler","x","level","cont"]
                                             b' <- cps (Pop +: Wrap fcnt +: env) [With l items b]
                                             return $ enter m item ++
                                                      sDef fcnt (param [lvl, cnt]) (exit m) :
                                                      sDef hcnt (param [x]) (hbody m x) :
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
            
--    cps env (s : ss)                    = sDef k (param [x]) (cps' (define s env) ss) :
    cps env (s : ss)                    = do [k,x] <- newNames ["cont","res"]
                                             ss' <- cps' env ss
                                             ss1 <- cps (Seq k +: env) [s]
                                             return $ sDef k (param [x]) ss' :
                                                      ss1

    cps env []                          = return []

instance CPS Decl where
    cps env (Class l n q cs b : ds)     = do b' <- cps env b
                                             ds' <- cps env ds
                                             return $ Class l n q cs b' : ds'

    cps env (Protocol l n q cs b : ds)  = do b' <- cps env b
                                             ds' <- cps env ds
                                             return $ Protocol l n q cs b' : ds'

    cps env (Extension l n q cs b : ds) = do b' <- cps env b
                                             ds' <- cps env ds
                                             return $ Extension l n q cs b' : ds'

    cps env (Actor l n q p k a b : ds)  = do b' <- cps env1 b
                                             ds' <- cps env ds
                                             return $ Actor l n q p k a b' : ds'
      where env1                        = env { ctxt = [Act] }

    cps env (Def l n q p k a b m : ds)
      | contDef env l n m               = do b' <- cps env1 b
                                             ds' <- cps env ds
                                             return $ Def l n q p (addContParam k) a b' m : ds'
      where env1                        = Meth contKW +: env { defd = bound p }

    cps env (d : ds)                    = do ds' <- cps env ds
                                             return $ d : ds'
    
    cps env []                          = return []


cps' env ss                             = nonlocals env <$> cps env ss

nonlocals env ss                        = nonlocals' env ss ss

nonlocals' env ss
  | null vs                             = id
  | otherwise                           = id -- (NonLocal l0 vs :)
  where vs                              = nub (bound ss `intersect` defd env)

--prfree x                                = trace ("Free " ++ prstr (dname x) ++ ": " ++ show (nub (free (dbody x)))) $ x

instance CPS Branch where
    cps env bs                          = mapM cp bs
      where cp (Branch e ss)            = Branch e <$> cps env ss

    
instance CPS Handler where
    cps env hs                          = mapM cp hs
      where cp (Handler ex ss)          = Handler ex <$> cps env ss


jump k                                  = sReturn (eCall (eVar k) [eNone]) : []


addContArg (Call l e pos kwd) c         = Call NoLoc e pos (KwdArg contKW c kwd)

addContParam kwd                        = KwdPar contKW Nothing Nothing kwd



finalH x f                              = eLambda [x] (eCall (eVar f) [eInt 0, raiseH])
  where raiseH                          = eLambda [] (eCall rtsRAISE [eVar x])

hbody env x hs                          = do hs' <- cps env hs
                                             return $ nonlocals' env hs' $
                                                      sTry [sRaise (eVar x)] hs' [] [] :
                                                      []



-- Only naive approximations below...

contCall env (Call l e ps ks)           = True          -- TODO: elaborate...
contCall env _                          = False

contDef env l n m
  | m == Async                          = True
  | isSync m                            = True
  | impure l n                          = True
  | otherwise                           = False
  where impure l n                      = True          -- TODO: elaborate...

inCont env                              = length (ctxt env) > 1


class MayBlock a where
    mayBlock                            :: Env -> a -> Bool

instance MayBlock a => MayBlock [a] where
    mayBlock env xs                     = any (mayBlock env) xs

instance MayBlock Branch where
    mayBlock env (Branch _ ss)          = mayBlock env ss

instance MayBlock Handler where
    mayBlock env (Handler _ ss)         = mayBlock env ss

instance MayBlock Stmt where
    mayBlock env (Expr _ e)             = contCall env e
    mayBlock env (Assign _ _ e)         = contCall env e
    mayBlock env (AugAssign _ _ _ e)    = contCall env e
    mayBlock env (If _ bs els)          = mayBlock env bs || mayBlock env els
    mayBlock env (While _ _ b els)      = mayBlock env b || mayBlock env els
    mayBlock env (For _ _ _ b els)      = mayBlock env b || mayBlock env els
    mayBlock env (Try _ b hs els fin)   = mayBlock env b || mayBlock env hs || mayBlock env fin
    mayBlock env (With _ _ b)           = mayBlock env b
    mayBlock env _                      = False

