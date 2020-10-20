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
import Acton.Env
import Acton.QuickType

convert                                 :: Env0 -> Module -> IO (Module, Env0)
convert env0 m                          = return (runCpsM $ cps env m, mapModules convEnv env0)
  where env                             = cpsEnv env0

type CpsM a                             = State CpsState a

type CpsState                           = ([Int], [Stmt])

runCpsM                                 :: CpsM a -> a
runCpsM m                               = evalState m ([1..], [])

contKW                                  = Internal CPSPass "cont" 0

newName                                 :: String -> CpsM Name
newName s                               = state (\(uniq:supply, stmts) -> (Internal CPSPass s uniq, (supply, stmts)))

prefix                                  :: [Stmt] -> CpsM ()
prefix ss                               = state (\(supply, stmts) -> ((), (supply, reverse ss ++ stmts)))

swapPrefixes                            :: [Stmt] -> CpsM [Stmt]
swapPrefixes ss                         = state (\(supply, stmts) -> (stmts, (supply, ss)))

withPrefixes                            :: CpsM a -> CpsM ([Stmt],a)
withPrefixes m                          = do ss0 <- swapPrefixes []
                                             r <- m
                                             ss1 <- swapPrefixes ss0
                                             return (reverse ss1, r)


data Frame                              = Meth   Name Type TFX              -- contParam, with type and effect
                                        | Seq    Name                       -- nextCont
                                        | Loop   Name                       -- loopEntry
                                        | Wrap   Name                       -- finalizer
                                        | Unwrap Name Name                  -- levelParam contParam
                                        | Pop
                                        deriving (Eq,Show)

instance Pretty Frame where
    pretty (Meth n t fx)                = text "Meth" <+> pretty t <+> pretty fx
    pretty (Seq n)                      = text "Seq" <+> pretty n
    pretty (Loop n)                     = text "Loop" <+> pretty n
    pretty (Wrap n)                     = text "Wrap" <+> pretty n
    pretty (Unwrap n n')                = text "Unwrap" <+> pretty n <+> pretty n'
    pretty Pop                          = text "Pop"

type CPSEnv                             = EnvF CPSX

data CPSX                               = CPSX { ctxtX :: [Frame] }

cpsEnv env0                             = setX env0 CPSX{ ctxtX = [] }

ctxt env                                = ctxtX $ envX env

infixr +:
frame +: env                            = modX env $ \x -> x{ ctxtX = frame : ctxtX x }

setCtxt ctx env                         = modX env $ \x -> x{ ctxtX = ctx }

methFX (Meth c t fx : ctx)              = fx
methFX (f : ctx)                        = methFX ctx
methFX []                               = fxPure

eCallCont fx t c arg                    = eCall (tApp (eQVar primRContc) [fx,t]) [eVar c, arg]

eCallCont2 c args                       = eCall (eVar c) args

pushH env h                             = sExpr (eCall (tApp (eQVar primPUSHc) [methFX $ ctxt env]) [h])

format (Int _ 0 _, cont)                = [sReturn cont]
format (lvl, cont)                      = [sExpr (eCall (eQVar primPOP) [lvl]), sReturn cont]

wrapC c f env                           = eCallCont2 c [level, eLambda' [] cont]
  where (level, cont)                   = f 0 env

unwrapL 0 lvl                           = eVar lvl
unwrapL n lvl                           = eCall (eDot (eQVar primWPlusInt) addKW) [eInt n, unwrapL 0 lvl]

seqcont n (Pop : ctx)                   = seqcont (n+1) ctx
seqcont n (Seq c : ctx)                 = (eInt n, eCallCont (methFX ctx) tNone c eNone)
seqcont n (Loop c : ctx)                = (eInt n, eCallCont (methFX ctx) tNone c eNone)
seqcont n (Meth c t fx : _)             = (eInt n, eCallCont fx tNone c eNone)
seqcont n (Wrap c : ctx)                = (eInt n, wrapC c seqcont ctx)
seqcont n (Unwrap lvl cnt : _)          = (unwrapL n lvl, eCallCont2 cnt [])

cntcont n (Pop : ctx)                   = cntcont (n+1) ctx
cntcont n (Seq c : ctx)                 = cntcont n ctx
cntcont n (Loop c : ctx)                = (eInt n, eCallCont (methFX ctx) tNone c eNone)
cntcont n (Wrap c : ctx)                = (eInt n, wrapC c cntcont ctx)
cntcont n (Unwrap lvl cnt : ctx)        = cntcont n ctx

brkcont n (Pop : ctx)                   = brkcont (n+1) ctx
brkcont n (Seq c : ctx)                 = brkcont n ctx
brkcont n (Loop c : ctx)                = seqcont n ctx
brkcont n (Wrap c : ctx)                = (eInt n, wrapC c brkcont ctx)
brkcont n (Unwrap lvl cnt : ctx)        = brkcont n ctx

retcont e n (Pop : ctx)                 = retcont e (n+1) ctx
retcont e n (Seq c : ctx)               = retcont e n ctx
retcont e n (Loop c : ctx)              = retcont e n ctx
retcont e n (Meth c t fx : _)           = (eInt n, eCallCont fx t c e)
retcont e n (Wrap c : ctx)              = (eInt n, wrapC c (retcont e) ctx)
retcont e n (Unwrap lvl cnt : ctx)      = retcont e n ctx

quicknext (Seq c : _)                   = Just (eVar c)
quicknext (Loop c : _)                  = Just (eVar c)
quicknext (Meth c t fx : _)             = Just (eVar c)
quicknext (Wrap c : ctx)                = Just (wrapC c seqcont ctx)
quicknext _                             = Nothing


preSuite env []                         = return []
preSuite env (s : ss)                   = do (prefixes,s') <- withPrefixes $ pre env s
                                             ss' <- preSuite (define (envOf s) env) ss
                                             return (prefixes ++ s' : ss')

cpsSuite env ss                         = do ss' <- preSuite env ss
                                             cps env ss'

class CPS a where
    cps                                 :: CPSEnv -> a -> CpsM a

instance CPS Module where
    cps env (Module m imps ss)          = Module m imps <$> cps env ss

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
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env ss
                                             return $ kDef env k (pospar [(x,t)]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []
      where t                           = typeOf env e

    cps env (Assign _ [PVar _ x _] e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             ss' <- cps env1 ss
                                             return $ kDef env k (pospar [(x,t)]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []
      where t                           = typeOf env e
            env1                        = define [(x,NVar t)] env

    cps env (MutAssign _ tg e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env (sMutAssign tg (eVar x) : ss)
                                             return $ kDef env k (pospar [(x,t)]) ss' :
                                                      sReturn (addContArg e (eVar k)) : []
      where t                           = typeOf env e

    cps env (Decl l ds : ss)            = do ds' <- mapM (cps env1) ds
                                             ss' <- cps env1 ss
                                             return $ sDecl ds' : ss'
      where env1                        = define (envOf ds) env
    
    cps env (s@Signature{} : ss)        = do ss' <- cps env1 ss
                                             return $ s' : ss'
      where s'                          = s{ typ = conv (typ s) }
            env1                        = define (envOf s) env

    cps env (s : ss)
      | not (needCont env s)            = do ss' <- cps env1 ss
                                             return $ s : ss'
      where env1                        = define (envOf s) env
    
    cps env [If _ bs els]               = do bs' <- mapM (cps env) bs
                                             els' <- cpsSuite env els
                                             return $ sIf bs' els' : []
    
    cps env [While _ e b els]           = do k    <- newName "loop"
                                             x    <- newName "res"
                                             b'   <- cpsSuite (Loop k +: env) b
                                             els' <- cpsSuite env els
                                             let body = sIf1 e b' els' : []
                                             return $ kDef env k (pospar [(x,tNone)]) body : 
                                                      jump k

    cps env [Try _ b [] [] fin]         = do fcnt <- newName "finalizer"
                                             x    <- newName "x"
                                             lvl  <- newName "level"
                                             cnt  <- newName "cont"
                                             fin' <- cpsSuite (Unwrap lvl cnt +: env) fin
                                             b'   <- cpsSuite (Pop +: Wrap fcnt +: env) b
                                             return $ kDef env fcnt (pospar [(lvl,tInt), (cnt,tCont0)]) fin' :
                                                      pushH env (finalH x fcnt) :
                                                      b'

    cps env [Try _ b hs [] []]          = do hcnt <- newName "handler"
                                             x    <- newName "x"
                                             body <- hbody env x hs
                                             b'   <- cpsSuite (Pop +: env) b
                                             return $ kDef env hcnt (pospar [(x,tException)]) body :
                                                      pushH env (eVar hcnt) :
                                                      b'

    cps env [Try _ b hs els []]         = do hcnt <- newName "handler"
                                             ecnt <- newName "else"
                                             x    <- newName "x"
                                             body <- hbody env x hs
                                             els' <- cpsSuite env els
                                             b' <- cpsSuite (Pop +: Seq ecnt +: env) b
                                             return $ kDef env hcnt (pospar [(x,tException)]) body :
                                                      kDef env ecnt (pospar [(x,tNone)]) els' :
                                                      pushH env (eVar hcnt) :
                                                      b'

    cps env [Try _ b hs [] fin]         = do fcnt <- newName "finalizer"
                                             hcnt <- newName "handler"
                                             x    <- newName "x"
                                             lvl  <- newName "level"
                                             cnt  <- newName "cont"
                                             fin' <- cpsSuite (Unwrap lvl cnt +: env) fin
                                             body <- hbody (Pop +: Wrap fcnt +: env) x hs
                                             b' <- cpsSuite (Pop +: Wrap fcnt +: env) b
                                             return $ kDef env fcnt (pospar [(lvl,tInt), (cnt,tCont0)]) fin' :
                                                      kDef env hcnt (pospar [(x,tException)]) (pushH env (finalH x fcnt) : body) :
                                                      pushH env (eVar hcnt) :
                                                      b'
                                          
    cps env [Try _ b hs els fin]        = do fcnt <- newName "finalizer"
                                             ecnt <- newName "else"
                                             hcnt <- newName "handler"
                                             x    <- newName "x"
                                             lvl  <- newName "level"
                                             cnt  <- newName "cont"
                                             fin' <- cpsSuite (Unwrap lvl cnt +: env) fin
                                             body <- hbody (Pop +: Wrap fcnt +: env) x hs
                                             els' <- cpsSuite (Pop +: Wrap fcnt +: env) els
                                             b' <- cpsSuite (Pop +: Seq ecnt +: Wrap fcnt +: env) b
                                             return $ kDef env fcnt (pospar [(lvl,tInt), (cnt,tCont0)]) fin' :
                                                      kDef env hcnt (pospar [(x,tException)]) (pushH env (finalH x fcnt) : body) :
                                                      kDef env ecnt (pospar [(x,tNone)]) (pushH env (finalH x fcnt) : els') :
                                                      pushH env (eVar hcnt) :
                                                      b'

    cps env (s : ss)                    = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env ss
                                             ss1 <- cps (Seq k +: env) [s]
                                             return $ kDef env k (pospar [(x,tNone)]) ss' :
                                                      ss1

    cps env []                          = return []

instance CPS Decl where
    cps env (Class l n q cs b)          = Class l n q cs <$> cpsSuite env1 b
      where env1                        = defineSelf (NoQ n) q $ defineTVars q $ setCtxt [] env

    cps env (Def l n q p KwdNIL (Just t) b d fx)
      | contDef env l n fx              = Def l n q (addContParam p t) KwdNIL (Just t) <$> cpsSuite env1 b <*> return d <*> return fx
      where env1                        = define (envOf p) $ defineTVars q $ Meth contKW t fx +: env

    cps env d@Def{}                     = return d

    cps env d                           = error ("cps unexpected: " ++ prstr d)
    

instance CPS Branch where
    cps env (Branch e ss)               = Branch e <$> cpsSuite env ss

    
instance CPS Handler where
    cps env (Handler ex ss)             = Handler ex <$> cpsSuite env1 ss
      where env1                        = define (envOf ex) env


jump k                                  = sReturn (eCall (eVar k) [eNone]) : []


addContArg (Call l e pos KwdNil) c      = Call NoLoc e (add pos c) KwdNil
  where add PosNil c                    = PosArg c PosNil
        add (PosArg e p) c              = PosArg e (add p c)

addContParam PosNIL t                   = PosPar contKW (Just t) Nothing PosNIL
addContParam (PosPar n a e p) t         = PosPar n a e (addContParam p t)


tCont0                                  = tFun fxPure posNil kwdNil tR


finalH x f                              = eLambda' [x] (eCall (eVar f) [eInt 0, raiseH])
  where raiseH                          = eLambda' [] (eCall (eQVar primRAISE) [eVar x])

hbody env x hs                          = do hs' <- mapM (cps env) hs
                                             return $ sTry [sRaise (eVar x)] hs' [] [] :
                                                      []

kDef env k p b                          = sDef k p tR b (methFX $ ctxt env)

contCall env (Call l (Var _ n) p KwdNil)
  | n == primAWAITf                     = True
  | isPrim n                            = False
  | n `elem` ns0                        = False
  where ns0                             = [qnStr,qnInt,qnLen,qnPrint]
        isPrim (QName m _)              = m == mPrim
        isPrim _                        = False
contCall env (Call l (Dot _ _ n) p KwdNil)
  | n `elem` ns0                        = False
  where ns0                             = attrKWs
contCall env (Call l e p KwdNil)        = True                      -- TODO: utilize type...
contCall env _                          = False

contDef env l n fx
  | impure l n                          = True
  | otherwise                           = False
  where impure l n                      = True                      -- TODO: utilize fx type...

inCont env                              = length (ctxt env) > 0


class NeedCont a where
    needCont                            :: CPSEnv -> a -> Bool

instance NeedCont a => NeedCont [a] where
    needCont env xs                     = any (needCont env) xs

instance NeedCont Branch where
    needCont env (Branch _ ss)          = needCont env ss

instance NeedCont Handler where
    needCont env (Handler _ ss)         = needCont env ss

instance NeedCont Stmt where
    needCont env (Expr _ e)             = contCall env e
    needCont env (Assign _ _ e)         = contCall env e
    needCont env (MutAssign _ _ e)      = contCall env e
    needCont env (If _ bs els)          = needCont env bs || needCont env els
    needCont env (While _ _ b els)      = needCont env b || needCont env els
    needCont env (Try _ b hs els fin)   = True
    needCont env _                      = False



------------------------------------------------
------------------------------------------------
------------------------------------------------


class PreCPS a where
    pre                                 :: CPSEnv -> a -> CpsM a
    preTop                              :: CPSEnv -> a -> CpsM a
    preTop env                          = pre env

instance (PreCPS a, EnvOf a) => PreCPS [a] where
    pre env []                          = return []
    pre env (a:as)                      = (:) <$> pre env a <*> pre env1 as
      where env1                        = define (envOf a) env

instance PreCPS a => PreCPS (Maybe a) where
    pre env Nothing                     = return Nothing
    pre env (Just a)                    = fmap Just (pre env a)
    
    preTop env Nothing                  = return Nothing
    preTop env (Just a)                 = fmap Just (preTop env a)

instance PreCPS Stmt where
    pre env (Expr l e)                  = Expr l <$> preTop env e
    pre env (Assign l ps e)             = Assign l ps <$> preTop env e
    pre env (MutAssign l t e)           = MutAssign l <$> pre env t <*> preTop env e
    pre env (Return l e)                = Return l <$> preTop env e
    pre env (If l bs els)               = If l <$> pre env bs <*> return els
    pre env (While l e b els)           = While l <$> pre env e <*> return b <*> return els
    pre env (Try l b hs els fin)        = return $ Try l b hs els fin
    pre env s                           = return s

instance PreCPS Branch where
    pre env (Branch e ss)               = Branch  <$> pre env e <*> return ss

instance PreCPS Exception where
    pre env (Exception e1 e2)           = Exception <$> pre env e1 <*> pre env e2

instance PreCPS PosArg where
    pre env (PosArg e p)                = PosArg <$> pre env e <*> pre env p
    pre env (PosStar e)                 = PosStar <$> pre env e
    pre env PosNil                      = return PosNil

instance PreCPS KwdArg where
    pre env (KwdArg n e p)              = KwdArg n <$> pre env e <*>pre env p
    pre env (KwdStar e)                 = KwdStar <$> pre env e
    pre env KwdNil                      = return KwdNil

instance PreCPS Expr where
    pre env e0@(Call l e ps KwdNil)
      | contCall env e0                 = do ps1 <- pre env ps                                          -- TODO: utilize type of e
                                             v <- newName "pre"
                                             prefix [sAssign (pVar' v) (Call l e ps1 KwdNil)]
                                             return (eVar v)
    pre env (Call l e ps KwdNil)        = Call l <$> pre env e <*> pre env ps <*> pure KwdNil
    pre env (TApp l e ts)               = TApp l <$> pre env e <*> pure ts
    pre env (Cond l e1 e e2)            = Cond l <$> pre env e1 <*> pre env e <*> pre env e2
    pre env (IsInstance l e c)          = IsInstance l <$> pre env e <*> return c
    pre env (BinOp l e1 Or e2)          = BinOp l <$> pre env e1 <*> pure Or <*> pre env e2
    pre env (BinOp l e1 And e2)         = BinOp l <$> pre env e1 <*> pure And <*> pre env e2
    pre env (UnOp l Not e)              = UnOp l Not <$> pre env e
    pre env (Dot l e n)                 = Dot l <$> pre env e <*> return n
    pre env (Rest l e n)                = Rest l <$> pre env e <*> return n
    pre env (DotI l e i)                = DotI l <$> pre env e <*> return i
    pre env (RestI l e i)               = RestI l <$> pre env e <*> return i
    pre env (Lambda l ps KwdNIL e fx)   = do (prefixes,e1) <- withPrefixes $ pre env1 e
                                             case prefixes of                                              -- TODO: utilize type of e (+below)
                                                 [] ->
                                                    return $ Lambda l ps KwdNIL e1 fx
                                                 _  -> do 
                                                    f <- newName "lambda"
                                                    prefix [sDecl [Def l f [] ps KwdNIL Nothing (prefixes ++ [sReturn e1]) NoDec fx]]
                                                    return (Var l0 (NoQ f))
      where env1                        = define (envOf ps) env
    pre env (Yield l e)                 = Yield l <$> pre env e
    pre env (YieldFrom l e)             = YieldFrom l <$> pre env e
    pre env (Tuple l es ks)             = Tuple l <$> pre env es <*> pre env ks
    pre env (List l es)                 = List l <$> pre env es
    pre env (Paren l e)                 = Paren l <$> pre env e
    pre env e                           = return e

    preTop env e0@(Call l e ps ks)
      | contCall env e0                 = Call l e <$> pre env ps <*> pre env ks
    preTop env e                        = pre env e


instance PreCPS Elem where
    pre env (Elem e)                    = Elem <$> pre env e
    pre env (Star e)                    = Star <$> pre env e


-- Convert types ----------------------------------------------------------------------------------------

convEnv te                              = conv te

class Conv a where
    conv                                :: a -> a

instance (Conv a) => Conv [a] where
    conv                                = map conv

instance (Conv a) => Conv (Name, a) where
    conv (n, x)                         = (n, conv x)

instance Conv NameInfo where
    conv (NClass q ps te)               = NClass q (conv ps) (conv te)
    conv (NSig sc dec)                  = NSig (conv sc) dec
    conv (NDef sc dec)                  = NDef (conv sc) dec
    conv (NVar t)                       = NVar (conv t)
    conv (NSVar t)                      = NSVar (conv t)
    conv ni                             = ni

instance Conv WTCon where
    conv (w,c)                          = (w, conv c)

instance Conv TSchema where
    conv (TSchema l q t)                = TSchema l q (conv t)

instance Conv Type where
    conv (TFun l fx p TNil{} t)
       | contFX fx                      = TFun l fx' (addCont (conv p) (conv t)) kwdNil tR
       | otherwise                      = TFun l fx' (conv p) kwdNil (conv t)
       where contFX (TFX _(FXAct _))    = True                                              -- TODO: refine this test!
             contFX fx                  = False
             fx'                        = conv fx
             addCont (TRow l k n t p) c = TRow l k n t (addCont p c)
             addCont p c                = posRow (tFun fx' (posRow c posNil) kwdNil tR) p
    conv (TCon l c)                     = TCon l (conv c)
    conv (TTuple l p k)                 = TTuple l (conv p) (conv k)
    conv (TOpt l t)                     = TOpt l (conv t)
    conv (TRow l k n t r)               = TRow l k n (conv t) (conv r)
    conv (TFX l x)                      = TFX l (conv x)
    conv t                              = t

instance Conv FX where
    conv (FXAct t)                      = FXMut (conv t)
    conv (FXMut t)                      = FXMut (conv t)
    conv FXAction                       = FXAction
    conv FXPure                         = FXPure

instance Conv TCon where
    conv (TC c ts)                      = TC c (conv ts)
