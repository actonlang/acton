-- Copyright (C) 2019-2021 Data Ductus AB
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

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
convert env0 m                          = return (runCpsM (convMod m), mapModules1 (conv env) env0)
  where env                             = cpsEnv env0
        convMod (Module m imps ss)      = do ss' <- preSuite env ss
                                             --traceM ("######## preCPS:\n" ++ render (vcat $ map pretty ss') ++ "\n########")
                                             Module m imps <$> cps env ss'

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


data Frame                              = Meth   Name Type                  -- contParam with type
                                        | Seq    Name [Name]                -- nextCont with extra args
                                        | Loop   Name [Name]                -- loopEntry with extra args
                                        deriving (Eq,Show)

instance Pretty Frame where
    pretty (Meth n t)                   = text "Meth" <+> pretty n
    pretty (Seq n ns)                   = text "Seq" <+> pretty n
    pretty (Loop n ns)                  = text "Loop" <+> pretty n

type CPSEnv                             = EnvF CPSX

data CPSX                               = CPSX { ctxtX :: [Frame], whereX :: Where, volatileX :: [Name] }

data Where                              = OnTop | InClass | InDef | InInnerLoop deriving (Eq,Show)

cpsEnv env0                             = setX env0 CPSX{ ctxtX = [], whereX = OnTop, volatileX = [] }

ctxt env                                = ctxtX $ envX env

infixr +:
frame +: env                            = modX env $ \x -> x{ ctxtX = frame : ctxtX x }

setInClass env                          = modX env $ \x -> x{ ctxtX = [], whereX = InClass }
setInDef env                            = modX env $ \x -> x{ ctxtX = [], whereX = InDef }
setInInnerLoop env                      = modX env $ \x -> x{ whereX = InInnerLoop }

onTop env                               = whereX (envX env) == OnTop
inClass env                             = whereX (envX env) == InClass
inDef env                               = whereX (envX env) == InDef
inInnerLoop env                         = whereX (envX env) == InInnerLoop

setVolatiles ns env                     = modX env $ \x -> x{ volatileX = ns }

volatiles env                           = volatileX (envX env)

eCallCont e (c,t)                       = eCall (tApp (eQVar primRContc) [t]) [c, e]

eCallCont0 (c, ns)                      = eCallCont eNone (c',tNone)
  where c'                              = kRef c ns g_skip tNone

cntcont (Seq _ _ : ctx)                 = cntcont ctx       -- 'continue'       followed by some cmd:   ignore it
cntcont (Loop c ns : ctx)               = (c, ns)           --                  inside a loop:          jump to its top

brkcont (Seq _ _ : ctx)                 = brkcont ctx       -- 'break'          followed by some cmd:   ignore it
brkcont (Loop _ _ : ctx)                = seqcont ctx       --                  inside a loop:          jump to what follows

seqcont (Seq c ns : ctx)                = (c, ns)           -- end of sequence  followed by some cmd:   jump to it
seqcont (Loop c ns : ctx)               = (c, ns)           --                  inside a loop:          jump to its top
seqcont (Meth c _ : _)                  = (c, [])           --                  in a method:            jump to its continuation

retcont (Seq _ _ : ctx)                 = retcont ctx       -- 'return'         followed by some cmd:   ignore it
retcont (Loop _ _ : ctx)                = retcont ctx       --                  inside a loop:          ignore it
retcont (Meth c t : _)                  = (eVar c, t)       --                  in a method:            jump to its continuation


class CPS a where
    cps                                 :: CPSEnv -> a -> CpsM a

instance CPS [Stmt] where
    cps env []
      | inCont env                      = return [sReturn $ eCallCont0 $ seqcont $ ctxt env]
    cps env (Continue _ : _)
      | inCont env                      = return [sReturn $ eCallCont0 $ cntcont $ ctxt env]
    cps env (Break _ : _)
      | inCont env                      = return [sReturn $ eCallCont0 $ brkcont $ ctxt env]
    cps env (Return _ Nothing : _)
      | inCont env                      = return [sReturn $ eCallCont eNone $ retcont $ ctxt env]
    cps env (Return _ (Just e) : _)
      | contCall env e                  = return [sReturn $ addContArg env (conv env e) $ fst $ retcont $ ctxt env]
      | inCont env                      = return [sReturn $ eCallCont (conv env e) $ retcont $ ctxt env]

    cps env (Assign _ [PVar _ n (Just _)] e :
             Return _ (Just e') : _)
      | contCall env e, e' == eVar n    = return [sReturn $ addContArg env (conv env e) $ fst $ retcont $ ctxt env]

    cps env [Expr _ e]
      | contCall env e                  = return [sReturn $ addContArg env (conv env e) $ cont $ seqcont $ ctxt env]
      where t                           = typeOf env e
            cont (c,ns)                 = if t == tNone then c' else eCall (tApp (eQVar primSKIPRESc) [t]) [c']
              where c'                  = kRef c ns g_skip t

    cps env (Expr _ e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env ss
                                             --traceM ("## kDef Expr " ++ prstr k ++ ", updates: " ++ prstrs nts)
                                             return $ kDef env k nts x t ss' :
                                                      sReturn (addContArg env (conv env e) (kRef k (dom nts) x t)) : []
      where t                           = typeOf env e
            nts                         = extraBinds env ss

    cps env ss0@(Assign _ [p] e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps (define [(x,NVar t)] env) (sAssign p (eVar x) : ss)
                                             --traceM ("## kDef Assign " ++ prstr k ++ ", updates: " ++ prstrs nts)
                                             return $ kDef env k nts x t ss' :
                                                      sReturn (addContArg env (conv env e) (kRef k (dom nts) x t)) : []
      where t                           = typeOf env e
            nts                         = extraBinds env ss0

    cps env (MutAssign _ tg e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env (sMutAssign tg (eVar x) : ss)
                                             --traceM ("## kDef MutAssign " ++ prstr k ++ ", updates: " ++ prstrs nts)
                                             return $ kDef env k nts x t ss' :
                                                      sReturn (addContArg env (conv env e) (kRef k (dom nts) x t)) : []
      where t                           = typeOf env e
            nts                         = extraBinds env ss

    cps env (Decl l ds : ss)            = do ds' <- mapM (cps env1) ds
                                             ss' <- cps env1 ss
                                             return $ sDecl ds' : ss'
      where env1                        = define (envOf ds) env
    
    cps env (s : ss)
      | not (needCont env s)            = do ss' <- cps env1 ss
                                             --traceM ("### SIMPLE: " ++ prstr s)
                                             return $ conv env s : ss'
      where env1                        = define (envOf s) env
    
    cps env s@[If _ [Branch e ss1] ss2]
      | isPUSH e                        = do k <- newName "try"
                                             x <- newName "res"
                                             ss1 <- cps env (map convPOPDROP ss1)
                                             ss2 <- cps env (map convPOPDROP ss2)
                                             let body = sIf1 (eVar x) ss1 ss2 : []
                                             --traceM ("## kDef PUSH " ++ prstr k ++ ", updates: " ++ prstrs nts)
                                             return $ kDef env k nts x tBool body :
                                                      sReturn (convPUSH e $ kRef k (dom nts) x tBool) : []
      where nts                         = extraBinds env s

    cps env [If _ bs els]               = do bs' <- mapM (cps env) bs
                                             els' <- cps env els
                                             return $ sIf bs' els' : []

    cps env (If l bs els : s@(Expr _ e) : _)
      | isRAISE e                       = cps env [If l [ Branch e (ss++[s]) | Branch e ss <- bs ] (els++[s])]

    cps env s@[While _ e b els]         = do k    <- newName "loop"
                                             x    <- newName "res"
                                             b'   <- cps (Loop k (dom nts) +: env) b
                                             els' <- cps env els
                                             let body = sIf1 (conv env e) b' els' : []
                                             --traceM ("## kDef While " ++ prstr k ++ ", updates: " ++ prstrs nts)
                                             return $ kDef env k nts x tNone body :
                                                      kJump k nts
      where nts                         = extraBinds env s

    cps env (s : ss)                    = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env1 ss
                                             s' <- cps (Seq k (dom nts) +: env) [s]
                                             --traceM ("## kDef Seq " ++ prstr k ++ ", live/updates: " ++ prstrs nts)
                                             return $ kDef env k nts x tNone ss' :
                                                      s'
      where env1                        = define (envOf s) env
            nts0                        = nvarsOf (envOf s) `restrict` free ss
            nts                         = nts0 ++ extraBinds env1 ss

    cps env []                          = return []

extraBinds env ss                       = [ (x, t) | x <- nub $ updatesOf ss, x `notElem` volatiles env, Just t <- [lookupVar x env] ]

volatileVars env stmts                  = nub $ vols env stmts
  where vols env []                     = []
        vols env (s:ss)                 = vol env s ++ vols (define (envOf s) env) ss
        vol env s@(If _ [Branch e ss] els)
          | isPUSH e, needCont env s    = updatesOf ss ++ vols env ss ++ vols env els
        vol env (If _ bs els)           = concat [ vols env ss | Branch _ ss <- bs ] ++ vols env els
        vol env (While _ _ ss els)      = vols env ss ++ vols env els
        vol env _                       = []

instance CPS Decl where
    cps env (Class l n q cs b)          = do b' <- cps env1 b
                                             return $ Class l n (conv env q) (conv env cs) b'
      where env1                        = defineSelf (NoQ n) q $ defineTVars q $ setInClass env

    cps env (Def l n q p KwdNIL (Just t) b dec fx)
      | contFX fx                       = do --traceM ("#### Converting " ++ prstr n)
                                             b' <- cps env2 b
                                             return $ Def l n q' (addContPar env dec p' fx t') KwdNIL (Just tR) (volinits ++ b') dec fx
      | otherwise                       = do --traceM ("#### Preserving " ++ prstr n)
                                             b' <- cps env1 b
                                             return $ Def l n q' p' KwdNIL (Just t') b' dec fx
      where env2                        = setVolatiles volvs $ Meth contKW t' +: env1
            env1                        = define (envOf p) $ defineTVars q $ setInDef env
            volvs                       = volatileVars env2 b
            volinits                    = [ sAssign (pVar v (tBox t)) (eCall (tApp (eQVar primBox) [t]) [eVar v])
                                          | (v,NVar t) <- envOf p, v `elem` volvs ]
            q'                          = conv env q
            p'                          = conv env p
            t'                          = conv env t

    cps env d                           = error ("cps unexpected: " ++ prstr d)
    

instance CPS Branch where
    cps env (Branch e ss)               = Branch (conv env e) <$> cps env ss


kJump k nts                             = sReturn (eCall (eVar k) $ eNone : map eVar (dom nts)) : []


addContArg env (Call l e p KwdNil) c    = Call NoLoc e (PosArg c p) KwdNil

addContPar env dec (PosPar n a Nothing p) fx t
  | inClass env && dec /= Static        = PosPar n a Nothing (addContPar0 p fx t)
addContPar env dec p fx t               = addContPar0 p fx t

addContPar0 p fx t                      = PosPar contKW (Just $ tCont1 fx t) Nothing p

tCont0                                  = tFun fxProc (posRow tNone posNil) kwdNil tR

tCont1 fx t                             = tFun fx (posRow t posNil) kwdNil tR

kDef env k nts x t b                    = sDef k (conv env $ pospar $ (x,t):nts) tR b fxProc

kRef k [] x t                           = eVar k
kRef k ns x t                           = eLambda' [(x,t)] $ eCall (eVar k) (map eVar (x:ns))

fxCall env test (Call _ Async{} p k)    = False
fxCall env test (Call _ e p k)          = test fx
  where TFun _ fx _ _ _                 = typeOf env e
fxCall env test e                       = False

contCall env e                          = fxCall env contFX e

mutCall env e                           = fxCall env mutFX e

contFX (TFX _ FXProc)                   = True
contFX _                                = False

mutFX (TFX _ FXMut)                     = True
mutFX (TFX _ FXProc)                    = True
mutFX _                                 = False

inCont env                              = length (ctxt env) > 0


convPOPDROP s@(If _ [Branch e _] _)
  | isPUSH e                            = s
convPOPDROP (If l bs els)               = If l [ Branch e (map convPOPDROP ss) | Branch e ss <- bs ] (map convPOPDROP els)
convPOPDROP (While l e ss els)          = While l e (map convPOPDROP ss) (map convPOPDROP els)
convPOPDROP (Expr l (Call l1 e ps ks))
  | Var l2 x <- e, x == primDROP        = Expr l (Call l1 (Var l2 primDROP_C) ps ks)
convPOPDROP (Assign l p (Call l1 e ps ks))
  | Var l2 x <- e, x == primPOP         = Assign l p (Call l1 (Var l2 primPOP_C) ps ks)
convPOPDROP s                           = s

convPUSH (Call _ (Var _ x) _ _) arg
  | x == primPUSH                       = eCall (eQVar primPUSH_Cc) [arg]
  | x == primPUSHF                      = eCall (eQVar primPUSHF_Cc) [arg]

class NeedCont a where
    needCont                            :: CPSEnv -> a -> Bool

instance (NeedCont a, EnvOf a) => NeedCont [a] where
    needCont env []                     = False
    needCont env (s : ss)               = needCont env s || needCont (define (envOf s) env) ss

instance NeedCont Branch where
    needCont env (Branch e ss)          = needCont env ss

instance NeedCont Stmt where
    needCont env (Return _ _)           = inCont env
    needCont env (Continue _)
      | inInnerLoop env                 = False
      | otherwise                       = inCont env
    needCont env (Break _)
      | inInnerLoop env                 = False
      | otherwise                       = inCont env
    needCont env (Expr _ e)             = contCall env e
    needCont env (Assign _ _ e)         = contCall env e
    needCont env (MutAssign _ _ e)      = contCall env e
    needCont env (If _ bs els)          = needCont env bs || needCont env els
    needCont env (While _ _ ss els)     = needCont (setInInnerLoop env) ss || needCont env els
    needCont env (Decl _ ds)            = needCont (define (envOf ds) env) ds
    needCont env _                      = False

instance NeedCont Decl where
    needCont env Class{}                = True
    needCont env d@Def{}                = contFX (dfx d)

------------------------------------------------
------------------------------------------------
------------------------------------------------


class PreCPS a where
    pre                                 :: CPSEnv -> a -> CpsM a
    preTop                              :: CPSEnv -> a -> CpsM a
    preTop env                          = pre env

instance PreCPS Module where
    pre env (Module m imps ss)          = Module m imps <$> preSuite env ss

preSuite env []                         = return []
preSuite env (s : ss)                   = do (prefixes,s') <- withPrefixes $ pre env s
                                             ss' <- preSuite (define (envOf s) env) ss
                                             return (prefixes ++ s' : ss')

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
    pre env (If l bs els)               = If l <$> pre env bs <*> preSuite env els
    pre env (While l e b els)           = While l <$> pre env e <*> preSuite env b <*> preSuite env els
    pre env (Decl l ds)                 = Decl l <$> pre env1 ds
      where env1                        = define (envOf ds) env
    pre env s                           = return s

instance PreCPS Decl where
    pre env (Class l n q cs b)          = Class l n q cs <$> pre env1 b
      where env1                        = defineSelf (NoQ n) q $ defineTVars q env
    pre env (Def l n q p _k a b d fx)   = Def l n q p _k a <$> preSuite env1 b <*> pure d <*> pure fx
      where env1                        = define (envOf p) $ defineTVars q env

instance PreCPS Branch where
    pre env (Branch e ss)               = Branch  <$> pre env e <*> preSuite env ss

instance PreCPS Handler where
    pre env (Handler ex ss)             = Handler ex <$> preSuite env1 ss
      where env1                        = define (envOf ex) env

instance PreCPS PosArg where
    pre env (PosArg e p)                = PosArg <$> pre env e <*> pre env p
    pre env PosNil                      = return PosNil

instance PreCPS Expr where
    pre env e0@(Call l e ps KwdNil)
      | mutCall env e0                  = do e1 <- pre env e
                                             ps1 <- pre env ps
                                             v <- newName "pre"
                                             prefix [sAssign (pVar v t) (Call l e1 ps1 KwdNil)]
                                             return (eVar v)
      | otherwise                       = Call l <$> pre env e <*> pre env ps <*> pure KwdNil
      where t                           = typeOf env e0
    pre env (Async l e)                 = Async l <$> pre env e
    pre env (TApp l e ts)               = TApp l <$> pre env e <*> pure ts
    pre env (Cond l e1 e e2)            = Cond l <$> pre env e1 <*> pre env e <*> pre env e2
    pre env (IsInstance l e c)          = IsInstance l <$> pre env e <*> return c
    pre env (BinOp l e1 Or e2)          = BinOp l <$> pre env e1 <*> pure Or <*> pre env e2
    pre env (BinOp l e1 And e2)         = BinOp l <$> pre env e1 <*> pure And <*> pre env e2
    pre env (UnOp l Not e)              = UnOp l Not <$> pre env e
    pre env (Dot l e n)                 = Dot l <$> pre env e <*> return n
    pre env (DotI l e i)                = DotI l <$> pre env e <*> return i
    pre env (RestI l e i)               = RestI l <$> pre env e <*> return i
    pre env (Lambda l p KwdNIL e fx)    = do (prefixes,e') <- withPrefixes $ preTop env1 e
                                             case prefixes of
                                                [] | contFX fx ->
                                                        let p' = conv env p; t' = conv env t
                                                            e1 = if contCall env e then addContArg env1 e' econt else eCallCont e' (econt,t')
                                                        in return $ Lambda l (addContPar0 p' fx t') KwdNIL e1 fx
                                                   | otherwise ->
                                                        return $ Lambda l p KwdNIL e' fx
                                                _ -> do
                                                    f <- newName "lambda"
                                                    prefix [sDecl [Def l f [] p KwdNIL (Just t) (prefixes ++ [sReturn e']) NoDec fx]]
                                                    return (Var l0 (NoQ f))
      where env1                        = define (envOf p) env
            t                           = typeOf env1 e
            econt                       = eVar contKW
    pre env (Yield l e)                 = Yield l <$> pre env e
    pre env (YieldFrom l e)             = YieldFrom l <$> pre env e
    pre env (Tuple l es KwdNil)         = Tuple l <$> pre env es <*> pure KwdNil
    pre env (List l es)                 = List l <$> pre env es
    pre env (Dict l as)                 = Dict l <$> pre env as
    pre env (Set l es)                  = Set l <$> pre env es
    pre env e                           = return e

    preTop env e0@(Call l e ps KwdNil)
      | mutCall env e0                  = Call l <$> pre env e <*> pre env ps <*> pure KwdNil
    preTop env e                        = pre env e


instance PreCPS Elem where
    pre env (Elem e)                    = Elem <$> pre env e

instance PreCPS Assoc where
    pre env (Assoc k v)                 = Assoc <$> pre env k <*> pre env v


-- Convert types ----------------------------------------------------------------------------------------

class Conv a where
    conv                                :: CPSEnv -> a -> a

instance (Conv a) => Conv [a] where
    conv env                            = map $ conv env

instance (Conv a) => Conv (Maybe a) where
    conv env                            = fmap $ conv env

instance (Conv a) => Conv (Name, a) where
    conv env (n, x)                     = (n, conv env x)

instance Conv NameInfo where
    conv env (NClass q ps te)           = NClass q (conv env ps) (conv env te)
    conv env (NSig sc dec)              = NSig (conv env sc) dec
    conv env (NDef sc dec)              = NDef (conv env sc) dec
    conv env (NVar t)                   = NVar (conv env t)
    conv env (NSVar t)                  = NSVar (conv env t)
    conv env ni                         = ni

instance Conv WTCon where
    conv env (w,c)                      = (w, conv env c)

instance Conv TSchema where
    conv env (TSchema l q t)            = TSchema l (conv env q) (conv env t)

instance Conv QBind where
    conv env (Quant v cs)               = Quant v (conv env cs)

instance Conv Type where
    conv env t0@(TFun l fx p TNil{} t)
       | contFX fx && t /= tR           = TFun l fx (addCont (conv env p) (conv env t)) kwdNil tR
       | otherwise                      = TFun l fx (conv env p) kwdNil (conv env t)
       where addCont p c                = posRow (tFun fx (posRow c posNil) kwdNil tR) p
    conv env (TCon l c)                 = TCon l (conv env c)
    conv env (TTuple l p k)             = TTuple l (conv env p) (conv env k)
    conv env (TOpt l t)                 = TOpt l (conv env t)
    conv env (TRow l k n t r)           = TRow l k n (conv env t) (conv env r)
    conv env t                          = t

instance Conv TCon where
    conv env (TC c ts)                  = TC c (conv env ts)

instance Conv PosPar where
    conv env (PosPar n t Nothing p)     = PosPar n (conv env t) Nothing (conv env p)
    conv env PosNIL                     = PosNIL

instance Conv Stmt where
    conv env (Expr l e)                 = Expr l (conv env e)
    conv env (Assign l [PVar _ n Nothing] e)
      | n `elem` volatiles env          = MutAssign l (eDot (eVar n) valKW) (conv env e)
    conv env (Assign l [PVar _ n (Just t)] e)
      | n `elem` volatiles env          = Assign l [pVar n (tBox t)] (eCall (tApp (eQVar primBox) [t]) [conv env e])
    conv env (Assign l ps e)            = Assign l (conv env ps) (conv env e)
    conv env (MutAssign l tg e)         = MutAssign l (conv env tg) (conv env e)
    conv env (Return l e)               = Return l (conv env e)
    conv env (If l bs els)              = If l (conv env bs) (conv env els)
    conv env (While l e b els)          = While l (conv env e) (conv env b) (conv env els)
    conv env (Signature l ns sc dec)    = Signature l ns (conv env sc) dec
    conv env s                          = s

instance Conv Branch where
    conv env (Branch e ss)              = Branch (conv env e) (conv env ss)

instance Conv Pattern where
    conv env (PVar l n t)               = PVar l n (conv env t)
    conv env p                          = p

instance Conv Expr where
    conv env (Var l (NoQ n))
      | n `elem` volatiles env          = Dot l (Var l (NoQ n)) valKW
    conv env (Var l n)
      | n == primASYNCf                 = Var l primASYNCc
      | n == primAFTERf                 = Var l primAFTERc
      | n == primAWAITf                 = Var l primAWAITc
      | otherwise                       = Var l n
    conv env (Call l e ps KwdNil)       = Call l (conv env e) (conv env ps) KwdNil
    conv env (Async l e)                = Async l (conv env e)
    conv env (TApp l e ts)              = TApp l (conv env e) (conv env ts)
    conv env (Cond l e1 e e2)           = Cond l (conv env e1) (conv env e) (conv env e2)
    conv env (IsInstance l e c)         = IsInstance l (conv env e) c
    conv env (BinOp l e1 Or e2)         = BinOp l (conv env e1) Or (conv env e2)
    conv env (BinOp l e1 And e2)        = BinOp l (conv env e1) And (conv env e2)
    conv env (UnOp l Not e)             = UnOp l Not (conv env e)
    conv env (Dot l e n)                = Dot l (conv env e) n
    conv env (DotI l e i)               = DotI l (conv env e) i
    conv env (RestI l e i)              = RestI l (conv env e) i
    conv env (Lambda l p KwdNIL e fx)   = Lambda l (conv env p) KwdNIL (conv env e) fx
    conv env (Yield l e)                = Yield l (conv env e)
    conv env (YieldFrom l e)            = YieldFrom l (conv env e)
    conv env (Tuple l es ks)            = Tuple l (conv env es) (conv env ks)
    conv env (List l es)                = List l (conv env es)
    conv env (Dict l as)                = Dict l (conv env as)
    conv env (Set l es)                 = Set l (conv env es)
    conv env e                          = e

instance Conv PosArg where
    conv env (PosArg e p)               = PosArg (conv env e) (conv env p)
    conv env PosNil                     = PosNil

instance Conv KwdArg where
    conv env (KwdArg n e p)             = KwdArg n (conv env e) (conv env p)
    conv env KwdNil                     = KwdNil

instance Conv Elem where
    conv env (Elem e)                   = Elem (conv env e)

instance Conv Assoc where
    conv env (Assoc k v)                = Assoc (conv env k) (conv env v)