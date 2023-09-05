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
convert env0 m                          = return (runCpsM (convMod m), mapModules1 conv env0)
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
                                        | Seq    Name                       -- nextCont
                                        | Loop   Name                       -- loopEntry
                                        deriving (Eq,Show)

instance Pretty Frame where
    pretty (Meth n t)                   = text "Meth" <+> pretty t
    pretty (Seq n)                      = text "Seq" <+> pretty n
    pretty (Loop n)                     = text "Loop" <+> pretty n

type CPSEnv                             = EnvF CPSX

data CPSX                               = CPSX { ctxtX :: [Frame], whereX :: Where }

data Where                              = OnTop | InClass | InDef deriving (Eq,Show)

cpsEnv env0                             = setX env0 CPSX{ ctxtX = [], whereX = OnTop }

ctxt env                                = ctxtX $ envX env

infixr +:
frame +: env                            = modX env $ \x -> x{ ctxtX = frame : ctxtX x }

setClassCtxt env                        = modX env $ \x -> x{ ctxtX = [], whereX = InClass }

setDefCtxt env                          = modX env $ \x -> x{ whereX = InDef }

onTop env                               = whereX (envX env) == OnTop
inClass env                             = whereX (envX env) == InClass
inDef env                               = whereX (envX env) == InDef

eCallCont t c arg                       = eCall (tApp (eQVar primRContc) [t]) [eVar c, arg]

format cont                             = [sReturn cont]

seqcont (Seq c : ctx)                   = eCallCont tNone c eNone                           -- end of sequence:     followed by some cmd    -   jump to it
seqcont (Loop c : ctx)                  = eCallCont tNone c eNone                           --                      inside a loop           -   jump to its top
seqcont (Meth c t : _)                  = eCallCont tNone c eNone                           --                      in a method             -   jump to its continuation

cntcont (Seq c : ctx)                   = cntcont ctx                                       -- 'continue':          followed by some cmd    -   ignore it
cntcont (Loop c : ctx)                  = eCallCont tNone c eNone                           --                      inside a loop           -   jump to its top

brkcont (Seq c : ctx)                   = brkcont ctx                                       -- 'break':             followed by some cmd    -   ignore it
brkcont (Loop c : ctx)                  = seqcont ctx                                       --                      in a loop               -   jump to what follows

retcont e (Seq c : ctx)                 = retcont e ctx                                     -- 'return':            followed by some cmd    -   ignore it
retcont e (Loop c : ctx)                = retcont e ctx                                     --                      in a loop               -   ignore it
retcont e (Meth c t : _)                = eCallCont t c e                                   --                      in a method             -   jump to its continuation

quicknext (Seq c : _)                   = Just (eVar c)
quicknext (Loop c : _)                  = Just (eVar c)
quicknext (Meth c t : _)                = Just (eVar c)
quicknext _                             = Nothing


cpsSuite env ss                         = cps env ss

class CPS a where
    cps                                 :: CPSEnv -> a -> CpsM a

instance CPS [Stmt] where
    cps env []
      | inCont env                      = return [sReturn $ seqcont (ctxt env)]
    cps env (Continue _ : _)
      | inCont env                      = return [sReturn $ cntcont (ctxt env)]
    cps env (Break _ : _)
      | inCont env                      = return [sReturn $ brkcont (ctxt env)]
    cps env (Return _ Nothing : _)
      | inCont env                      = return [sReturn $ retcont eNone (ctxt env)]
    cps env (Return _ (Just e) : _)
      | contCall env e,
        Just c <- quicknext (ctxt env)  = return [sReturn (addContArg env (conv e) c)]
      | inCont env                      = return [sReturn $ retcont (conv e) (ctxt env)]

    cps env (Assign _ [PVar _ n _] e : 
             Return _ (Just e') : _)
      | contCall env e, e' == eVar n,
        Just c <- quicknext (ctxt env)  = return [sReturn (addContArg env (conv e) c)]

    cps env [Expr _ e]
      | contCall env e,
        Just c <- quicknext (ctxt env)  = return [sReturn (addContArg env (conv e) (cont c))]
      where t                           = typeOf env e
            cont c                      = if t == tNone then c else eCall (tApp (eQVar primSKIPRESc) [t]) [c]

    cps env (s : Return _ e : _)
      | e == Just eNone                 = cps env [s]

    cps env (Expr _ e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env ss
                                             return $ kDef env k (pospar [(x,t)]) ss' :
                                                      sReturn (addContArg env (conv e) (eVar k)) : []
      where t                           = typeOf env e

    cps env (Assign _ [PVar _ x _] e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             ss' <- cps env1 ss
                                             return $ kDef env k (pospar [(x,t)]) ss' :
                                                      sReturn (addContArg env (conv e) (eVar k)) : []
      where t                           = typeOf env e
            env1                        = define [(x,NVar t)] env

    cps env (MutAssign _ tg e : ss)
      | contCall env e                  = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env (sMutAssign tg (eVar x) : ss)
                                             return $ kDef env k (pospar [(x,t)]) ss' :
                                                      sReturn (addContArg env (conv e) (eVar k)) : []
      where t                           = typeOf env e

    cps env (Decl l ds : ss)            = do ds' <- mapM (cps env1) ds
                                             ss' <- cps env1 ss
                                             return $ sDecl ds' : ss'
      where env1                        = define (envOf ds) env
    
    cps env (s : ss)
      | not (needCont env s)            = do ss' <- cps env1 ss
                                             return $ conv s : ss'
      where env1                        = define (envOf s) env
    
    cps env [If _ [Branch e ss1] ss2]
      | isPUSH e                        = do k <- newName "try"
                                             x <- newName "res"
                                             ss1 <- cpsSuite env (map convX ss1)
                                             ss2 <- cpsSuite env (map convX ss2)
                                             let body = sIf1 (eVar x) ss1 ss2 : []
                                             return $ kDef env k (pospar [(x,tBool)]) body :
                                                      sReturn (convPUSH e $ eVar k) : []

    cps env [If _ bs els]               = do bs' <- mapM (cps env) bs
                                             els' <- cpsSuite env els
                                             return $ sIf bs' els' : []

    cps env [While _ e b els]           = do k    <- newName "loop"
                                             x    <- newName "res"
                                             b'   <- cpsSuite (Loop k +: env) b
                                             els' <- cpsSuite env els
                                             let body = sIf1 (conv e) b' els' : []
                                             return $ kDef env k (pospar [(x,tNone)]) body : 
                                                      jump k

    cps env (s : ss)                    = do k <- newName "cont"
                                             x <- newName "res"
                                             ss' <- cps env1 ss
                                             ss1 <- cps (Seq k +: env) [s]
                                             return $ kDef env k (pospar [(x,tNone)]) ss' :
                                                      ss1
      where env1                        = define (envOf s) env

    cps env []                          = return []


instance CPS Decl where
    cps env (Class l n q cs b)          = do b' <- cpsSuite env1 b
                                             return $ Class l n (conv q) (conv cs) b'
      where env1                        = defineSelf (NoQ n) q $ defineTVars q $ setClassCtxt env

    cps env (Def l n q p KwdNIL (Just t) b dec fx)
      | contFX fx                       = do b' <- cpsSuite env1 b
                                             return $ Def l n q' (addContPar env dec p' fx t') KwdNIL (Just tR) b' dec fx
      | otherwise                       = return $ Def l n q' p' KwdNIL (Just t') (conv b) dec fx
      where env1                        = define (envOf p) $ defineTVars q $ Meth contKW t' +: setDefCtxt env
            q'                          = conv q
            p'                          = conv p
            t'                          = conv t

    cps env d                           = error ("cps unexpected: " ++ prstr d)
    

instance CPS Branch where
    cps env (Branch e ss)               = Branch (conv e) <$> cpsSuite env ss

    
jump k                                  = sReturn (eCall (eVar k) [eNone]) : []


addContArg env (Call l e p KwdNil) c    = Call NoLoc e (PosArg c p) KwdNil

addContPar env dec (PosPar n a Nothing p) fx t
  | inClass env && dec /= Static        = PosPar n a Nothing (addContPar0 p fx t)
addContPar env dec p fx t               = addContPar0 p fx t

addContPar0 p fx t                      = PosPar contKW (Just $ tCont1 fx t) Nothing p

tCont0                                  = tFun fxProc (posRow tNone posNil) kwdNil tR

tCont1 fx t                             = tFun fx (posRow t posNil) kwdNil tR

kDef env k p b                          = sDef k (conv p) tR b fxProc

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


convX s@(If _ [Branch e _] _)
  | isPUSH e                            = s
convX (If l bs els)                     = If l [ Branch e (map convX ss) | Branch e ss <- bs ] (map convX els)
convX (While l e ss els)                = While l e (map convX ss) (map convX els)
convX (Expr l (Call l1 e ps ks))
  | Var l2 x <- e, x == primDROP        = Expr l (Call l1 (Var l2 primDROP_C) ps ks)
convX (Assign l p (Call l1 e ps ks))
  | Var l2 x <- e, x == primPOP         = Assign l p (Call l1 (Var l2 primPOP_C) ps ks)
convX s                                 = s

isPUSH (Call _ (Var _ x) _ _)           = x `elem` [primPUSH,primPUSHF]
isPUSH _                                = False

convPUSH (Call _ (Var _ x) _ _) arg
  | x == primPUSH                       = eCall (eQVar primPUSH_Cc) [arg]
  | x == primPUSHF                      = eCall (eQVar primPUSHF_Cc) [arg]

class NeedCont a where
    needCont                            :: CPSEnv -> a -> Bool

instance (NeedCont a, EnvOf a) => NeedCont [a] where
    needCont env []                     = False
    needCont env (s : ss)               = needCont env s || needCont (define (envOf s) env) ss

instance NeedCont Branch where
    needCont env (Branch e ss)          = isPUSH e || needCont env ss

instance NeedCont Stmt where
    needCont env (Return _ (Just e))    = inCont env
    needCont env (Continue _)           = inCont env
    needCont env (Break _)              = inCont env
    needCont env (Expr _ e)             = contCall env e
    needCont env (Assign _ _ e)         = contCall env e
    needCont env (MutAssign _ _ e)      = contCall env e
    needCont env (If _ bs els)          = needCont env bs || needCont env els
    needCont env (While _ _ b els)      = needCont env b || needCont env els
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
    pre env e0@(Lambda l p KwdNIL e fx)
      | contFX fx                       = do (prefixes,e') <- withPrefixes $ preTop env1 e
                                             case prefixes of
                                                [] -> let p' = conv p; t' = conv t
                                                          e1 = if contCall env e then addContArg env1 e' (eVar contKW) else eCallCont t' contKW e'
                                                      in return $ Lambda l (addContPar0 p' fx t') KwdNIL e1 fx
                                                _ -> do
                                                    f <- newName "lambda"
                                                    prefix [sDecl [Def l f [] p KwdNIL (Just t) (prefixes ++ [sReturn e']) NoDec fx]]
                                                    return (Var l0 (NoQ f))
      | otherwise                       = do e' <- pre env1 e
                                             return $ Lambda l p KwdNIL e' fx
      where env1                        = define (envOf p) env
            t                           = typeOf env1 e
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
    conv                                :: a -> a

instance (Conv a) => Conv [a] where
    conv                                = map conv

instance (Conv a) => Conv (Maybe a) where
    conv                                = fmap conv

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
    conv (TSchema l q t)                = TSchema l (conv q) (conv t)

instance Conv QBind where
    conv (Quant v cs)                   = Quant v (conv cs)

instance Conv Type where
    conv t0@(TFun l fx p TNil{} t)
       | contFX fx && t /= tR           = TFun l fx (addCont (conv p) (conv t)) kwdNil tR
       | otherwise                      = TFun l fx (conv p) kwdNil (conv t)
       where addCont p c                = posRow (tFun fx (posRow c posNil) kwdNil tR) p
    conv (TCon l c)                     = TCon l (conv c)
    conv (TTuple l p k)                 = TTuple l (conv p) (conv k)
    conv (TOpt l t)                     = TOpt l (conv t)
    conv (TRow l k n t r)               = TRow l k n (conv t) (conv r)
    conv t                              = t

instance Conv TCon where
    conv (TC c ts)                      = TC c (conv ts)

instance Conv PosPar where
    conv (PosPar n t Nothing p)         = PosPar n (conv t) Nothing (conv p)
    conv PosNIL                         = PosNIL

instance Conv Stmt where
    conv (Expr l e)                     = Expr l (conv e)
    conv (Assign l ps e)                = Assign l (conv ps) (conv e)
    conv (MutAssign l tg e)             = MutAssign l (conv tg) (conv e)
    conv (Return l e)                   = Return l (conv e)
    conv (If l bs els)                  = If l (conv bs) (conv els)
    conv (While l e b els)              = While l (conv e) (conv b) (conv els)
    conv (Signature l ns sc dec)        = Signature l ns (conv sc) dec
    conv s                              = s

instance Conv Branch where
    conv (Branch e ss)                  = Branch (conv e) (conv ss)

instance Conv Pattern where
    conv (PVar l n t)                   = PVar l n (conv t)
    conv p                              = p

instance Conv Expr where
    conv (Var l n)
      | n == primASYNCf                 = Var l primASYNCc
      | n == primAFTERf                 = Var l primAFTERc
      | n == primAWAITf                 = Var l primAWAITc
      | otherwise                       = Var l n
    conv (Call l e ps KwdNil)           = Call l (conv e) (conv ps) KwdNil
    conv (TApp l e ts)                  = TApp l (conv e) (conv ts)
    conv (Cond l e1 e e2)               = Cond l (conv e1) (conv e) (conv e2)
    conv (IsInstance l e c)             = IsInstance l (conv e) c
    conv (BinOp l e1 Or e2)             = BinOp l (conv e1) Or (conv e2)
    conv (BinOp l e1 And e2)            = BinOp l (conv e1) And (conv e2)
    conv (UnOp l Not e)                 = UnOp l Not (conv e)
    conv (Dot l e n)                    = Dot l (conv e) n
    conv (DotI l e i)                   = DotI l (conv e) i
    conv (RestI l e i)                  = RestI l (conv e) i
    conv (Lambda l p KwdNIL e fx)       = Lambda l (conv p) KwdNIL (conv e) fx
    conv (Yield l e)                    = Yield l (conv e)
    conv (YieldFrom l e)                = YieldFrom l (conv e)
    conv (Tuple l es ks)                = Tuple l (conv es) (conv ks)
    conv (List l es)                    = List l (conv es)
    conv (Dict l as)                    = Dict l (conv as)
    conv (Set l es)                     = Set l (conv es)
    conv e                              = e

instance Conv PosArg where
    conv (PosArg e p)                   = PosArg (conv e) (conv p)
    conv PosNil                         = PosNil

instance Conv KwdArg where
    conv (KwdArg n e p)                 = KwdArg n (conv e) (conv p)
    conv KwdNil                         = KwdNil

instance Conv Elem where
    conv (Elem e)                       = Elem (conv e)

instance Conv Assoc where
    conv (Assoc k v)                    = Assoc (conv k) (conv v)