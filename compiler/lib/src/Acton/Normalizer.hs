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

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Acton.Normalizer where

import Acton.Syntax
import Acton.Names
import Acton.Env
import Acton.QuickType
import Acton.Prim
import Acton.Builtin
import Data.List
import Pretty
import Utils
import Control.Monad.State.Strict
import Debug.Trace

normalize                           :: Env0 -> Module -> IO (Module, Env0)
normalize env0 m                    = return (evalState (norm env m) (0,[]), env0')
  where env                         = normEnv env0
        env0'                       = mapModules convEnv env0


--  Normalization:
--  X All module aliases are replaced by their original module name
--  X All parameters are positional
--  X Comprehensions are translated into loops
--  X String literals are concatenated and delimited by double quotes
--  X Tuple (and list) patterns are replaced by a var pattern followed by explicit element assignments
--  - With statemenmts are replaced by enter/exit prim calls + exception handling
--  X The assert statement is replaced by a prim call ASSERT
--  X Return without argument is replaced by return None
                                                                                                                 --  - The else branch of a while loop is replaced by an explicit if statement enclosing the loop
--  X Superclass lists are transitively closed


-- Normalizing monad
type NormM a                        = State (Int,[(Name,PosPar,Expr)]) a

newName                             :: String -> NormM Name
newName s                           = do (n,ts) <- get
                                         put (n+1,ts)
                                         return $ Internal NormPass s n

addComp                             :: (Name,PosPar,Expr) -> NormM ()
addComp (n,p,e)                     = state (\(i,ts) -> ((),(i,(n,p,e):ts)))

getComps                            :: NormM [(Name,PosPar,Expr)]
getComps                            = state (\(n,ts) -> (ts, (n,[])))

type NormEnv                        = EnvF NormX

data NormX                          = NormX {
                                        contextX :: [ContextMark],
                                        rtypeX :: Maybe Type,
                                        lambdavarsX :: PosPar,
                                        classattrsX :: [Name],
                                        selfparamX :: Maybe Name
                                      }

data ContextMark                    = DROP | LOOP | FINAL deriving (Eq,Show)

setContext c env                    = modX env $ \x -> x{ contextX = c }

pushMark c env                      = modX env $ \x -> x{ contextX = c :  contextX x }

context env                         = contextX $ envX env

setRet t env                        = modX env $ \x -> x{ rtypeX = t }

getRet env                          = fromJust $ rtypeX $ envX env

addLambdavars p env                 = modX env $ \x -> x{ lambdavarsX = joinP (lambdavarsX x) p }
  where joinP PosNIL p              = p
        joinP (PosPar n mbt mbe p') p
                                    = PosPar n mbt mbe (joinP p' p)

getLambdavars env                   = lambdavarsX $ envX env

classattrs env                      = classattrsX $ envX env

selfparam env                       = selfparamX $ envX env

setClassAttrs ns env                = modX env $ \x -> x{ classattrsX = ns }

setSelfParam n env                  = modX env $ \x -> x{ selfparamX = Just n }

normEnv env0                        = setX env0 NormX{ contextX = [], rtypeX = Nothing, lambdavarsX = PosNIL, classattrsX = [], selfparamX = Nothing }


-- Normalize terms ---------------------------------------------------------------------------------------

normSuite env []                    = return []
normSuite env (s : ss)              = do s' <- norm' env s
                                         comps <- getComps
                                         ss' <- normSuite (define (envOf s) env) ss
                                         defs <- mapM mkCompFun comps
                                         return (concat defs ++ s' ++ ss')
  where mkCompFun (f,lambound,comp) = do w <- newName "w"
                                         r <- newName "res"
                                         let env0 = define (envOf lambound) env
                                             fx = fxOf env0 comp
                                             (tw,w1,tr,e0,stmt) = transComp env0 w r comp
                                             body = sAssign (pVar w tw) w1 :
                                                    sAssign (pVar r tr) e0 :
                                                    stmt :
                                                    sReturn (eVar r) : []
                                         norm' env (sDef f lambound tr body fx)

        transComp env w r (ListComp _ (Elem e) co)
                                    = (tw, w1, tr, e0, compStmt co e1)
          where env1                = define (envOf co) env
                te                  = typeOf env1 e
                tr                  = tList te
                tw                  = tSequenceW tr te
                e0                  = List NoLoc []
                e1                  = eCall (eDot (eVar w) appendKW) [eVar r, e]
                w1                  = eCall (tApp (eQVar witSequenceList) [te]) []
        transComp env w r (SetComp _ (Elem annot_e) co)
                                    = (tw, w1, tr, e0, compStmt co e1)
          where env1                = define (envOf co) env
                te                  = typeOf env1 annot_e
                tr                  = tSet te
                tw                  = tSetW tr te
                (w0, e)             = unAnnot (tHashableW te) annot_e
                e0                  = eCall (tApp (eQVar primMkSet) [te]) [w0, Set NoLoc []]
                e1                  = eCall (eDot (eVar w) (name "add")) [eVar r, e]
                w1                  = eCall (tApp (eQVar witSetSet) [te]) [w0]
        transComp env w r (DictComp _ (Assoc annot_k v) co)
                                    = (tw, w1, tr, e0, compStmt co e1)
          where env1                = define (envOf co) env
                tk                  = typeOf env1 annot_k
                tv                  = typeOf env1 v
                tr                  = tDict tk tv
                tw                  = tMappingW tr tk tv
                (w0, k)             = unAnnot (tHashableW tk) annot_k
                e0                  = eCall (tApp (eQVar primMkDict) [tv,tk]) [w0, Dict NoLoc []]
                e1                  = eCall (eDot (eDot (eVar w) (Internal Witness "Indexed" 0)) setitemKW) [eVar r, k, v]
                w1                  = eCall (tApp (eQVar witMappingDict) [tk,tv]) [w0]

        compStmt (CompFor l p e c) x = For l p e [compStmt c x] []
        compStmt (CompIf l e c) x   = If l [Branch e [compStmt c x]] []
        compStmt (NoComp) x         = sExpr x


normPat                             :: NormEnv -> Pattern -> NormM (Pattern,Suite)
normPat _ (PWild l a)               = do n <- newName "ignore"
                                         return (PVar l n $ conv a,[])
normPat _ (PVar l n a)              = return (PVar l n $ conv a,[])
normPat env (PParen _ p)            = normPat env p
normPat env p@(PTuple _ pp kp)      = do v <- newName "tup"
                                         ss <- norm (define [(v, NVar t)] env) $ normPP v 0 pp ++ normKP v [] kp
                                         return (pVar v $ conv t, ss)
  where normPP v n (PosPat p pp)    = Assign NoLoc [p] (DotI NoLoc (eVar v) n) : normPP v (n+1) pp
        normPP v n (PosPatStar p)   = [Assign NoLoc [p] (foldl (RestI NoLoc) (eVar v) [0..n-1])]
        normPP _ _ PosPatNil        = []
        normKP v ns (KwdPat n p kp) = Assign NoLoc [p] (Dot NoLoc (eVar v) n) : normKP v (n:ns) kp
        normKP v ns (KwdPatStar p)  = [Assign NoLoc [p] (foldl (Rest NoLoc) (eVar v) (reverse ns))]
        normKP _ _ KwdPatNil        = []
        t                           = typeOf env p
normPat env p@(PList _ ps pt)       = do v <- newName "lst"
                                         ss <- norm env $ normList v 0 ps pt
                                         return (pVar v $ conv t, ss)
  where normList v n (p:ps) pt      = s : normList v (n+1) ps pt
          where s                   = Assign NoLoc [p] (eCall (eDot (eQVar qnIndexed) getitemKW)
                                        [eVar v, Int NoLoc n (show n)])
        normList v n [] (Just p)    = [Assign NoLoc [p] (eCall (eDot (eQVar qnSliceable) getsliceKW)
                                        [eVar v, Int NoLoc n (show n), None NoLoc, None NoLoc])]
        normList v n [] Nothing     = []
        t                           = typeOf env p



class Norm a where
    norm                            :: NormEnv -> a -> NormM a
    norm'                           :: NormEnv -> a -> NormM [a]
    norm' env x                     = (:[]) <$> norm env x

instance (Norm a, EnvOf a) => Norm [a] where
    norm env []                     = return []
    norm env (a:as)                 = do as1 <- norm' env a
                                         as2 <- norm env1 as
                                         return (as1++as2)
      where env1                    = define (envOf a) env

instance Norm a => Norm (Maybe a) where
    norm env Nothing                = return Nothing
    norm env (Just a)               = Just <$> norm env a

instance Norm Module where
    norm env (Module m imps ss)     = Module m imps <$> normSuite env ss

handle env x hs                     = do bs <- sequence [ branch e b | Handler e b <- hs ]
                                         return $ [sIf bs [sExpr $ eCall (eQVar primRAISE) [eVar x]]]
  where branch (ExceptAll _) b      = Branch (eBool True) <$> norm env b
        branch (Except _ y) b       = Branch (eIsInstance x y) <$> norm env b
        branch (ExceptAs _ y z) b   = Branch (eIsInstance x y) <$> (bind:) <$> norm env' b
          where env'                = define [(z,NVar t)] env
                bind                = sAssign (pVar z $ conv t) (eVar x)
                t                   = tCon $ TC y []

exitContext env s
  | DROP:c <- context env           = sDROP : exitContext (setContext c env) s
  | FINAL:c <- context env          = [sRAISE $ exn s]
  | LOOP:c <- context env           = if s `elem` [sBreak,sContinue] then [s] else exitContext (setContext c env) s
  | otherwise                       = [s]
  where exn (Break _)               = eCall (eQVar primBRK) []
        exn (Continue _)            = eCall (eQVar primCNT) []
        exn (Return _ (Just e))     = eCall (eQVar primRET) [e]

sDROP                               = sExpr (eCall (eQVar primDROP) [])
sPOP x                              = sAssign (pVar x tBaseException) (eCall (eQVar primPOP) [])
ePUSH                               = eCall (eQVar primPUSH) []
ePUSHF                              = eCall (eQVar primPUSHF) []
sSEQ                                = sExpr (eCall (eQVar primRAISE) [eCall (eQVar primSEQ) []])
sRAISE e                            = sExpr (eCall (eQVar primRAISE) [e])

-- TODO: maybe less approximation?
isVal Var{}                         = True
isVal Int{}                         = True
isVal Float{}                       = True
isVal Imaginary{}                   = True
isVal Bool{}                        = True
isVal None{}                        = True
isVal Strings{}                     = True
isVal BStrings{}                    = True
isVal Lambda{}                      = True
isVal (Call _ (TApp _ (Var _ n) _) (PosArg e PosNil) KwdNil)
                                    = n == primCAST && isVal e
isVal (TApp _ e _)                  = isVal e
isVal (Dot _ e _)                   = isVal e
isVal (DotI _ e _)                  = isVal e
isVal (Paren _ e)                   = isVal e
isVal _                             = False

instance Norm Stmt where
    norm env (Expr l e)             = Expr l <$> norm env e
    norm env (MutAssign l t e)      = MutAssign l <$> norm env t <*> norm env e
    norm env (Assert l e mbe)       = do e' <- normBool env e
                                         mbe' <- norm env mbe
                                         return $ Expr l $ eCall (eQVar primASSERT) [e', maybe eNone id mbe']
    norm env (Pass l)               = return $ Pass l
    norm env (Raise l e)            = do e' <- norm env e
                                         return $ Expr l $ eCall (eQVar primRAISE) [e']
    norm env (If l bs els)          = If l <$> norm env bs <*> normSuite env els
    norm env (While l e b els)      = While l (eBool True) <$> normSuite (pushMark LOOP env) (sIf1 e [sPass] (els++[sBreak]) : b) <*> return []
    norm env (Data l mbp ss)        = Data l <$> norm env mbp <*> norm env ss
    norm env (VarAssign l ps e)     = VarAssign l <$> norm env ps <*> norm env e
    norm env (After l e e')         = After l <$> norm env e <*> norm env e'
    norm env (Signature l ns t d)   = return $ Signature l ns (conv t) d
    norm env s                      = error ("norm unexpected stmt: " ++ prstr s)

    norm' env (Decl l ds)           = do (eqs,ds) <- normDecls env ds
                                         return $ eqs ++ [Decl l ds]

    norm' env (Try l b [] els [])   = norm env (b ++ els)
    norm' env (Try l b hs els [])   = do b <- norm (pushMark DROP env) b
                                         els <- norm (define (envOf b) env) els
                                         x <- newName "x"
                                         hdl <- handle env x hs
                                         return [sIf [Branch ePUSH (b ++ sDROP : els)] (sPOP x : hdl)]
      where ePUSH                   = eCall (eQVar primPUSH) []
    norm' env (Try l b hs els fin)  = do ss <- norm' (pushMark FINAL env) try0
                                         x <- newName "xx"
                                         fin <- norm (define [(x,NVar tBaseException)] env) fin
                                         return [sIf [Branch ePUSHF (ss++mbseq)] (sPOP x : fin ++ relays x)]
      where try0                    = Try l b hs els []
            relays x                = iff [ Branch (eIsInstance x n) s | (n,s) <- map (relay x) ctrl, valid s] [sRAISE $ eVar x]
            relay _ SEQ             = (primSEQ, [sPass])
            relay _ BRK             = (primBRK, exitContext env sBreak)
            relay _ CNT             = (primCNT, exitContext env sContinue)
            relay x RET             = (primRET, downcast : exitContext env ret)
              where x'              = Derived x (globalName "RET")
                    downcast        = sAssign (pVar x' tRET) (eCAST tBaseException tRET (eVar x))
                    ret             = sReturn (eCAST tValue (getRet env) (eDot (eVar x') attrVal))
            valid [Expr{}]          = False
            valid [Assign{},Expr{}] = False
            valid _                 = True
            mbseq                   = if SEQ `elem` ctrl then [sSEQ] else []
            ctrl                    = nub (flows try0)
            iff [] els              = els
            iff bs els              = [sIf bs els]

    norm' env s@(Break l)           = return $ exitContext env s
    norm' env s@(Continue l)        = return $ exitContext env s
    norm' env (Return l Nothing)    = return $ exitContext env (Return l $ Just eNone)
    norm' env (Return l (Just e))   = do e <- norm env e
                                         case isVal e of
                                             True -> return $ retContext e
                                             False -> do
                                                 n <- newName "tmp"
                                                 return $ sAssign (pVar n $ conv t) e : retContext (eVar n)
      where retContext e            = exitContext env $ Return l $ Just e
            t                       = typeOf env e

    norm' env (Assign l ps e)       = do e' <- norm env e
                                         (ps1,stmts) <- unzip <$> mapM (normPat env) ps
                                         ps2 <- norm env ps1
                                         let p'@(PVar _ n _) : ps' = ps2
                                         return $ Assign l [p'] e' : [ Assign l [p] (eVar n) | p <- ps' ] ++ concat stmts
      where t                       = typeOf env e
    norm' env s@(For l p e b els)   = do i <- newName "iter"
                                         v <- newName "val"
                                         norm env [sAssign (pVar i $ conv t) e,
                                                   handleStop (While l (eBool True) (body v i) []) els]
      where t@(TCon _ (TC c [t']))  = typeOf env e
            next i                  = eCall (eDot (eVar i) nextKW) []
            handleStop loop els     = Try l [loop] [Handler (Except l0 qnStopIteration) (mkBody els)] [] []
            body v i                = sAssign (pVar v t') (next i) : sAssign p (eVar v) : b
    {-
    with EXPRESSION as PATTERN:
        SUITE
    ===>
    $mgr = EXPRESSION
    $val = $mgr.__enter__()
    $exc = False
    try:
        PATTERN = $val
        SUITE
    except Exception as ex:
        $exc = True
        if not $mgr.__exit__(ex):
            raise
    finally:
        if not $exc:
            $mgr.__exit__(None)
    -}
    norm' env s@(With l (i:is) b)   = do notYet l s                     -- TODO: remove
                                         m <- newName "mgr"
                                         v <- newName "val"
                                         x <- newName "exc"
                                         (e,mbp,ss) <- normItem env i
                                         b' <- normSuite env1 (ss ++ b)
                                         return undefined
      where env1                    = define (envOf i) env
    norm' env (With l [] b)         = normSuite env b
    norm' env s                     = do s' <- norm env s
                                         return [s']

normItem env (WithItem e Nothing)   = do e' <- norm env e
                                         return (e', Nothing, [])
normItem env (WithItem e (Just p))  = do e' <- norm env e
                                         (p',ss) <- normPat env p
                                         return (e', Just p', ss)

normDecls env ds                    = do (pres, ds) <- unzip <$> mapM (normDecl env1 ns) ds
                                         pre <- norm env (concat pres)
                                         return (pre, ds)
      where env1                    = define (envOf ds) env
            ns                      = bound ds

normDecl env ns d@Class{}           = do d <- norm env1 d{ dbody = props ++ b }
                                         pre <- norm env pre
                                         return (pre, d)
      where (pre,te,b)              = fixupClassAttrs (dname d) ns (dbody d)
            env1                    = define (envOf pre ++ te) $ setClassAttrs (dom te) env
            props                   = [ Signature NoLoc [w] (monotype t) Property | (w,NVar t) <- te ]
normDecl env ns d                   = do d <- norm env d
                                         return ([], d)


-- The type-checker may leave witness bindings on the level of classes, even though our class
-- syntax does not yet support this in the same way as is does for actors. But the creation and
-- reduction of witnesses that mutually depend on classes becomes so much easier if we allow
-- ourselves to make use of this planned feature already today. The code below thus implements
-- class level bindings, albeit limited to witnesses, by transforming them into either global
-- binding prefixes (if the circular class dependencies actually got eliminated during witness
-- reduction), __init__ method locals (if they are only referenced during initialization) or
-- proper instance attributes (in the general case).

fixupClassAttrs n ns b
  | null eqs                        = ([], [], b)
  | null attr                       = --trace ("### Lift out attrs " ++ prstrs (bound pre) ++ " in class " ++ prstr n) $
                                      (pre, [], defs)
  | null te                         = --trace ("### Init attrs " ++ prstrs (dom te) ++ " in class " ++ prstr n) $
                                      ([], [], map initS defs)
  | null pre                        = --trace ("### Dynamic attrs " ++ prstrs (dom te) ++ " in class " ++ prstr n) $
                                      ([], te, map initS defs)
  | otherwise                       = --trace ("### Fixup wits " ++ prstrs (bound eqs) ++ " in class " ++ prstr n) $
                                      (pre, te, map initS defs)
  where (eqs, defs)                 = splitA [] [] b

        splitA eqs defs []          = (reverse eqs, reverse defs)
        splitA eqs defs (s:ss)      = case s of
                                        Assign _ [PVar _ (Internal Witness _ _) (Just _)] _ ->
                                            splitA (s:eqs) defs ss
                                        _ ->
                                            splitA eqs (s:defs) ss

        (dyn, par)                  = dvars [] [] $ concat [ ds | Decl _ ds <- defs ]

        dvars dyn par []            = (dyn, par)
        dvars dyn par (d:ds)
          | dname d == initKW       = dvars dyn ([ n | n@(Internal Witness _ _) <- bound (pos d) ] ++ par) ds
          | otherwise               = dvars (free d ++ dyn) par ds

        (pre, attr)                 = splitG ns [] [] eqs

        splitG ns pre attr []       = (reverse pre, reverse attr)
        splitG ns pre attr (eq:eqs)
          | null fvs                = splitG ns (eq:pre) attr eqs
          | otherwise               = splitG (bound eq ++ ns) pre (eq:attr) eqs
          where fvs                 = free (expr eq) `intersect` (par++ns)

        initS (Decl l ds)           = Decl l (map initD ds)
        initS s                     = s

        initD d@Def{}
          | dname d == initKW,
            Just self <- selfPar d  = d{ dbody = map (initA $ eVar self) attr ++ dbody d }
        initD d                     = d

        initA self (Assign _ [PVar _ w _] e)
          | w `elem` dyn            = sMutAssign (eDot self w) e
        initA self s                = s

        te                          = [ (w, NVar t) | Assign _ [PVar _ w (Just t)] _ <- attr, w `elem` dyn ]


instance Norm Decl where
    norm env (Def l n q p k t b d x doc)
                                    = do p' <- joinPar <$> norm env0 p <*> norm (define (envOf p) env0) k
                                         b' <- normSuite env1 b
                                         return $ Def l n q p' KwdNIL (conv t) (ret b') d x doc
      where env1                    = setContext [] $ setRet t $ define (envOf p ++ envOf k) env0
            env0                    = defineTVars q env00
            env00                   = case p of
                                        PosPar self _ _ _ | not $ null $ classattrs env, d /= Static ->
                                            setSelfParam self env
                                        _ ->
                                            env
            ret b | fallsthru b     = b ++ [sReturn eNone]
                  | otherwise       = b
    norm env (Actor l n q p k b doc)
                                    = do p' <- joinPar <$> norm env0 p <*> norm (define (envOf p) env0) k
                                         b' <- normSuite env1 b
                                         return $ Actor l n q p' KwdNIL b' doc
      where env1                    = setContext [] $ define (envOf p ++ envOf k) env0
            env0                    = define [(selfKW, NVar t0)] $ defineTVars q env
            t0                      = tCon $ TC (NoQ n) (map tVar $ qbound q)
    norm env (Class l n q as b doc) = Class l n q as <$> norm env1 b <*> return doc
      where env1                    = defineSelf (NoQ n) q $ defineTVars q env
    norm env d                      = error ("norm unexpected: " ++ prstr d)


catStrings ss                       = map (quote . escape '"') ss
  where escape c []                 = []
        escape c ('\\':x:xs)        = '\\' : x : escape c xs
        escape c (x:xs)
          | x == c                  = '\\' : x : escape c xs
          | otherwise               = x : escape c xs
        quote s                     = '"' : s ++ "\""


normInst env ts e                   = norm env e

normBool env e
  | t == tBool                      = norm env e
  | TOpt _ t' <- t, Var{} <- e      = return $ eBinOp (eCall (tApp (eQVar primISNOTNONE) [t']) [e]) And (eCall (eDot (eCAST t t' e) boolKW) [])
  | otherwise                       = do e' <- norm env e
                                         return $ eCall (eDot e' boolKW) []
  where t                           = typeOf env e

instance Norm Expr where
    norm env (Var l (NoQ n))
      | n `elem` classattrs env,
        Just self <- selfparam env  = return $ eDot (eVar self) n
    norm env (Var l nm)             = return $ Var l nm
    norm env (Int l i s)            = Int l <$> return i <*> return s
    norm env (Float l f s)          = Float l <$> return f <*> return s
    norm env (Imaginary l i s)      = Imaginary l <$> return i <*> return s
    norm env (Bool l b)             = Bool l <$> return b
    norm env (None l)               = return $ None l
    norm env (NotImplemented l)     = return $ NotImplemented l
    norm env (Ellipsis l)           = return $ Ellipsis l
    norm env (Strings l ss)         = return $ Strings l (catStrings ss)
    norm env (BStrings l ss)        = return $ BStrings l (catStrings ss)
    norm env (Call l e p k)         = Call l <$> norm env e <*> norm env (joinArg p k) <*> pure KwdNil
    norm env (TApp l e ts)          = TApp l <$> normInst env ts e <*> pure (conv ts)
    norm env (Dot l (Var l' x) n)
      | NClass{} <- findQName x env = pure $ Dot l (Var l' x) n
    norm env (Dot l e n)
      | TTuple _ p k <- t,
        n `notElem` valueKWs        = DotI l <$> norm env e <*> pure (nargs p + narg n k)
      | otherwise                   = Dot l <$> norm env e <*> pure n
      where t                       = typeOf env e
    norm env (Async l e)            = Async l <$> norm env e
    norm env (Await l e)            = Await l <$> norm env e
    norm env (Cond l e1 e2 e3)      = Cond l <$> norm env e1 <*> normBool env e2 <*> norm env e3
    norm env (IsInstance l e c)     = IsInstance l <$> norm env e <*> pure c
    norm env (BinOp l e1 Or e2)     = BinOp l <$> norm env e1 <*> pure Or <*> norm env e2
    norm env (BinOp l e1 And e2)    = BinOp l <$> norm env e1 <*> pure And <*> norm env e2
    norm env (UnOp l Not e)         = UnOp l Not <$> normBool env e
    norm env (Rest l e n)           = RestI l <$> norm env e <*> pure (nargs p + narg n k)
      where TTuple _ p k            = typeOf env e
    norm env (DotI l e i)           = DotI l <$> norm env e <*> pure i
    norm env (RestI l e i)          = RestI l <$> norm env e <*> pure i
    norm env (Lambda l p k e fx)    = do p' <- joinPar <$> norm env p <*> norm (define (envOf p) env) k
                                         let env1 = define (envOf p ++ envOf k) (addLambdavars p' env)
                                         eta <$> (Lambda l p' KwdNIL <$> norm env1 e <*> pure fx)
    norm env (Yield l e)            = Yield l <$> norm env e
    norm env (YieldFrom l e)        = YieldFrom l <$> norm env e
    norm env (Tuple l ps ks)        = Tuple l <$> norm env (joinArg ps ks) <*> pure KwdNil
    norm env (List l es)            = List l <$> norm env es
    norm env e@ListComp{}           = deferComp env e
    norm env (Dict l as)            = Dict l <$> norm env as
    norm env e@DictComp{}           = deferComp env e
    norm env (Set l es)             = Set l <$> norm env es
    norm env e@SetComp{}            = deferComp env e
    norm env (Paren l e)            = norm env e
    norm env e                      = error ("norm unexpected: " ++ prstr e)

deferComp env e                     = do f <- newName "compfun"
                                         let p = getLambdavars env
                                         addComp (f,p,e)
                                         return (Call NoLoc (eVar f) (posarg $ map eVar $ pospars' p) KwdNil)

eta (Lambda _ p KwdNIL (Call _ e p' KwdNil) fx)
  | eq1 p p'                        = e
  where
    eq1 (PosPar n _ _ p) (PosArg e p')  = eVar n == e && eq1 p p'
    eq1 (PosSTAR n _) (PosStar e)       = eVar n == e
    eq1 PosNIL PosNil                   = True
    eq1 _ _                             = False
eta e                               = e

nargs (TRow _ _ _ _ r)              = 1 + nargs r
nargs (TStar _ _ _)                 = 1
nargs (TNil _ _)                    = 0

narg n (TRow _ _ n' _ r)
  | n == n'                         = 0
  | otherwise                       = 1 + narg n r
narg n (TStar _ _ _)
  | n == attrKW                     = 0
narg n k                            = error ("### Bad narg " ++ prstr n ++ " " ++ prstr k)

instance Norm Pattern where
    norm env (PWild l a)            = return $ PWild l (conv a)
    norm env (PVar l n a)           = return $ PVar l n (conv a)
    norm env (PTuple l ps ks)       = PTuple l <$> norm env ps <*> norm env ks
    norm env (PList l ps p)         = PList l <$> norm env ps <*> norm env p        -- TODO: eliminate here
    norm env (PParen l p)           = norm env p

instance Norm Branch where
    norm env (Branch e ss)          = Branch <$> normBool env e <*> normSuite env ss

instance Norm Handler where
    norm env (Handler ex b)         = Handler ex <$> normSuite env1 b
      where env1                    = define (envOf ex) env

instance Norm PosPar where
    norm env (PosPar n t e p)       = PosPar n (conv t) <$> norm env e <*> norm (define [(n,NVar $ fromJust t)] env) p
    norm env (PosSTAR n t)          = return $ PosSTAR n (conv t)
    norm env PosNIL                 = return PosNIL

instance Norm KwdPar where
    norm env (KwdPar n t e k)       = KwdPar n (conv t) <$> norm env e <*> norm (define [(n,NVar $ fromJust t)] env) k
    norm env (KwdSTAR n t)          = return $ KwdSTAR n (conv t)
    norm env KwdNIL                 = return KwdNIL

joinPar (PosPar n t e p) k          = PosPar n t e (joinPar p k)
joinPar (PosSTAR n t) k             = PosPar n t Nothing (kwdToPosPar k)
joinPar PosNIL k                    = kwdToPosPar k

kwdToPosPar (KwdPar n t e k)        = PosPar n t e (kwdToPosPar k)
kwdToPosPar (KwdSTAR n t)           = PosPar n t Nothing PosNIL
kwdToPosPar KwdNIL                  = PosNIL

joinArg (PosArg e p) k              = PosArg e (joinArg p k)
joinArg (PosStar e) k               = PosArg e (kwdToPosArg k)
joinArg PosNil k                    = kwdToPosArg k

kwdToPosArg (KwdArg n e k)          = PosArg e (kwdToPosArg k)
kwdToPosArg (KwdStar e)             = PosArg e PosNil
kwdToPosArg KwdNil                  = PosNil


instance Norm PosArg where
    norm env (PosArg e p)           = PosArg <$> norm env e <*> norm env p
    norm env (PosStar e)            = PosStar <$> norm env e
    norm env PosNil                 = return PosNil

instance Norm KwdArg where
    norm env (KwdArg n e k)         = KwdArg n <$> norm env e <*> norm env k
    norm env (KwdStar e)            = KwdStar <$> norm env e
    norm env KwdNil                 = return KwdNil

instance Norm PosPat where
    norm env (PosPat p ps)          = PosPat <$> norm env p <*> norm env ps
    norm env (PosPatStar p)         = PosPatStar <$> norm env p
    norm env PosPatNil              = return PosPatNil

instance Norm KwdPat where
    norm env (KwdPat n p ps)        = KwdPat n <$> norm env p <*> norm env ps
    norm env (KwdPatStar p)         = KwdPatStar <$> norm env p
    norm env KwdPatNil              = return KwdPatNil

instance Norm Comp where
    norm env (CompFor l p e c)      = CompFor l <$> norm env p <*> norm env e <*> norm (define (envOf p) env) c
    norm env (CompIf l e c)         = CompIf l <$> normBool env e <*> norm env c
    norm env NoComp                 = return NoComp

instance Norm Elem where
    norm env (Elem e)               = Elem <$> norm env e
    norm env (Star e)               = Star <$> norm env e               -- TODO: eliminate here

instance Norm Assoc where
    norm env (Assoc k v)            = Assoc <$> norm env k <*> norm env v
    norm env (StarStar e)           = StarStar <$> norm env e           -- TODO: eliminate here


-- Convert function types ---------------------------------------------------------------------------------

convEnv env m (n, i)                = [(n, conv i)]


class Conv a where
    conv                            :: a -> a

instance (Conv a) => Conv [a] where
    conv                            = map conv

instance (Conv a) => Conv (Maybe a) where
    conv                            = fmap conv

instance (Conv a) => Conv (Name, a) where
    conv (n, x)                     = (n, conv x)

instance Conv NameInfo where
    conv (NAct q p k te doc)        = NAct q (joinRow p k) kwdNil (conv te) doc
    conv (NClass q ps te doc)       = NClass q (conv ps) (conv te) doc
    conv (NSig sc dec doc)          = NSig (conv sc) dec doc
    conv (NDef sc dec doc)          = NDef (conv sc) dec doc
    conv (NVar t)                   = NVar (conv t)
    conv (NSVar t)                  = NSVar (conv t)
    conv ni                         = ni

instance Conv WTCon where
    conv (w,c)                      = (w, conv c)

instance Conv TSchema where
    conv (TSchema l q t)            = TSchema l q (conv t)

instance Conv Type where
    conv (TFun l fx p k t)          = TFun l fx (joinRow p k) kwdNil (conv t)
    conv (TCon l c)                 = TCon l (conv c)
    conv (TTuple l p k)             = TTuple l (joinRow p k) kwdNil
    conv (TOpt l t)                 = TOpt l (conv t)
    conv (TRow l k n t r)           = TRow l PRow nWild (conv t) (conv r)
    conv (TStar l k r)              = TRow l PRow nWild (TTuple l (conv r) kwdNil) posNil
    conv (TNil l k)                 = TNil l PRow
    conv t                          = t

instance Conv TCon where
    conv (TC c ts)                  = TC c (conv ts)

joinRow (TRow l k n t p) r          = TRow l PRow nWild (conv t) (joinRow p r)
joinRow (TStar l k p) r             = TRow l PRow nWild (TTuple l (conv p) kwdNil) (conv r)
joinRow (TNil _ _) r                = conv r
-- To be removed:
joinRow p (TNil _ _)                = conv p
joinRow p r                         = error ("##### joinRow " ++ prstr p ++ "  AND  " ++ prstr r)
