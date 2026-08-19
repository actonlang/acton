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

{-# LANGUAGE FlexibleInstances #-}
module Acton.Names where

import Utils
import Acton.Syntax
import Acton.Builtin
import Debug.Trace
import qualified Data.Set as Set


isWitness (Internal Witness _ _)    = True
isWitness _                         = False

isInternal (Internal _ _ _)         = True
isInternal _                        = False

isUnboxed (Internal BoxPass _ _)    = True
isUnboxed _                         = False


self                                = Name NoLoc "self"

localName n                         = Derived n suffixLocal
newactName n                        = Derived n suffixNewact

selfKW'                             = Internal Witness "self" 0
thisKW'                             = Internal Witness "this" 0

g_self                              = globalName "self"

g_act                               = globalName "act"
g_skip                              = globalName "skip"

altInit                             = globalName "init"

suffixLocal                         = globalName "local"
suffixNewact                        = globalName "newact"
suffixClass                         = globalName "class"
suffixMethods                       = globalName "methods"
suffixNew                           = globalName "new"
suffixWitness                       = globalName "witness"

paramNames                          = globalNames ""
pNames                              = globalNames "p"
xNames                              = globalNames "x"
yNames                              = globalNames "y"
tmpNames                            = globalNames "tmp"

attrKW                              = globalName "kw"


deriveQ (NoQ n)                     = n
deriveQ (QName (ModName m) n)       = deriveMod n m
deriveQ (GName m n)
  | m == mBuiltin                   = n
deriveQ (GName (ModName m) n)       = deriveMod n m

deriveMod n0 []                     = n0
deriveMod n0 (n:m)                  = deriveMod (Derived n0 n) m

deriveT (TVar _ v)                  = tvname v
deriveT (TCon _ c)                  = deriveQ (tcname c)

witAttr qn                          = Internal Witness (nstr $ deriveQ qn) 0

extensionName [] c                  = Derived (globalName "ext") (deriveQ $ tcname c)
extensionName (p:_) c
  | length ts == length vs          = n0
  | otherwise                       = foldl Derived n0 (map deriveT ts)
  where ts                          = tcargs c
        vs                          = [ v | TVar _ v <- ts ]
        n0                          = Derived (deriveQ $ tcname p) (deriveQ $ tcname c)


-- Mutually recursive groups -------

declnames (Extension{} : ds)        = declnames ds
declnames (d : ds)                  = dname d : declnames ds
declnames []                        = []

dname' (Extension _ _ c us _ _)     = extensionName us c
dname' d                            = dname d

splitDeclGroup []                   = []
splitDeclGroup (d:ds)               = join $ split (free d) [d] ds
  where split vs ds0 []             = [reverse ds0]
        split vs ds0 (d:ds)
          | any (`elem` ws) vs      = split (free d++vs) (d:ds0) ds
          | otherwise               = reverse ds0 : split (free d) [d] ds
          where ws                  = declnames (d:ds)
        join []                     = []
        join dss
          | not $ null dss1         = concat dss1 : join dss2
          where (dss1,dss2)         = span (all tydecl) dss
                tydecl Def{}        = False
                tydecl _            = True
        join (ds:dss)               = ds : join dss


-- Data variables ------------------

class DataVars a where
    datavars                        :: Int -> a -> [Name]      -- Variables in any lhs data pattern, with minimum arity

instance DataVars a => DataVars [a] where
    datavars n                      = concatMap (datavars n)

instance DataVars Stmt where
    datavars n (Assign _ ps _)      = datavars n ps
    datavars n (Data _ p b)         = maybe [] (datavars n) p
    datavars n (While _ e b els)    = datavars n b ++ datavars n els
    datavars n (For _ p e b els)    = datavars n b ++ datavars n els
    datavars n (If _ bs els)        = concatMap (datavars n) bs ++ datavars n els
    datavars n _                    = []

instance DataVars Branch where
    datavars n (Branch e ss)        = datavars n ss

instance DataVars Pattern where
    datavars n (PTuple _ ps ks)     = bound ps ++ bound ks
    datavars n (PList _ ps p)       = bound ps ++ bound p
    datavars n (PParen _ p)         = bound p
    datavars n (PData _ v ixs)
      | length ixs >= n             = [v]
    datavars n _                    = []


-- Special attributes variables ----

methods b                           = [ n | Decl _ ds <- b, Def{dname=n} <- ds ]

statevars b                         = concat [ bound ps | VarAssign _ ps _ <- b ]


isHidden n@(Name _ str)             = length (takeWhile (=='_') str) == 1 || n == resumeKW || n == cleanupKW
isHidden _                          = True

isPrivateName                       :: Name -> Bool
isPrivateName n                     = case nstr n of
                                        ('_':_) -> True
                                        _       -> False

isPublicName                        :: Name -> Bool
isPublicName                        = not . isPrivateName


-- Free and bound names ------------

class Vars a where
    free                            :: a -> [Name]
    freeQ                           :: a -> [QName]
    bound                           :: a -> [Name]
    nmap                            :: (ModName->ModName) -> a -> a

    free x                          = free $ freeQ x
    freeQ x                         = []
    bound x                         = []
    nmap f                          = id

qns `diffQ` ns
  | hasMany ns                      = filter fSet qns
  | otherwise                       = filter fList qns
  where fList (NoQ n)               = n `notElem` ns
        fList _                     = True
        fSet (NoQ n)                = not (Set.member n boundNames)
        fSet _                      = True
        boundNames                  = Set.fromList ns
        hasMany xs                  = length (take 9 xs) > 8

instance Vars a => Vars [a] where
    free                            = concatMap free
    freeQ                           = concatMap freeQ
    bound                           = concatMap bound
    nmap f                          = map (nmap f)

instance Vars a => Vars (Maybe a) where
    free                            = maybe [] free
    freeQ                           = maybe [] freeQ
    bound                           = maybe [] bound
    nmap f                          = fmap (nmap f)

instance Vars Stmt where
    freeQ (Expr _ e)                = freeQ e
    freeQ (Assign _ ps e)           = freeQ ps ++ freeQ e
    freeQ (MutAssign _ t e)         = freeQ t ++ freeQ e
    freeQ (AugAssign _ t op e)      = freeQ t ++ freeQ e
    freeQ (Assert _ e mbe)          = freeQ e ++ freeQ mbe
    freeQ (Pass _)                  = []
    freeQ (Delete _ t)              = freeQ t
    freeQ (Return _ e)              = freeQ e
    freeQ (Raise _ e)               = freeQ e
    freeQ (Break _)                 = []
    freeQ (Continue _)              = []
    freeQ (If _ branches els)       = freeQ branches ++ freeQ els
    freeQ (While _ e b els)         = freeQ e ++ freeQ b ++ freeQ els
    freeQ (For _ p e b els)         = freeQ p ++ freeQ e ++ (freeQ b `diffQ` bound p) ++ freeQ els
    freeQ (Try _ b hs els fin)      = freeQ b ++ freeQ hs ++ freeQ els ++ freeQ fin
    freeQ (With _ items b)          = freeQ items ++ (freeQ b `diffQ` bound items)
    freeQ (Data _ p b)              = freeQ p ++ freeQ b
    freeQ (VarAssign _ ps e)        = freeQ ps ++ freeQ e
    freeQ (After _ e e')            = freeQ e ++ freeQ e'
    freeQ (Decl _ ds)               = freeQ ds `diffQ` bound ds
    freeQ (Signature _ ns t d)      = freeQ t

    bound (Assign _ ps _)           = bound ps
    bound (VarAssign _ ps e)        = bound ps
    bound (AugAssign _ t _ e)       = free t
    bound (Decl _ ds)               = bound ds
    bound (Signature _ ns t d)      = ns
    bound (If _ bs els)             = bound bs ++ bound els
    bound (While _ _ b els)         = bound b ++ bound els
    bound (With _ items b)          = bound b
    bound _                         = []


assigned stmts                      = concatMap assig stmts
  where assig (While _ e b els)     = assigned b ++ assigned els
        assig (For _ p e b els)     = assigned b ++ assigned els ++ bound p
        assig (With _ items b)      = assigned b ++ bound items
        assig (Try _ b hs els fin)  = assigned b ++ concat [ bound ex ++ assigned b | Handler ex b <- hs ] ++ assigned els ++ assigned fin
        assig (If _ bs els)         = concat [ assigned b | Branch _ b <- bs ] ++ assigned els
        assig (Assign _ ps _)       = bound ps
        assig (VarAssign _ ps _)    = bound ps
        assig s                     = bound s


instance Vars Decl where
    freeQ (Def _ n q ps ks t b d fx _)
                                    = (freeQ ps ++ freeQ ks ++ freeQ b ++ freeQ fx) `diffQ` (bound q ++ bound ps ++ bound ks ++ assigned b)
    freeQ (Actor _ n q ps ks b _)   = (freeQ ps ++ freeQ ks ++ freeQ b) `diffQ` (self : bound q ++ bound ps ++ bound ks ++ assigned b)
    freeQ (Class _ n q cs b _)      = (freeQ cs ++ freeQ b) `diffQ` (bound q ++ assigned b)
    freeQ (Protocol _ n q ps b _)   = (freeQ ps ++ freeQ b) `diffQ` (bound q ++ assigned b)
    freeQ (Typedef _ n q t _)       = freeQ t `diffQ` bound q
    freeQ (Extension _ q c ps b _)  = (freeQ c ++ freeQ ps ++ freeQ b) `diffQ` (bound q ++ assigned b)

    bound (Def _ n _ _ _ _ _ _ _ _) = [n]
    bound (Actor _ n _ _ _ _ _)     = [n]
    bound (Class _ n _ _ _ _)       = [n]
    bound (Protocol _ n _ _ _ _)    = [n]
    bound (Typedef _ n _ _ _)       = [n]
    bound (Extension _ _ _ _ _ _)   = []

instance Vars Branch where
    freeQ (Branch e ss)             = freeQ e ++ freeQ ss
    bound (Branch e ss)             = bound ss

instance Vars Handler where
    freeQ (Handler ex ss)           = freeQ ex ++ (freeQ ss `diffQ` bound ex)
    bound (Handler ex ss)           = bound ss ++ bound ex

instance Vars Expr where
    freeQ (Var _ n)                 = [n]
    freeQ (Int _ _ str)             = []
    freeQ (Float _ _ str)           = []
    freeQ (Imaginary _ _ str)       = []
    freeQ (Bool _ v)                = []
    freeQ (None _)                  = []
    freeQ (NotImplemented _)        = []
    freeQ (Ellipsis _)              = []
    freeQ (Strings _ ss)            = []
    freeQ (BStrings _ ss)           = []
    freeQ (Call _ e ps ks)          = freeQ e ++ freeQ ps ++ freeQ ks
    freeQ (TApp _ e ts)             = freeQ e ++ freeQ ts
    freeQ (Let _ ss e)              = freeQ ss ++ (freeQ e `diffQ` bound ss)
    freeQ (Async _ e)               = freeQ e
    freeQ (Await _ e)               = freeQ e
    freeQ (Index _ e ix)            = freeQ e ++ freeQ ix
    freeQ (Slice _ e sl)            = freeQ e ++ freeQ sl
    freeQ (Cond _ e1 e e2)          = freeQ [e1,e,e2]
    freeQ (IsInstance _ e c)        = freeQ e ++ freeQ c
    freeQ (BinOp _ e1 o e2)         = freeQ [e1,e2]
    freeQ (CompOp _ e ops)          = freeQ e ++ freeQ ops
    freeQ (UnOp _ o e)              = freeQ e
    freeQ (Dot _ e n)               = freeQ e  
    freeQ (Opt _ e _)               = freeQ e
    freeQ (OptChain _ e)            = freeQ e
    freeQ (Rest _ e n)              = freeQ e
    freeQ (DotI _ e i)              = freeQ e
    freeQ (RestI _ e i)             = freeQ e
    freeQ (Lambda _ ps ks e fx)     = freeQ ps ++ freeQ ks ++ (freeQ e `diffQ` (bound ps ++ bound ks))
    freeQ (Yield _ e)               = freeQ e
    freeQ (YieldFrom _ e)           = freeQ e
    freeQ (Tuple _ ps ks)           = freeQ ps ++ freeQ ks
    freeQ (List _ es)               = freeQ es
    freeQ (ListComp _ e co)         = (freeQ e `diffQ` bound co) ++ freeQ co
    freeQ (Dict _ es)               = freeQ es
    freeQ (DictComp _ e co)         = (freeQ e `diffQ` bound co) ++ freeQ co
    freeQ (Set _ es)                = freeQ es
    freeQ (SetComp _ e co)          = (freeQ e `diffQ` bound co) ++ freeQ co
    freeQ (Paren _ e)               = freeQ e
    freeQ (UnBox t e)               = freeQ e
    freeQ (Box t e)                 = freeQ e

instance Vars Name where
    free n                          = [n]

instance Vars ModName where
    free (ModName (n:ns))           = [n]

instance Vars QName where
    free (QName m n)                = free m
    free (NoQ n)                    = free n
    free (GName m n)                = free m
    
    freeQ n                         = [n]

    nmap f (QName m n)              = QName (f m) n
    nmap f (NoQ n)                  = NoQ n
    nmap f (GName m n)              = GName (f m) n

instance Vars Except where
    freeQ (ExceptAll _)             = []
    freeQ (Except _ x)              = freeQ x
    freeQ (ExceptAs _ x n)          = freeQ x

    bound (ExceptAll _)             = []
    bound (Except _ x)              = []
    bound (ExceptAs _ x n)          = [n]

instance Vars PosPar where
    freeQ (PosPar n t e p)          = freeQ t ++ freeQ e ++ freeQ p
    freeQ (PosSTAR n t)             = freeQ t
    freeQ PosNIL                    = []

    bound (PosPar n t e p)          = n : bound p
    bound (PosSTAR n t)             = [n]
    bound PosNIL                    = []

instance Vars KwdPar where
    freeQ (KwdPar n t e k)          = freeQ t ++ freeQ e ++ freeQ k
    freeQ (KwdSTAR n t)             = freeQ t
    freeQ KwdNIL                    = []

    bound (KwdPar n t e k)          = n : bound k
    bound (KwdSTAR n t)             = [n]
    bound KwdNIL                    = []

instance Vars (PosPar,KwdPar) where
    freeQ (ppar,kpar)               = freeQ ppar ++ freeQ kpar

    bound (ppar,kpar)               = bound ppar ++ bound kpar

instance Vars Elem where
    freeQ (Elem e)                  = freeQ e
    freeQ (Star e)                  = freeQ e

    bound (Elem p)                  = bound p
    bound (Star p)                  = bound p

instance Vars Assoc where
    freeQ (Assoc k v)               = freeQ k ++ freeQ v
    freeQ (StarStar e)              = freeQ e

instance Vars WithItem where
    freeQ (WithItem e p)            = freeQ e ++ freeQ p

    bound (WithItem e p)            = bound p

instance Vars PosArg where
    freeQ (PosArg e p)              = freeQ e ++ freeQ p
    freeQ (PosStar e)               = freeQ e
    freeQ PosNil                    = []

instance Vars KwdArg where
    freeQ (KwdArg n e k)            = freeQ e ++ freeQ k
    freeQ (KwdStar e)               = freeQ e
    freeQ KwdNil                    = []

instance Vars OpArg where
    freeQ (OpArg o e)               = freeQ e

instance Vars Sliz where
    freeQ (Sliz _ e1 e2 e3)         = freeQ e1 ++ freeQ e2 ++ freeQ e3

instance Vars Comp where
    freeQ (CompFor _ pat e c)       = (freeQ e ++ freeQ c) `diffQ` bound pat
    freeQ (CompIf _ e c)            = freeQ e ++ freeQ c
    freeQ NoComp                    = []

    bound (CompFor _ pat e c)       = bound pat ++ bound c
    bound (CompIf _ e c)            = bound c
    bound NoComp                    = []

instance Vars PosPat where
    freeQ (PosPat p ps)             = freeQ p ++ freeQ ps
    freeQ (PosPatStar p)            = freeQ p
    freeQ PosPatNil                 = []

    bound (PosPat p ps)             = bound p ++ bound ps
    bound (PosPatStar p)            = bound p
    bound PosPatNil                 = []

instance Vars KwdPat where
    freeQ (KwdPat n p ps)           = freeQ p ++ freeQ ps
    freeQ (KwdPatStar p)            = freeQ p
    freeQ KwdPatNil                 = []

    bound (KwdPat n p ps)           = bound p ++ bound ps
    bound (KwdPatStar p)            = bound p
    bound KwdPatNil                 = []

instance Vars Pattern where
    freeQ (PWild _ _)               = []
    freeQ (PVar _ n a)              = []
    freeQ (PTuple _ ps ks)          = freeQ ps ++ freeQ ks
    freeQ (PList _ ps p)            = freeQ ps ++ freeQ p
    freeQ (PParen _ p)              = freeQ p
    freeQ (PData _ n ixs)           = freeQ ixs

    bound (PWild _ _)               = []
    bound (PVar _ n _)              = [n]
    bound (PTuple _ ps ks)          = bound ps ++ bound ks
    bound (PList _ ps p)            = bound ps ++ bound p
    bound (PParen _ p)              = bound p
    bound (PData _ n ixs)           = [n]

-- Env-threaded reference walks ----

-- A Walk is the hook record for one env-threaded traversal of the typed AST.
-- The structural recursion is written once, in the Summ instances below; a
-- client starts from plainWalk and overrides only the hooks its analysis
-- needs.  The env and result types are parameters, so clients thread
-- environments this module knows nothing about.  Env hooks advance the
-- environment at binder points; result hooks contribute extra facts at
-- reference points.  wDot owns its receiver: the fold does not descend into
-- it, so the hook decides how (and whether) the receiver is walked.

data Walk env r                     = Walk {
                                        wSeq       :: env -> Stmt -> env,             -- past one suite statement
                                        wSuiteEnv  :: env -> Suite -> env,            -- past a whole suite (try/else)
                                        wDecls     :: env -> [Decl] -> env,           -- into a mutually recursive group
                                        wDecl      :: env -> Decl -> (r, env),        -- decl header facts + body env
                                        wLocal     :: env -> env,                     -- into a local scope
                                        wLet       :: env -> Suite -> env,            -- past a let suite
                                        wPar       :: env -> Name -> Maybe Type -> env,
                                        wPat       :: env -> Pattern -> env,          -- loop/comprehension binder
                                        wItem      :: env -> WithItem -> env,
                                        wExcept    :: env -> Except -> env,
                                        wQBinds    :: env -> QBinds -> env,
                                        wAssignRhs :: env -> [Pattern] -> env,        -- into an assignment rhs
                                        wVar       :: env -> QName -> r,
                                        wDot       :: env -> Expr -> Name -> r,       -- owns the receiver
                                        wCall      :: env -> Expr -> r,               -- extra facts for a callee
                                        wCond      :: env -> Expr -> r,               -- extra facts for a condition
                                        wIter      :: env -> Expr -> r,               -- extra facts for an iteratee
                                        wTarg      :: env -> Pattern -> r,            -- extra facts for a target
                                        wTCon      :: env -> QName -> r,              -- type constructor reference
                                        wTypeName  :: env -> QName -> r }             -- type name in a dynamic check

plainWalk                           :: Monoid r => Walk env r
plainWalk                           = Walk {
                                        wSeq       = const,
                                        wSuiteEnv  = const,
                                        wDecls     = const,
                                        wDecl      = \env _ -> (mempty, env),
                                        wLocal     = id,
                                        wLet       = const,
                                        wPar       = \env _ _ -> env,
                                        wPat       = const,
                                        wItem      = const,
                                        wExcept    = const,
                                        wQBinds    = const,
                                        wAssignRhs = const,
                                        wVar       = none,
                                        wDot       = \_ _ _ -> mempty,
                                        wCall      = none,
                                        wCond      = none,
                                        wIter      = none,
                                        wTarg      = none,
                                        wTCon      = none,
                                        wTypeName  = none }
  where none _ _                    = mempty

class Summ a where
    summ                            :: Monoid r => Walk env r -> env -> a -> r

summSuite                           :: Monoid r => Walk env r -> env -> Suite -> r
summSuite w env []                  = mempty
summSuite w env (s:ss)              = summ w env s <> summSuite w (wSeq w env s) ss

summWithItems                       :: Monoid r => Walk env r -> env -> [WithItem] -> (r, env)
summWithItems w env []              = (mempty, env)
summWithItems w env (item:items)    = (summ w env item <> more, env')
  where (more, env')                = summWithItems w (wItem w env item) items

summPosPar                          :: Monoid r => Walk env r -> env -> PosPar -> (r, env)
summPosPar w env (PosPar n t e p)   = (summ w env t <> summ w env e <> more, env')
  where (more, env')                = summPosPar w (wPar w env n t) p
summPosPar w env (PosSTAR n t)      = (summ w env t, wPar w env n t)
summPosPar w env PosNIL             = (mempty, env)

summKwdPar                          :: Monoid r => Walk env r -> env -> KwdPar -> (r, env)
summKwdPar w env (KwdPar n t e k)   = (summ w env t <> summ w env e <> more, env')
  where (more, env')                = summKwdPar w (wPar w env n t) k
summKwdPar w env (KwdSTAR n t)      = (summ w env t, wPar w env n t)
summKwdPar w env KwdNIL             = (mempty, env)

summComp                            :: Monoid r => Walk env r -> env -> Comp -> (r, env)
summComp w env (CompFor _ p e c)    = (summ w env e <> wIter w env e <> summ w env p <> more, env')
  where (more, env')                = summComp w (wPat w env p) c
summComp w env (CompIf _ e c)       = (summ w env e <> wCond w env e <> more, env')
  where (more, env')                = summComp w env c
summComp w env NoComp               = (mempty, env)

instance Summ a => Summ [a] where
    summ w env                      = mconcat . map (summ w env)

instance Summ a => Summ (Maybe a) where
    summ w env                      = maybe mempty (summ w env)

instance Summ Stmt where
    summ w env (Expr _ e)           = summ w env e
    summ w env (Assign _ ps e)      = mconcat [ summ w env p <> wTarg w env p | p <- ps ] <>
                                      summ w (wAssignRhs w env ps) e
    summ w env (MutAssign _ t e)    = summ w env t <> summ w env e
    summ w env (AugAssign _ t _ e)  = summ w env t <> summ w env e
    summ w env (Assert _ e mbe)     = summ w env e <> wCond w env e <> summ w env mbe
    summ w env (Pass _)             = mempty
    summ w env (Delete _ t)         = summ w env t
    summ w env (Return _ mbe)       = summ w env mbe
    summ w env (Raise _ e)          = summ w env e
    summ w env (Break _)            = mempty
    summ w env (Continue _)         = mempty
    summ w env (If _ bs els)        = summ w env bs <> summSuite w env els
    summ w env (While _ e b els)    = summ w env e <> wCond w env e <> summSuite w env b <>
                                      summSuite w env els
    summ w env (For _ p e b els)    = summ w env p <> wTarg w env p <> summ w env e <> wIter w env e <>
                                      summSuite w (wPat w env p) b <> summSuite w env els
    summ w env (Try _ b hs els fin) = summSuite w env b <> summ w env hs <>
                                      summSuite w (wSuiteEnv w env b) els <> summSuite w env fin
    summ w env (With _ items b)     = itemRefs <> summSuite w env' b
      where (itemRefs, env')        = summWithItems w env items
    summ w env (Data _ mbp b)       = summ w env mbp <> summSuite w env b
    summ w env (VarAssign _ ps e)   = mconcat [ summ w env p <> wTarg w env p | p <- ps ] <> summ w env e
    summ w env (After _ e e')       = summ w env e <> summ w env e'
    summ w env (Signature _ _ sc _) = summ w env sc
    summ w env (Decl _ ds)          = mconcat [ decl d | d <- ds ]
      where env'                    = wDecls w env ds
            decl d                  = hdr <> summSuite w benv (declbody d)
              where (hdr, benv)     = wDecl w env' d

instance Summ Branch where
    summ w env (Branch e ss)        = summ w env e <> wCond w env e <> summSuite w env ss

instance Summ Handler where
    summ w env (Handler ex ss)      = summ w env ex <> summSuite w (wExcept w env ex) ss

instance Summ Except where
    summ w env (ExceptAll _)        = mempty
    summ w env (Except _ qn)        = wTypeName w env qn
    summ w env (ExceptAs _ qn _)    = wTypeName w env qn

instance Summ WithItem where
    summ w env (WithItem e p)       = summ w env e <> summ w env p

instance Summ Expr where
    summ w env (Var _ n)            = wVar w env n
    summ w env (Int _ _ _)          = mempty
    summ w env (Float _ _ _)        = mempty
    summ w env (Imaginary _ _ _)    = mempty
    summ w env (Bool _ _)           = mempty
    summ w env (None _)             = mempty
    summ w env (NotImplemented _)   = mempty
    summ w env (Ellipsis _)         = mempty
    summ w env (Strings _ _)        = mempty
    summ w env (BStrings _ _)       = mempty
    summ w env (Call _ f ps ks)     = summ w env f <> summ w env ps <> summ w env ks <> wCall w env f
    summ w env (Let _ ss e)         = summSuite w env' ss <> summ w (wLet w env' ss) e
      where env'                    = wLocal w env
    summ w env (TApp _ f ts)        = summ w env f <> summ w env ts
    summ w env (Async _ e)          = summ w env e
    summ w env (Await _ e)          = summ w env e
    summ w env (Index _ e ix)       = summ w env e <> summ w env ix
    summ w env (Slice _ e sl)       = summ w env e <> summ w env sl
    summ w env (Cond _ e1 c e2)     = summ w env e1 <> summ w env c <> wCond w env c <> summ w env e2
    summ w env (IsInstance _ e c)   = summ w env e <> wTypeName w env c
    summ w env (BinOp _ l _ r)      = summ w env l <> summ w env r
    summ w env (CompOp _ e ops)     = summ w env e <> summ w env ops
    summ w env (UnOp _ Not e)       = summ w env e <> wCond w env e
    summ w env (UnOp _ _ e)         = summ w env e
    summ w env (Dot _ e n)          = wDot w env e n
    summ w env (Rest _ e _)         = summ w env e
    summ w env (DotI _ e _)         = summ w env e
    summ w env (RestI _ e _)        = summ w env e
    summ w env (Opt _ e _)          = summ w env e
    summ w env (OptChain _ e)       = summ w env e
    summ w env (Lambda _ p k e fx)  = parRefs <> kwdRefs <> summ w benv e <> summ w env fx
      where (parRefs, envP)         = summPosPar w (wLocal w env) p
            (kwdRefs, benv)         = summKwdPar w envP k
    summ w env (Yield _ mbe)        = summ w env mbe
    summ w env (YieldFrom _ e)      = summ w env e
    summ w env (Tuple _ ps ks)      = summ w env ps <> summ w env ks
    summ w env (List _ es)          = summ w env es
    summ w env (ListComp _ e c)     = refs <> summ w env' e
      where (refs, env')            = summComp w (wLocal w env) c
    summ w env (Dict _ as)          = summ w env as
    summ w env (DictComp _ a c)     = refs <> summ w env' a
      where (refs, env')            = summComp w (wLocal w env) c
    summ w env (Set _ es)           = summ w env es
    summ w env (SetComp _ e c)      = refs <> summ w env' e
      where (refs, env')            = summComp w (wLocal w env) c
    summ w env (Paren _ e)          = summ w env e
    summ w env (Box t e)            = summ w env t <> summ w env e
    summ w env (UnBox t e)          = summ w env t <> summ w env e

instance Summ Elem where
    summ w env (Elem e)             = summ w env e
    summ w env (Star e)             = summ w env e

instance Summ Assoc where
    summ w env (Assoc k v)          = summ w env k <> summ w env v
    summ w env (StarStar e)         = summ w env e

instance Summ OpArg where
    summ w env (OpArg _ e)          = summ w env e

instance Summ Sliz where
    summ w env (Sliz _ e1 e2 e3)    = summ w env e1 <> summ w env e2 <> summ w env e3

instance Summ PosArg where
    summ w env (PosArg e p)         = summ w env e <> summ w env p
    summ w env (PosStar e)          = summ w env e
    summ w env PosNil               = mempty

instance Summ KwdArg where
    summ w env (KwdArg _ e k)       = summ w env e <> summ w env k
    summ w env (KwdStar e)          = summ w env e
    summ w env KwdNil               = mempty

instance Summ PosPat where
    summ w env (PosPat p ps)        = summ w env p <> summ w env ps
    summ w env (PosPatStar p)       = summ w env p
    summ w env PosPatNil            = mempty

instance Summ KwdPat where
    summ w env (KwdPat _ p ps)      = summ w env p <> summ w env ps
    summ w env (KwdPatStar p)       = summ w env p
    summ w env KwdPatNil            = mempty

instance Summ Pattern where
    summ w env (PWild _ t)          = summ w env t
    summ w env (PVar _ _ t)         = summ w env t
    summ w env (PParen _ p)         = summ w env p
    summ w env (PTuple _ ps ks)     = summ w env ps <> summ w env ks
    summ w env (PList _ ps p)       = summ w env ps <> summ w env p
    summ w env (PData _ n ixs)      = wTypeName w env (NoQ n) <> summ w env ixs

instance Summ TSchema where
    summ w env (TSchema _ q t)      = summ w env q <> summ w (wQBinds w env q) t

instance Summ QBind where
    summ w env (QBind _ cs)         = summ w env cs

instance Summ TCon where
    summ w env (TC qn ts)           = wTCon w env qn <> summ w env ts

instance Summ Type where
    summ w env (TCon _ tc)          = summ w env tc
    summ w env (TFun _ fx p k t)    = summ w env fx <> summ w env p <> summ w env k <> summ w env t
    summ w env (TTuple _ p k)       = summ w env p <> summ w env k
    summ w env (TOpt _ t)           = summ w env t
    summ w env (TRow _ _ _ t r)     = summ w env t <> summ w env r
    summ w env (TStar _ _ r)        = summ w env r
    summ w env (TUnboxed _ t)       = summ w env t
    summ w env _                    = mempty


instance Vars ModuleItem where
    bound (ModuleItem qn Nothing)   = free qn
    bound (ModuleItem qn (Just n))  = free n

instance Vars ImportItem where
    free (ImportItem n1 as)         = []

    bound (ImportItem n Nothing)    = free n
    bound (ImportItem n (Just as))  = free as

instance Vars TSchema where
    freeQ (TSchema _ q t)           = freeQ q ++ freeQ t

    nmap f (TSchema l q t)          = TSchema l (nmap f q) (nmap f t)

instance Vars TVar where
    freeQ (TV k v)                  = []

instance Vars TUni where
    freeQ (UV k l v)                = []

instance Vars TCon where
    freeQ (TC n ts)                 = freeQ n ++ freeQ ts

    nmap f (TC n ts)                = TC (nmap f n) (nmap f ts)

instance Vars QBind where
    freeQ (QBind v cs)              = freeQ cs

    nmap f (QBind v cs)             = QBind v (nmap f cs)

instance Vars Type where
    freeQ (TVar _ v)                = freeQ v
    freeQ (TUni _ u)                = freeQ u
    freeQ (TFun _ es p k t)         = freeQ es ++ freeQ p ++ freeQ k ++ freeQ t
    freeQ (TTuple _ p k)            = freeQ p ++ freeQ k
    freeQ (TOpt _ t)                = freeQ t
    freeQ (TCon  _ c)               = freeQ c
    freeQ (TRow _ _ _ t r)          = freeQ t ++ freeQ r
    freeQ (TStar _ _ r)             = freeQ r
    freeQ (TUnboxed _ t)            = freeQ t
    freeQ _                         = []

    nmap f (TCon l c)               = TCon l (nmap f c)
    nmap f (TFun l fx p k t)        = TFun l (nmap f fx) (nmap f p) (nmap f k) (nmap f t)
    nmap f (TTuple l p k)           = TTuple l (nmap f p) (nmap f k)
    nmap f (TOpt l t)               = TOpt l (nmap f t)
    nmap f (TRow l k n t r)         = TRow l k n (nmap f t) (nmap f r)
    nmap f (TStar l k r)            = TStar l k (nmap f r)
    nmap f (TUnboxed l t)           = TUnboxed l (nmap f t)
    nmap f t                        = t
