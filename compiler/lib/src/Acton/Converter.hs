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
module Acton.Converter where

import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Prim
import Acton.Env
import Acton.Subst
import Acton.TypeM
import Acton.TypeEnv
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

pruneStmts xs (Signature l ns t d : ss)
  | null ns'                            = pruneStmts xs ss
  | otherwise                           = Signature l ns' t d : pruneStmts xs ss
  where ns'                             = ns `intersect` xs
pruneStmts xs (Decl l ds : ss)
  | null ds'                            = pruneStmts xs ss
  | otherwise                           = Decl l ds' : pruneStmts xs ss
  where ds'                             = filter ((`elem`xs) . dname) ds
pruneStmts xs (s@Assign{} : ss)
  | null ns                             = pruneStmts xs ss
  | otherwise                           = s : pruneStmts xs ss
  where ns                              = bound s `intersect` xs
pruneStmts xs (s : ss)                  = s : pruneStmts xs ss
pruneStmts xs []                        = []

pruneBody env n ss                      = pruneStmts xs ss
  where xs                              = directAttrs env n

convProtocol env n0 q ps0 eq wmap b     = mainClass : sibClasses
  where ps                              = trim ps0
        q1                              = qSelf' : noqual env q
        p0                              = TC (NoQ n0) $ map tVar $ qbound q
        t0                              = tCon $ convProto p0
        w0                              = witAttr (NoQ n0)
        main                            = head bases
        bases                           = [ convProto p | (ws,p) <- ps0, null (catRight ws) ] ++ [cValue]

        immsibs                         = [ (witAttr w, tCon $ convProto p, inherited ws0) | ([w],ws0,p) <- ps ]

        mainClass                       = --trace ("###  MRO for " ++ prstr n0 ++ ": " ++ prstrs ps0) $
                                          --trace ("  # SIBS for " ++ prstr n0 ++ ": " ++ prstrs [ sibName ws n0 | (ws,_,_,_,_) <- allsibs ]) $
                                          Class NoLoc n0 q1 bases mainClassBody
          where mainClassBody           = qsigs ++ psigs ++ Decl NoLoc [mainInit] : convStmts tSelf' eq1 (pruneBody env (NoQ n0) b)
                psigs                   = [ Signature NoLoc [n] (monotype t) Property | (n,t,False) <- immsibs ]
                mainInit                = Def NoLoc initKW [] mainParams KwdNIL (Just tNone) (mkBody mainInitBody) NoDec fxPure
                mainParams              = wit2par ((selfKW',tSelf) : qpars ++ [ (n,t) | (n,t,_) <- immsibs ]) PosNIL
                mainInitBody            = bindWits eq0 ++ initCall (tcargs main) mainArgs main ++ mainCopies
                mainArgs                = witArgs (tcname main) wmap ++ [ eVar n | (n,_,True) <- immsibs ]
                mainCopies              = qcopies ++ [ MutAssign NoLoc (eDot (eVar selfKW') n) (eVar n) | (n,t,False) <- immsibs ]
                eq0                     = Eqn (tvarWit tvSelf p0) t0 (eVar selfKW') : eq
                eq1                     = Eqn (tvarWit tvSelf p0) t0 (eVar selfKW') : qcopies' ++ eq

        allsibs                         = [ sib ws ws0 p | (ws,ws0,p) <- ps, not (null ws) ]
          where sib ws ws0 p            = (ws, tcname p, us, witArgs w0 wmap, inherited ws0)
                  where us              = us0 ++ us1
                        us1             = [ convProto p | (ws',p) <- ps0, catRight ws' == ws ] ++ [cValue]
                        us0             = [ TC (baseGName w) (tcargs $ head us1) | w <- zipWith (:) (wheads ws0) (wtails ws0) ]
                        w0              = if inherited ws0 then tcname main else tcname p

        sibClasses                      = [ Class NoLoc (sibName ws n0) q1 us (sibClassBody ws n (head us) wes inh) | (ws,n,us,wes,inh) <- allsibs ]

        sibClassBody ws n p wes inh     = qsigs ++ psigs ++ Decl NoLoc [sibInit] : convStmts tSelf' eq1 (pruneBody env n b)
          where sibInit                 = Def NoLoc initKW [] sibParams KwdNIL (Just tNone) (mkBody sibInitBody) NoDec fxPure
                sibParams               = wit2par ((selfKW',tSelf) : qpars ++ sibSubParams ++ sibCtxt) PosNIL
                sibCtxt                 = witCtxt ps ws ++ [(w0,t0)]
                sibInitBody             = bindWits eq0 ++ initCall (tcargs p) (wes ++ sibSubArgs ++ sibCtxtArgs) p ++ sibCopies
                sibCopies               = qcopies ++ [ MutAssign NoLoc (eDot (eVar selfKW') w0) (eVar w0) ]
                sibSubParams            = [ (witAttr (last ws'), tCon $ convProto p') | (ws',_,p') <- ps, truePrefix ws ws' ]
                sibSubArgs              = [ eVar (witAttr (last ws')) | (ws',_,p') <- ps, truePrefix ws ws' ]
                sibCtxtArgs             = (if inh then id else init) [ eVar n | (n,t) <- sibCtxt ]
                eq0                     = Eqn (tvarWit tvSelf p0) t0 (eVar w0) : eq
                eq1                     = Eqn (tvarWit tvSelf p0) t0 (eDot (eVar selfKW') w0) : qcopies' ++ eq

        qsigs                           = [ Signature NoLoc [qualAttr p v n0] (monotype $ impl2type (tVar v) p) Property | (v,p) <- quals env q ]
        psigs                           = [ Signature NoLoc [w0] (monotype t0) Property ]
        qpars                           = [ (tvarWit v p, impl2type (tVar v) p) | (v,p) <- quals env q ]
        qcopies                         = [ MutAssign NoLoc (eDot (eVar selfKW') $ qualAttr p v n0) (eVar $ tvarWit v p) | (v,p) <- quals env q ]
        qcopies'                        = [ Eqn (tvarWit v p) (impl2type (tVar v) p) (eDot (eVar selfKW') $ qualAttr p v n0) | (v,p) <- quals env q ]

inherited (Left _ : _)                  = True
inherited _                             = False

wtails (w:ws)
  | null ws'                            = []
  | otherwise                           = ws' : wtails ws
  where ws'                             = catRight ws

wheads (Right w:ws)
  | null $ catRight ws                  = []
  | otherwise                           = w : wheads ws
wheads (Left w:ws)                      = w : wheads ws

groupBranch ([],_) m                    = m
groupBranch p@(w:_,_) m                 = Map.insertWith (++) w [p] m

convExtension env n1 c0 q ps0 eq wmap b = mainClass : sibClasses
  where pss                             = Map.elems $ foldr groupBranch Map.empty ps0
        ps                              = trim ps0
        q1                              = noqual env q
        tvs                             = map tVar $ qbound q1
        t0                              = tCon c0
        w0                              = witAttr (tcname main)
        ts                              = tcargs main
        main                            = head bases
        bases                           = [ instProto t0 p | (ws,p) <- ps0, null (catRight ws) ] ++ [cValue]

        mainClass                       = --trace ("###  mro for " ++ prstr n1 ++ ": " ++ prstrs ps0) $
                                          --trace ("### branches for " ++ prstr n1 ++ "\n" ++ render (nest 4 $ vcat [ commaSep pretty ps1 | ps1 <- pss ])) $
                                          --trace ("  # sibs for " ++ prstr n1 ++ ": " ++ prstrs [ sibName ws n1 | (ws,_,_,_,_) <- allsibs ]) $
                                          Class NoLoc n1 q1 bases mainClassBody
          where mainClassBody           = qsigs ++ Decl NoLoc [mainInit] : convStmts t0 eq1 (pruneBody env (tcname main) b)
                mainInit                = Def NoLoc initKW [] mainParams KwdNIL (Just tNone) (mkBody mainInitBody) NoDec fxPure
                mainParams              = wit2par ((selfKW',tSelf) : qpars) PosNIL
                mainInitBody            = bindWits eq0 ++ initCall ts (witArgs (tcname main) wmap ++ sibSubs []) main ++ qcopies
                eq0                     = Eqn thisKW' (tCon main) (eVar selfKW') : eq
                eq1                     = Eqn thisKW' (tCon main) (eVar selfKW') : qcopies' ++ eq

        allsibs                         = [ sib ws ws0 p | (ws,ws0,p) <- ps, not (null ws) ]
          where sib ws ws0 p            = (ws, tcname p, us, witArgs w0 wmap, inherited ws0)
                  where us              = us0 ++ us1
                        us1             = [ instProto t0 p | (ws',p) <- ps0, catRight ws' == ws ] ++ [cValue]
                        us0             = [ TC (baseGName w) (tcargs $ head us1) | w <- zipWith (:) (wheads ws0) (wtails ws0) ]
                        w0              = if inherited ws0 then tcname main else tcname p

        sibClasses                      = [ Class NoLoc (sibName ws n1) q1 us (sibClassBody ws n (head us) wes inh) | (ws,n,us,wes,inh) <- allsibs ]

        sibClassBody ws n p wes inh     = qsigs ++ Decl NoLoc [sibInit] : convStmts t0 eq1 (pruneBody env n b)
          where sibInit                 = Def NoLoc initKW [] sibParams KwdNIL (Just tNone) (mkBody sibInitBody) NoDec fxPure
                sibParams               = wit2par ((selfKW',tSelf) : qpars ++ sibCtxt) PosNIL
                sibCtxt                 = witCtxt ps ws ++ [(w0,tCon main)]
                sibInitBody             = bindWits eq0 ++ initCall ts (wes ++ sibSubs ws ++ sibArgs ws) p ++ qcopies
                eq0                     = Eqn thisKW' (tCon main) (eVar  w0) : eq
                eq1                     = Eqn thisKW' (tCon main) (eDot (eVar selfKW') w0) : qcopies' ++ eq

        sibSubs ws                      = [ eCall (tApp (eVar $ sibName ws' n1) tvs) (qargs ++ eVar selfKW' : sibArgs ws) | (ws',_,p') <- ps, truePrefix ws ws' ]

        sibArgs []                      = []
        sibArgs ws                      = map eVar (map witAttr $ init ws) ++ [eVar w0]

        qsigs                           = [ Signature NoLoc [qualAttr p v n1] (monotype $ impl2type (tVar v) p) Property | (v,p) <- quals env q ]
        qpars                           = [ (tvarWit v p, impl2type (tVar v) p) | (v,p) <- quals env q ]
        qargs                           = [ eVar n | (n,p) <- qpars ]
        qcopies                         = [ MutAssign NoLoc (eDot (eVar selfKW') $ qualAttr p v n1) (eVar $ tvarWit v p) | (v,p) <- quals env q ]
        qcopies'                        = [ Eqn (tvarWit v p) (impl2type (tVar v) p) (eDot (eVar selfKW') $ qualAttr p v n1) | (v,p) <- quals env q ]


trim ps0                                = nubBy eqWit [ (catRight ws0, ws0, p) | (ws0,p) <- ps0 ]
  where eqWit (ws1,_,_) (ws2,_,_)       = ws1 == ws2

truePrefix pre ws
  | Just [_] <- stripPrefix pre ws      = True
  | otherwise                           = False

witCtxt ps ws                           = ctxt $ tail $ reverse ws
  where ctxt []                         = []
        ctxt (w:ws)                     = (witAttr w, fromJust $ lookup (w:ws) typemap) : ctxt ws
        typemap                         = [ (reverse ws, tCon $ convProto p) | (ws,_,p) <- ps ]

initCall ts args p
  | tcname p == qnValue                 = []
  | otherwise                           = [Expr NoLoc (eCall (tApp (eDot (eQVar (tcname p)) initKW) ts) (eVar selfKW' : args))]

witArgs w wmap                          = case lookup w wmap of
                                            Just es -> es
                                            Nothing -> []   -- must be 'value'
                                            -- Nothing -> trace ("##### wmap empty for " ++ prstrs ws) []

noqual env q                            = [ Quant v (filter (not . isProto env . tcname) us) | Quant v us <- q ]

quals env q                             = [ (v, p) | Quant v ps <- q, p <- ps, isProto env (tcname p) ]

qualAttr p v n                          = Derived (tvarWit v p) n

sibName ws n                            = Derived (baseName ws) n

baseName [w]                            = deriveQ w
baseName (w : ws)                       = Derived (baseName ws) (deriveQ w)

baseGName ws                            = modOf (head ws) $ baseName ws

modOf (NoQ _)                           = NoQ
modOf (QName m _)                       = QName m
modOf (GName m _)                       = GName m


convProto (TC n ts)                     = TC n (tSelf' : convSelf tSelf' ts)

instProto t (TC n ts)                   = TC n (t : convSelf t ts)

tvSelf'                                 = TV KType (Internal Typevar "S" 0)
tSelf'                                  = tVar tvSelf'
qSelf'                                  = Quant tvSelf' []
selfKW'                                 = Internal Witness "self" 0
thisKW'                                 = Internal Witness "this" 0

convSelf t0 t                           = subst [(tvSelf, t0)] t

convStmts t0 eq stmts                   = map conv stmts
  where conv (Signature l ns sc Static) = Signature l ns (convSelf t0 sc) NoDec
        conv (Signature l ns sc _)      = Signature l ns (convSelf t0 $ convS sc) NoDec
        conv (Decl l ds)                = Decl l (map convD ds)
        conv s                          = s
        convS (TSchema l q t)           = TSchema l (q) (convT t)
        convT (TFun l fx p k t)         = TFun l fx (posRow (tVar tvSelf) p) k t
        convT t                         = t
        convD (Def l n q p k t b _ x)   = Def l n (convSelf t0 q) (wit2par [(selfKW',tSelf)] $ convSelf t0 p) (convSelf t0 k) (convSelf t0 t) b' NoDec x
          where b'                      = bindWits eq ++ b
        convD d                         = d

-- Convert a TEnv -------------------------------------------------------------------------------------------

convEnvProtos env                       = mapModules conv env
  where
    conv env1 m (n, NDef sc d)          = [(n, NDef (convS sc) d)]
    conv env1 m (n, NSig sc d)          = [(n, NSig (convS sc) d)]
    conv env1 m (n, NAct q p k te)      = [(n, NAct (noqual env q) (qualWRow env q p) k (concat $ map (conv env m) te))]
    conv env1 m ni@(n, NProto q us te)  = map (fromClass env) $ convProtocol (define [ni] env) n q us [] [] (fromTEnv te)
    conv env1 m ni@(n, NExt q c us te)  = map (fromClass env) $ convExtension (define [ni] env) n c q us [] [] (fromTEnv te)
    conv env1 m (n, NClass q us te)     = [(n, NClass (noqual env q) us (convClassTEnv env q te))]
    conv env1 m ni                      = [ni]
    convS (TSchema l q t)               = TSchema l (noqual env q) (convT q t)
    convT q (TFun l x p k t)            = TFun l x (qualWRow env q p) k t
    convT q t                           = t

fromClass env (Class _ n q us b)        = (n, NClass q (leftpath us) (fromStmts env b))

fromStmts env (Signature _ ns sc d : ss)= [ (n, NSig (convS sc) d) | n <- ns ] ++ fromStmts env ss
  where convS (TSchema l q t)           = TSchema l (noqual env q) (convT q t)
        convT q (TFun l x p k t)        = TFun l x (qualWRow env q p) k t
        convT q t                       = t
fromStmts env (Decl _ ds : ss)          = fromDefs env ds ++ fromStmts env ss
fromStmts env (Assign _ [PVar _ n (Just t)] _ : ss)
                                        = (n, NVar t) : fromStmts env ss
fromStmts env (_ : ss)                  = fromStmts env ss
fromStmts env []                        = []

fromDefs env (Def l n q p k a _ d fx : ds)
                                        = (n, NDef (TSchema NoLoc q (TFun NoLoc fx (prowOf' p d) (krowOf k) (fromJust a))) d) : fromDefs env ds
  where prowOf' p Static                = prowOf p
        prowOf' (PosPar _ _ _ p) _      = prowOf p
fromDefs env (_ : ds)                   = fromDefs env ds
fromDefs env []                         = []

fromTEnv ((n, NSig sc dec) : te)        = Signature NoLoc [n] sc dec : fromTEnv te
fromTEnv ((n, NDef sc dec) : te)        = Decl NoLoc [def] : fromTEnv te
  where TSchema _ q (TFun _ fx p k t)   = sc
        def                             = Def NoLoc n q (pPar' dec p) (kPar attrKW k) (Just t) [sNotImpl] dec fx
        pPar' Static p                  = pPar pNames p
        pPar' _ p                       = pPar pNames (posRow tSelf p)
fromTEnv ((n, NVar t) : te)             = sAssign (pVar n t) eNotImpl : fromTEnv te
fromTEnv (_ : te)                       = fromTEnv te
fromTEnv []                             = []

convClassTEnv env q0 te                 = [ (n, conv i) | (n,i) <- te ]
  where conv (NSig sc dec)              = NSig (convS sc) dec
        conv (NDef sc dec)              = NDef (convS sc) dec
        conv i                          = i
        convS (TSchema l q t)           = TSchema l (noqual env q) (convT q t)
        convT q (TFun l x p k t)        = TFun l x (qualWRow env (q0++q) p) k t
        convT q t                       = t


{-

f : [A(Eq),B,C(Ord)] => (A,B) -> C                          f : [A,B,C] => (Eq[A],Ord[C],A,B) -> C

class c[A(Eq),B] (b[A]):                                    class c[A,B] (b[A]):
    f : [C(Ord)] => (A,B) -> C                                  f : [C] => (Eq[A],Ord[C],A,B) -> C
    @static                                                     @static
    g : [C(Ord)] => (Self,Self) -> C                            g : [C] => (Eq[A],Ord[C],Self,Self) -> C

protocol p[A(Eq),B] (q[A]):                                 class p[S,A,B] (q[S,A]):
    f : [C(Ord)] => (A,B) -> C                                  __init__ : (Eq[A]) -> None
    @static                                                     f : [C] => (S,Ord[C],A,B) -> C
    g : [C(Ord)] => (Self,Self) -> C                            g : [C] => (Ord[C],S,S) -> C

extension c[A(Eq),B] (p[A,B]):                              class p$c[A,B] (p[c[A,B],A,B]):
    ...                                                         __init__ : (Eq[A]) -> None

actor[ a[A(Eq),B] (b[A]):                                   class a[A,B] ($Actor[]):
    f : [C(Ord)] => action(A,B) -> C                            __init__ : (Eq[A],b[A]) -> None
                                                                f        : [C] => action(Ord[C],A,B) -> C
                                                                f$local  : [C] => proc(Ord[C],A,B) -> C

-}