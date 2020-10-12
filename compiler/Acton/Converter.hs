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

pruneStmts xs (Signature l ns t d : ss)
  | null ns'                            = pruneStmts xs ss
  | otherwise                           = Signature l ns' t d : pruneStmts xs ss
  where ns'                             = ns `intersect` xs
pruneStmts xs (Decl l ds : ss)
  | null ds'                            = pruneStmts xs ss
  | otherwise                           = Decl l ds' : pruneStmts xs ss
  where ds'                             = filter ((`elem`xs) . dname) ds
pruneStmts xs (s : ss)                  = s : pruneStmts xs ss
pruneStmts xs []                        = []

pruneDefs env n ss                      = pruneStmts xs ss
  where xs                              = directAttrs env n

convProtocol env n0 q ps0 eq wmap b     = mainClass : sibClasses
  where ps                              = trim ps0
        q1                              = qSelf' : noqual env q
        p0                              = TC (NoQ n0) $ map tVar $ tybound q
        t0                              = tCon $ convProto p0
        w0                              = witAttr (NoQ n0)
        main                            = [ convProto p | ([],p,_) <- ps ]         -- may be empty!

        immsibs                         = [ (witAttr w, tCon $ convProto p, inh) | ([w],p,inh) <- ps ]

        mainClass                       = Class NoLoc n0 q1 main mainClassBody
          where mainClassBody           = qsigs ++ psigs ++ Decl NoLoc [mainInit] : pruneDefs env (NoQ n0) (convStmts eq1 b)
                psigs                   = [ Signature NoLoc [n] (monotype t) Property | (n,t,False) <- immsibs ]
                mainInit                = Def NoLoc initKW [] mainParams KwdNIL (Just tNone) (mkBody mainInitBody) NoDec fxPure
                mainParams              = wit2par ((selfKW',tSelf) : qpars ++ [ (n,t) | (n,t,_) <- immsibs ]) PosNIL
                mainInitBody            = bindWits eq0 ++ map (initCall (tcargs $ head main) mainArgs) main ++ mainCopies
                mainArgs                = witArgs (map tcname main) wmap ++ [ eVar n | (n,_,True) <- immsibs ]
                mainCopies              = qcopies ++ [ MutAssign NoLoc (eDot (eVar selfKW') n) (eVar n) | (n,t,False) <- immsibs ]
                eq0                     = (tvarWit tvSelf p0, t0, eVar selfKW') : eq
                eq1                     = (tvarWit tvSelf p0, t0, eVar selfKW') : qcopies' ++ eq

        allsibs                         = [ (ws, tcname p, sibBase ws p inh, witArgs (path ws inh) wmap, inh) | (ws,p,inh) <- ps, not (null ws) ]
          where sibBase ws p inh        = TC (modOf (tcname p) $ baseName (path ws inh)) (tVar tvSelf' : tcargs p)
                path ws inh             = if inh then tcname (head main) : ws else ws

        sibClasses                      = [ Class NoLoc (sibName ws n0) q1 [p] (sibClassBody ws n p wes inh) | (ws,n,p,wes,inh) <- allsibs ]

        sibClassBody ws n p wes inh     = qsigs ++ psigs ++ Decl NoLoc [sibInit] : pruneDefs env n (convStmts eq1 b)
          where psigs                   = [ Signature NoLoc [w0] (monotype t0) Property ]
                sibInit                 = Def NoLoc initKW [] sibParams KwdNIL (Just tNone) (mkBody sibInitBody) NoDec fxPure
                sibParams               = wit2par ((selfKW',tSelf) : qpars ++ sibSubParams ++ sibCtxt) PosNIL
                sibCtxt                 = witCtxt ps ws ++ [(w0,t0)]
                sibInitBody             = bindWits eq0 ++ [initCall (tcargs p) (wes ++ sibSubArgs ++ sibCtxtArgs) p] ++ sibCopies
                sibCopies               = qcopies ++ [ MutAssign NoLoc (eDot (eVar selfKW') w0) (eVar w0) ]
                sibSubParams            = [ (witAttr (last ws'), tCon $ convProto p') | (ws',p',_) <- ps, truePrefix ws ws' ]
                sibSubArgs              = [ eVar (witAttr (last ws')) | (ws',p',_) <- ps, truePrefix ws ws' ]
                sibCtxtArgs             = (if inh then id else init) [ eVar n | (n,t) <- sibCtxt ]
                eq0                     = (tvarWit tvSelf p0, t0, eVar w0) : eq
                eq1                     = (tvarWit tvSelf p0, t0, eDot (eVar selfKW') w0) : qcopies' ++ eq

        qsigs                           = [ Signature NoLoc [qualAttr p v n0] (monotype $ impl2type (tVar v) p) Property | (v,p) <- quals env q ]
        qpars                           = [ (tvarWit v p, impl2type (tVar v) p) | (v,p) <- quals env q ]
        qcopies                         = [ MutAssign NoLoc (eDot (eVar selfKW') $ qualAttr p v n0) (eVar $ tvarWit v p) | (v,p) <- quals env q ]
        qcopies'                        = [ (tvarWit v p, impl2type (tVar v) p, eDot (eVar selfKW') $ qualAttr p v n0) | (v,p) <- quals env q ]

convExtension env n1 n0 q ps0 eq wmap b = mainClass : sibClasses
  where ps                              = trim ps0
        q1                              = noqual env q
        tvs                             = map tVar $ tybound q1
        t0                              = tCon (TC n0 (map tVar $ tybound q))
        w0                              = witAttr (tcname main)
        ts                              = tcargs main
        main                            = head [ instProto t0 p | ([],p,_) <- ps ]       -- never empty!

        mainClass                       = Class NoLoc n1 q1 [main] mainClassBody
          where mainClassBody           = qsigs ++ Decl NoLoc [mainInit] : pruneDefs env (tcname main) (convStmts eq1 b)
                mainInit                = Def NoLoc initKW [] mainParams KwdNIL (Just tNone) (mkBody mainInitBody) NoDec fxPure
                mainParams              = wit2par ((selfKW',tSelf) : qpars) PosNIL
                mainInitBody            = bindWits eq0 ++ initCall ts (witArgs [tcname main] wmap ++ sibSubs []) main : qcopies
                eq0                     = (thisKW', tCon main, eVar selfKW') : eq
                eq1                     = (thisKW', tCon main, eVar selfKW') : qcopies' ++ eq

        allsibs                         = [ (ws, tcname p, sibBase ws p inh, witArgs (path ws inh) wmap, inh) | (ws,p,inh) <- ps, not (null ws) ]
          where sibBase ws p inh        = TC (modOf (tcname p) $ baseName (path ws inh)) ts
                path ws inh             = if inh then tcname main : ws else ws

        sibClasses                      = [ Class NoLoc (sibName ws n1) q1 [p] (sibClassBody ws n p wes inh) | (ws,n,p,wes,inh) <- allsibs ]

        sibClassBody ws n p wes inh     = qsigs ++ Decl NoLoc [sibInit] : pruneDefs env n (convStmts eq1 b)
          where sibInit                 = Def NoLoc initKW [] sibParams KwdNIL (Just tNone) (mkBody sibInitBody) NoDec fxPure
                sibParams               = wit2par ((selfKW',tSelf) : qpars ++ sibCtxt) PosNIL
                sibCtxt                 = witCtxt ps ws ++ [(w0,tCon main)]
                sibInitBody             = bindWits eq0 ++ [initCall ts (wes ++ sibSubs ws ++ sibArgs ws) p] ++ qcopies
                eq0                     = (thisKW', tCon main, eVar  w0) : eq
                eq1                     = (thisKW', tCon main, eDot (eVar selfKW') w0) : qcopies' ++ eq

        sibSubs ws                      = [ eCall (tApp (eVar $ sibName ws' n1) tvs) (qargs ++ eVar selfKW' : sibArgs ws) | (ws',p',inh) <- ps, truePrefix ws ws' ]

        sibArgs []                      = []
        sibArgs ws                      = map eVar (map witAttr $ init ws) ++ [eVar w0]

        qsigs                           = [ Signature NoLoc [qualAttr p v n1] (monotype $ impl2type (tVar v) p) Property | (v,p) <- quals env q ]
        qpars                           = [ (tvarWit v p, impl2type (tVar v) p) | (v,p) <- quals env q ]
        qargs                           = [ eVar n | (n,p) <- qpars ]
        qcopies                         = [ MutAssign NoLoc (eDot (eVar selfKW') $ qualAttr p v n1) (eVar $ tvarWit v p) | (v,p) <- quals env q ]
        qcopies'                        = [ (tvarWit v p, impl2type (tVar v) p, eDot (eVar selfKW') $ qualAttr p v n1) | (v,p) <- quals env q ]


trim ps                                 = nubBy eqWit [ (catMaybes ws, p, head ws == Nothing) | (ws,p) <- ps ]
  where eqWit (ws1,_,_) (ws2,_,_)       = ws1 == ws2

truePrefix pre ws
  | Just [_] <- stripPrefix pre ws      = True
  | otherwise                           = False

witCtxt ps ws                           = ctxt $ tail $ reverse ws
  where ctxt []                         = []
        ctxt (w:ws)                     = (witAttr w, fromJust $ lookup (w:ws) typemap) : ctxt ws
        typemap                         = [ (reverse ws, tCon $ convProto p) | (ws,p,_) <- ps ]

initCall ts args p                      = Expr NoLoc (eCall (tApp (eDot (eQVar (tcname p)) initKW) ts) (eVar selfKW' : args))

witArgs ws wmap                         = case lookup (head ws) wmap of
                                            Just es -> es
                                            -- Nothing -> trace ("##### wmap empty for " ++ prstrs ws) []

mkBody []                               = [Pass NoLoc]
mkBody b                                = b

noqual env q                            = [ Quant v (filter (not . isProto env . tcname) us) | Quant v us <- q ]

quals env q                             = [ (v, p) | Quant v ps <- q, p <- ps, isProto env (tcname p) ]

qualAttr p v n                          = Derived (tvarWit v p) n

sibName ws n                            = Derived (baseName ws) n

baseName [w]                            = deriveQ w
baseName (w : ws)                       = Derived (baseName ws) (deriveQ w)

modOf (NoQ _)                           = NoQ
modOf (QName m _)                       = QName m


convProto (TC n ts)                     = TC n (tVar tvSelf' : ts)

instProto t (TC n ts)                   = TC n (t : ts)

tvSelf'                                 = TV KType (Internal Typevar "S" 0)
qSelf'                                  = Quant tvSelf' []
selfKW'                                 = Internal Witness "self" 0
thisKW'                                 = Internal Witness "this" 0

convSelf t                              = subst [(tvSelf, tVar tvSelf')] t

convStmts eq stmts                      = map conv stmts
  where conv (Signature l ns sc Static) = Signature l ns (convSelf sc) NoDec
        conv (Signature l ns sc _)      = Signature l ns (convSelf $ convS sc) NoDec
        conv (Decl l ds)                = Decl l (map convD ds)
        conv s                          = s
        convS (TSchema l q t)           = TSchema l (q) (convT t)
        convT (TFun l fx p k t)         = TFun l fx (posRow (tVar tvSelf) p) k t
        convT t                         = t
        convD (Def l n q p k t b d x)   = Def l n (convSelf q) (wit2par [(selfKW',tSelf)] $ convSelf p) (convSelf k) (convSelf t) b' d x
          where b'                      = bindWits eq ++ b
        convD d                         = d

-- Convert a TEnv -------------------------------------------------------------------------------------------

convEnvProtos env                       = mapModules (convert env) env
  where convert env1 []                 = []
        convert env1 (ni:te)            = let te1 = conv env1 ni in te1 ++ convert (define te1 env1) te
        conv env1 (n, NDef sc d)        = [(n, NDef (convS sc) d)]
        conv env1 (n, NSig sc d)        = [(n, NSig (convS sc) d)]
        conv env1 (n, NAct q p k te)    = [(n, NAct (noqual env q) (qualWRow env q p) k (concat $ map (conv env1) te))]
        conv env1 (n, NProto q us te)   = map (fromClass env1) $ convProtocol env n q us [] [] (fromSigs env te)
        conv env1 (n, NExt n0 q us te)  = map (fromClass env1) $ convExtension env n n0 q us [] [] []
        conv env1 (n, NClass q us te)   = [(n, NClass (noqual env q) us (convClassTEnv env q te))]
        conv env1 ni                    = [ni]
        convS (TSchema l q t)           = TSchema l (noqual env q) (convT q t)
        convT q (TFun l x p k t)        = TFun l x (qualWRow env q p) k t
        convT q t                       = t

fromClass env (Class _ n q [] b)        = (n, NClass q [] (fromStmts b))
fromClass env (Class _ n q [u] b)       = (n, NClass q (findAncestry env u) (fromStmts b))

fromStmts (Signature _ ns sc dec : ss)  = [ (n, NSig sc dec) | n <- ns ] ++ fromStmts ss
fromStmts (Decl _ ds : ss)              = fromDefs ds ++ fromStmts ss
fromStmts (_ : ss)                      = fromStmts ss
fromStmts []                            = []

fromDefs (Def l n q p k a b d fx : ds)  = (n, NDef (TSchema NoLoc q (TFun NoLoc fx (prowOf' p d) (krowOf k) (fromJust a))) d) : fromDefs ds
  where prowOf' p Static                = prowOf p
        prowOf' (PosPar _ _ _ p) _      = prowOf p
fromDefs (_ : ds)                       = fromDefs ds
fromDefs []                             = []

fromSigs env ((n, NSig sc dec) : te)    = Signature NoLoc [n] (convS sc) dec : fromSigs env te
  where convS (TSchema l q t)           = TSchema l (noqual env q) (convT q t)
        convT q (TFun l x p k t)        = TFun l x (qualWRow env q p) k t
        convT q t                       = t
fromSigs env (_ : te)                   = fromSigs env te
fromSigs env []                         = []

convClassTEnv env q0 te                 = [ (n, conv i) | (n,i) <- te ]
  where conv (NSig sc dec)              = NSig (convS sc) NoDec
        conv (NDef sc dec)              = NDef (convS sc) NoDec
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
                                                                f : [C] => act[Self](Ord[C],A,B) -> C           (Self as state id)
                                                                f$local : [C] => act[Self](Ord[C],A,B) -> C     (Self as state id)

-}