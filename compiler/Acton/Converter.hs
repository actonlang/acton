module Acton.Converter where

import Pretty
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
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

protoClasses env n0 q ps eq wmap b      = mainClass : sibClasses
  where q1                              = Quant tvSelf' [] : noqual env q
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

extClasses env n1 n0 q ps eq wmap b     = mainClass : sibClasses
  where q1                              = noqual env q
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

witAttr qn                              = Derived (name "w") (deriveQ qn)

qualAttr p v n                          = Derived (tvarWit v p) n

sibName ws n                            = Derived (baseName ws) n

baseName [w]                            = deriveQ w
baseName (w : ws)                       = Derived (baseName ws) (deriveQ w)

modOf (NoQ _)                           = NoQ
modOf (QName m _)                       = QName m


convProto (TC n ts)                     = TC n (tVar tvSelf' : ts)

instProto t (TC n ts)                   = TC n (t : ts)

tvSelf'                                 = TV KType (Internal Typevar "S" 0)
selfKW'                                 = Internal Witness "self" 0
thisKW'                                 = Internal Witness "this" 0

convSubst t                             = subst [(tvSelf, tVar tvSelf')] t

convStmts eq stmts                      = map conv stmts
  where conv (Signature l ns sc Static) = Signature l ns (convSubst sc) NoDec
        conv (Signature l ns sc _)      = Signature l ns (convSubst $ convSchema sc) NoDec
        conv (Decl l ds)                = Decl l (map (convDecl eq) ds)
        conv s                          = s

convDecl eq (Def l n q p k t b d x)     = Def l n (convSubst q) (wit2par [(selfKW',tSelf)] $ convSubst p) (convSubst k) (convSubst t) b' d x
  where b'                              = bindWits eq ++ b
convDecl eq d                           = d

convSchema (TSchema l q t)              = TSchema l q (convT t)
  where convT (TFun l fx p k t)         = TFun l fx (posRow (tVar tvSelf) p) k t
        convT t                         = t

convTEnv te                             = [ (n, conv i) | (n,i) <- te ]
  where conv (NSig sc Static)           = NSig sc NoDec
        conv (NSig sc _)                = NSig (convSchema sc) NoDec
        conv (NDef sc Static)           = NDef sc NoDec
        conv (NDef sc _)                = NDef (convSchema sc) NoDec
        conv i                          = i
