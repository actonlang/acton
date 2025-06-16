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

module Tests.ProtExtElim where

import Data.Maybe
import Data.List
import Acton.Env
import Pretty
import Utils
import Acton.Syntax


-- Transform a file containing protocol definitions, extensions and signatures.
-- Intended use is to translate large parts of module __builtin__ to C.
transform m@(Module nm is stmts)        = Module nm is (map (Decl NoLoc . (:[])) (trans env ps) ++
                                                        map (Decl NoLoc . (:[])) (concatMap (transExt env) es) ++
                                                        trans env ss)
  where (ps,cs,es,ss)                   = splitStmts stmts
        -- ignore classes; builtin classes have handcrafted C code.
        -- for now, no defs or actors.
        env                             = newTransEnv (protocolsOf m)

-----------------------------------------------------------------------------------------------------
 
data TransEnv                           = TransEnv {protocols :: [(Name,Decl)],
                                                    decor :: Deco,
                                                    fstpar :: Maybe Type,
                                                    master :: Maybe TCon
                                                   }
                                                    
newTransEnv ps                          = TransEnv ps NoDec Nothing Nothing

splitStmts stmts                        = sp stmts [] [] [] []  -- protocols, classes, extensions, signatures
  where sp [] ps cs es ss               = (reverse ps,reverse cs,reverse es,reverse ss)
        sp (Decl _ ds : stmts ) ps cs es ss
                                         = spdecl ds stmts ps cs es ss
        sp (s@Signature{} : stmts) ps cs es ss
                                        = sp stmts ps cs es (s  : ss)
        spdecl [] stmts ps cs es ss     = sp stmts ps cs es ss
        spdecl (p@Protocol{} : ds) stmts ps cs es ss
                                        = spdecl ds stmts (p : ps) cs es ss
        spdecl (c@Class{} : ds) stmts ps cs es ss
                                        = spdecl ds stmts ps (c : cs) es ss
        spdecl (e@Extension{} : ds) stmts ps cs es ss  -- ignore defs
                                        = spdecl ds stmts ps cs (e : es) ss
        spdecl (d@Def{} : ds) stmts ps cs es ss  
                                        = spdecl ds stmts ps cs es ss
                                        
class Transform a where
    trans                               :: TransEnv -> a -> a

instance Transform a => Transform [a] where
    trans env as                        = map (trans env) as
   
instance Transform Module where
    trans env (Module nm is ss)         = Module nm is $ trans env ss

instance Transform Stmt where
    trans env (Decl loc ds)             = Decl loc $ trans env ds
    trans env (Signature loc ns t d)    = Signature loc ns (trans env{decor=d} t) (if d == Static then NoDec else d) 
    trans env stmt                      = stmt

instance Transform Decl where
    trans env (Protocol loc n qs bs ss) = case transParents tv bs of
                                               [] -> Class loc n (tBind v:qs1) [] ss2
                                               b:_ -> Class loc n  (tBind v:qs1) [trans env b] (addWitnesses env bs1 ss2)
      where v                           = head (drop 15 tvarSupply Utils.\\ tybound qs)  -- we just prefer letters later in the alphabet for this type variable...
            tv                          = tVar v
            ss1                         = trans env{fstpar = Just tv} (addSigs (protocols env) bs ++ ss)
            initSig                     = Signature NoLoc [name "__init__"] (monotype (tFun0 (map tCon (maybe [] (:[]) (master env) ++ws)) tNone)) Static
            (qs1,ws)                    = transParams qs
            ss2                         = initSig : addWitnesses env ws ss1   
            cs                          = chains (protocols env) bs
            bs1                         = transParents tv (if null cs then [] else map head (tail cs))
    trans env d                         = d

instance Transform TSchema where
    trans env (TSchema loc bs t)        = TSchema loc (trans env bs) (trans env t)
                                              
instance Transform Type where
    trans env (TVar loc (TV k (Name _ "Self")))
                                        = fromJust (fstpar env)
    trans env v@TVar{}                  = v
    trans env (TTuple loc p k)          = TTuple loc (trans env p) (trans env k)
    trans env (TFun loc fx p k r)       = TFun loc fx p1 (trans env k) (trans env r)
       where p1                         = if decor env == Static
                                          then trans env p
                                          else maybe (trans env p) (flip posRow (trans env p)) (fstpar env)
    trans env (TOpt loc t)              = TOpt loc $ trans env t
    trans env (TRow loc k nm s r)       = TRow loc k nm (trans env s) (trans env r)
--    trans env (TCon loc tc)             = maybe (TCon NoLoc (trans env tc)) (const (TExist NoLoc (trans env tc))) (lookup (noq (tcname tc)) (protocols env))
--    trans env (TExist _ tc)             = TExist NoLoc (trans env tc)
    trans env t                         = t

instance Transform TCon where
    trans env (TC qn ts)                = TC (trans env qn) (trans env ts)

instance Transform TBind where
    trans env (TBind tv tcs)            = TBind tv (trans env tcs)

instance Transform QName where
    trans env (QName _ nm)              = NoQ nm
    trans env qn                        = qn

instance Transform Name where
    trans env nm                        = nm

-- adds witnesses to superprotocols other than the first mentioned.
addWitnesses env ws ss                  = map mkSig ws ++ ss
  where mkSig tc                         = Signature NoLoc [name ('_' : nstr (noq (tcname tc)))] (monotype (tCon (trans env tc))) NoDec

transParents tv bs                      = map addP bs
   where addP (TC qn ts)                = TC qn (tv : ts)

-- transforms [A(Eq), B, C(Hashable)] into ([A,B,C],[Eq[A],Hashable[C]])
transParams                             :: QBinds -> (QBinds,[TCon])
transParams qs                          = trP qs [] []
   where trP [] ws qs1                  = (reverse qs1,reverse ws)
         trP (TBind tv cs:qs) ws qs1    = trP qs ([TC nm [tVar tv] | TC nm _ <- cs]++ws) (tBind tv : qs1) 

---------------------------------------------------------------------------------------------------------
--
-- To transform an extension we assume for the moment that only one protocol is mentioned.
-- We find all ancestor protocols and replace this tree with linear chains with only single
-- inheritance.
--
-- As an example, if the inheritance structure is
--
--  Sequence-> Sliceable -> Indexed
--          |
--          -> Collection -> Iterable
--          |
--          -> Plus
--
-- we form the three chains  Sequence->Sliceable->Indexed ,  Collection->Iterable ,  Plus,
-- each of which will be transformed to one class definition.
--
-- transExt constructs the chains and calls transChain for each chain to build the class definition.
---------------------------------------------------------------------------------------------------------

transExt                                :: TransEnv -> Decl -> [Decl]
transExt env e@(Extension l nm qs bs ss)
         | length bs /= 1               = error "For now, an extension must implement exactly one protocol"
         | otherwise                    = transChain Nothing env (head bs) e cs
         where as                       = head bs : ancestors env (noq (tcname (head bs)))
               cs                       = chains (protocols env) as

transChain                              :: Maybe Type -> TransEnv  -> TCon -> Decl -> [[TCon]] -> [Decl]
transChain _ _ _ _ []                   = []
transChain mb env b e (c : cs)          = c2{dname = c2nm, dbody = sigs} : transChain (Just witType) env{master = Just b} b e cs
   where cn                             = noq (tcname (head c))
         prot                           = fromJust (lookup cn (protocols env))
         c1                             = trans env prot{qual = qual e ++ qual prot} 
         ts                             = trans env (tCon (mkTC (dqname e) (qual e)) : tcargs (head (bounds e)))
         (_,ws)                         = transParams (qual e)
         c2                             = substAll ts c1 
         tc                             = head (bounds c2)
         c2nm                           = Derived (dname c2) (noq (dqname e))
         witType                        = maybe (tCon tc) id mb
         sigs                           = maybe [] (\(TCon _ (TC nm _))->[Signature NoLoc [name ('_':nstr (noq nm))] (monotype witType) NoDec]) mb
                                          ++ nub (dbody c2) -- nub (addWitnesses env ws (dbody c2))


substAll ts (Class l nm qs bs ss)       = Class l nm (nub $ map tBind (ufree ts)) [tc] (subst2 s ss)
   where s                              = tVars qs `zip` ts
         tc                             = subst2 s (mkTC (NoQ nm) qs)


addSigs                                 :: [(Name,Decl)] -> [TCon] -> [Stmt]
addSigs ps []                           = []
addSigs ps (TC n qs : _)                = addSigs ps (subst2 s (bounds p)) ++ subst2 s methodSigs
  where p                               = fromJust (lookup (noq n) ps)
        s                               = tVars (qual p) `zip` qs
        methodSigs                      = [sig | sig@(Signature _ _ sc _) <- dbody p, isFunSchema sc ] 
          where isFunSchema sc          = isFunType (sctype sc)
                isFunType (TFun {})     = True
                isFunType _             = False

subst2                                  :: Subst a => Substitution -> a -> a
subst2 s                                = subst s2 . subst s1
  where (s1,s2)                         = partition (\p -> null (ufree(snd p) `intersect` (ufree (dom s)))) (red s)
        red []                          = []
        red ((tv,TVar _ tv') : ps)
            |tv == tv'                  = red ps
        red (p:ps)                      = p : red ps

mkTC nm qs                              = TC nm $ map (\(TBind tv _) -> tVar tv) qs

tVars qs                                = map (\(TBind tv _) -> tv) qs
 
protocolsOf (Module _ _ ss)             = [(dname p,p) |  Decl _ ds <- ss, p@(Protocol{}) <- ds]

parentsOf p                             = map (noq . tcname) (bounds p)

ancestors                               ::  TransEnv -> Name -> [TCon]
ancestors env n                         = ps ++ concatMap (ancestors env) (map (noq . tcname) ps)
    where ps                            = bounds (fromJust (lookup n (protocols env)))
  
chains                                  :: [(Name,Decl)] -> [TCon] -> [[TCon]]
chains ps []                            = []
chains ps (b:bs)                        = cs : chains ps [ b | b@(TC qn ts) <- bs, qn `notElem` map tcname cs ]
   where cs                             = fstParents ps b
         fstParents ps b@(TC qn ts)     = case lookup (noq qn) ps of
                                             Just (Protocol _ _ _ bs _) -> case bs of
                                                                        [] -> [b]
                                                                        p:_ -> b : fstParents ps p
                                             Nothing -> error ("cannot find protocol "++show qn) 
 