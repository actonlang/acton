{-# LANGUAGE FlexibleInstances #-}
module Tests.ProtExtElim2 where

import Acton.Env
import Pretty
import Utils
import Acton.Syntax

{-
  * transform translates (n,NProto{}) pairs in the TEnv to (n,NClass{}) ditto, where in the result only single inheritance remains, 
  * keeping the first superprotocol link only. Remaining superclasses are instead represented as delegation links in the body of the
  * NClass to other chains of superclasses linked by single inheritance. In spite of keeping single inheritance, methods from superclasses 
  * are included in each class to simplify C code generation.
  *
  * Extensions are also translated, where each (n,NExt{}) pair will typically translate to several (n,NClass{}) pairs. 
-}
 
transform                               :: TEnv -> (TEnv,[Name]) -- snd component is list of protocol names
transform tenv                          = (cs1 ++ concat cs3 ++ cs6, map fst ss1)
   where (ss1,ss2)                      = partition isProt tenv
         (ss3,ss4)                      = partition isExt ss2
         (ss5,ss6)                      = partition isClass ss4
         -- ss5 is ignored; builtin classes have handwritten C code
         isProt (_,NProto{})            = True
         isProt _                       = False
         isExt (_,NExt{})               = True
         isExt _                        = False
         isClass (_,NClass{})           = True
         isClass _                      = False
         env0                           = newTransEnv tenv
         cs1                            = trans env0 ss1
         cs6                            = trans env0 ss6
         cs3                            = map (transExt cs1 env0) ss3

data TransEnv                           = TransEnv {tenv :: TEnv,
                                                    decor :: Decoration,
                                                    fstpar :: Maybe Type
                                                   }

newTransEnv tenv                        = TransEnv tenv NoDec Nothing

class Transform a where
    trans                               :: TransEnv -> a -> a

instance Transform a => Transform [a] where
    trans env as                        = map (trans env) as

instance Transform a => Transform (Maybe a) where
    trans env Nothing                   = Nothing
    trans env (Just a)                  = Just (trans env a)

instance Transform b => Transform (a,b) where
   trans env (a,b)                      = (a,trans env b)

instance Transform NameInfo where
   trans env (NProto qs bs sigs)        = case transParents tv bs of
                                               []   -> NClass (tBind v:qs1) [] sigs2
                                               b:bs -> NClass (tBind v:qs1) [trans env b] (addWitnesses env bs1 sigs2)
      where v                           = head (drop 15 tvarSupply Utils.\\ tybound qs)
            tv                          = tVar v
            sigs1                       = trans env{fstpar = Just tv} (addSigs env bs ++ sigs)
            (qs1,ws)                    = transParams qs
            sigs2                       = addWitnesses env ws sigs1
            cs                          = chains (tenv env) bs
            bs1                         = if null cs then [] else map head (tail cs)
   trans env (NSig sc d)                = NSig (trans env{decor = d} sc) (if d == Static then NoDec else d) 
   trans env ni                         = ni

instance Transform TSchema where
    trans env (TSchema loc bs t)        = TSchema loc bs (trans env t)
                                              
instance Transform Type where
    trans env (TVar loc (TV k (Name _ "Self")))
                                        = fromJust (fstpar env)
    trans env v@TVar{}                  = v
    trans env (TTuple loc p k)          = TTuple loc (trans env p) (trans env k)
    trans env (TFun loc fx p k r)       = TFun loc fx p1 (trans env k) (trans env r)
       where p1                         = if decor env == Static
                                          then trans env p
                                          else maybe (trans env p) (\t -> TRow loc PRow (name "???") (monotype (trans env t)) (trans env p)) (fstpar env)
    trans env (TOpt loc t)              = TOpt loc $ trans env t
    trans env (TRow loc k nm s r)       = TRow loc k nm (trans env s) (trans env r)
    trans env (TCon _ tc)               = case lookup (noqual (tcname tc)) (tenv env) of
                                              Just (NProto {}) -> TExist NoLoc (trans env tc)
                                              _                -> TCon NoLoc (trans env tc)
    trans env (TExist _ tc)             = TExist NoLoc (trans env tc)
    trans env t                         = t

instance Transform TCon where
    trans env (TC qn ts)                = TC (trans env qn) (trans env ts)

instance Transform TBind where
    trans env (TBind tv tcs)            = TBind tv (trans env tcs)
instance Transform QName where
    trans env (QName _ nm)              = NoQual nm
    trans env qn                        = qn

instance Transform Name where
    trans env nm                        = nm
    
addWitnesses env ws ss                  = map mkSig ws ++ ss
  where mkSig tc                        = (name ('_' : nstr (noqual (tcname tc))), NSig (monotype (tCon (trans env tc))) NoDec)

transParents tv bs                      = map addP bs
   where addP (TC qn ts)                = TC qn (tv : ts)

-- transforms [A(Eq), B, C(Hashable)] into ([A,B,C],[Eq[A],Hashable[C]])
transParams                             :: [TBind] -> ([TBind],[TCon])
transParams qs                          = trP qs [] []
   where trP [] ws qs1                  = (reverse qs1,reverse ws)
         trP (TBind tv cs:qs) ws qs1    = trP qs ([TC nm [tVar tv] | TC nm _ <- cs]++ws) (tBind tv : qs1) 

addSigs                                 :: TransEnv -> [TCon] -> TEnv
addSigs env []                          = []
addSigs env (TC qn qs : _)              = addSigs env (subst2 s bs) ++ subst2 s methodSigs
  where NProto qs1 bs sigs              = fromJust (lookup (noqual qn) (tenv env))
        s                               = tVars qs1 `zip` trans env qs
        methodSigs                      = filter isFunSig sigs
          where isFunSig (n,NSig sc _)  = isFunType (sctype sc)
                isFunSig p              = error ("Internal error, Env.addSigs: non-signature in protocol: " ++ show p)
                isFunType (TFun {})     = True
                isFunType _             = False

subst2                                  :: Subst a => Substitution -> a -> a
subst2 s                                = subst s2 . subst s1
  where (s1,s2)                         = partition (\p -> null (tyfree(snd p) `intersect` (tyfree (dom s)))) (red s)
        red []                          = []
        red ((tv,TVar _ tv') : ps)
            |tv == tv'                  = red ps
        red (p:ps)                      = p : red ps

mkTC nm qs                              = TC nm $ map (\(TBind tv _) -> tVar tv) qs

tVars qs                                = map (\(TBind tv _) -> tv) qs

transExt                                :: TEnv -> TransEnv -> (Name,NameInfo) -> [(Name,NameInfo)]
transExt ctenv env (w,e@(NExt _ _ bs))  = transChain Nothing ctenv env e (chains (tenv env) bs)

chains                                  :: TEnv -> [TCon] -> [[TCon]]
chains tenv []                          = []
chains tenv (b:bs)                      = cs : chains tenv [ b | b@(TC qn ts) <- bs, qn `notElem` map tcname cs ]
   where cs                             = fstParents tenv b
         fstParents tenv b@(TC qn ts)   = case lookup (noqual qn) tenv of
                                             Just (NProto _ ps _) -> case ps of
                                                                        [] -> [b]
                                                                        p:_ -> b : fstParents tenv p
                                             Nothing -> error ("cannot find protocol "++show qn) 

transChain                              :: Maybe Type -> TEnv -> TransEnv  -> NameInfo -> [[TCon]] -> TEnv
transChain _ _ _ _ []                     = []
transChain mb ctenv env e@(NExt qn qs bs) (c:cs)
                                        = (c2nm,NClass qs2 bs2 sigs) : transChain (Just witType) ctenv env e cs
   where cn                             = noqual (tcname (head c))
         c1                             = (cn,fromJust (lookup cn ctenv))
         ts                             = trans env (tCon (mkTC qn qs) : tcargs (head bs))
         (_,ws)                         = transParams qs
         (_,c2@(NClass qs2 bs2 te2))    = substAll ts c1
         tc                             = head bs2
         c2nm                           = Internal (nstr cn ++ '_' : nstr (noqual qn)) 0 GenPass  -- pass chosen just to get prettyprinting without suffix...
         witType                        = maybe (tCon tc) id mb
         sigs                           = maybe [] (\(TCon _ (TC qn _))->[(name ('_':nstr (noqual qn)),NSig (monotype witType) NoDec)]) mb 
                                          ++ nub (addWitnesses env ws te2)

substAll                                :: [Type] -> (Name,NameInfo) -> (Name,NameInfo)
substAll ts (n,NClass qs bs ss)        = (n,NClass (nub $ map tBind (tyfree ts)) [tc] (subst2 s ss))
  where s                               = tVars qs `zip` ts
        tc                              = subst2 s (mkTC (NoQual n) qs)
