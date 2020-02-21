module Acton.ProtExtElim where

import Data.Graph
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Acton.Env
import Pretty
import Utils
import Control.Monad.State.Strict
import Acton.Syntax
import Debug.Trace

--------------------------------------------------------------------------------------------------
--
--   Transformation of a protocol
--     - Transform the protocol to a class with same name
--     - Add new type var as first parameter to the protocol itself and all its parents
--     - For type params with supertype requirements, remove that requirement and add an instance variable of that type (and add to __init__ method)
--     - Keep only first parent as supertype; move the others to types of new instance variables
--     - In method types, substitute Self with the new type variable
--     - add new type variable as first param to all non-static methods; remove static declaration from static methods
--     - For protocols used as types in method signatures, add and bind a new typevar in EXISTS construct
--
--   Transformation of an extension
--     - find all ancestors of the protocol involved and create chains of single inheritance (using non-translated protocols)
--     - translate all protocols involved as above and, in each translated protocol.  
--     - for each chain, create one class with name formed from the names of the type to be extended and the first (i.e. least) protocol P in the chain.
--       We call the class generated from P as above Pcl.
--          - the first type param to class P is substituted with the type to be extended. All type params to following protocols in the chain substituted
--            according to the inheritance.
--          - methods in the class are the concatenation of the methods in the protocols in the chain.
--          - the created class has the translation of the protocol class (with type substituted in) as supertype.
--          - all methods gets a new first witness param.
--          - every new class except the one based on the chain starting at P gets an instance variable with type the transform of P.
--
--  Transformation of protocols is handled by the type class Transform. Since one extension will typically result in several classes,
--  the transformation is done separately, by function transExt.
--
---------------------------------------------------------------------------------------------------

data TransEnv                           = TransEnv {protocols :: [(Name,Decl)],
                                                    dec :: Decoration,
                                                    fstpar :: Type,
                                                    sub :: Substitution}

newTransEnv ps                          = TransEnv ps NoDec undefined []

-----------------------------------------------------------------------------------------------------

-- transform a file containing protocol definitions and extensions. Here, all protocols involved are supposed to be in this single file.
transform m@(Module nm is ss)           = Module nm is (ss' ++ ss3)
  where (ss1,ss2)                       = partition isExt ss
        m1                              = Module nm is ss2
        env                             = newTransEnv (protocolsOf m1)
        Module _ _   ss'                = trans env m1
        ps                              = [(dname c,c) | Decl _ ds <- ss', c@(Class {}) <- ds] 
        ss3                             = map (Decl NoLoc) [transExt ps env e | Decl _ [e] <- ss1]
        isExt (Decl _ [Extension{}])    = True
        isExt _                         = False


class Transform a where
    trans                               :: TransEnv -> a -> a

instance Transform a => Transform [a] where
    trans env as                        = map (trans env) as
   
instance Transform Module where
    trans env (Module nm is ss)         = Module nm is $ trans env ss

instance Transform Stmt where
    trans env (Decl loc ds)             = Decl loc $ trans env ds
    trans env stmt                      = stmt

instance Transform Decl where
    trans env (Protocol loc n qs bs ss) = case transParents tv bs of
                                               [] -> Class loc n qs1 [] ss2
                                               b:bs -> Class loc n qs1 [b] (addWitnesses bs ss2)
      where v                           = head (drop 15 tvarSupply Utils.\\ tybound qs)
            tv                          = tVar v
            ss1                         = trans env{fstpar = tv, sub = [(self,tv)]} ss 
            (qs1,ws)                    = transParams v qs
            ss2                         = addWitnesses ws ss1   
    trans env (Signature loc ns t)      = Signature loc ns (subst (sub env) (trans env t))
    trans env ds                        = ds

instance Transform TSchema where
    trans env (TSchema loc bs t d)      = TSchema loc bs (trans (env{dec = d}) t) (if d==StaticMethod then NoDec else d)
                                              

instance Transform Type where
    trans env (TTuple loc r)            = TTuple loc $ trans env r
    trans env (TFun loc fx p k r)       = TFun loc fx p1 (trans env k) (trans env r)
       where p1                         = if dec env == StaticMethod
                                          then trans env p
                                          else TRow loc (name "???") (monotype (fstpar env)) (trans env p)
    trans env (TRecord loc k)           = TRecord loc $ trans env k
    trans env (TOpt loc t)              = TOpt loc $ trans env t
    trans env (TRow loc nm s r)         = TRow loc nm (trans env s) (trans env r)
    trans env (TCon loc tc)             = TCon loc (maybe tc (const (opaque tc)) (lookup (noqual (tcname tc)) (protocols env)))
    trans env t                         = t

instance Transform TCon where
    trans env (TC qn ts)                = TC qn (fstpar env : ts) -- add type param to protocols

-- tvSelf from  Env.hs has the wrong kind
self                                   = TV KWild nSelf
   
opaque (TC qn ts)                       = TC (noQual "EXISTS") [t, tCon(TC qn (t:ts))]
   where t = tVar (TV KType (name "T"))

-- adds witnesses to superprotocols other than the first mentioned.
addWitnesses ws ss                      = map mkSig ws ++ ss
  where mkSig tc                        = Decl NoLoc [Signature NoLoc [name ('_' : nstr (noqual (tcname tc)))] (monotype (tCon tc))]

transParents tv bs                      = map addP bs
   where addP (TC qn ts)                = TC qn (tv : ts)

transParams tv qs                       = trP qs [] []
   where trP [] ws qs1                  = (tBind tv : reverse qs1,ws)
         trP (TBind tv cs:qs) ws qs1    = trP qs ([TC nm [tVar tv] | TC nm _ <- cs]++ws) (tBind tv : qs1) 

---------------------------------------------------------------------------------------------------------
--
-- To transform an extension we assume for the moment that only one protocol is mentioned.
-- We find all ancestor protocols and replace this tree with linear chains with only single
-- inheritance.

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
--
---------------------------------------------------------------------------------------------------------

transExt ps env e@(Extension l nm qs bs ss)
         | length bs /= 1               = error "For now, an extension must implement exactly one protocol"
         | otherwise                    = transChain Nothing ps e (chains vs) 
         where as                       = ancestors (protocols env) (noqual (tcname (head bs)))
               (graph,f1,_)             = graphFromEdges [(p,dname p,parentsOf p) | p <- as]
               vs                       = map (fst3 . f1) (topSort graph)

transChain _ _ _ []                     = []
transChain mb ps e (cs : css)           = c2{dname = c2nm, dbody = sigs} : transChain (Just witType) ps e css
   where cs1                            = map (fromJust . flip lookup ps) cs
         ts                             = tCon (mkTC (dqname e) (qual e)) : tcargs (head (bounds e))
         cs2@(c2:_)                     = substAll ts cs1
         tc                             = head (bounds c2)
         c2nm                           = Internal (nstr (dname c2) ++ '_' : nstr (noqual (dqname e))) 0 CPass
         witType                        = maybe (tCon tc) id mb
         sigs                           = maybe [] (\(TCon _ (TC nm _))->[Decl NoLoc [Signature NoLoc [name ('_':nstr (noqual nm))] (monotype witType)]]) mb ++
                                          trans ((newTransEnv []){fstpar = tCon (mkTC (NoQual c2nm) (qual c2))}) (dropWhile notMeth $ concatMap dbody cs2)


substAll _ []                           = []
substAll ts (Class l nm qs bs ss : cs)  = Class l nm (nub $ map tBind (tyfree ts)) [tc] (subst s ss) : substAll ts1 cs
  where s                               = red (zip (map (\(TBind tv _) -> tv) qs) ts)
        ts1                             = subst s (tcargs (head bs))
        tc                              = subst s (mkTC (NoQual nm) qs)

        red []                          = []
        red ((tv,TVar _ tv') : ps)
            |tv == tv'                  = red ps
        red (p:ps)                      = p : red ps

mkTC nm qs                             = TC nm $ map (\(TBind tv _) -> tVar tv) qs

notMeth (Decl _ (s:_))                  = notFunType (sctype (dtyp s))
   where notFunType (TFun {})           = False
         notFunType _                   = True

protocolsOf (Module _ _ ss)             = [(dname p,p) |  Decl _ ds <- ss, p@(Protocol{}) <- ds]

parentsOf p                             = map (noqual . tcname) (bounds p)

ancestors ps n                          = p : concatMap (ancestors ps) (parentsOf p)
    where p                             = fromJust (lookup n ps)
  
fst3 (a,_,_)                            = a

chains :: [Decl] -> [[Name]]
chains []                               = []
chains (e:es)                           = c e es []
   where c n [] as                      = [reverse (dname n:as)]  
         c n (e:es) as
            | null (parentsOf n)        = reverse (dname n:as) : c e es []
            | otherwise                 = let (y,ys) = find (head (parentsOf n)) (e:es) []
                                          in c y ys (dname n:as)

         find k [] es                   = error (show k ++ " not found")
         find k (x:xs) es
             |dname x == k              = (x,reverse es ++ xs)
             |otherwise                 = find k xs (x:es)

