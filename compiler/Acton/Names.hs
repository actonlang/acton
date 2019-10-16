{-# LANGUAGE FlexibleInstances #-}
module Acton.Names where

import Utils
import Acton.Syntax
import Debug.Trace


self                                = Name NoLoc "self"

modrefs b                           = concat (map mrefs b)
  where mrefs (Import _ is)         = concat [ qref n | ModuleItem n _ <- is ]
        mrefs (FromImport _ n is)   = mref n
        mrefs (FromImportAll _ n)   = mref n
        mref (ModRef (0,Just n))    = qref n
        mref (ModRef _)             = []
        qref (QName n _)            = [n]

declnames (Extension{} : ds)        = declnames ds
declnames (Signature _ ns _ _: ds)  = ns ++ declnames ds
declnames (d : ds)                  = dname d : declnames ds
declnames []                        = []

splitDeclGroup []                   = []
splitDeclGroup (d:ds)               = split (free d) [d] ds
  where split vs ds0 []             = [reverse ds0]
        split vs ds0 (d:ds)
          | any (`elem` ws) vs      = split (free d++vs) (d:ds0) ds
          | otherwise               = reverse ds0 : split (free d) [d] ds
          where ws                  = declnames (d:ds)


-- Control flow --------------------

class Fallsthru a where
    fallsthru                       :: a -> Bool        -- Statement doesn't break control flow
    
instance Fallsthru a => Fallsthru [a] where
    fallsthru                       = all fallsthru
    
instance Fallsthru Stmt where
    fallsthru Return{}              = False
    fallsthru Raise{}               = False
    fallsthru Break{}               = False
    fallsthru Continue{}            = False
    fallsthru (While _ e b els)     = fallsthru b || fallsthru els
    fallsthru (For _ p e b els)     = fallsthru b || fallsthru els
    fallsthru (With _ items b)      = fallsthru b
    fallsthru (Try _ b hs els fin)  = (fallsthru (b++els) || any fallsthru hs) && fallsthru fin
    fallsthru (If _ bs els)         = any fallsthru bs || fallsthru els
    fallsthru _                     = True
    
instance Fallsthru Branch where
    fallsthru (Branch e ss)         = fallsthru ss

instance Fallsthru Handler where
    fallsthru (Handler ex ss)       = fallsthru ss


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
    datavars n (PTuple _ ps)        = bound ps
    datavars n (PList _ ps)         = bound ps
    datavars n (PParen _ p)         = bound p
    datavars n (PData _ v ixs)
      | length ixs >= n             = [v]
    datavars n _                    = []


-- State variables -----------------

statevars b                         = concat [ bound ps | VarAssign _ ps _ <- b ]


-- Free and bound names ------------

class Vars a where
    free                            :: a -> [Name]
    bound                           :: a -> [Name]

    free x                          = []
    bound x                         = []

instance Vars a => Vars [a] where
    free                            = concatMap free
    bound                           = concatMap bound

instance Vars a => Vars (Maybe a) where
    free                            = maybe [] free
    bound                           = maybe [] bound

instance Vars Stmt where
    free (Expr _ e)                 = free e
    free (Assign _ ps e)            = free ps ++ free e
    free (AugAssign _ p op e)       = free p ++ bound p ++ free e
    free (Assert _ es)              = free es
    free (Pass _)                   = []
    free (Delete _ p)               = free p ++ bound p
    free (Return _ e)               = free e
    free (Raise _ e)                = free e
    free (Break _)                  = []
    free (Continue _)               = []
    free (If _ branches els)        = free branches ++ free els
    free (While _ e b els)          = free e ++ free b ++ free els
    free (For _ p e b els)          = free p ++ free e ++ (free b \\ bound p) ++ free els
    free (Try _ b hs els fin)       = free b ++ free hs ++ free els ++ free fin
    free (With _ items b)           = free items ++ (free b \\ bound items)
    free (Data _ p b)               = free p ++ free b
    free (VarAssign _ ps e)         = free ps ++ free e
    free (Decl _ ds)                = free ds

    bound (Assign _ ps _)           = bound ps
    bound (VarAssign _ ps e)        = bound ps
    bound (Data _ p b)              = bound p ++ (filter istemp $ bound b)
    bound (While _ e b els)         = bound b ++ bound els
    bound (For _ p e b els)         = bound b ++ bound els ++ bound p
    bound (With _ items b)          = bound b ++ bound items
    bound (Try _ b hs els fin)      = bound b ++ concatMap bound hs ++ bound els ++ bound fin
    bound (If _ bs els)             = concatMap bound bs ++ bound els
    bound (Delete _ p)              = bound p
    bound (Decl _ ds)               = bound ds
    bound _                         = []

instance Vars Decl where
    free (Def _ n q ps annot b md)  = (free ps ++ free b) \\ (n : bound ps ++ bound b)
    free (Actor _ n q ps annot b)   = (free ps ++ free b) \\ (n : self : bound ps ++ bound b)
    free (Class _ n q cs b)         = (free cs ++ free b) \\ (n : bound b)
    free (Protocol _ n q cs b)      = (free cs ++ free b) \\ (n : bound b)
    free (Extension _ n q cs b)     = (free n ++ free cs ++ free b) \\ bound b
    free (Signature _ ns t dec)     = free t

    bound (Def _ n _ _ _ _ _)       = [n]
    bound (Actor _ n _ _ _ _)       = [n]
    bound (Class _ n _ _ _)         = [n]
    bound (Protocol _ n _ _ _)      = [n]
    bound (Extension _ n _ _ _)     = []
    bound (Signature _ ns t dec)    = ns

instance Vars Branch where
    free (Branch e ss)              = free e ++ free ss
    bound (Branch e ss)             = bound ss
    
instance Vars Handler where
    free (Handler ex ss)            = free ex ++ (free ss \\ bound ex)
    bound (Handler ex ss)           = bound ss ++ bound ex

instance Vars Expr where
    free (Var _ n)                  = [n]
    free (Int _ _ str)              = []
    free (Float _ _ str)            = []
    free (Imaginary _ _ str)        = []
    free (Bool _ v)                 = []
    free (None _)                   = []
    free (NotImplemented _)         = []
    free (Ellipsis _)               = []
    free (Strings _ ss)             = []
    free (BStrings _ ss)            = []
    free (UStrings _ ss)            = []
    free (Call _ e es)              = free e ++ free es
    free (Await _ e)                = free e
    free (Ix _ e ix)                = free e ++ free ix
    free (Cond _ e1 e e2)           = free [e1,e,e2]
    free (BinOp _ e1 o e2)          = free [e1,e2]
    free (CompOp _ e ops)           = free e ++ free ops
    free (UnOp _ o e)               = free e
    free (Dot _ e n)                = free e
    free (DotI _ e i)               = free e
    free (Lambda _ ps e)            = free ps ++ (free e \\ bound ps)
    free (Tuple _ es)               = free es
    free (Yield _ e)                = free e
    free (YieldFrom _ e)            = free e
    free (Generator _ e co)         = (free e \\ bound co) ++ free co
    free (List _ es)                = free es
    free (ListComp _ e co)          = (free e \\ bound co) ++ free co
    free (Dict _ es)                = free es
    free (DictComp _ e co)          = (free e \\ bound co) ++ free co
    free (Set _ es)                 = free es
    free (SetComp _ e co)           = (free e \\ bound co) ++ free co
    free (Record _ fs)              = free fs
    free (RecordComp _ n e co)      = ((n : free e) \\ bound co) ++ free co
    free (Paren _ e)                = free e

instance Vars Name where
    free n                          = [n]

instance Vars QName where
    free (QName n ns)               = [n]

instance Vars Exception where
    free (Exception e1 e2)          = free e1 ++ free e2

instance Vars Except where
    free (ExceptAll _)              = []
    free (Except _ x)               = free x
    free (ExceptAs _ x n)           = free x

    bound (ExceptAll _)             = []
    bound (Except _ x)              = []
    bound (ExceptAs _ x n)          = [n]

instance Vars Params where
    free (Params p s1 k s2)         = free p ++ free s1 ++ free k ++ free s2

    bound (Params p s1 k s2)        = bound p ++ bound s1 ++ bound k ++ bound s2

instance Vars Param where
    free (Param n annot v)          = free v

    bound (Param n annot v)         = [n]
    
instance Vars StarPar where
    bound (StarPar _ n annot)       = [n]
    bound NoStar                    = []

instance Vars e => Vars (Elem e) where
    free (Elem e)                   = free e
    free (Star e)                   = free e

    bound (Elem p)                  = bound p
    bound (Star p)                  = bound p
    
instance Vars Assoc where
    free (Assoc k v)                = free k ++ free v
    free (StarStarAssoc e)          = free e

instance Vars Field where
    free (Field n e)                = free e
    free (StarStarField e)          = free e
    
    bound (Field n e)               = [n]
    bound (StarStarField e)         = []

instance Vars WithItem where
    free (WithItem e p)             = free e ++ free p

    bound (WithItem e p)            = bound p
    
instance Vars Arg where
    free (Arg e)                    = free e
    free (KwArg n e)                = free e
    free (StarArg e)                = free e
    free (StarStarArg e)            = free e

    bound (KwArg n e)               = [n]
    bound _                         = []

instance Vars OpArg where
    free (OpArg o e)                = free e

instance Vars Index where
    free (Index _ e)                = free e
    free (Slice _ e1 e2 e3)         = free e1 ++ free e2 ++ free e3

instance Vars Comp where
    free (CompFor _ pat e c)        = (free e ++ free c) \\ bound pat
    free (CompIf _ e c)             = free e ++ free c
    free NoComp                     = []

    bound (CompFor _ pat e c)       = bound pat ++ bound c
    bound (CompIf _ e c)            = bound c
    bound NoComp                    = []

instance Vars Pattern where
    free (PVar _ n a)               = []
    free (PIx _ e ix)               = free e ++ free ix
    free (PDot _ e n)               = free e
    free (PTuple _ ps)              = free ps
    free (PList _ ps)               = free ps
    free (PParen _ p)               = free p
    free (PData _ n ixs)            = free ixs

    bound (PVar _ n a)              = [n]
    bound (PIx _ e ix)              = []
    bound (PDot _ e n)              = []
    bound (PTuple _ ps)             = bound ps
    bound (PList _ ps)              = bound ps
    bound (PParen _ p)              = bound p
    bound (PData _ n ixs)           = [n]

instance Vars ModuleItem where
    bound (ModuleItem qn Nothing)   = free qn
    bound (ModuleItem qn (Just n))  = free n

instance Vars ImportItem where
    free (ImportItem n1 as)         = []
    
    bound (ImportItem n Nothing)    = free n
    bound (ImportItem n (Just as))  = free as

instance Vars ModRef where 
    bound (ModRef (0, n))           = free n
    bound _                         = []

instance Vars TSchema where
    free (TSchema _ q t)            = free q ++ free t

instance Vars TVar where
    free (TV v)                     = []

instance Vars TCon where
    free (TC n ts)                  = free n ++ free ts

instance Vars TBind where
    free (TBind v cs)               = free cs

instance Vars PosRow where
    free (PosRow t p)               = free t ++ free p
    free (PosVar v)                 = free v
    free PosNil                     = []

instance Vars KwdRow where
    free (KwdRow n t k)              = free t ++ free k
    free (KwdVar v)                  = free v
    free KwdNil                      = []

instance Vars Type where
    free (TVar _ v)                 = free v
    free (TFun _ es p k t)          = free es ++ free p ++ free k ++ free t
    free (TTuple _ p)               = free p
    free (TRecord _ k)              = free k
    free (TOpt _ t)                 = free t
    free (TCon  _ c)                = free c
    free (TAt  _ c)                 = free c
    free _                          = []


-- Type variables and oSubstitution ----------

openFX (OSchema vs cs (OFun fx r t))
  | Just fx1 <- open fx             = OSchema (v:vs) cs (OFun fx1 r t)
  where open (OKwd n t fx)          = OKwd n t <$> open fx
        open (OPos t fx)            = OPos t <$> open fx
        open ONil                   = Just (OVar v)
        open _                      = Nothing
        v:_                         = schemaVars \\ vs
openFX t                            = t

closeFX (OSchema vs cs (OFun fx r t))
  | [v] <- soletail fx \\ vs1,
    v `elem` vs                     = OSchema (vs\\[v]) cs (OFun (oSubst [(v,ONil)] fx) r t)
  where vs1                         = oTyvars t ++ oTyvars r ++ oTyvars cs
        soletail (OKwd _ t fx)      = soletail fx \\ oTyvars t
        soletail (OPos t fx)        = soletail fx \\ oTyvars t
        soletail (OVar v)           = [v]
        soletail _                  = []
closeFX t                           = t

class OSubst t where
    oSubst                          :: OSubstitution -> t -> t
    oTyvars                         :: t -> [OVar]

instance OSubst a => OSubst (Name,a) where
    oSubst s (n, t)                 = (n, oSubst s t)
    oTyvars (n, t)                  = oTyvars t

instance OSubst a => OSubst [a] where
    oSubst s                        = map (oSubst s)
    oTyvars                         = concat . map oTyvars

instance OSubst a => OSubst (Maybe a) where
    oSubst s                        = maybe Nothing (Just . oSubst s)
    oTyvars                         = maybe [] oTyvars

instance OSubst OType where
    oSubst s (OVar l)               = case lookup l s of
                                        Just t  -> t
                                        Nothing -> OVar l
    oSubst s (OFun act row t)       = OFun (oSubst s act) (oSubst s row) (oSubst s t)
    oSubst s (ORecord row)          = ORecord (oSubst s row)
    oSubst s (ODict t1 t2)          = ODict (oSubst s t1) (oSubst s t2)
    oSubst s (OTuple pos)           = OTuple (oSubst s pos)
    oSubst s (OList t)              = OList (oSubst s t)
    oSubst s (OSet t)               = OSet (oSubst s t)
    oSubst s (OMsg t)               = OMsg (oSubst s t)
    oSubst s (OPos t r)             = OPos (oSubst s t) (oSubst s r)
    oSubst s (OStar1 t r)           = OStar1 (oSubst s t) (oSubst s r)
    oSubst s (OKwd n t r)           = OKwd n (oSubst s t) (oSubst s r)
    oSubst s (OStar2 t r)           = OStar2 (oSubst s t) (oSubst s r)
    oSubst s (OSchema vs cs t)      = OSchema vs' (oSubst s' cs) (oSubst s' t)
      where pruned_s                = [ (v,t) | (v,t) <- s, v `notElem` vs ]
            newvars                 = oTyvars (oSubst pruned_s $ map OVar ((oTyvars t ++ oTyvars cs) \\ vs))
            clashvars               = vs `intersect` newvars
            freshvars               = take (length clashvars) (schemaVars \\ (newvars++vs))
            renaming_s              = (clashvars `zip` map OVar freshvars)
            s'                      = renaming_s ++ pruned_s
            vs'                     = freshvars ++ (vs \\ clashvars)
    oSubst s tcon                   = tcon
    
    oTyvars (OVar v)                = [v]
    oTyvars (OFun act row t)        = oTyvars act ++ oTyvars row ++ oTyvars t
    oTyvars (ORecord row)           = oTyvars row
    oTyvars (ODict t1 t2)           = oTyvars t1 ++ oTyvars t2
    oTyvars (OTuple row)            = oTyvars row
    oTyvars (OList t)               = oTyvars t
    oTyvars (OSet t)                = oTyvars t
    oTyvars (OMsg t)                = oTyvars t
    oTyvars (OPos t r)              = oTyvars t ++ oTyvars r
    oTyvars (OStar1 t r)            = oTyvars t ++ oTyvars r
    oTyvars (OKwd n t r)            = oTyvars t ++ oTyvars r
    oTyvars (OStar2 t r)            = oTyvars t ++ oTyvars r
    oTyvars (OSchema vs cs t)       = (oTyvars t ++ oTyvars cs) \\ vs
    oTyvars tcon                    = []

instance OSubst Qonstraint where
    oSubst s (QEqu l v t1 t2)       = QEqu l v (oSubst s t1) (oSubst s t2)
    oSubst s (QIn l v t1 t2)        = QIn l v (oSubst s t1) (oSubst s t2)
    oSubst s (QDot l v t1 n t2)     = QDot l v (oSubst s t1) n (oSubst s t2)
    oSubst s (QIx l v t1 t2 t3)     = QIx l v (oSubst s t1) (oSubst s t2) (oSubst s t3)
    oSubst s (QMod l v t1 t2)       = QMod l v (oSubst s t1) (oSubst s t2)
    oSubst s (QPlus l v t1)         = QPlus l v (oSubst s t1)
    oSubst s (QNum l v t1)          = QNum l v (oSubst s t1)
    oSubst s (QBool l v t1)         = QBool l v (oSubst s t1)

    oTyvars (QEqu _ _ t1 t2)        = oTyvars t1 ++ oTyvars t2
    oTyvars (QIn _ _ t1 t2)         = oTyvars t1 ++ oTyvars t2
    oTyvars (QDot _ _ t1 n t2)      = oTyvars t1 ++ oTyvars t2
    oTyvars (QIx _ _ t1 t2 t3)      = oTyvars t1 ++ oTyvars t2 ++ oTyvars t3
    oTyvars (QMod _ _ t1 t2)        = oTyvars t1 ++ oTyvars t2
    oTyvars (QPlus _ _ t1)          = oTyvars t1
    oTyvars (QNum _ _ t1)           = oTyvars t1
    oTyvars (QBool _ _ t1)          = oTyvars t1

instance OSubst InfoTag where
    oSubst s (GEN l t)              = GEN l (oSubst s t)
    oSubst s (INS l t)              = INS l (oSubst s t)

    oTyvars (GEN _ t)               = oTyvars t
    oTyvars (INS _ t)               = oTyvars t

-----------------

class Subst t where
    subst                           :: Substitution -> t -> t
    tyvars                          :: t -> [TVar]
    tybound                         :: t -> [TVar]
    tybound _                       = []

instance Subst a => Subst (Name,a) where
    subst s (n, t)                  = (n, subst s t)
    tyvars (n, t)                   = tyvars t
    tybound (n, t)                  = tybound t

instance Subst a => Subst [a] where
    subst s                         = map (subst s)
    tyvars                          = concat . map tyvars
    tybound                         = concat . map tybound

instance Subst a => Subst (Maybe a) where
    subst s                         = maybe Nothing (Just . subst s)
    tyvars                          = maybe [] tyvars
    tybound                         = maybe [] tybound

instance Subst TSchema where
    subst s (TSchema l q t)         = TSchema l q' t'
      where vs                      = [ v | TBind v cs <- q ]
            pruned_s                = [ (v,t) | (v,t) <- s, v `notElem` vs ]
            newvars                 = tyvars (subst pruned_s $ map (TVar NoLoc) ((tyvars t ++ tyvars q) \\ vs))
            clashvars               = vs `intersect` newvars
            freshvars               = take (length clashvars) (tvarSupply \\ (newvars++vs))
            renaming_s              = (clashvars `zip` map (TVar NoLoc) freshvars)
            s'                      = renaming_s ++ pruned_s
            t'                      = subst s' t
            q'                      = [ TBind (subst s' v) (subst s' cs) | TBind v cs <- q ]
    
    tyvars (TSchema _ q t)          = (tyvars q ++ tyvars t) \\ tybound q
    
    tybound (TSchema _ q t)         = tybound q

instance Subst TVar where
    subst s v                       = case lookup v s of
                                        Just (TVar _ tv) -> tv
                                        _ -> v
    tyvars v                        = [v]
        
instance Subst TCon where
    subst s (TC n ts)               = TC n (subst s ts)
    tyvars (TC n ts)                = tyvars ts

instance Subst TBind where
    subst s (TBind v cs)            = TBind v (subst s cs)
    tyvars (TBind v cs)             = v : tyvars cs
    tybound (TBind v cs)            = [v]

instance Subst PosRow where
    subst s (PosRow t p)            = PosRow (subst s t) (subst s p)
    subst s (PosVar v)              = PosVar (subst s v)
    subst s PosNil                  = PosNil
    
    tyvars (PosRow t p)             = tyvars t ++ tyvars p
    tyvars (PosVar (Just v))        = [v]
    tyvars _                        = []

instance Subst KwdRow where
    subst s (KwdRow n t k)          = KwdRow n (subst s t) (subst s k)
    subst s (KwdVar v)              = KwdVar (subst s v)
    subst s KwdNil                  = KwdNil

    tyvars (KwdRow n t k)           = tyvars t ++ tyvars k
    tyvars (KwdVar (Just v))        = [v]
    tyvars _                        = []

instance Subst Type where
    subst s (TVar l v)              = case lookup v s of
                                         Just t -> t
                                         _ -> TVar l v
    subst s (TFun l fx p k t)       = TFun l fx (subst s p) (subst s k) (subst s t)
    subst s (TTuple l p)            = TTuple l (subst s p)
    subst s (TRecord l k)           = TRecord l (subst s k)
    subst s (TOpt l t)              = TOpt l (subst s t)
    subst s (TUnion l as)           = TUnion l as
    subst s (TCon l c)              = TCon l (subst s c)
    subst s (TAt l c)               = TAt l (subst s c)
    subst s (TSelf l)               = TSelf l
    subst s (TNone l)               = TNone l

    tyvars (TVar _ v)               = [v]
    tyvars (TFun _ fx p k t)        = tyvars p ++ tyvars k ++ tyvars t
    tyvars (TTuple _ p)             = tyvars p
    tyvars (TRecord _ k)            = tyvars k
    tyvars (TOpt _ t)               = tyvars t
    tyvars (TUnion _ as)            = []
    tyvars (TCon _ c)               = tyvars c
    tyvars (TAt _ c)                = tyvars c
    tyvars (TSelf _)                = []
    tyvars (TNone _)                = []

-- Names free in embedded lambda
-- Called during translation to ensure that lambdas contain no state variables
-- Will become defunct once lambda-lifting works directly on Acton code

lambdafree s                        = lfreeS s
  where lfreeS (Expr _ e)           = lfree e
        lfreeS (Assign _ ps e)      = concatMap lfreeP ps ++ lfree e
        lfreeS (AugAssign _ p op e) = lfreeP p ++ lfree e
        lfreeS (Assert _ es)        = concatMap lfree es
        lfreeS (Delete _ p)         = lfreeP p
        lfreeS (Return _ e)         = maybe [] lfree e
        lfreeS (Raise _ ex)         = maybe [] (\(Exception e1 e2) -> lfree e1 ++ maybe [] lfree e2) ex
        lfreeS (If _ branches els)  = concatMap lfree [ e | Branch e _ <- branches ]
        lfreeS (While _ e b els)    = lfree e
        lfreeS (For _ p e b els)    = lfreeP p ++ lfree e
        lfreeS (With _ items b)     = concat [ lfree e ++ maybe [] lfreeP p | WithItem e p <- items ]
        lfreeS (Data _ p b)         = maybe [] lfreeP p
        lfreeS (VarAssign _ ps e)   = concatMap lfreeP ps ++ lfree e
        lfreeS (Decl _ ds)          = concatMap lfreeD ds
        lfreeS _                    = []

        lfreeD (Class _ n q cs b)   = concatMap lfreeS b
        lfreeD (Protocol _ n q cs b)    = concatMap lfreeS b
        lfreeD (Extension _ n q cs b)   = concatMap lfreeS b
        lfreeD _                    = []

        lfree (Call _ e es)         = lfree e ++ concatMap (lfree . argcore) es
        lfree (Await _ e)           = lfree e
        lfree (Ix _ e ix)           = lfree e ++ concatMap lfreeI ix
        lfree (Cond _ e1 e e2)      = concatMap lfree [e1,e,e2]
        lfree (BinOp _ e1 o e2)     = concatMap lfree [e1,e2]
        lfree (CompOp _ e ops)      = lfree e ++ concatMap lfree [ e | OpArg _ e <- ops ]
        lfree (UnOp _ o e)          = lfree e
        lfree (Dot _ e n)           = lfree e
        lfree (DotI _ e i)          = lfree e
        lfree (Lambda _ ps e)       = concatMap lfree (paramcores ps) ++ (free e \\ bound ps)  -- NOTE: free e, not lambdafree e!
        lfree (Tuple _ es)          = concatMap (lfree . elemcore) es
        lfree (Yield _ e)           = maybe [] lfree e
        lfree (YieldFrom _ e)       = lfree e
        lfree (Generator _ e c)     = (lfree (elemcore e) \\ bound c) ++ lfreeC c
        lfree (List _ es)           = concatMap (lfree . elemcore) es
        lfree (ListComp _ e c)      = (lfree (elemcore e) \\ bound c) ++ lfreeC c
        lfree (Dict _ es)           = concatMap lfreeA es
        lfree (DictComp _ e c)      = (lfreeA e \\ bound c) ++ lfreeC c
        lfree (Set _ es)            = concatMap (lfree . elemcore) es
        lfree (SetComp _ e c)       = (lfree (elemcore e) \\ bound c) ++ lfreeC c
        lfree (Record _ fs)         = concatMap (lfree . fieldcore) fs
        lfree (RecordComp _ n e c)  = ((n : lfree e) \\ bound c) ++ lfreeC c
        lfree (Paren _ e)           = lfree e
        lfree _                     = []
        
        lfreeI (Index _ e)          = lfree e
        lfreeI (Slice _ e1 e2 e3)   = maybe [] lfree e1 ++ maybe [] lfree e2 ++ maybe [] (maybe [] lfree) e3
        
        lfreeA (Assoc k e)          = lfree k ++ lfree e
        lfreeA (StarStarAssoc e)    = lfree e

        lfreeC (CompFor _ p e c)    = lfree e ++ lfreeP p ++ (lfreeC c \\ bound p)
        lfreeC (CompIf _ e c)       = lfree e ++ lfreeC c
        lfreeC NoComp               = []
        
        lfreeP (PVar _ n a)         = []
        lfreeP (PTuple _ ps)        = concatMap (lfreeP . elemcore) ps
        lfreeP (PList _ ps)         = concatMap (lfreeP . elemcore) ps
        lfreeP (PIx _ e ix)         = lfree e ++ concatMap lfreeI ix
        lfreeP (PDot _ e n)         = lfree e
        lfreeP (PParen _ p)         = lfreeP p
        lfreeP (PData _ n ixs)      = concatMap lfree ixs


