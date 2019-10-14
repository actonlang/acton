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

declnames (Decorator _ _ _ d : ds)  = declnames (d:ds)
declnames (Extension{} : ds)        = declnames ds
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
    free (TypeSig _ ns t)           = free t
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

    bound (TypeSig _ ns t)          = ns
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
    free (Decorator _ qn es s)      = free qn ++ free es ++ free s

    bound (Def _ n _ _ _ _ _)       = [n]
    bound (Actor _ n _ _ _ _)       = [n]
    bound (Class _ n _ _ _)         = [n]
    bound (Protocol _ n _ _ _)      = [n]
    bound (Extension _ n _ _ _)     = []
    bound (Decorator _ _ _ d)       = bound d

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
    free (Except _ e)               = free e
    free (ExceptAs _ e n)           = free e

    bound (ExceptAll _)             = []
    bound (Except _ e)              = []
    bound (ExceptAs _ e n)          = [n]

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


-- Type variables and substitution ----------

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
    v `elem` vs                     = OSchema (vs\\[v]) cs (OFun (subst [(v,ONil)] fx) r t)
  where vs1                         = tyvars t ++ tyvars r ++ tyvars cs
        soletail (OKwd _ t fx)      = soletail fx \\ tyvars t
        soletail (OPos t fx)        = soletail fx \\ tyvars t
        soletail (OVar v)           = [v]
        soletail _                  = []
closeFX t                           = t

class Subst t where
    subst                           :: OSubst -> t -> t
    tyvars                          :: t -> [OVar]

instance Subst a => Subst (Name,a) where
    subst s (n, t)                  = (n, subst s t)
    tyvars (n, t)                   = tyvars t

instance Subst a => Subst [a] where
    subst s                         = map (subst s)
    tyvars                          = concat . map tyvars

instance Subst a => Subst (Maybe a) where
    subst s                         = maybe Nothing (Just . subst s)
    tyvars                          = maybe [] tyvars

instance Subst OType where
    subst s (OVar l)                = case lookup l s of
                                        Just t  -> t
                                        Nothing -> OVar l
    subst s (OFun act row t)        = OFun (subst s act) (subst s row) (subst s t)
    subst s (ORecord row)           = ORecord (subst s row)
    subst s (ODict t1 t2)           = ODict (subst s t1) (subst s t2)
    subst s (OTuple pos)            = OTuple (subst s pos)
    subst s (OList t)               = OList (subst s t)
    subst s (OSet t)                = OSet (subst s t)
    subst s (OMsg t)                = OMsg (subst s t)
    subst s (OPos t r)              = OPos (subst s t) (subst s r)
    subst s (OStar1 t r)            = OStar1 (subst s t) (subst s r)
    subst s (OKwd n t r)            = OKwd n (subst s t) (subst s r)
    subst s (OStar2 t r)            = OStar2 (subst s t) (subst s r)
    subst s (OSchema vs cs t)       = OSchema vs' (subst s' cs) (subst s' t)
      where pruned_s                = [ (v,t) | (v,t) <- s, v `notElem` vs ]
            newvars                 = tyvars (subst pruned_s $ map OVar ((tyvars t ++ tyvars cs) \\ vs))
            clashvars               = vs `intersect` newvars
            freshvars               = take (length clashvars) (schemaVars \\ (newvars++vs))
            renaming_s              = (clashvars `zip` map OVar freshvars)
            s'                      = renaming_s ++ pruned_s
            vs'                     = freshvars ++ (vs \\ clashvars)
    subst s tcon                    = tcon
    
    tyvars (OVar v)                 = [v]
    tyvars (OFun act row t)         = tyvars act ++ tyvars row ++ tyvars t
    tyvars (ORecord row)            = tyvars row
    tyvars (ODict t1 t2)            = tyvars t1 ++ tyvars t2
    tyvars (OTuple row)             = tyvars row
    tyvars (OList t)                = tyvars t
    tyvars (OSet t)                 = tyvars t
    tyvars (OMsg t)                 = tyvars t
    tyvars (OPos t r)               = tyvars t ++ tyvars r
    tyvars (OStar1 t r)             = tyvars t ++ tyvars r
    tyvars (OKwd n t r)             = tyvars t ++ tyvars r
    tyvars (OStar2 t r)             = tyvars t ++ tyvars r
    tyvars (OSchema vs cs t)        = (tyvars t ++ tyvars cs) \\ vs
    tyvars tcon                     = []

instance Subst Qonstraint where
    subst s (QEqu l v t1 t2)        = QEqu l v (subst s t1) (subst s t2)
    subst s (QIn l v t1 t2)         = QIn l v (subst s t1) (subst s t2)
    subst s (QDot l v t1 n t2)      = QDot l v (subst s t1) n (subst s t2)
    subst s (QIx l v t1 t2 t3)      = QIx l v (subst s t1) (subst s t2) (subst s t3)
    subst s (QMod l v t1 t2)        = QMod l v (subst s t1) (subst s t2)
    subst s (QPlus l v t1)          = QPlus l v (subst s t1)
    subst s (QNum l v t1)           = QNum l v (subst s t1)
    subst s (QBool l v t1)          = QBool l v (subst s t1)

    tyvars (QEqu _ _ t1 t2)         = tyvars t1 ++ tyvars t2
    tyvars (QIn _ _ t1 t2)          = tyvars t1 ++ tyvars t2
    tyvars (QDot _ _ t1 n t2)       = tyvars t1 ++ tyvars t2
    tyvars (QIx _ _ t1 t2 t3)       = tyvars t1 ++ tyvars t2 ++ tyvars t3
    tyvars (QMod _ _ t1 t2)         = tyvars t1 ++ tyvars t2
    tyvars (QPlus _ _ t1)           = tyvars t1
    tyvars (QNum _ _ t1)            = tyvars t1
    tyvars (QBool _ _ t1)           = tyvars t1

instance Subst InfoTag where
    subst s (GEN l t)               = GEN l (subst s t)
    subst s (INS l t)               = INS l (subst s t)

    tyvars (GEN _ t)                = tyvars t
    tyvars (INS _ t)                = tyvars t
{-}
instance Subst TCon where
    subst s (TC n ts)               = TC n $ subst s ts

instance Subst TBind where
    subst s (TBind v cs)            = TBind $ subst s v $ subst s cs

instance Subst PosRow where
    subst s (PosRow t p)            = PosRow $ subst s t $ subst s p
    subst s (PosVar v)              = PosVar $ subst s v
    subst s PosNil                  = PosNil

instance Subst KwdRow where
    subst s (KwdRow n t k)           = KwdRow $ subst s n $ subst s t $ subst s k
    subst s (KwdVar v)               = KwdVar $ subst s v
    subst s KwdNil                   = KwdNil

instance Subst Type where
    subst s (TSelf l)               = TSelf l
    subst s (TVar l v)              = TVar l $ subst s v
    subst s (TFun l es p k t)       = TFun l $ subst s es $ subst s p $ subst s k $ subst s t
    subst s (TTuple l p)            = TTuple l $ subst s p
    subst s (TRecord l k)           = TRecord l $ subst s k
    subst s (TOpt l t)              = TOpt l $ subst s t
    subst s (TUnion l as)           = TUnion l $ return as
    subst s (TCon l c)              = TCon l $ subst s c
    subst s (TAt l c)               = TAt l $ subst s c
    subst s (TNone l)               = TNone l
-}
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
        lfreeD (Decorator _ n as d) = concatMap (lfree . argcore) as ++ lfreeD d
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


