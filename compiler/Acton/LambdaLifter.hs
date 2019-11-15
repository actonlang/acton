{-# LANGUAGE FlexibleInstances #-}
module Acton.LambdaLifter(liftModule) where

import Control.Monad.State
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Prim
import Pretty
import Prelude hiding((<>))

liftModule (Module n imp stmts) = return $ Module n imp (reverse lled ++ stmts')
  where
    (stmts',(lled,_))           = runL (ll env0 stmts)


type LiftM a                    = State LiftState a

type LiftState                  = ([Stmt],[Int])        -- lifted defs, name supply

runL                            :: LiftM a -> (a, ([Stmt],[Int]))
runL m                          = runState m ([],[1..])

newName                         :: LiftM Name
newName                         = state (\(defs,uniq:supply) -> (Name l0 ("lambda____"++show uniq), (defs,supply)))

liftToTop                       :: Stmt -> LiftM Stmt
liftToTop stmt                  = state (\(lifted,supply) -> (Pass l0, (stmt:lifted,supply)))

-----------------

defmap env ss                   = concat $ map defm ss
  where
    defm (If _ branches els)    = concat $ map (defmap env) $ els : [ ss | Branch _ ss <- branches ]
    defm (While _ _ b els)      = defmap env b ++ defmap env els
    defm (For _ _ _ b els)      = defmap env b ++ defmap env els
    defm (Try _ b hs els fin)   = concat $ map (defmap env) $ b : els : fin : [ ss | Handler _ ss <- hs ]
    defm (With _ _ b)           = defmap env b
    defm (Decl _ ds)            = concat $ map defm' ds
    defm _                      = []

    defm' (Def _ n _ p k _ b _) = [(n, nub (intersect (free b \\ bvs) (locals env)))]
      where bvs                 = bound p ++ bound k ++ bound b
    defm' (Class _ n _ _ b)     = [(n, [])]                                                             -- TODO: elaborate...

expand funfree0 funfree         = map exp1 funfree
  where exp1 (f,vs)             = (f, nub (concat (vs : catMaybes [ lookup v funfree0 | v <- vs ])))

iterexpand funfree
  | len funfree == len funfree' = funfree
  | otherwise                   = iterexpand funfree'
  where funfree'                = expand funfree funfree
        len                     = map (length . snd)


llBody env ss                   = nonnull <$> filter relevant <$> ll env1 ss
  where funfree                 = expand (freemap env) $ iterexpand (defmap env ss)
        vs                      = bound ss \\ map fst funfree
        env1                    = extFuns funfree  $ extLocals vs env
        relevant Pass{}         = False
        relevant _              = True
        nonnull []              = [Pass l0]
        nonnull ss              = ss

-- P :: list of all enclosing function names
-- L :: list of all local variables and parameters in scope
-- F :: map of all function names in scope to their (expanded) set of free variables
-- N :: map of all function names in scope to their unique top-level names
--
-- For each statement-list ss:
-- Split (bound ss) into function names fs and other names vs
-- (Ensure no f in fs has multiple definitions or overlaps with vs)
-- Extend L with vs
-- Let M be a map of each f in fs to its free variables
-- For each f in fs and each g in M(f) such that M(g) exists: add M(g) to M(f)
-- Iterate until M reaches a fixpoint
-- For each f in fs and each g in M(f) such that F(g) exists: add F(g) to M(f)
-- For each f in fs: restrict M(f) to L
-- Extend F with M
-- Extend N with by mapping each f in fs to (P ++ "_" ++ f)
-- Transform any non-def s in ss:
--   Remove all nonlocal v
--   Replace any call g(es), with g is in F, to N(g)(*F(g),es)
--   Replace any call e(es), where e is not a g in F, to e[0](*e[1:],es)
--   Replace other references to any g in in F to (N(g), *F(g))
-- For each (def f vs b) in ss:
--   Append (f ++ "_") to P
--   Extend L with vs
--   Recursively transform b into b'
--   Remove f from ss and lift out def N(g) (*F(g),vs) b'

--------------------------------------------------------------------------------

class Lift e where
    ll                      :: Env -> e -> LiftM e

data Env                    = Env { prefix  :: [(Ctx,String)], 
                                    locals  :: [Name],
                                    freemap :: [(Name,[Name])],
                                    namemap :: [(Name,Name)] } 
                            deriving (Eq,Show)

data Ctx                    = InClass | InDef deriving (Eq,Show)

instance Pretty (Ctx,String) where
    pretty (InClass,s)      = text "C:" <> pretty s
    pretty (InDef,s)        = text "D:" <> pretty s

env0                        = Env { prefix = [], locals = [], freemap = [], namemap = [] }

extPrefix c n env           = env { prefix = (c,nstr n) : prefix env }

extLocals vs env            = env { locals = vs ++ locals env }

extFuns m env               = env { freemap = [ (f, restrict vs) | (f,vs) <- m ] ++ freemap env,
                                    namemap = [ (f, newname env f) | (f,_) <- m ] ++ namemap env }
  where restrict vs         = intersect vs (locals env)

onTop env                   = null (prefix env)

inClass env                 = case prefix env of (InClass,_):_ -> True; _ -> False

inDef env                   = case prefix env of (InDef,_):_ -> True; _ -> False
        
newname env n               = Internal (intercalate "_" (map snd (reverse (prefix env))) ++ "___" ++ nstr n) 0 GenPass

topname env n               = case lookup n (namemap env) of
                                Just n -> n
                                _      -> n

extraArgs env n             = case lookup n (freemap env) of
                                Just vs -> vs
                                _       -> []

addParams vs ps             = foldr (\n p -> PosPar n Nothing Nothing p) ps vs


instance Lift a => Lift [a] where
    ll env                              = traverse (ll env)
instance Lift a => Lift (Maybe a) where
    ll env                              = traverse (ll env)


instance Lift Stmt where
    ll env (Expr l e)                   = Expr l <$> ll env e
    ll env (Assign l pats e)            = Assign l <$> ll env pats <*> ll env e
    ll env (AugAssign l pat op e)       = AugAssign l <$> ll env pat <*> pure op <*> ll env e
    ll env (Assert l es)                = Assert l <$> ll env es
    ll env s@(Pass _)                   = pure s
    ll env (Delete l target)            = Delete l <$> ll env target
    ll env (Return l e)                 = Return l <$> ll env e
    ll env (Raise l e)                  = Raise l <$> ll env e
    ll env s@(Break _)                  = pure s
    ll env s@(Continue _)               = pure s
    ll env (If l branches els)          = If l <$> ll env branches <*> ll env els
    ll env (While l e b els)            = While l <$> ll env e <*> ll env b <*> ll env els
    ll env (For l target e b els)       = For l <$> ll env target <*> ll env e <*> ll env b <*> ll env els
    ll env (Try l b hs els fin)         = Try l <$> ll env b <*> ll env hs <*> ll env els <*> ll env fin
    ll env (With l items b)             = With l <$> ll env items <*> ll env b
    ll env (Decl l ds)
      | onTop env                       = Decl l <$> ll env ds
      | inClass env                     = Decl l <$> ll env ds
      | otherwise                       = Decl l <$> ll env ds >>= liftToTop

instance Lift Decl where
    ll env (Def l n q ps _ks ann b m)
      | onTop env                       = Def l n q ps _ks ann <$> llBody env1 b <*> pure m
      | inClass env                     = Def l n q ps _ks ann <$> llBody env1 b <*> pure m
      | otherwise                       = Def l (topname env n) q ps' _ks ann <$> llBody env1 b <*> pure m
      where env1                        = extLocals (bound ps) $ extPrefix InDef n env
            ps'                         = addParams vs ps
            vs                          = extraArgs env n
    ll env (Class l n q cs b)           = Class l n q cs <$> ll env1 b
      where bvs                         = bound b
            env1                        = extPrefix InClass n $ extLocals bvs env
    

closure n vs                            = eCall (eQVar primCLOS) (map eVar $ n:vs)

instance Lift Expr where
    ll env e =
      case e of
        Var l (NoQual n) | Just vs <- lookup n (freemap env) 
                                -> pure $ closure (topname env n) vs
        Var l n                 -> pure e
        Int _ _ str             -> pure e
        Float _ _ str           -> pure e
        Imaginary _ _ str       -> pure e
        Bool _ v                -> pure e
        None _                  -> pure e
        NotImplemented _        -> pure e
        Ellipsis _              -> pure e
        Strings _ ss            -> pure e
        BStrings _ ss           -> pure e
        Call l e@(Var _ (NoQual n)) p _k
          | Just vs <- lookup n (freemap env)
                                -> Call l (eVar (topname env n)) <$> ll env (extras vs p) <*> return _k
          where extras vs p     =  foldr (PosArg . eVar) p vs
        Call l e p _k           -> Call l <$> ll env e <*> ll env p <*> return _k
        Index l e ix            -> Index l <$> ll env e <*> ll env ix
        Slice l e sl            -> Slice l <$> ll env e <*> ll env sl
        Cond l e1 e e2          -> Cond l <$> ll env e1 <*> ll env e <*> ll env e2
        BinOp l e1 o e2         -> BinOp l <$> ll env e1 <*> pure o <*> ll env e2
        CompOp l e ops          -> CompOp l <$> ll env e <*> ll env ops
        UnOp l o e              -> UnOp l o <$> ll env e
        Dot l e n               -> Dot l <$> ll env e <*> pure n
        Lambda l ps _k e        -> do nn <- newName
                                      let env1 = extLocals (bound ps) $ extPrefix InDef nn env
                                      b <- llBody env1 [Return l0 (Just e)]
                                      liftToTop $ Decl l0 $ [Def l nn [] ps' _k Nothing b NoMod]
                                      return (closure nn vs)
          where ps'             =  addParams vs ps
                vs              =  intersect (free e) (locals env) \\ bound ps

        Await l e               -> Await l <$> ll env e
        Yield l e               -> Yield l <$> ll env e
        YieldFrom l e           -> YieldFrom l <$> ll env e
        Tuple l es              -> Tuple l <$> ll env es
        TupleComp l e co        -> TupleComp l <$> ll env1 e <*> ll env co
          where env1            =  extLocals (bound co) env
        Record l ks             -> Record l <$> ll env ks
        RecordComp l n e co     -> RecordComp l n <$> ll env1 e <*> ll env co
          where env1            =  extLocals (bound co) env
        List l es               -> List l <$> ll env es
        ListComp l e co         -> ListComp l <$> ll env1 e <*> ll env co
          where env1            =  extLocals (bound co) env
        Dict l es               -> Dict l <$> ll env es
        DictComp l e co         -> DictComp l <$> ll env1 e <*> ll env co
          where env1            =  extLocals (bound co) env
        Set l es                -> Set l <$> ll env es
        SetComp l e co          -> SetComp l <$> ll env1 e <*> ll env co
          where env1            =  extLocals (bound co) env
        Paren l e               -> Paren l <$> ll env e

instance Lift Exception where
    ll env (Exception e1 e2)        = Exception <$> ll env e1 <*> ll env e2

instance Lift Branch where
    ll env (Branch e ss)            = Branch <$> ll env e <*> ll env ss

instance Lift Handler where
    ll env (Handler ex ss)          = Handler ex <$> ll env ss
    
instance Lift Elem where
    ll env (Elem e)                 = Elem <$> ll env e
    ll env (Star e)                 = Star <$> ll env e

instance Lift Assoc where
    ll env (Assoc k v)              = Assoc <$> ll env k <*> ll env v
    ll env (StarStar e)             = StarStar <$> ll env e

instance Lift WithItem where
    ll env (WithItem e n)           = WithItem <$> ll env e <*> ll env n
    
instance Lift OpArg where
    ll env (OpArg o e)              = OpArg o <$> ll env e

instance Lift PosArg where
    ll env (PosArg e p)             = PosArg <$> ll env e <*> ll env p
    ll env PosNil                   = pure PosNil

instance Lift KwdArg where
    ll env (KwdArg n e k)           = KwdArg n <$> ll env e <*> ll env k
    ll env KwdNil                   = pure KwdNil

instance Lift Slice where
    ll env (Sliz l e1 e2 e3)        = Sliz l <$> ll env e1 <*> ll env e2 <*> ll env e3

instance Lift Comp where
    ll env (CompFor l target e c)   = CompFor l <$> ll env1 target <*> ll env e <*> ll env1 c
      where env1                    = extLocals (bound target) env
    ll env (CompIf l e c)           = CompIf l <$> ll env e <*> ll env c
    ll env NoComp                   = pure NoComp
    
instance Lift Pattern where
    ll env (PVar l n a)             = return (PVar l n a)
    ll env (PIndex l e ix)          = PIndex l <$> ll env e <*> ll env ix
    ll env (PSlice l e sl)          = PSlice l <$> ll env e <*> ll env sl
    ll env (PDot l e n)             = PDot l <$> ll env e <*> return n
    ll env (PParen l p)             = PParen l <$> ll env p
