{-# LANGUAGE FlexibleInstances #-}
module Acton.LambdaLifter(liftModule) where

import Control.Monad.State.Strict
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Prim
import Acton.Printer
import Pretty
import Prelude hiding((<>))

liftModule (Module n imp stmts) = return $ Module n imp (reverse lams ++ reverse defs ++ stmts')
  where (stmts',(lams,defs,_))  = runL (ll env0 stmts)


-- A closed function outside a class:
--   * Takes all its free variables as extra parameters (to the left)

-- A function call:
--   * Takes

-- A closed method (function on the top level of a class):
--   * Accesses all its free variables via its first parameter "self" (which already existed prior to ll)

-- A closed

-- L :: list of all local variables and parameters in scope (locals)
-- F :: map of all function names in scope to their (expanded) set of free variables (freemap)
-- N :: map of all function names in scope to their unique aliases (namemap)

-- For each statement-list ss:
--   Split (bound ss) into function names fs and other names vs

--   Let M be a map of each f in fs to its free variables (funfree)
--   For each f in fs and each g in M(f) such that M(g) exists: add M(g) to M(f)
--   Iterate until M reaches a fixpoint
--   For each f in fs and each g in M(f) such that F(g) exists: add F(g) to M(f)
--   For each f in fs: restrict M(f) to L

--   Extend F with M
--   Extend L with vs
--   Extend N by mapping each f to a globally unique name

--   If ss is the body of a class:
--     
--   Else:
--     Transform any non-def s in ss:
--       Replace any call g(es) in s, with g in F, by N(g)(*F(g),es)
--       Replace any call e(es) in s, where e is not a g in F, by CLOSCALL(e,es)
--       Replace other references to any g in F by CLOS(N(g),*F(g))
--     For each  def f(ws):b  in ss:
--       Extend L with ws
--       Recursively transform b into b'
--       Remove f from ss and lift out  def N(f) (*F(f),ws):b'

-- Lift monad ----------------------------------------------------------------------------------------------------------

type LiftM a                    = State LiftState a

type LiftState                  = ([Stmt],[Stmt],[Int])        -- lifted defs, name supply

runL                            :: LiftM a -> (a, LiftState)
runL m                          = runState m ([],[],[1..])

newName                         :: String -> LiftM Name
newName s                       = state (\(totop,tonext,uniq:supply) -> (Internal s uniq LLiftPass, (totop,tonext,supply)))

swapLifted                      :: [Stmt] -> LiftM [Stmt]
swapLifted stmts                = state (\(totop,tonext,supply) -> (tonext, (totop,stmts,supply)))

liftToTop                       :: Stmt -> LiftM Stmt
liftToTop stmt                  = state (\(totop,tonext,supply) -> (Pass l0, (stmt:totop,tonext,supply)))

liftToNext                      :: Stmt -> LiftM Stmt
liftToNext stmt                 = state (\(totop,tonext,supply) -> (Pass l0, (totop,stmt:tonext,supply)))

-- Environment ---------------------------------------------------------------------------------------------------------

data Env                        = Env { 
                                    prefix  :: [(Ctx,String)], 
                                    locals  :: [Name],
                                    freemap :: [(Name,[Name])],
                                    namemap :: [(Name,Name)],
                                    selfpar :: Maybe Name,
                                    selfref :: [Name] 
                                  } 
                                  deriving (Eq,Show)

data Ctx                        = InClass | InDef deriving (Eq,Show)

instance Pretty (Ctx,String) where
    pretty (InClass,s)          = text "C:" <> pretty s
    pretty (InDef,s)            = text "D:" <> pretty s

instance Pretty (Name, [Name]) where
    pretty (n,ns)               = pretty n <+> braces (commaSep pretty ns)


env0                            = Env { prefix = [], locals = [], freemap = [], namemap = [], selfpar = Nothing, selfref = [] }

extPrefix c n env               = env { prefix = (c,nstr n) : prefix env }

extLocals vs env
  | any (isSelf env) vs         = env { locals = vs ++ locals env, selfpar = Nothing }
  | otherwise                   = env { locals = vs ++ locals env }

extFuns m env                   = env { freemap = [ (f, restrict vs) | (f,vs) <- m ] ++ freemap env }
  where restrict vs             = intersect vs (locals env)

extMap m env                    = env { namemap = m ++ namemap env }

setSelf n env                   = env{ selfpar = n }

viaSelf vs env                  = env{ selfref = vs ++ selfref env }

findFree n env                  = lookup n (freemap env)

isSelf env n                    = Just n == selfpar env

selfRef env n                   = n `elem` selfref env

onTop env                       = null (prefix env)

inClass env                     = case prefix env of (InClass,_):_ -> True; _ -> False

classContext env                = case [ c | (InClass,c) <- prefix env ] of [] -> False; _ -> True

-- inDef env                       = case prefix env of (InDef,_):_ -> True; _ -> False
        
-- newname env n                   = Internal (intercalate "_" (map snd (reverse (prefix env))) ++ "___" ++ nstr n) 0 GenPass

liftedname env n                = case lookup n (namemap env) of
                                    Just n -> n
                                    _      -> n

extraArgs env n                 = case findFree n env of
                                    Just vs -> vs
                                    _       -> []

-- Helpers ------------------------------------------------------------------------------------------------------------------

defmap env ss                   = concat $ map defm ss
  where
    defm (If _ branches els)    = concat $ map (defmap env) $ els : [ ss | Branch _ ss <- branches ]
    defm (While _ _ b els)      = defmap env b ++ defmap env els
    defm (For _ _ _ b els)      = defmap env b ++ defmap env els
    defm (Try _ b hs els fin)   = concat $ map (defmap env) $ b : els : fin : [ ss | Handler _ ss <- hs ]
    defm (With _ _ b)           = defmap env b
    defm (Decl _ ds)            = concat $ map defmD ds
    defm _                      = []

    defmD d@Def{}               = [(dname d, free d)]
    defmD d@Class{}             = [(dname d, free d)]
    defmD d                     = []

expand funfree0 funfree         = map exp1 funfree
  where exp1 (f,vs)             = (f, nub (concat (vs : catMaybes [ lookup v funfree0 | v <- vs ])))

iterexpand funfree
  | len funfree == len funfree' = funfree
  | otherwise                   = iterexpand funfree'
  where funfree'                = expand funfree funfree
        len                     = map (length . snd)

defBody env ss                  = do --traceM ("-- defBody: funfree: " ++ prstrs funfree ++ ", vs: " ++ prstrs vs)
                                     ns <- mapM (newName . nstr) (dom funfree)
                                     nonnull <$> filter relevant <$> ll (extMap (ns `zip` dom funfree) env1) ss
  where funfree                 = expand (freemap env) $ iterexpand funfree0
        funfree0                = defmap env ss
        vs                      = bound ss \\ dom funfree
        env1                    = extFuns funfree  $ extLocals vs env
        relevant Pass{}         = False
        relevant _              = True
        nonnull []              = [Pass l0]
        nonnull ss              = ss

classBody env ss                = do --traceM ("-- classBody: funfree: " ++ prstrs funfree ++ 
                                     --                    ", allfree: " ++ prstrs allfree ++ ", vs: " ++ prstrs vs)
                                     ns <- mapM (newName . nstr) vs
                                     nonnull <$> filter relevant <$> ll (extMap (ns `zip` vs) env1) ss
  where funfree                 = defmap env ss
        allfree                 = nub $ concat [ vs | (m,vs) <- funfree ]
        vs                      = bound ss \\ signames ss
        env1                    = viaSelf allfree $ viaSelf vs env                    -- TODO: exclude classmeths/classattrs
        relevant Pass{}         = False
        relevant _              = True
        nonnull []              = [Pass l0]
        nonnull ss              = ss


addParams vs ps                 = foldr (\n p -> PosPar n Nothing Nothing p) ps vs

closure n vs                    = eCall (eQVar primCLOS) (map eVar $ n:vs)

signames ss                     = concatMap snames ss
  where snames (Decl _ ds)      = concatMap sigs ds
        snames _                = []
        sigs d@Signature{}      = dvars d
        sigs _                  = []

----------------------------------------------------------------------------------------------------------

class Lift e where
    ll                                  :: Env -> e -> LiftM e

instance Lift a => Lift [a] where
    ll env                              = traverse (ll env)

instance Lift a => Lift (Maybe a) where
    ll env                              = traverse (ll env)


instance Lift Stmt where
    ll env (Expr l e)                   = Expr l <$> ll env e
    ll env (Assign l pats e)            = Assign l <$> ll env pats <*> ll env e
    ll env (Update l ts e)              = Update l <$> ll env ts <*> ll env e
    ll env (IUpdate l t op e)           = IUpdate l <$> ll env t <*> pure op <*> ll env e
    ll env (Assert l e mbe)             = Assert l <$> ll env e <*> ll env mbe
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
      | otherwise                       = Decl l <$> ll env ds >>= liftToNext

instance Lift Decl where
    ll env (Def l n q ps _ks ann b m)
      | inClass env                     = do --traceM ("## ll Def (method) " ++ prstr n)
                                             Def l n q ps2 _ks ann <$> defBody env1 b <*> pure m
      where env1                        = setSelf p1 $ extLocals (bound ps1) $ extPrefix InDef n env
            (p1,t1,ps1)                 = case ps of PosPar n t _ ps1 -> (Just n, t, ps1); _ -> (Nothing, Nothing, ps)
            ps2                         = PosPar selfKW t1 Nothing ps1
    ll env (Def l n q ps _ks ann b m)
      | onTop env                       = do --traceM ("## ll Def (on top) " ++ prstr n)
                                             Def l n q ps _ks ann <$> defBody env1 b <*> pure m
      | otherwise                       = do --traceM ("## ll Def (nested) " ++ prstr n)
                                             Def l (liftedname env n) q ps' _ks ann <$> defBody env1 b <*> pure m
      where env1                        = extLocals (bound ps) $ extPrefix InDef n env
            ps'                         = addParams vs ps
            vs                          = extraArgs env n
    ll env (Class l n q cs b)
      | onTop env                       = do --traceM ("## ll Class (on top) " ++ prstr n)
                                             Class l n q cs <$> ll env1 b
      | otherwise                       = do --traceM ("## ll Class (nested) " ++ prstr n)
                                             prev <- swapLifted []
                                             b' <- classBody env1 b
                                             lifted <- swapLifted prev
                                             return $ Class l n q cs (b' ++ reverse lifted)
      where bvs                         = bound b
            env1                        = extPrefix InClass n env
    ll env d                            = return d
    

instance Lift Expr where
    ll env (Var l (NoQual n))
      | selfRef env n                   = pure $ Dot l0 (Var l0 (NoQual selfKW)) n
      | isSelf env n                    = pure $ Var l0 (NoQual selfKW)
      | Just vs <- findFree n env       = pure $ closure (liftedname env n) vs
    ll env e@Var{}                      = pure e
    ll env e@Int{}                      = pure e
    ll env e@Float{}                    = pure e
    ll env e@Imaginary{}                = pure e
    ll env e@Bool{}                     = pure e
    ll env e@None{}                     = pure e
    ll env e@NotImplemented{}           = pure e
    ll env e@Ellipsis{}                 = pure e
    ll env e@Strings{}                  = pure e
    ll env e@BStrings{}                 = pure e
    ll env (Call l (Var _ (NoQual n)) p _k)
      | Just vs <- findFree n env       = Call l (eVar (liftedname env n)) <$> ll env (extras vs p) <*> return _k
      where extras vs p                 = foldr (PosArg . eVar) p vs
    ll env (Call l e p _k)              = Call l <$> ll env e <*> ll env p <*> return _k
    ll env (Index l e ix)               = Index l <$> ll env e <*> ll env ix
    ll env (Slice l e sl)               = Slice l <$> ll env e <*> ll env sl
    ll env (Cond l e1 e e2)             = Cond l <$> ll env e1 <*> ll env e <*> ll env e2
    ll env (BinOp l e1 o e2)            = BinOp l <$> ll env e1 <*> pure o <*> ll env e2
    ll env (CompOp l e ops)             = CompOp l <$> ll env e <*> ll env ops
    ll env (UnOp l o e)                 = UnOp l o <$> ll env e
    ll env (Dot l e n)                  = Dot l <$> ll env e <*> pure n
    ll env (Lambda l ps _k e)           = do nn <- newName "lambda"
                                             let env1 = extLocals (bound ps) $ extPrefix InDef nn env
                                             --traceM ("## ll Lambda (nested)")
                                             b <- defBody env1 [Return l0 (Just e)]
                                             liftToTop $ Decl l0 $ [Def l nn [] ps' _k Nothing b NoMod]
                                             return (closure nn vs)
      where ps'                         = addParams vs ps
            vs                          = intersect (free e) (locals env) \\ bound ps
    ll env (Await l e)                  = Await l <$> ll env e
    ll env (Yield l e)                  = Yield l <$> ll env e
    ll env (YieldFrom l e)              = YieldFrom l <$> ll env e
    ll env (Tuple l es)                 = Tuple l <$> ll env es
    ll env (TupleComp l e co)           = TupleComp l <$> ll env1 e <*> ll env co
      where env1                        = extLocals (bound co) env
    ll env (Record l ks)                = Record l <$> ll env ks
    ll env (RecordComp l n e co)        = RecordComp l n <$> ll env1 e <*> ll env co
      where env1                        = extLocals (bound co) env
    ll env (List l es)                  = List l <$> ll env es
    ll env (ListComp l e co)            = ListComp l <$> ll env1 e <*> ll env co
      where env1                        = extLocals (bound co) env
    ll env (Dict l es)                  = Dict l <$> ll env es
    ll env (DictComp l e co)            = DictComp l <$> ll env1 e <*> ll env co
      where env1                        = extLocals (bound co) env
    ll env (Set l es)                   = Set l <$> ll env es
    ll env (SetComp l e co)             = SetComp l <$> ll env1 e <*> ll env co
      where env1                        = extLocals (bound co) env
    ll env (Paren l e)                  = Paren l <$> ll env e

instance Lift Exception where
    ll env (Exception e1 e2)            = Exception <$> ll env e1 <*> ll env e2

instance Lift Branch where
    ll env (Branch e ss)                = Branch <$> ll env e <*> ll env ss

instance Lift Handler where
    ll env (Handler ex ss)              = Handler ex <$> ll env ss
    
instance Lift Elem where
    ll env (Elem e)                     = Elem <$> ll env e
    ll env (Star e)                     = Star <$> ll env e

instance Lift Assoc where
    ll env (Assoc k v)                  = Assoc <$> ll env k <*> ll env v
    ll env (StarStar e)                 = StarStar <$> ll env e

instance Lift WithItem where
    ll env (WithItem e n)               = WithItem <$> ll env e <*> ll env n
    
instance Lift OpArg where
    ll env (OpArg o e)                  = OpArg o <$> ll env e

instance Lift PosArg where
    ll env (PosArg e p)                 = PosArg <$> ll env e <*> ll env p
    ll env PosNil                       = pure PosNil

instance Lift KwdArg where
    ll env (KwdArg n e k)               = KwdArg n <$> ll env e <*> ll env k
    ll env KwdNil                       = pure KwdNil

instance Lift Slice where
    ll env (Sliz l e1 e2 e3)            = Sliz l <$> ll env e1 <*> ll env e2 <*> ll env e3

instance Lift Comp where
    ll env (CompFor l target e c)       = CompFor l <$> ll env1 target <*> ll env e <*> ll env1 c
      where env1                        = extLocals (bound target) env
    ll env (CompIf l e c)               = CompIf l <$> ll env e <*> ll env c
    ll env NoComp                       = pure NoComp
    
instance Lift Pattern where
    ll env (PVar l n a)                 = return (PVar l n a)
    ll env (PParen l p)                 = PParen l <$> ll env p

instance Lift Target where
    ll env (TaVar l n)                  = return (TaVar l n)
    ll env (TIndex l e ix)              = TIndex l <$> ll env e <*> ll env ix
    ll env (TSlice l e sl)              = TSlice l <$> ll env e <*> ll env sl
    ll env (TDot l e n)                 = TDot l <$> ll env e <*> return n
    ll env (TParen l p)                 = TParen l <$> ll env p
