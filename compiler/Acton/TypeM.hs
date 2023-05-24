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
module Acton.TypeM where

import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Control.Exception

import Acton.Syntax
import Acton.Printer
import Utils

-- Type inference monad ------------------------------------------------------------------

type TVarMap                            = Map TVar Type

data TypeState                          = TypeState {
                                                nextint         :: Int,
                                                effectstack     :: [(TFX,Type)],
                                                deferred        :: Constraints,
                                                currsubst       :: TVarMap
                                          }

initTypeState s                         = TypeState { nextint = 1, effectstack = [], deferred = [], currsubst = s }

type TypeM a                            = ExceptT TypeError (State TypeState) a

runTypeM                                :: TypeM a -> a
runTypeM m                              = runTypeM' [] m

runTypeM'                               :: Substitution -> TypeM a -> a
runTypeM' s m                           = case evalState (runExceptT m) (initTypeState $ Map.fromList s) of
                                            Right x  -> x
                                            Left err -> error ("Unhandled TypeM exception: " ++ prstr loc ++ ": " ++ prstr str)
                                              where (loc,str) = typeError err

currentState                            :: TypeM TypeState
currentState                            = lift $ state $ \st -> (st, st)

rollbackState                           :: TypeState -> TypeM ()
rollbackState st                        = lift $ state $ \_ -> ((), st)

newUnique                               :: TypeM Int
newUnique                               = lift $ state $ \st -> (nextint st, st{ nextint = nextint st + 1 })

pushFX                                  :: TFX -> Type -> TypeM ()
pushFX fx ret                           = lift $ state $ \st -> ((), st{ effectstack = (fx,ret) : effectstack st })

currFX                                  :: TypeM TFX
currFX                                  = lift $ state $ \st -> (fst $ head $ effectstack st, st)

currRet                                 :: TypeM Type
currRet                                 = lift $ state $ \st -> (snd $ head $ effectstack st, st)

popFX                                   :: TypeM ()
popFX                                   = lift $ state $ \st -> ((), st{ effectstack = tail (effectstack st) })

defer                                   :: Constraints -> TypeM ()
defer cs                                = lift $ state $ \st -> ((), st{ deferred = cs ++ deferred st })

collectDeferred                         :: TypeM Constraints
collectDeferred                         = lift $ state $ \st -> (deferred st, st{ deferred = [] })

substitute                              :: TVar -> Type -> TypeM ()
substitute tv t                         = lift $ {-trace ("  #substitute " ++ prstr tv ++ " ~ " ++ prstr t) $ -}
                                          state $ \st -> ((), st{ currsubst = Map.insert tv t (currsubst st)})

getSubstitution                         :: TypeM (Map TVar Type)
getSubstitution                         = lift $ state $ \st -> (currsubst st, st)

setSubstitution                         :: Map TVar Type -> TypeM ()
setSubstitution s                       = lift $ state $ \st -> ((), st{ currsubst = s })


-- Name generation ------------------------------------------------------------------------------------------------------------------

newWitness                              = Internal Witness "" <$> newUnique

newTmp                                  = Internal Tempvar "" <$> newUnique

newTVarOfKind k                         = TVar NoLoc <$> TV k <$> Internal Typevar (str k) <$> newUnique
  where str KType                       = ""
        str KFX                         = "x"
        str PRow                        = "p"
        str KRow                        = "k"
        str _                           = ""

newTVarToken n                          = TVar NoLoc $ TV KWild $ Internal Typevar "z" n

newTVars ks                             = mapM newTVarOfKind ks

newTVar                                 = newTVarOfKind KType

-- Type errors ---------------------------------------------------------------------------------------------------------------------

data TypeError                      = TypeError SrcLoc String
                                    | RigidVariable TVar
                                    | InfiniteType TVar
                                    | ConflictingRow TVar
                                    | KwdNotFound Name
                                    | PosElemNotFound
                                    | EscapingVar [TVar] TSchema
                                    | NoSelStatic Name TCon
                                    | NoSelInstByClass Name TCon
                                    | NoMut Name
                                    | LackSig Name
                                    | LackDef Name
                                    | NoRed Constraint
                                    | NoSolve [Constraint]
                                    | NoUnify Type Type
                                    deriving (Show)

instance Control.Exception.Exception TypeError

instance HasLoc TypeError where
    loc (TypeError l str)           = l
    loc (RigidVariable tv)          = loc tv
    loc (InfiniteType tv)           = loc tv
    loc (ConflictingRow tv)         = loc tv
    loc (KwdNotFound n)             = loc n
    loc (PosElemNotFound)           = NoLoc     -- TODO: supply position
    loc (EscapingVar tvs t)         = loc tvs
    loc (NoSelStatic n u)           = loc n
    loc (NoSelInstByClass n u)      = loc n
    loc (NoMut n)                   = loc n
    loc (LackSig n)                 = loc n
    loc (LackDef n)                 = loc n
    loc (NoRed c)                   = loc c
    loc (NoSolve cs)                = loc cs
    loc (NoUnify t1 t2)             = loc t1

typeError                           :: TypeError -> (SrcLoc, String)
typeError err                       = (loc err, render (expl err))
  where
    expl (TypeError l str)          = text str
    expl (RigidVariable tv)         = text "Type" <+> pretty tv <+> text "is rigid"
    expl (InfiniteType tv)          = text "Type" <+> pretty tv <+> text "is infinite"
    expl (ConflictingRow tv)        = text "Row" <+> pretty tv <+> text "has conflicting extensions"
    expl (KwdNotFound n)            = text "Keyword element" <+> quotes (pretty n) <+> text "is not found"
    expl (PosElemNotFound)          = text "Positional element is not found"
    expl (EscapingVar tvs t)        = text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                      pretty (head tvs) <+> text "escapes"
    expl (NoSelStatic n u)          = text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"
    expl (NoSelInstByClass n u)     = text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u
    expl (NoMut n)                  = text "Non @property attribute" <+> pretty n <+> text "cannot be mutated"
    expl (LackSig n)                = text "Declaration lacks accompanying signature"
    expl (LackDef n)                = text "Signature lacks accompanying definition"
    expl (NoRed c)                  = text "Cannot infer" <+> pretty c
    expl (NoSolve cs)               = text "Cannot solve" <+> commaSep pretty cs
    expl (NoUnify t1 t2)            = text "Cannot unify" <+> pretty t1 <+> text "and" <+> pretty t2

tyerr x s                           = throwError $ TypeError (loc x) (s ++ " " ++ prstr x)
tyerrs xs s                         = throwError $ TypeError (loc $ head xs) (s ++ " " ++ prstrs xs)
rigidVariable tv                    = throwError $ RigidVariable tv
infiniteType tv                     = throwError $ InfiniteType tv
conflictingRow tv                   = throwError $ ConflictingRow tv
kwdNotFound n | n == name "_"       = throwError $ PosElemNotFound
              | otherwise           = throwError $ KwdNotFound n
escapingVar tvs t                   = throwError $ EscapingVar tvs t
noSelStatic n u                     = throwError $ NoSelStatic n u
noSelInstByClass n u                = throwError $ NoSelInstByClass n u
noMut n                             = throwError $ NoMut n
lackSig ns                          = throwError $ LackSig (head ns)
lackDef ns                          = throwError $ LackDef (head ns)
noRed c                             = throwError $ NoRed c
noSolve cs                          = throwError $ NoSolve cs
noUnify t1 t2                       = throwError $ NoUnify t1 t2
