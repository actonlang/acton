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

import qualified Control.Exception
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char
import Error.Diagnose

import Acton.Syntax
import Acton.Printer
import Utils
import Pretty

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
                                              where (loc,str) : _ = typeError err

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
substitute tv t                         = lift $
                                          --trace ("  #substitute " ++ prstr tv ++ " ~ " ++ prstr t) $
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
                                    | KwdNotFound ErrInfo Name
                                    | KwdUnexpected ErrInfo Name
                                    | PosElemNotFound ErrInfo String
                                    | IncompatError ErrInfo String
                                    | EscapingVar [TVar] TSchema
                                    | NoSelStatic Name TCon
                                    | NoSelInstByClass Name TCon
                                    | NoMut Name
                                    | LackSig Name
                                    | LackDef Name
                                    | SurplusRow PosRow
                                    | NoRed Constraint
                                    | NoSolve (Maybe Type) [Type] [Constraint]
                                    | NoUnify ErrInfo Type Type
                                    deriving (Show)

instance Control.Exception.Exception TypeError

instance HasLoc TypeError where
    loc (TypeError l str)           = l
    loc (RigidVariable tv)          = loc tv
    loc (InfiniteType tv)           = loc tv
    loc (ConflictingRow tv)         = loc tv
    loc (KwdNotFound _ n)           = loc n
    loc (KwdUnexpected _ n)         = loc n
    loc (PosElemNotFound info s)    = loc info -- NoLoc     -- TODO: supply position
    loc (EscapingVar tvs t)         = loc tvs
    loc (NoSelStatic n u)           = loc n
    loc (NoSelInstByClass n u)      = loc n
    loc (NoMut n)                   = loc n
    loc (LackSig n)                 = loc n
    loc (LackDef n)                 = loc n
    loc (SurplusRow p)              = NoLoc     -- TODO: supply position
    loc (NoRed c)                   = loc c
    loc (NoSolve _ _ _)             = NoLoc
    loc (NoUnify info t1 t2)        = loc info

intro t mbe                            = case mbe of
                                             Nothing ->  pretty t
                                             Just e ->   text "The type of the indicated expression" <+> text "(" Pretty.<>
                                                           (if isGen t then text "which we call" else text "inferred to be") <+> pretty t Pretty.<> text ")"
   where isGen (TCon _ (TC (NoQ (Name _ ('t' : ds))) [])) = all isDigit ds
         isGen _ = False
         
explainViolation c                   = case info c of
                                          Simple l s -> text s
                                          DfltInfo l n mbe ts -> -- text (show n)  $+$ 
                                               (case c of 
                                                  Cast _ t1 t2  -> intro t1 mbe <+> text "is not a subclass of" <+> pretty t2
                                                  Sub _ _ t1 t2 -> intro t1 mbe <+> text "is not a subtype of" <+> pretty t2
                                                  Impl _ _ t p -> intro  t mbe <+> text "does not implement" <+> pretty p
                                                  Sel _ _ t n t0 -> intro t mbe <+> text "does not have an attribute" <+> pretty n <+> text "with type" <+> pretty t0
                                                  _ -> pretty c <+> text "does not hold") 
                                          DeclInfo _ _ n sc msg -> text msg  -- $+$ pretty n <+> text "is inferred to have type"<+> pretty sc

explainRequirement c                = case info c of
                                          Simple l s -> text s
                                          DfltInfo l n mbe ts ->
                                             (if ts /= []
                                              then text (concatMap (\(n,s,t) -> Pretty.print n ++ " has had its polymorphic type "
                                                            ++  Pretty.print s ++ " instantiated to " ++ Pretty.print t) ts++", so ")  
                                                                    
                                              else empty) Pretty.<> 
                                               (case c of
                                                   Cast _ t1 t2 -> intro t1 mbe <+> text "must be a subclass of" <+> pretty t2
                                                   Sub i _ t1 t2 -> intro t1 mbe <+> text "must be a subtype of" <+> pretty t2 
                                                   Impl _ _ t p -> intro t mbe <+> text "must implement" <+> pretty p
                                                   Sel _ _ t n t0 -> intro t mbe <+> text "must have an attribute" <+> pretty n <+> text "with type" <+> pretty t0
                                                                          Pretty.<> text "; no such type is known."
                                                   _ -> pretty c <+> text "must hold")  
                                          DeclInfo _ _ n sc msg -> text msg   -- $+$ pretty n <+> text "is inferred to have type"<+> pretty sc



useless vs c                           = case c of
                                             Cast _ t1 t2 -> f t1 || f t2
                                             Sub _ _ t1 t2 -> f t1 || f t2
                                             Impl _ _ t p -> f t
                                             Sel _ _ t n t0 -> f t || f t0
                                             Mut _ t1 n t2 -> True   -- TODO
                                             Seal _ _ -> True        -- TODO
     where f (TVar _ v) = notElem v (tvSelf : vs)
           f _          = False

--typeReport :: TypeError -> Report
typeReport (TypeError l msg) filename src           = Err Nothing msg [(locToPosition l filename src, This msg)] []
typeReport (RigidVariable tv) filename src          = Err Nothing msg [(locToPosition (loc tv) filename src, This msg)] []
                                                      where msg = render (text "Type" <+> pretty tv <+> text "is rigid")
typeReport (InfiniteType tv) filename src           = Err Nothing msg [(locToPosition (loc tv) filename src, This msg)] []
                                                      where msg = render (text "Type" <+> pretty tv <+> text "is infinite")
typeReport (ConflictingRow tv) filename src         = Err Nothing msg [(locToPosition (loc tv) filename src, This msg)] []
                                                      where msg = render (text "Type" <+> pretty tv <+> text "has conflicting extensions")
typeReport (KwdNotFound info n) filename src        = Err Nothing "Keyword argument missing" [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Keyword element" <+> quotes (pretty n) <+> text "is not found")
typeReport (KwdUnexpected info n) filename src      = Err Nothing "Unexpected keyword argument" [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Unexpected keyword argument" <+> quotes (pretty n))
typeReport (PosElemNotFound info s) filename src    = Err Nothing s [(locToPosition (loc info) filename src, This s)] []
typeReport (EscapingVar tvs t) filename src         = Err Nothing msg [(locToPosition (loc tvs) filename src, This msg)] []
                                                      where msg = render (text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                                                  pretty (head tvs) <+> text "escapes")
typeReport (NoSelStatic n u) filename src           = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance")
typeReport (NoSelInstByClass n u) filename src      = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u)
typeReport (NoMut n) filename src                   = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Non @property attribute" <+> pretty n <+> text "cannot be mutated")
typeReport (LackSig n) filename src                 = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Declaration lacks accompanying signature")
typeReport (LackDef n) filename src                 = Err Nothing msg [(locToPosition (loc n) filename src, This msg)] []
                                                      where msg = render (text "Signature lacks accompanying definition")
typeReport (NoRed c) filename src
    | DeclInfo l1 l2 n _ _ <- info c = Err
                                         Nothing
                                         "Constraint violation"
                                         [ (locToPosition l1 filename src, This (render (explainRequirement c <+> parens (explainRequirement c{info = dummyInfo}))))
                                         , (locToPosition l2 filename src, Where (Pretty.print n ++ " is defined here"))
                                         ]
                                         []
    | otherwise                      = Err
                                          Nothing
                                          "Constraint violation"
                                          [(locToPosition (loc c) filename src, This (render (explainRequirement c)))]
                                          []

typeReport (NoSolve mbt vs cs) filename src         =
    let header = trace (show (head cs)) $ case length cs of
                    0 -> "Unable to give good error message: please report example"
                    1 -> "Cannot satisfy the following constraint:"
                    _ -> "Cannot satisfy the following simultaneous constraints for the unknown " ++
                         (if length vs == 1
                          then "type " ++ case head vs of
                                          TCon _ tc -> nameStr (noq (tcname tc))
                                          _ -> show (head vs)
                          else "types")
        -- Each constraint gets its own complete error message with source line
        constraint_messages = concatMap (typeError . NoRed) cs
        -- Filter out empty positions and merge their messages into the first real position
        (noLocs, withLocs) = partition ((==NoLoc) . fst) constraint_messages
        withLocsMsgs = case (withLocs, noLocs) of
            ([], []) -> [(NoLoc, "Error: No location information")]
            ([], (l,m):_) -> [(l,m)]
            ((l,m):rest, extras) -> (l, m ++ "\n" ++ concatMap snd extras) : rest
    in Err
        Nothing
        header
        [(locToPosition l filename src, This m) | (l,m) <- withLocsMsgs]
        []
  where
        nameStr (Name _ str) = str

typeReport (NoUnify (Simple l msg) _ _) filename src = Err Nothing "Type unification error" [(locToPosition l filename src, This msg)] []
typeReport (NoUnify info t1 t2) filename src        =
    case (loc t1, loc t2) of
        (l1@Loc{}, l2@Loc{}) -> Err
                                 Nothing
                                 "Type unification error"
                                 [ (locToPosition l1 filename src, This "First type appears here")
                                 , (locToPosition l2 filename src, This "Second type appears here")
                                 ]
                                 []
        _                     -> Err
                                 Nothing
                                 "Type unification error"
                                 [(locToPosition (getLoc[loc info, loc t1, loc t2]) filename src, This msg)]
                                 []
    where msg = render (text "Incompatible types" <+> pretty t1 <+> text "and" <+> pretty t2)

typeReport (IncompatError info msg) filename src    =
    case info of
        DeclInfo l1 l2 n sc msg1    -> Err
                                         Nothing
                                         "Incompatible types"
                                         [ (locToPosition l1 filename src, This msg)
                                         , (locToPosition l2 filename src, Where (Pretty.print n ++ " is defined here"))
                                         ]
                                         []
        _                           -> Err
                                         Nothing
                                         "Incompatible types"
                                         [(locToPosition (loc info) filename src, This msg)]
                                         []
typeReport (SurplusRow p) filename src =
                                    Err Nothing "Too many arguments supplied" [(locToPosition NoLoc filename src, This (prstr (label p)))] []


typeError                           :: TypeError -> [(SrcLoc, String)]
typeError (TypeError l str)          = [(l, str)]
typeError (RigidVariable tv)         = [(loc tv, render (text "Type" <+> pretty tv <+> text "is rigid"))]
typeError (InfiniteType tv)          = [(loc tv, render (text "Type" <+> pretty tv <+> text "is infinite"))]
typeError (ConflictingRow tv)        = [(loc tv, render (text "Type" <+> pretty tv <+> text "has conflicting extensions"))]
typeError (KwdNotFound _ n)          = [(loc n, render (text "Keyword element" <+> quotes (pretty n) <+> text "is not found"))]
typeError (KwdUnexpected _ n)        = [(loc n, render (text "Keyword element" <+> quotes (pretty n) <+> text "is not expected"))]
typeError (PosElemNotFound info s)   = [(loc info, s)]
typeError (EscapingVar tvs t)        = [(loc tvs, render (text "Type annotation" <+> pretty t <+> text "is too general, type variable" <+>
                                        pretty (head tvs) <+> text "escapes"))]
typeError (NoSelStatic n u)          = [(loc n, render (text "Static method" <+> pretty n <+> text "cannot be selected from" <+> pretty u <+> text "instance"))]
typeError (NoSelInstByClass n u)     = [(loc n, render (text "Instance attribute" <+> pretty n <+> text "cannot be selected from class" <+> pretty u))]
typeError (NoMut n)                  = [(loc n, render (text "Non @property attribute" <+> pretty n <+> text "cannot be mutated"))]
typeError (LackSig n)                = [(loc n, render (text "Declaration lacks accompanying signature"))]
typeError (LackDef n)                = [(loc n, render (text "Signature lacks accompanying definition"))]
typeError (NoRed c)
    | DeclInfo l1 l2 _ _ _ <- info c = [(min l1 l2,""), (max l1 l2,render (explainRequirement c <+> parens (explainRequirement c{info = dummyInfo})))]
--    | DfltInfo l n mbe is <- info c  = [(loc c, render (explainRequirement c <+> parens (text ("errcode " ++ show n))))]
    | otherwise                      = [(loc c, render (explainRequirement c))]
typeError (NoSolve mbt vs cs)        = case length cs of
                                           0 -> [(NoLoc, "Unable to give good error message: please report example")]
                                           1 ->  (NoLoc, "Cannot satisfy the following constraint:\n") : concatMap mkReq cs
                                           _ ->  (NoLoc, "Cannot satisfy the following simultaneous constraints for the unknown "
                                                         ++ (if length vs==1 then "type " else "types ") ++ render(commaList vs)  ++":\n")
                                                : concatMap mkReq cs
         where mkReq                 = typeError . NoRed
typeError (NoUnify info t1 t2)       = case (loc t1, loc t2) of
                                          (l1@Loc{},l2@Loc{}) -> [(l1, ""),(l2,render(text "Incompatible types" <+> pretty t1 <+> text "and" <+> pretty t2))]
                                          _ ->  [(getLoc[loc info, loc t1, loc t2],render(text "Incompatible types" <+> pretty t1 <+> text "and" <+> pretty t2))]
typeError (IncompatError info msg)   = case info of
                                           DeclInfo l1 l2 f sc _ -> [(min l1 l2,""),(max l1 l2,msg)]
                                           _ -> [(loc info, msg)]



tyerr x s                           = throwError $ TypeError (loc x) (s ++ " " ++ prstr x)
tyerrs xs s                         = throwError $ TypeError (loc $ head xs) (s ++ " " ++ prstrs xs)
rigidVariable tv                    = throwError $ RigidVariable tv
infiniteType tv                     = throwError $ InfiniteType tv
conflictingRow tv                   = throwError $ ConflictingRow tv
kwdNotFound info n                  = throwError $ incompatError info (render(text ("keyword " ++ elemSpec info) <+> quotes (pretty n) <+> text ("is missing" ++ elemSuffix info)))
kwdUnexpected info n                = throwError $ KwdUnexpected info n
escapingVar tvs t                   = throwError $ EscapingVar tvs t
noSelStatic n u                     = throwError $ NoSelStatic n u
noSelInstByClass n u                = throwError $ NoSelInstByClass n u
noMut n                             = throwError $ NoMut n
lackSig ns                          = throwError $ LackSig (head ns)
lackDef ns                          = throwError $ LackDef (head ns)
surplusRow p                        = throwError $ SurplusRow p
noRed c                             = throwError $ NoRed c
noSolve mbt vs cs                   = throwError $ NoSolve mbt vs cs
noUnify info t1 t2                  = throwError $ NoUnify info t1 t2

posElemNotFound b c n               = throwError $ incompatError (info c) ("too " ++ (if b then "few " else "many positional ") ++ elemSpec (info c) ++ elemSuffix (info c))
 
incompatError info msg             = case info of
                                        DeclInfo l1 l2 f sc msg1 -> IncompatError info (msg ++ Pretty.print f)
                                        _ -> IncompatError info msg

elemSpec DeclInfo{}               = "argument(s)"
elemSpec _                        = "component(s)"

elemSuffix DeclInfo{}             = " in call to "
elemSuffix _                      = " in tuple"

-- elemHint DeclInfo{}               = " Hint: The previous definition may have been implicit, using positional notation."
-- elemHint _                        = ""

dummyInfo                         = DfltInfo NoLoc 0 Nothing []


--mkErrorDiagnostic :: String -> String -> Report String -> Diagnostic String
mkErrorDiagnostic filename src report =
  let diag = addFile mempty filename src
  in addReport (addFile diag filename src) report

-- | Convert internal locations to Diagnose positions
locToPosition :: SrcLoc -> String -> String -> Position
locToPosition NoLoc _ _ =
  Position (0,0) (0,0) ""  -- Empty position
locToPosition (Loc start end) filename src =
  -- Convert byte offsets to line/col positions by counting in source
  let startPos = offsetToLineCol start src
      (endLine, endCol) = offsetToLineCol end src
      -- For multi-line spans, adjust end line to match original error format
      finalEndPos = if endLine > fst startPos
                   then (endLine - 1, endCol)
                   else (endLine, endCol)
  in Position startPos finalEndPos filename

-- | Helper to convert byte offset to line/col tuple
offsetToLineCol :: Int -> String -> (Int, Int)
offsetToLineCol offset src =
  let beforeOffset = take offset src
      lines = splitLines beforeOffset
      lineNum = length lines
      colNum = if null lines
               then 1
               else (length (last lines) + 1)
  in (lineNum, colNum)
  where
    splitLines [] = [""]
    splitLines s =
      let (first, rest) = break (=='\n') s
      in first : case rest of
                  [] -> []
                  (_:rest') -> splitLines rest'
