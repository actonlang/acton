-- SPDX-License-Identifier: BSD-3-Clause

module Acton.WitnessForwarding
  ( ForwardContext
  , ForwardPlan(..)
  , ForwardStep(..)
  , buildForwardContext
  , classQBinds
  , emptyForwardContext
  , forwardPlan
  , forwardPlans
  , slotType
  ) where

import qualified Acton.Boxing as B
import Acton.Builtin
import Acton.Env
import Acton.NameInfo
import Acton.Names
import Acton.Subst
import Acton.Syntax

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- Semantic forwarding plans -----------------------------------------------------------------------------

data ForwardStep             = ForwardStep
  { forwardStepOwner         :: TCon
  , forwardStepField         :: Name
  , forwardStepTarget        :: TCon
  } deriving (Eq,Show)

data ForwardPlan             = ForwardPlan
  { forwardSlotName          :: Name
  , forwardTargetSlot        :: Type
  , forwardProviderType      :: TCon
  , forwardProviderRoot      :: TCon
  , forwardProviderSteps     :: [ForwardStep]
  , forwardProviderSlot      :: Type
  } deriving (Eq,Show)

data ForwardObject           = ForwardObject
  { objectRoot               :: TCon
  , objectSteps              :: [ForwardStep]
  , objectType               :: TCon
  } deriving (Eq,Show)

data ForwardContext          = ForwardContext
  { localObjectsByProto      :: Map.Map QName [ForwardObject]
  , localObjectsByType       :: Map.Map QName [ForwardObject]
  }

emptyForwardContext          :: ForwardContext
emptyForwardContext          = ForwardContext Map.empty Map.empty


-- Current-module index ----------------------------------------------------------------------------------

-- Current-module classes are already resident in the typed tree.  Index their
-- witness objects once; imported objects are deliberately not enumerated here,
-- but are discovered through ModuleInfo's exact witness buckets below.
buildForwardContext          :: EnvF x -> [Name] -> ForwardContext
buildForwardContext env names= finish $ foldl' indexObject emptyForwardContext objects
  where roots                = [ TC (NoQ n) (map tVar $ qbound q)
                               | n@Derived{} <- names
                               , Just (NClass q _ _ _) <- [lookupName n env]
                               , nullConArgs env (NoQ n)
                               ]
        objects              = concatMap (walkObject env) roots
        finish context       = context
          { localObjectsByProto = Map.map reverse (localObjectsByProto context)
          , localObjectsByType = Map.map reverse (localObjectsByType context)
          }

        indexObject context object
                              = context
          { localObjectsByProto = foldl' (insert object) (localObjectsByProto context) protoKeys
          , localObjectsByType = foldl' (insert object) (localObjectsByType context) typeKeys
          }
          where owners       = map snd $ findAncestry env (objectType object)
                protoKeys    = uniqueKeys $ map tcname owners
                typeKeys     = uniqueKeys [ qn | owner <- owners, Just qn <- [witnessTypeKey owner] ]

        insert object objectsByKey key
                              = Map.insertWith (++) key [object] objectsByKey

walkObject                  :: EnvF x -> TCon -> [ForwardObject]
walkObject env root          = walk [] [] root
  where walk seen steps tc
          | tcname tc `elem` seen
                              = []
          | otherwise         = ForwardObject root steps tc :
                                concat [ walk (tcname tc : seen) (steps ++ [ForwardStep tc field target]) target
                                       | (field,target) <- witnessFields env tc
                                       ]

uniqueKeys                  :: Ord a => [a] -> [a]
uniqueKeys                   = go Set.empty
  where go _ []              = []
        go seen (key:keys)
          | Set.member key seen
                              = go seen keys
          | otherwise         = key : go (Set.insert key seen) keys


-- Resolution --------------------------------------------------------------------------------------------

forwardPlans                :: EnvF x -> ForwardContext -> TCon -> [ForwardPlan]
forwardPlans env context targetTc
  | not $ forwardClass (noq $ tcname targetTc)
                              = []
  | otherwise                 = [ plan
                                | (n,info) <- fullAttrEnv env targetTc
                                , forwardTarget info
                                , forwardableSlot n
                                , n `notElem` inherited
                                , n `notElem` direct
                                , Just targetSlot <- [slotType env targetTc n]
                                , Just plan <- [resolveForward env context targetTc n targetSlot]
                                ]
  where inherited            = [ n | (_,n) <- inheritedAttrs env (tcname targetTc) ]
        direct               = directNoDecMethods env (tcname targetTc)

forwardPlan                 :: EnvF x -> ForwardContext -> TCon -> Name -> Maybe ForwardPlan
forwardPlan env context tc n = first [ plan | plan <- forwardPlans env context tc, forwardSlotName plan == n ]

resolveForward              :: EnvF x -> ForwardContext -> TCon -> Name -> Type -> Maybe ForwardPlan
resolveForward env context targetTc n targetSlot
                              = first [ ForwardPlan n targetSlot providerTc root steps providerSlot
                                      | ownerTc <- slotOwners env targetTc n
                                      , ForwardObject root steps providerTc <- providerObjects env context ownerTc
                                      , providerTc /= targetTc
                                      , providerCoversOwner env providerTc ownerTc
                                      , concreteProvider env providerTc n
                                      , Just providerSlot <- [slotType env providerTc n]
                                      , compatibleSlots targetSlot providerSlot
                                      ]

providerObjects             :: EnvF x -> ForwardContext -> TCon -> [ForwardObject]
providerObjects env context owner
                              = uniqueObjects (local ++ imported)
  where local                = case witnessTypeKey owner of
                                Just qn -> Map.findWithDefault [] qn (localObjectsByType context)
                                Nothing -> Map.findWithDefault [] (tcname owner) (localObjectsByProto context)
        imported             = concatMap (witnessObjects env) (indexedWitnesses env owner)

indexedWitnesses            :: EnvF x -> TCon -> [Witness]
indexedWitnesses env owner   = uniqueWitnesses $ concatMap query (importedModuleInfos env)
  where query mi             = case witnessTypeKey owner of
                                Just qn -> moduleWitnessesByType mi qn
                                Nothing -> moduleWitnessesByProto mi (tcname owner)

witnessObjects              :: EnvF x -> Witness -> [ForwardObject]
witnessObjects env (WClass _ _ _ rootName path _)
                              = case classQBinds env rootName' of
                                  Just q
                                    | nullConArgs env rootName'
                                      -> let root = TC rootName' (map tVar $ qbound q)
                                         in follow root path (ForwardObject root [] root)
                                  _   -> []
  where rootName'            = localQName env rootName

        follow _ [] object   = [object]
        follow root (Left _ : rest) object
                              = follow root rest object
        follow root (Right witness : rest) object
                              = concat [ follow root rest $ ForwardObject root (objectSteps object ++ [step]) target
                                       | target <- witnessFieldTargets env (objectType object) (witAttr witness)
                                       , let step = ForwardStep (objectType object) (witAttr witness) target
                                       ]
witnessObjects _ WInst{}     = []

uniqueObjects               :: [ForwardObject] -> [ForwardObject]
uniqueObjects                = uniqueBy show

uniqueWitnesses             :: [Witness] -> [Witness]
uniqueWitnesses              = uniqueBy (show . key)
  where key (WClass _ t p n path _)
                              = (n,path,tcname p,t)
        key (WInst _ t p n path)
                              = (n,path,tcname p,t)

uniqueBy                    :: Ord b => (a -> b) -> [a] -> [a]
uniqueBy key                 = go Set.empty
  where go _ []              = []
        go seen (value:values)
          | Set.member valueKey seen
                              = go seen values
          | otherwise         = value : go (Set.insert valueKey seen) values
          where valueKey      = key value


-- Witness object paths ----------------------------------------------------------------------------------

witnessFields               :: EnvF x -> TCon -> [(Name,TCon)]
witnessFields env tc         = [ (n,target)
                               | (n,info) <- fullAttrEnv env tc
                               , isWitness n
                               , TCon _ fieldTc <- [fieldType info]
                               , target <- witnessTargets env tc fieldTc
                               ]

witnessFieldTargets         :: EnvF x -> TCon -> Name -> [TCon]
witnessFieldTargets env tc n = case lookup n (fullAttrEnv env tc) of
  Just info
    | TCon _ fieldTc <- fieldType info
                              -> witnessTargets env tc fieldTc
  _                           -> []

fieldType                   :: NameInfo -> Type
fieldType (NDef sc Property _)
                              = sctype sc
fieldType (NSig sc Property _)
                              = sctype sc
fieldType (NVar t)           = t
fieldType (NSVar t)          = t
fieldType _                  = tWild

witnessTargets              :: EnvF x -> TCon -> TCon -> [TCon]
witnessTargets env owner field
                              = case concreteWitnessTarget env owner field of
                                  Just concrete -> [concrete,field]
                                  Nothing       -> [field]

concreteWitnessTarget       :: EnvF x -> TCon -> TCon -> Maybe TCon
concreteWitnessTarget env owner field
                              = do q <- classQBinds env qn
                                   return $ TC qn (take (length $ qbound q) (tcargs owner ++ tcargs field))
  where qn                   = derivedWitnessQName (tcname owner) (tcname field)

derivedWitnessQName         :: QName -> QName -> QName
derivedWitnessQName (NoQ owner) field
                              = NoQ (Derived (noq field) owner)
derivedWitnessQName (GName m owner) field
                              = GName m (Derived (noq field) owner)
derivedWitnessQName (QName m owner) field
                              = GName m (Derived (noq field) owner)

classQBinds                 :: EnvF x -> QName -> Maybe QBinds
classQBinds env qn           = case tryQName qn env of
                                Just (NClass q _ _ _) -> Just q
                                _                     -> Nothing

localQName                 :: EnvF x -> QName -> QName
localQName env (GName m n)
  | Just m == thismod env    = NoQ n
localQName env (QName m n)
  | Just m == thismod env    = NoQ n
localQName _ qn             = qn


-- Slot compatibility -----------------------------------------------------------------------------------

slotOwners                  :: EnvF x -> TCon -> Name -> [TCon]
slotOwners env targetTc n    = [ ownerTc
                               | (_,ownerTc) <- drop 1 (findAncestry env targetTc)
                               , ownsSlotDirectly env ownerTc n
                               ]

ownsSlotDirectly           :: EnvF x -> TCon -> Name -> Bool
ownsSlotDirectly env tc n    = case findAttrInfoIn n te of
                                Just NDef{}  -> True
                                Just NSig{}  -> True
                                Just NVar{}  -> True
                                Just NSVar{} -> True
                                _            -> False
  where (_,te)              = findCon env tc

providerCoversOwner        :: EnvF x -> TCon -> TCon -> Bool
providerCoversOwner env provider owner
                              = any ((== owner) . snd) (findAncestry env provider)

concreteProvider           :: EnvF x -> TCon -> Name -> Bool
concreteProvider env tc n    = case lookup n (fullAttrEnv env tc) of
                                Just NDef{}  -> True
                                Just NVar{}  -> True
                                Just NSVar{} -> True
                                _            -> False

slotType                   :: EnvF x -> TCon -> Name -> Maybe Type
slotType env tc n            = case lookup n (fullAttrEnv env tc) of
                                Just (NDef _ Static _) -> Just rt
                                Just (NSig _ Static _) -> Just rt
                                Just (NDef _ NoDec _)  -> Just methodSlot
                                Just (NSig _ NoDec _)  -> Just methodSlot
                                _                      -> Nothing
  where rt                  = B.rtypeOf env tc n
        methodSlot          = vsubst [(tvSelf,tCon tc)] $ addSelf rt (Just NoDec)

compatibleSlots            :: Type -> Type -> Bool
compatibleSlots (TFun _ fx1 p1 k1 t1) (TFun _ fx2 p2 k2 t2)
                              = fx1 == fx2 && k1 == k2 && t1 == t2 && dropFirstRow p1 == dropFirstRow p2
compatibleSlots _ _         = False

dropFirstRow               :: Type -> Type
dropFirstRow (TRow _ _ _ _ r)= r
dropFirstRow r              = r

directNoDecMethods         :: EnvF x -> QName -> [Name]
directNoDecMethods env qn    = case findQName qn env of
                                NClass _ _ te _ -> [ n | (n,NDef _ NoDec _) <- te ]
                                _               -> []

nullConArgs                :: EnvF x -> QName -> Bool
nullConArgs env qn           = case findQName qn env of
                                NClass _ _ te _ -> case lookup initKW te of
                                  Just (NDef sc _ _) -> initArity sc == 0
                                  Just (NSig sc _ _) -> initArity sc == 0
                                  _                  -> False
                                _               -> False
  where initArity (TSchema _ _ (TFun _ _ row _ _))
                              = arity row
        initArity _          = 1

witnessTypeKey             :: TCon -> Maybe QName
witnessTypeKey (TC _ (TCon _ tc : _))
                              = Just (tcname tc)
witnessTypeKey (TC _ (TVar _ tv : _))
                              = Just (NoQ $ tvname tv)
witnessTypeKey _            = Nothing

forwardTarget              :: NameInfo -> Bool
forwardTarget (NDef _ NoDec _)
                              = True
forwardTarget (NSig _ NoDec _)
                              = True
forwardTarget _             = False

forwardableSlot            :: Name -> Bool
forwardableSlot n            = n `notElem` ([initKW,serializeKW,deserializeKW] ++ valueKWs)

forwardClass               :: Name -> Bool
forwardClass Derived{}       = True
forwardClass _               = False

first                       :: [a] -> Maybe a
first (x:_)                  = Just x
first []                     = Nothing
