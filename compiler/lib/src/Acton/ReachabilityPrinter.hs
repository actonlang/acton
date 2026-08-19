-- SPDX-License-Identifier: BSD-3-Clause

-- | Human-readable views of reachability information.
--
-- 'prettyRows' shows the dependency rows recorded in one TYDB interface;
-- 'prettySelection' shows the closed set selected for a whole project.  The
-- former is produced by 'prepareReachabilityRows' and the latter by
-- 'selectProgram'.  These renderings are used by @acton sig
-- --reachability@ and contain no analysis or selection logic of their own.
module Acton.ReachabilityPrinter (prettySelection, prettyRows) where

import Prelude hiding ((<>))

import qualified Acton.InterfaceRows as Rows
import qualified Acton.Reachability as Reachability
import qualified Acton.ReachabilityRows as ReachRows
import qualified Acton.Syntax as A

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Pretty


-- Closed selection --------------------------------------------------------------------------------------

prettySelection :: Set.Set A.ModName -> Reachability.Selection -> String
prettySelection whole selection = render $
    text "reachability" $+$ nest 2 (vcatOrNone $ map prettyModule modules)
  where
    modules = Set.toAscList $ whole `Set.union` Map.keysSet keysByModule
    selectedKeys = Set.unions
      [ Reachability.selectedDeclarations selection
      , Reachability.selectedTops selection
      , Reachability.selectedOpaqueTops selection
      , Set.map fst $ Reachability.selectedMembers selection
      , Set.map fst $ Reachability.selectedAttrs selection
      , Set.map fst $ Reachability.selectedStaticInitializers selection
      , Set.map fst $ Reachability.selectedInstanceInitializers selection
      , Set.map fst $ Reachability.selectedGenerated selection
      , Reachability.selectedConstructed selection
      , Reachability.selectedInitialized selection
      ]
    keysByModule = Set.foldl' addKey Map.empty selectedKeys
    addKey byModule key@(ReachRows.TopKey mn _) =
      Map.insertWith Set.union mn (Set.singleton key) byModule

    membersByTop = groupSet $ Reachability.selectedMembers selection
    attrsByTop = groupSet $ Reachability.selectedAttrs selection
    staticInitializersByTop = groupSet $ Reachability.selectedStaticInitializers selection
    instanceInitializersByTop = groupSet $ Reachability.selectedInstanceInitializers selection
    generatedByTop = groupSet $ Reachability.selectedGenerated selection

    prettyModule mn =
      let keys = Set.toAscList $ Map.findWithDefault Set.empty mn keysByModule
          header = text "module" <+> prettyModuleName mn <>
            flags [ "whole" | Set.member mn whole ]
      in header $+$ nest 2 (vcat $ map prettyTop keys)

    prettyTop key@(ReachRows.TopKey _ name) =
      let topFlags =
            [ "declaration" | Set.member key $ Reachability.selectedDeclarations selection ] ++
            [ "body"        | Set.member key $ Reachability.selectedTops selection ] ++
            [ "opaque"      | Set.member key $ Reachability.selectedOpaqueTops selection ] ++
            [ "constructed" | Set.member key $ Reachability.selectedConstructed selection ] ++
            [ "initialized" | Set.member key $ Reachability.selectedInitialized selection ]
          members              = Map.findWithDefault Set.empty key membersByTop
          selectedAttrs        = Map.findWithDefault Set.empty key attrsByTop
          staticInitializers   = Map.findWithDefault Set.empty key staticInitializersByTop
          instanceInitializers = Map.findWithDefault Set.empty key instanceInitializersByTop
          generated            = Map.findWithDefault Set.empty key generatedByTop
          attrs = Set.toAscList $ Set.fromList
            ([ n | Rows.Attr n <- Set.toAscList members ] ++
             Set.toAscList selectedAttrs ++
             Set.toAscList staticInitializers ++
             Set.toAscList instanceInitializers ++
             [ n | ReachRows.AttrRef n <- Set.toAscList generated ])
          methods = Set.toAscList $ Set.fromList
            ([ n | Rows.Method n <- Set.toAscList members ] ++
             [ n | ReachRows.MethodRef n <- Set.toAscList generated ])
          constructor = Set.member Rows.InitRest members
          memberDocs =
            [ text "constructor" <> flags ["body"] | constructor ] ++
            map (prettyAttr members selectedAttrs staticInitializers
                   instanceInitializers generated) attrs ++
            map (prettyMethod members generated) methods
      in text (A.rawstr name) <> flags topFlags $+$ nest 2 (vcat memberDocs)

    prettyAttr members selectedAttrs staticInitializers instanceInitializers generated name =
      text "attr" <+> text (A.rawstr name) <> flags
        ([ "declaration"          | Set.member (Rows.Attr name) members ] ++
         [ "used"                 | Set.member name selectedAttrs ] ++
         [ "static initializer"   | Set.member name staticInitializers ] ++
         [ "instance initializer" | Set.member name instanceInitializers ] ++
         [ "generated"            | Set.member (ReachRows.AttrRef name) generated ])

    prettyMethod members generated name =
      text "method" <+> text (A.rawstr name) <> flags
        ([ "body"      | Set.member (Rows.Method name) members ] ++
         [ "generated" | Set.member (ReachRows.MethodRef name) generated ])


-- Persisted analysis ------------------------------------------------------------------------------------

prettyRows :: A.ModName -> Maybe A.Name -> ReachRows.ReachabilityRows -> String
prettyRows moduleName target rows = render $
    text "reachability in" <+> prettyModuleName moduleName $+$
    nest 2 (vcat $ moduleDocs ++ map prettyTop (Map.toAscList $ ReachRows.reachTopRows rows))
  where
    membersByTop = groupMap $ ReachRows.reachMemberRows rows
    slotsByTop   = groupMap $ ReachRows.reachSlotRows rows

    moduleDocs = case target of
      Just _  -> []
      Nothing ->
        [ summary "module initialization" (ReachRows.reachModuleSummary rows)
        , summary "whole module" (ReachRows.reachWholeSummary rows)
        ]

    prettyTop (key@(ReachRows.TopKey _ name),info) =
      let mShape = Map.lookup key (ReachRows.reachShapeRows rows)
          heading = maybe (text "name") (text . shapeLabel . ReachRows.shapeKind) mShape <+>
            text (A.rawstr name) <> flags [ "opaque" | ReachRows.OpaqueTop{} <- [info] ]
          body = case info of
            ReachRows.LocalTop _ dependencies -> dependencies
            ReachRows.OpaqueTop dependencies  -> dependencies
          members = Map.toAscList $ Map.findWithDefault Map.empty key membersByTop
          slots = Map.toAscList $ Map.findWithDefault Map.empty key slotsByTop
          reflection = Map.lookup key (ReachRows.reachReflectableRows rows)
          details = [summary "dependencies" body] ++
            maybe [] (pure . prettyShape) mShape ++
            map prettyMember members ++ map prettySlot slots ++
            maybe [] prettyReflection reflection
      in heading $+$ nest 2 (vcat details)

    prettyShape shape = vcat $
      [ text "lineage:" <+> commaSep prettyTopKey (ReachRows.shapeLineage shape) ] ++
      maybe [] (pure . prettyConstructor) (ReachRows.shapeConstructor shape) ++
      [ text "abstract:" <+> commaSep prettyMemberRef (ReachRows.shapeAbstracts shape)
      | not (null $ ReachRows.shapeAbstracts shape)
      ]

    prettyConstructor (provider,constructor) =
      let (kind,dependencies) = case constructor of
            ReachRows.StoredConstructor deps    -> ("stored",Just deps)
            ReachRows.GeneratedConstructor deps -> ("generated",Just deps)
            ReachRows.InheritedConstructor deps -> ("inherited",Just deps)
            ReachRows.OpaqueConstructor         -> ("opaque",Nothing)
          heading = text "constructor:" <+> prettyTopKey provider <> flags [kind]
      in heading $+$ nest 2 (maybe empty (summary "dependencies") dependencies)

    prettyMember (member,info) =
      let label = case member of
            Rows.Method name       -> text "method" <+> text (A.rawstr name)
            Rows.Attr name         -> text "attr" <+> text (A.rawstr name)
            Rows.StaticInit name   -> text "static initializer" <+> text (A.rawstr name)
            Rows.InstanceInit name -> text "instance initializer" <+> text (A.rawstr name)
            Rows.InitRest          -> text "constructor body"
          bodyLabel = case member of
            Rows.Attr{} -> "declaration dependencies"
            _           -> "dependencies"
          details = [summary bodyLabel $ ReachRows.memberSummary info] ++
            maybe [] (pure . summary "static initializer")
              (ReachRows.memberStaticInitSummary info) ++
            maybe [] (pure . summary "instance initializer")
              (ReachRows.memberInstanceInitSummary info)
      in label $+$ nest 2 (vcat details)

    prettySlot (ref,slot) =
      let provider = ReachRows.slotProvider slot
          (kind,dependencies) = case ReachRows.slotDecl slot of
            ReachRows.StoredSlot member   -> ("stored " ++ memberKeyLabel member,Nothing)
            ReachRows.AttributeSlot       -> ("attribute",Nothing)
            ReachRows.AbstractSlot        -> ("abstract",Nothing)
            ReachRows.OpaqueSlot          -> ("opaque",Nothing)
          heading                         = text "slot" <+> prettyMemberRef ref <+> text "->" <+> prettyTopKey provider <> flags [kind]
      in heading $+$ nest 2 (maybe empty (summary "dependencies") dependencies)

    prettyReflection (ReachRows.ReflectableAttrs []) = []
    prettyReflection (ReachRows.ReflectableAttrs attrs)
                                          = [text "reflectable attrs:" <+> commaSep (text . A.rawstr) attrs]


-- Shared presentation ----------------------------------------------------------------------------------

summary :: String -> ReachRows.ReachSummary -> Doc
summary label dependencies                =
    text label <> colon $+$ nest 2 (vcatOrNone $ map prettyEdge $ ReachRows.reachEdges dependencies)

prettyEdge :: ReachRows.ReachEdge -> Doc
prettyEdge edge = case edge of
    ReachRows.Declare mn name                 -> text "declare" <+> prettyName mn name
    ReachRows.Need mn name                    -> text "need" <+> prettyName mn name
    ReachRows.Construct mn name               -> text "construct" <+> prettyName mn name
    ReachRows.Direct mn name ref              -> text "direct" <+> prettyName mn name <> dot <> prettyMemberRef ref
    ReachRows.Dispatch mn name ref            -> text "dispatch" <+> prettyName mn name <> dot <> prettyMemberRef ref
    ReachRows.Reflect mn name                 -> text "reflect" <+> prettyName mn name
    ReachRows.DynamicSerialization            -> text "dynamic serialization"
    ReachRows.DeclareAttr mn name attr        -> text "declare attr" <+> prettyName mn name <> dot <> text (A.rawstr attr)

prettyTopKey :: ReachRows.TopKey -> Doc
prettyTopKey (ReachRows.TopKey mn name)   = prettyName mn name

prettyName :: A.ModName -> A.Name -> Doc
prettyName mn name                        = prettyModuleName mn <> dot <> text (A.rawstr name)

prettyModuleName :: A.ModName -> Doc
prettyModuleName                          = text . intercalate "." . A.modPath

prettyMemberRef :: ReachRows.MemberRef -> Doc
prettyMemberRef (ReachRows.MethodRef name)    = text "method" <+> text (A.rawstr name)
prettyMemberRef (ReachRows.AttrRef name)      = text "attr" <+> text (A.rawstr name)

memberKeyLabel :: Rows.MemberKey -> String
memberKeyLabel (Rows.Method name)       = "method " ++ A.rawstr name
memberKeyLabel (Rows.Attr name)         = "attr " ++ A.rawstr name
memberKeyLabel (Rows.StaticInit name)   = "static initializer " ++ A.rawstr name
memberKeyLabel (Rows.InstanceInit name) = "instance initializer " ++ A.rawstr name
memberKeyLabel Rows.InitRest            = "constructor body"

shapeLabel :: ReachRows.ShapeKind -> String
shapeLabel ReachRows.ClassShape           = "class"
shapeLabel ReachRows.ActorShape           = "actor"
shapeLabel ReachRows.WitnessShape         = "witness"
shapeLabel ReachRows.ProtocolShape        = "protocol"

flags :: [String] -> Doc
flags []                                  = empty
flags xs                                  = space <> brackets (commaSep text xs)

vcatOrNone :: [Doc] -> Doc
vcatOrNone []                             = text "none"
vcatOrNone ds                             = vcat ds

groupSet :: (Ord owner, Ord item) => Set.Set (owner,item) -> Map.Map owner (Set.Set item)
groupSet                                  = Set.foldl' add Map.empty
  where
    add groups (owner,item)               = Map.insertWith Set.union owner (Set.singleton item) groups

groupMap :: (Ord owner, Ord item) => Map.Map (owner,item) value -> Map.Map owner (Map.Map item value)
groupMap                                  = Map.foldlWithKey' add Map.empty
  where
    add groups (owner,item) value         = Map.insertWith Map.union owner (Map.singleton item value) groups
