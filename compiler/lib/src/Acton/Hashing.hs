module Acton.Hashing
  ( TopLevelItem(..)
  , topLevelItems
  , nameHashesFromItems
  , implDepsFromItems
  , splitDeps
  , externalModules
  , computeHashes
  , buildNameHashes
  , refreshImplHashes
  , modulePubHashFromIface
  , moduleImplHashFromNameHashes
  , nameKey
  , qnameKey
  ) where

import qualified Acton.Env as Env
import qualified Acton.NameInfo as I
import qualified Acton.Names as Names
import Acton.Prim (mPrim)
import qualified Acton.Syntax as A
import qualified InterfaceFiles
import qualified Pretty

import Data.Binary (encode)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl', intercalate, nub)
import qualified Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set


data TopLevelItem = TLDecl A.Name A.Decl | TLStmt A.Name A.Stmt

-- | Render a local name as a stable string key.
nameKey :: A.Name -> String
nameKey = A.nstr

-- | Render a qualified name as a stable string key.
qnameKey :: A.QName -> String
qnameKey qn = case qn of
  A.GName m n -> modNameToString m ++ "." ++ A.nstr n
  A.QName m n -> modNameToString m ++ "." ++ A.nstr n
  A.NoQ n -> A.nstr n

-- | Render a module name as dot-separated text.
modNameToString :: A.ModName -> String
modNameToString m = intercalate "." (A.modPath m)

-- | Extract hashable top-level items from a module.
topLevelItems :: A.Module -> [TopLevelItem]
topLevelItems (A.Module _ _ suite) = concatMap items suite
  where
    items stmt = case stmt of
      A.Decl _ ds ->
        [ TLDecl (Names.dname' d) d | d <- ds ]
      A.Signature _ ns _ _ ->
        [ TLStmt n stmt | n <- ns ]
      A.Assign _ ps _ ->
        [ TLStmt n stmt | n <- nub (Names.bound ps) ]
      A.VarAssign _ ps _ ->
        [ TLStmt n stmt | n <- nub (Names.bound ps) ]
      _ -> []

-- | Collect pretty-printed fragments per top-level name.
nameFragmentsFromItems :: [TopLevelItem] -> M.Map A.Name [String]
nameFragmentsFromItems items =
  foldl' addFrag M.empty items
  where
    addFrag acc item =
      let (n, frag) = case item of
            TLDecl name decl -> (name, Pretty.print decl)
            TLStmt name stmt -> (name, Pretty.print stmt)
      in M.insertWith (flip (++)) n [frag] acc

-- | Hash each name's pretty-printed fragments.
nameHashesFromItems :: [TopLevelItem] -> M.Map A.Name B.ByteString
nameHashesFromItems items =
  M.map (SHA256.hash . B.pack . intercalate "\n") (nameFragmentsFromItems items)

-- | Collect qualified-name dependencies for each item body.
implDepsFromItems :: [TopLevelItem] -> M.Map A.Name [A.QName]
implDepsFromItems items =
  foldl' addDeps M.empty items
  where
    addDeps acc item =
      let (n, deps) = case item of
            TLDecl name decl -> (name, Names.freeQ decl)
            TLStmt name stmt -> (name, Names.freeQ stmt)
      in M.insertWith (++) n deps acc

-- | Split deps into locals and external qualified names for hashing.
splitDeps :: A.ModName
          -> Env.Env0
          -> Data.Set.Set A.Name
          -> M.Map A.Name [A.QName]
          -> (M.Map A.Name [A.Name], M.Map A.Name [A.QName])
splitDeps mn env localNames depMap =
  let toLocalExt qns = foldl' step (Data.Set.empty, Data.Set.empty) qns
      step (locals, externals) qn =
        case Env.unalias env qn of
          A.GName m _ | m == mPrim -> (locals, externals)
          A.QName m _ | m == mPrim -> (locals, externals)
          A.GName m n
            | m == mn && Data.Set.member n localNames -> (Data.Set.insert n locals, externals)
            | m == mn -> (locals, externals)
            | otherwise -> (locals, Data.Set.insert (A.GName m n) externals)
          A.QName m n
            | m == mn && Data.Set.member n localNames -> (Data.Set.insert n locals, externals)
            | m == mn -> (locals, externals)
            | otherwise -> (locals, Data.Set.insert (A.GName m n) externals)
          A.NoQ n
            | Data.Set.member n localNames -> (Data.Set.insert n locals, externals)
            | otherwise -> (locals, externals)
      pairs = M.map toLocalExt depMap
      localMap = M.map (Data.Set.toList . fst) pairs
      extMap = M.map (Data.Set.toList . snd) pairs
  in (localMap, extMap)

-- | Collect referenced external modules from dependency lists.
externalModules :: M.Map A.Name [A.QName] -> Data.Set.Set A.ModName
externalModules deps =
  Data.Set.fromList $ mapMaybe modOf (concat (M.elems deps))
  where
    modOf qn = case qn of
      A.GName m _ -> Just m
      A.QName m _ -> Just m
      A.NoQ _ -> Nothing

-- | Compute final hashes by folding in local and external deps.
computeHashes :: M.Map A.Name B.ByteString
              -> M.Map A.Name [A.Name]
              -> M.Map A.Name [(A.QName, B.ByteString)]
              -> M.Map A.Name B.ByteString
computeHashes selfHashes localDeps extDeps =
  foldl' addScc M.empty (stronglyConnComp nodes)
  where
    nodes = [ (n, n, M.findWithDefault [] n localDeps) | n <- M.keys selfHashes ]

    addScc acc scc = case scc of
      AcyclicSCC n ->
        let depHashes = depHashList acc (M.findWithDefault [] n localDeps) (M.findWithDefault [] n extDeps)
            finalHash = SHA256.hash (BL.toStrict (encode (selfHashes M.! n, depHashes)))
        in M.insert n finalHash acc
      CyclicSCC ns ->
        let nsSet = Data.Set.fromList ns
            selfHashesSorted = [ selfHashes M.! n | n <- Data.List.sortOn nameKey ns ]
            outsideDeps = Data.Set.toList $ Data.Set.fromList
              [ d | n <- ns, d <- M.findWithDefault [] n localDeps, Data.Set.notMember d nsSet ]
            externalDeps = Data.Set.toList $ Data.Set.fromList (concat [ M.findWithDefault [] n extDeps | n <- ns ])
            depHashes = depHashList acc outsideDeps externalDeps
            groupHash = SHA256.hash (BL.toStrict (encode (selfHashesSorted, depHashes)))
            insertOne m n =
              let finalHash = SHA256.hash (BL.toStrict (encode (selfHashes M.! n, groupHash, nameKey n)))
              in M.insert n finalHash m
        in foldl' insertOne acc ns

    depHashList acc locals externals =
      let lookupHash n = case M.lookup n acc of
                           Just h -> Just h
                           Nothing -> M.lookup n selfHashes
          localHashes =
            [ h | n <- Data.List.sortOn nameKey locals
                , Just h <- [lookupHash n] ]
          externalHashes =
            [ h | (_, h) <- Data.List.sortOn (qnameKey . fst) externals ]
      in localHashes ++ externalHashes

-- | Build NameHashInfo entries from per-name hashes and deps.
buildNameHashes :: Data.Set.Set A.Name
                -> M.Map A.Name B.ByteString
                -> M.Map A.Name B.ByteString
                -> M.Map A.Name I.NameInfo
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> M.Map A.Name [A.Name]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> M.Map A.Name [(A.QName, B.ByteString)]
                -> [InterfaceFiles.NameHashInfo]
buildNameHashes nameKeys nameSrcHashes nameImplHashes nameInfoMap pubSigLocalDeps pubSigExtHashes implLocalDeps implExtHashes pubExtHashes =
  let hashNameInfo info = SHA256.hash (BL.toStrict $ encode (I.stripDocsNI info))
      selfPubHashes = M.map hashNameInfo nameInfoMap
      selfImplHashes = nameImplHashes
      pubHashes = computeHashes selfPubHashes pubSigLocalDeps pubSigExtHashes
      implHashes = computeHashes selfImplHashes implLocalDeps implExtHashes
      namesSorted = Data.List.sortOn nameKey (Data.Set.toList nameKeys)
  in
    [ InterfaceFiles.NameHashInfo
        { InterfaceFiles.nhName = n
        , InterfaceFiles.nhSrcHash = M.findWithDefault B.empty n nameSrcHashes
        , InterfaceFiles.nhPubHash = M.findWithDefault B.empty n pubHashes
        , InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
        , InterfaceFiles.nhPubDeps = M.findWithDefault [] n pubExtHashes
        , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
        }
    | n <- namesSorted
    ]

-- | Refresh impl hashes and impl deps for existing name hashes.
refreshImplHashes :: [InterfaceFiles.NameHashInfo]
                  -> M.Map A.Name B.ByteString
                  -> M.Map A.Name [A.Name]
                  -> M.Map A.Name [(A.QName, B.ByteString)]
                  -> [InterfaceFiles.NameHashInfo]
refreshImplHashes nameHashes nameImplHashes implLocalDeps implExtHashes =
  let implHashes = computeHashes nameImplHashes implLocalDeps implExtHashes
      infoMap = M.fromList [ (InterfaceFiles.nhName nh, nh) | nh <- nameHashes ]
      namesSorted = Data.List.sortOn nameKey (M.keys nameImplHashes)
  in
    [ let nh = infoMap M.! n
      in nh { InterfaceFiles.nhImplHash = M.findWithDefault B.empty n implHashes
            , InterfaceFiles.nhImplDeps = M.findWithDefault [] n implExtHashes
            }
    | n <- namesSorted
    ]

-- | Hash the module public interface entries.
modulePubHashFromIface :: I.NameInfo -> [InterfaceFiles.NameHashInfo] -> B.ByteString
modulePubHashFromIface nmod nameHashes =
  let I.NModule iface _ = nmod
      pubHashMap = M.fromList
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhPubHash nh)
        | nh <- nameHashes
        ]
      pubNamesSorted = Data.List.sortOn nameKey (map fst iface)
      pubEntries = [ (n, M.findWithDefault B.empty n pubHashMap) | n <- pubNamesSorted ]
  in SHA256.hash (BL.toStrict $ encode pubEntries)

-- | Hash the module impl entries from per-name impl hashes.
moduleImplHashFromNameHashes :: [InterfaceFiles.NameHashInfo] -> B.ByteString
moduleImplHashFromNameHashes infos =
  let items =
        [ (InterfaceFiles.nhName nh, InterfaceFiles.nhImplHash nh)
        | nh <- Data.List.sortOn (nameKey . InterfaceFiles.nhName) infos
        ]
  in SHA256.hash (BL.toStrict (encode items))
