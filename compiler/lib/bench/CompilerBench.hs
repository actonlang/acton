import qualified Acton.CommandLineParser as C
import qualified Acton.Compile as Compile
import qualified Acton.Env as Env
import qualified Acton.Hashing as Hashing
import qualified Acton.Kinds as Kinds
import qualified Acton.NameInfo as NameInfo
import qualified Acton.Parser as Parser
import qualified Acton.Syntax as Syntax
import qualified Acton.Types as Types

import Control.DeepSeq (rnf)
import qualified Control.Exception as E
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
import Data.Graph (SCC(..), stronglyConnComp)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Stats
import System.Clock (TimeSpec, toNanoSecs)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Mem (performGC)
import qualified InterfaceFiles

-- Usage:
--   stack build libacton:exe:compiler-bench
--   stack exec compiler-bench -- --parse TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --kinds TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --types TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --hash REPS TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --hash-breakdown REPS TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --hash-minimal REPS N +RTS -T -RTS
--   stack exec compiler-bench -- --front TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --front-docs TYPES_PATH SOURCE.act +RTS -T -RTS
--   stack exec compiler-bench -- --pipeline TYPES_PATH SOURCE.act +RTS -T -RTS
--
-- The direct modes stop after the named pass. --front runs the normal compiler
-- front pass without docs, --front-docs includes project front-pass work such
-- as docs, and --pipeline runs front plus back passes.

data DirectMode = ParseOnly | KindsOnly | TypesOnly

elapsed label t0 t1 = putStrLn $ label ++ " " ++ show (diffUTCTime t1 t0)

printStats label before after =
    putStrLn $ label ++ " alloc " ++ show alloc
                    ++ " copied " ++ show copied
                    ++ " max_live " ++ show (max_live_bytes after)
                    ++ " max_mem " ++ show (max_mem_in_use_bytes after)
                    ++ " gc_elapsed " ++ show gcElapsed
                    ++ " gcs " ++ show collections
  where alloc = allocated_bytes after - allocated_bytes before
        copied = copied_bytes after - copied_bytes before
        gcElapsed = gc_elapsed_ns after - gc_elapsed_ns before
        collections = gcs after - gcs before

getStats enabled =
    if enabled then Just <$> getRTSStats else return Nothing

printStatsMaybe label (Just before) (Just after) = printStats label before after
printStatsMaybe _ _ _                            = return ()

sccSummary :: Map.Map Syntax.Name B.ByteString
           -> Map.Map Syntax.Name [Syntax.Name]
           -> (Int, Int, Int, Int)
sccSummary selfHashes localDeps =
    let nodes = [ (n, n, Map.findWithDefault [] n localDeps) | n <- Map.keys selfHashes ]
        sccs = stronglyConnComp nodes
        edgeCount = sum [ length ds | (_, _, ds) <- nodes ]
        cyclicSizes =
          [ case scc of
              AcyclicSCC _ -> 0
              CyclicSCC ns -> length ns
          | scc <- sccs
          ]
        cyclicCount = length (filter (> 0) cyclicSizes)
        maxCyclic = maximum (0 : cyclicSizes)
    in (length nodes, edgeCount, cyclicCount, maxCyclic)

measureRepeated :: Bool -> String -> Int -> IO Int -> IO Int
measureRepeated statsEnabled label reps action = do
    performGC
    s0 <- getStats statsEnabled
    t0 <- getCurrentTime
    total <- loop reps 0
    t1 <- getCurrentTime
    performGC
    s1 <- getStats statsEnabled
    elapsed label t0 t1
    printStatsMaybe (label ++ "_stats") s0 s1
    putStrLn $ label ++ "_result reps " ++ show reps ++ " total " ++ show total
    return total
  where
    loop 0 total = return total
    loop n total = do
      count <- action
      let total' = total + count
      total' `seq` loop (n - 1) total'

forceHTEnv = HashMap.foldl' forceHNameInfo () where
    forceHNameInfo () (NameInfo.HNModule _ te _) = forceHTEnv te
    forceHNameInfo () hni                        = hni `seq` ()

fmtTime :: TimeSpec -> String
fmtTime t = show seconds ++ "s"
  where seconds = (fromIntegral (toNanoSecs t) / 1000000000.0 :: Double)

printFrontTiming :: Compile.FrontTiming -> IO ()
printFrontTiming ft =
    putStrLn $ "front_timing env " ++ fmtTime (Compile.ftEnv ft)
            ++ " kinds " ++ fmtTime (Compile.ftKinds ft)
            ++ " types " ++ fmtTime (Compile.ftTypes ft)
            ++ " reconstruct " ++ fmtTime (Compile.ftTypeReconstruct ft)
            ++ " reconstruct_after_progress " ++ fmtTime (Compile.ftTypeAfterProgress ft)
            ++ " force " ++ fmtTime (Compile.ftTypeForce ft)
            ++ " hash " ++ fmtTime (Compile.ftTypeHash ft)

printBackTiming :: Compile.BackTiming -> IO ()
printBackTiming bt =
    putStrLn $ "back_timing normalize " ++ fmtTime (Compile.btNormalize bt)
            ++ " deactorize " ++ fmtTime (Compile.btDeactorize bt)
            ++ " cps " ++ fmtTime (Compile.btCPS bt)
            ++ " llift " ++ fmtTime (Compile.btLLift bt)
            ++ " boxing " ++ fmtTime (Compile.btBoxing bt)
            ++ " codegen " ++ fmtTime (Compile.btCodeGen bt)
            ++ " render " ++ fmtTime (Compile.btRender bt)
            ++ maybe "" (\t -> " write " ++ fmtTime t) (Compile.btWrite bt)

sysRootFromTypesPath :: FilePath -> FilePath
sysRootFromTypesPath = takeDirectory . takeDirectory . takeDirectory

benchGopts :: C.GlobalOptions
benchGopts = C.GlobalOptions
    { C.color = C.Never
    , C.quiet = False
    , C.noProgress = True
    , C.timing = True
    , C.tty = False
    , C.verbose = False
    , C.verboseZig = False
    , C.jobs = 0
    }

benchOpts :: FilePath -> Bool -> C.CompileOptions
benchOpts typesPath skipBuild =
    Compile.defaultCompileOptions
      { C.ignore_compiler_version = True
      , C.skip_build = skipBuild
      , C.syspath = sysRootFromTypesPath typesPath
      }

nameHashMapFromHeader :: [InterfaceFiles.NameHashInfo] -> Map.Map Syntax.Name InterfaceFiles.NameHashInfo
nameHashMapFromHeader infos =
    Map.fromList [ (InterfaceFiles.nhName info, info) | info <- infos ]

resolveNameHashMap :: Compile.Paths -> Syntax.ModName -> IO (Maybe (Map.Map Syntax.Name InterfaceFiles.NameHashInfo))
resolveNameHashMap paths mn = do
    mty <- Env.findTyFile (Compile.searchPath paths) mn
    case mty of
      Nothing -> return Nothing
      Just ty -> do
        mh <- InterfaceFiles.readHeaderMaybe ty
        return $ case mh of
          Just (_, _, _, _, _, _, nameHashes, _, _, _) -> Just (nameHashMapFromHeader nameHashes)
          Nothing -> Nothing

resolveDepHashMap :: String
                  -> (InterfaceFiles.NameHashInfo -> B.ByteString)
                  -> Map.Map Syntax.ModName (Map.Map Syntax.Name InterfaceFiles.NameHashInfo)
                  -> Map.Map Syntax.Name [Syntax.QName]
                  -> Map.Map Syntax.Name [(Syntax.QName, B.ByteString)]
resolveDepHashMap label getHash extMaps deps =
    Map.map (map resolveOne) deps
  where
    resolveOne qn =
      case qn of
        Syntax.GName m n -> (qn, lookupHash qn m n)
        Syntax.QName m n -> (qn, lookupHash qn m n)
        Syntax.NoQ n     -> error ("unexpected local " ++ label ++ " dependency " ++ Syntax.nstr n)

    lookupHash qn m n =
      case Map.lookup m extMaps >>= Map.lookup n of
        Just info -> getHash info
        Nothing   -> error ("missing " ++ label ++ " hash for " ++ Hashing.qnameKey qn)

data HashBenchState = HashBenchState
    { hbsModName  :: Syntax.ModName
    , hbsEnv      :: Env.Env0
    , hbsParsed   :: Syntax.Module
    , hbsTChecked :: Syntax.Module
    , hbsNMod     :: NameInfo.NameInfo
    , hbsExtMaps  :: Map.Map Syntax.ModName (Map.Map Syntax.Name InterfaceFiles.NameHashInfo)
    }

topLevelItemName :: Hashing.TopLevelItem -> Syntax.Name
topLevelItemName item =
    case item of
      Hashing.TLDecl n _ -> n
      Hashing.TLStmt n _ -> n

prepareHashBench :: FilePath -> FilePath -> IO HashBenchState
prepareHashBench typesPath sourcePath = do
    src <- readFile sourcePath
    let opts = benchOpts typesPath True
    paths <- Compile.findPaths sourcePath opts
    env0 <- Env.initEnv typesPath False
    let modName = Compile.modName paths

    parsed <- Parser.parseModule modName sourcePath src Nothing
    E.evaluate (rnf parsed)
    env <- Env.mkEnv (Compile.searchPath paths) env0 parsed
    E.evaluate (forceHTEnv (Env.hnames env))
    E.evaluate (forceHTEnv (Env.hmodules env))
    kchecked <- Kinds.check env parsed
    E.evaluate (rnf kchecked)
    (nmod, tchecked, typeEnv, tests) <- Types.reconstruct Nothing Nothing env kchecked Nothing
    E.evaluate (rnf nmod)
    E.evaluate (rnf tchecked)
    E.evaluate (forceHTEnv (Env.hnames typeEnv))
    E.evaluate (forceHTEnv (Env.hmodules typeEnv))
    E.evaluate (length tests)

    let NameInfo.NModule _ fullIface _ = nmod
        srcItems = Hashing.topLevelItems parsed
        implItems = Hashing.topLevelItems tchecked
        nameKeys = Set.fromList (map topLevelItemName srcItems ++ map topLevelItemName implItems)
        nameInfoMap = Map.fromList fullIface
        hashEnv = Env.setMod modName env
        (_, pubSigExtDeps) = Hashing.pubSigSplitDepsFromNameInfoMap modName hashEnv nameKeys nameInfoMap
        (_, implExtDeps) = Hashing.implSplitDepsFromItems modName hashEnv nameKeys implItems
        extMods = Set.toList (Hashing.externalModules pubSigExtDeps `Set.union` Hashing.externalModules implExtDeps)
    extMaps <- fmap Map.fromList $
      mapM (\mn -> do
              m <- resolveNameHashMap paths mn
              case m of
                Just hmap -> return (mn, hmap)
                Nothing -> error ("missing interface hashes for " ++ show (Syntax.modPath mn)))
           extMods

    return HashBenchState
      { hbsModName = modName
      , hbsEnv = env
      , hbsParsed = parsed
      , hbsTChecked = tchecked
      , hbsNMod = nmod
      , hbsExtMaps = extMaps
      }

hashBenchFromHashes :: Syntax.ModName
                    -> Env.Env0
                    -> NameInfo.NameInfo
                    -> Map.Map Syntax.ModName (Map.Map Syntax.Name InterfaceFiles.NameHashInfo)
                    -> [Hashing.TopLevelItem]
                    -> Map.Map Syntax.Name B.ByteString
                    -> Map.Map Syntax.Name B.ByteString
                    -> Map.Map Syntax.Name B.ByteString
                    -> (B.ByteString, B.ByteString, [InterfaceFiles.NameHashInfo])
hashBenchFromHashes mn env nmod extMaps implItems nameSrcHashes nameImplHashes selfPubHashes =
    let NameInfo.NModule _ fullIface _ = nmod
        nameInfoMap = Map.fromList fullIface
        nameSrcKeys = Map.keysSet nameSrcHashes
        nameImplKeys = Map.keysSet nameImplHashes
        nameKeys = Set.union nameSrcKeys nameImplKeys
        hashEnv = Env.setMod mn env
        (pubSigLocalDeps, pubSigExtDeps) = Hashing.pubSigSplitDepsFromNameInfoMap mn hashEnv nameKeys nameInfoMap
        (implLocalDeps, implExtDeps) = Hashing.implSplitDepsFromItems mn hashEnv nameKeys implItems
        (pubLocalDeps, pubExtDeps) =
          Hashing.mergePubDeps pubSigLocalDeps pubSigExtDeps implLocalDeps implExtDeps
        pubSigExtHashes = resolveDepHashMap "pub" InterfaceFiles.nhPubHash extMaps pubSigExtDeps
        pubExtHashes = resolveDepHashMap "pub" InterfaceFiles.nhPubHash extMaps pubExtDeps
        implExtHashes = resolveDepHashMap "impl" InterfaceFiles.nhImplHash extMaps implExtDeps
        pubHashes = Hashing.computeHashesSortedDeps selfPubHashes pubSigLocalDeps pubSigExtHashes
        implHashes = Hashing.computeHashesSortedDeps nameImplHashes implLocalDeps implExtHashes
        nameHashes =
          Hashing.assembleNameHashes
            nameKeys
            nameSrcHashes
            pubHashes
            implHashes
            pubLocalDeps
            implLocalDeps
            pubExtHashes
            implExtHashes
        modulePubHash = Hashing.modulePubHashFromIface nmod nameHashes
        moduleImplHash = Hashing.moduleImplHashFromNameHashes nameHashes
    in (modulePubHash, moduleImplHash, nameHashes)

hashBenchOnce :: Syntax.ModName
              -> Env.Env0
              -> Syntax.Module
              -> Syntax.Module
              -> NameInfo.NameInfo
              -> Map.Map Syntax.ModName (Map.Map Syntax.Name InterfaceFiles.NameHashInfo)
              -> (B.ByteString, B.ByteString, [InterfaceFiles.NameHashInfo])
hashBenchOnce mn env parsed tchecked nmod extMaps =
    let NameInfo.NModule _ fullIface _ = nmod
        srcItems = Hashing.topLevelItems parsed
        nameSrcHashes = Hashing.nameHashesFromItems srcItems
        implItems = Hashing.topLevelItems tchecked
        nameImplHashes = Hashing.nameHashesFromItems implItems
        nameInfoMap = Map.fromList fullIface
        selfPubHashes = Hashing.nameInfoHashes nameInfoMap
    in hashBenchFromHashes mn env nmod extMaps implItems nameSrcHashes nameImplHashes selfPubHashes

runHashBench :: Int -> FilePath -> FilePath -> IO ()
runHashBench reps typesPath sourcePath = do
    statsEnabled <- getRTSStatsEnabled
    bench <- prepareHashBench typesPath sourcePath

    s0 <- getStats statsEnabled
    t0 <- getCurrentTime
    total <- hashLoop reps 0 bench
    t1 <- getCurrentTime
    s1 <- getStats statsEnabled
    elapsed "hash_only" t0 t1
    printStatsMaybe "hash_only_stats" s0 s1
    putStrLn $ "hash_only_result reps " ++ show reps ++ " total_names " ++ show total
  where
    hashLoop 0 total _ = return total
    hashLoop n total bench = do
      let result@(_, _, nameHashes) =
            hashBenchOnce
              (hbsModName bench)
              (hbsEnv bench)
              (hbsParsed bench)
              (hbsTChecked bench)
              (hbsNMod bench)
              (hbsExtMaps bench)
      E.evaluate (rnf result)
      hashLoop (n - 1) (total + length nameHashes) bench

runHashBreakdown :: Int -> FilePath -> FilePath -> IO ()
runHashBreakdown reps typesPath sourcePath = do
    statsEnabled <- getRTSStatsEnabled
    bench <- prepareHashBench typesPath sourcePath

    let parsed = hbsParsed bench
        tchecked = hbsTChecked bench
        nmod = hbsNMod bench
        NameInfo.NModule _ fullIface _ = nmod
        nameInfoMap = Map.fromList fullIface
        srcItems = Hashing.topLevelItems parsed
        implItems = Hashing.topLevelItems tchecked

    E.evaluate (length srcItems)
    E.evaluate (length implItems)
    E.evaluate (Map.size nameInfoMap)
    parsedRef <- newIORef parsed
    tcheckedRef <- newIORef tchecked
    fullIfaceRef <- newIORef fullIface
    nameInfoMapRef <- newIORef nameInfoMap
    srcItemsRef <- newIORef srcItems
    implItemsRef <- newIORef implItems

    _ <- measureRepeated statsEnabled "hash_extract_src_items" reps $ do
      parsed' <- readIORef parsedRef
      let items = Hashing.topLevelItems parsed'
      E.evaluate (length items)

    _ <- measureRepeated statsEnabled "hash_extract_impl_items" reps $ do
      tchecked' <- readIORef tcheckedRef
      let items = Hashing.topLevelItems tchecked'
      E.evaluate (length items)

    _ <- measureRepeated statsEnabled "hash_ast_src" reps $ do
      srcItems' <- readIORef srcItemsRef
      let hashes = Hashing.nameHashesFromItems srcItems'
      E.evaluate (rnf hashes)
      return (Map.size hashes)

    _ <- measureRepeated statsEnabled "hash_ast_impl" reps $ do
      implItems' <- readIORef implItemsRef
      let hashes = Hashing.nameHashesFromItems implItems'
      E.evaluate (rnf hashes)
      return (Map.size hashes)

    _ <- measureRepeated statsEnabled "hash_ast_src_impl" reps $ do
      srcItems' <- readIORef srcItemsRef
      implItems' <- readIORef implItemsRef
      let srcHashes = Hashing.nameHashesFromItems srcItems'
          implHashes = Hashing.nameHashesFromItems implItems'
      E.evaluate (rnf srcHashes)
      E.evaluate (rnf implHashes)
      return (Map.size srcHashes + Map.size implHashes)

    _ <- measureRepeated statsEnabled "hash_pub_nameinfo" reps $ do
      nameInfoMap' <- readIORef nameInfoMapRef
      let hashes = Hashing.nameInfoHashes nameInfoMap'
      E.evaluate (rnf hashes)
      return (Map.size hashes)

    _ <- measureRepeated statsEnabled "hash_prepare" reps $ do
      srcItems' <- readIORef srcItemsRef
      implItems' <- readIORef implItemsRef
      nameInfoMap' <- readIORef nameInfoMapRef
      let hashEnv = Env.setMod (hbsModName bench) (hbsEnv bench)
          inputs = Hashing.prepareNameHashInputs (hbsModName bench) hashEnv srcItems' implItems' nameInfoMap'
      E.evaluate (rnf ( Hashing.nhiNameKeys inputs
                      , Hashing.nhiNameSrcHashes inputs
                      , Hashing.nhiNameImplHashes inputs
                      , Hashing.nhiSelfPubHashes inputs
                      , Hashing.nhiPubSigLocalDeps inputs
                      , Hashing.nhiPubSigExtDeps inputs
                      , Hashing.nhiImplLocalDeps inputs
                      , Hashing.nhiImplExtDeps inputs
                      ))
      return (Set.size (Hashing.nhiNameKeys inputs))

    let nameSrcHashes = Hashing.nameHashesFromItems srcItems
        nameImplHashes = Hashing.nameHashesFromItems implItems
        selfPubHashes = Hashing.nameInfoHashes nameInfoMap
    E.evaluate (rnf nameSrcHashes)
    E.evaluate (rnf nameImplHashes)
    E.evaluate (rnf selfPubHashes)
    nameSrcHashesRef <- newIORef nameSrcHashes
    nameImplHashesRef <- newIORef nameImplHashes
    selfPubHashesRef <- newIORef selfPubHashes

    _ <- measureRepeated statsEnabled "hash_finish_nameinfo_map" reps $ do
      fullIface' <- readIORef fullIfaceRef
      let m = Map.fromList fullIface'
      E.evaluate (rnf m)
      return (Map.size m)

    _ <- measureRepeated statsEnabled "hash_finish_keys" reps $ do
      nameSrcHashes' <- readIORef nameSrcHashesRef
      nameImplHashes' <- readIORef nameImplHashesRef
      let nameSrcKeys = Map.keysSet nameSrcHashes'
          nameImplKeys = Map.keysSet nameImplHashes'
          nameKeys = Set.union nameSrcKeys nameImplKeys
      E.evaluate (rnf nameKeys)
      return (Set.size nameKeys)

    _ <- measureRepeated statsEnabled "hash_finish_pub_sig_deps" reps $ do
      nameInfoMap' <- readIORef nameInfoMapRef
      let deps = Hashing.pubSigDepsFromNameInfoMap nameInfoMap'
      E.evaluate (rnf deps)
      return (sum (map length (Map.elems deps)))

    _ <- measureRepeated statsEnabled "hash_finish_pub_sig_split_direct" reps $ do
      nameSrcHashes' <- readIORef nameSrcHashesRef
      nameImplHashes' <- readIORef nameImplHashesRef
      nameInfoMap' <- readIORef nameInfoMapRef
      let nameKeys' = Set.union (Map.keysSet nameSrcHashes') (Map.keysSet nameImplHashes')
          hashEnv' = Env.setMod (hbsModName bench) (hbsEnv bench)
          (localDeps, extDeps) = Hashing.pubSigSplitDepsFromNameInfoMap (hbsModName bench) hashEnv' nameKeys' nameInfoMap'
      E.evaluate (rnf (localDeps, extDeps))
      return (sum (map length (Map.elems localDeps)) + sum (map length (Map.elems extDeps)))

    _ <- measureRepeated statsEnabled "hash_finish_impl_deps" reps $ do
      implItems' <- readIORef implItemsRef
      let deps = Hashing.implDepsFromItems implItems'
      E.evaluate (rnf deps)
      return (sum (map length (Map.elems deps)))

    let nameSrcKeys = Map.keysSet nameSrcHashes
        nameImplKeys = Map.keysSet nameImplHashes
        nameKeys = Set.union nameSrcKeys nameImplKeys
        pubSigDepsRaw = Hashing.pubSigDepsFromNameInfoMap nameInfoMap
        implDepsRaw = Hashing.implDepsFromItems implItems
        hashEnv = Env.setMod (hbsModName bench) (hbsEnv bench)
        (pubSigLocalDepsLegacy, pubSigExtDepsLegacy) = Hashing.splitDeps (hbsModName bench) hashEnv nameKeys pubSigDepsRaw
        (pubSigLocalDeps, pubSigExtDeps) = Hashing.pubSigSplitDepsFromNameInfoMap (hbsModName bench) hashEnv nameKeys nameInfoMap
        (implLocalDepsSplit, implExtDepsSplit) = Hashing.splitDeps (hbsModName bench) hashEnv nameKeys implDepsRaw
        (implLocalDeps, implExtDeps) = Hashing.implSplitDepsFromItems (hbsModName bench) hashEnv nameKeys implItems
        (pubLocalDeps, pubExtDeps) =
          Hashing.mergePubDeps pubSigLocalDeps pubSigExtDeps implLocalDeps implExtDeps
        pubSigExtHashes = resolveDepHashMap "pub" InterfaceFiles.nhPubHash (hbsExtMaps bench) pubSigExtDeps
        pubExtHashes = resolveDepHashMap "pub" InterfaceFiles.nhPubHash (hbsExtMaps bench) pubExtDeps
        implExtHashes = resolveDepHashMap "impl" InterfaceFiles.nhImplHash (hbsExtMaps bench) implExtDeps
        pubHashes = Hashing.computeHashesSortedDeps selfPubHashes pubSigLocalDeps pubSigExtHashes
        implHashes = Hashing.computeHashesSortedDeps nameImplHashes implLocalDeps implExtHashes
        nameHashes =
          Hashing.assembleNameHashes
            nameKeys
            nameSrcHashes
            pubHashes
            implHashes
            pubLocalDeps
            implLocalDeps
            pubExtHashes
            implExtHashes
        localDepSetMap = Map.map Set.fromList
        extDepSetMap = Map.map Set.fromList
        (implLocalDepsDirect, implExtDepsDirect) =
          Hashing.implSplitDepsFromItems (hbsModName bench) hashEnv nameKeys implItems
    if localDepSetMap pubSigLocalDeps /= localDepSetMap pubSigLocalDepsLegacy ||
       extDepSetMap pubSigExtDeps /= extDepSetMap pubSigExtDepsLegacy
      then error "direct pub signature dependency split does not match legacy split"
      else pure ()
    if localDepSetMap implLocalDepsDirect /= localDepSetMap implLocalDepsSplit ||
       extDepSetMap implExtDepsDirect /= extDepSetMap implExtDepsSplit
      then error "direct impl dependency split does not match legacy split"
      else pure ()
    E.evaluate (rnf (nameKeys, pubSigDepsRaw, implDepsRaw))
    E.evaluate (rnf (pubSigLocalDeps, pubSigExtDeps, implLocalDeps, implExtDeps, pubLocalDeps, pubExtDeps))
    E.evaluate (rnf (pubSigLocalDepsLegacy, pubSigExtDepsLegacy, implLocalDepsSplit, implExtDepsSplit))
    E.evaluate (rnf (pubSigExtHashes, pubExtHashes, implExtHashes))
    E.evaluate (rnf (pubHashes, implHashes, nameHashes))
    nameKeysRef <- newIORef nameKeys
    pubSigDepsRawRef <- newIORef pubSigDepsRaw
    implDepsRawRef <- newIORef implDepsRaw
    pubSigLocalDepsRef <- newIORef pubSigLocalDeps
    pubSigExtDepsRef <- newIORef pubSigExtDeps
    implLocalDepsRef <- newIORef implLocalDeps
    implExtDepsRef <- newIORef implExtDeps
    pubLocalDepsRef <- newIORef pubLocalDeps
    pubExtDepsRef <- newIORef pubExtDeps
    pubSigExtHashesRef <- newIORef pubSigExtHashes
    pubExtHashesRef <- newIORef pubExtHashes
    implExtHashesRef <- newIORef implExtHashes
    pubHashesRef <- newIORef pubHashes
    implHashesRef <- newIORef implHashes
    nameHashesRef <- newIORef nameHashes

    let (pubSccNodes, pubSccEdges, pubSccCycles, pubSccMax) = sccSummary selfPubHashes pubSigLocalDeps
        (implSccNodes, implSccEdges, implSccCycles, implSccMax) = sccSummary nameImplHashes implLocalDeps
    putStrLn $ "hash_scc_pub_shape nodes " ++ show pubSccNodes
      ++ " edges " ++ show pubSccEdges
      ++ " cyclic_sccs " ++ show pubSccCycles
      ++ " max_cyclic " ++ show pubSccMax
    putStrLn $ "hash_scc_impl_shape nodes " ++ show implSccNodes
      ++ " edges " ++ show implSccEdges
      ++ " cyclic_sccs " ++ show implSccCycles
      ++ " max_cyclic " ++ show implSccMax

    _ <- measureRepeated statsEnabled "hash_finish_split_deps" reps $ do
      nameKeys' <- readIORef nameKeysRef
      pubSigDepsRaw' <- readIORef pubSigDepsRawRef
      implDepsRaw' <- readIORef implDepsRawRef
      let hashEnv' = Env.setMod (hbsModName bench) (hbsEnv bench)
          (pubSigLocalDeps', pubSigExtDeps') = Hashing.splitDeps (hbsModName bench) hashEnv' nameKeys' pubSigDepsRaw'
          (implLocalDeps', implExtDeps') = Hashing.splitDeps (hbsModName bench) hashEnv' nameKeys' implDepsRaw'
      E.evaluate (rnf (pubSigLocalDeps', pubSigExtDeps', implLocalDeps', implExtDeps'))
      return (Map.size pubSigLocalDeps' + Map.size implLocalDeps')

    _ <- measureRepeated statsEnabled "hash_finish_impl_split_direct" reps $ do
      nameKeys' <- readIORef nameKeysRef
      implItems' <- readIORef implItemsRef
      let hashEnv' = Env.setMod (hbsModName bench) (hbsEnv bench)
          (implLocalDeps', implExtDeps') = Hashing.implSplitDepsFromItems (hbsModName bench) hashEnv' nameKeys' implItems'
      E.evaluate (rnf (implLocalDeps', implExtDeps'))
      return (Map.size implLocalDeps')

    _ <- measureRepeated statsEnabled "hash_finish_union_pub_deps" reps $ do
      pubSigLocalDeps' <- readIORef pubSigLocalDepsRef
      pubSigExtDeps' <- readIORef pubSigExtDepsRef
      implLocalDeps' <- readIORef implLocalDepsRef
      implExtDeps' <- readIORef implExtDepsRef
      let (pubLocalDeps', pubExtDeps') =
            Hashing.mergePubDeps pubSigLocalDeps' pubSigExtDeps' implLocalDeps' implExtDeps'
      E.evaluate (rnf (pubLocalDeps', pubExtDeps'))
      return (Map.size pubLocalDeps' + Map.size pubExtDeps')

    _ <- measureRepeated statsEnabled "hash_finish_resolve_ext_hashes" reps $ do
      pubSigExtDeps' <- readIORef pubSigExtDepsRef
      pubExtDeps' <- readIORef pubExtDepsRef
      implExtDeps' <- readIORef implExtDepsRef
      let pubSigExtHashes' = resolveDepHashMap "pub" InterfaceFiles.nhPubHash (hbsExtMaps bench) pubSigExtDeps'
          pubExtHashes' = resolveDepHashMap "pub" InterfaceFiles.nhPubHash (hbsExtMaps bench) pubExtDeps'
          implExtHashes' = resolveDepHashMap "impl" InterfaceFiles.nhImplHash (hbsExtMaps bench) implExtDeps'
      E.evaluate (rnf (pubSigExtHashes', pubExtHashes', implExtHashes'))
      return (Map.size pubSigExtHashes' + Map.size pubExtHashes' + Map.size implExtHashes')

    _ <- measureRepeated statsEnabled "hash_finish_scc_pub" reps $ do
      selfPubHashes' <- readIORef selfPubHashesRef
      pubSigLocalDeps' <- readIORef pubSigLocalDepsRef
      let summary@(nodeCount, edgeCount, cyclicCount, maxCyclic) = sccSummary selfPubHashes' pubSigLocalDeps'
      E.evaluate (rnf summary)
      return (nodeCount + edgeCount + cyclicCount + maxCyclic)

    _ <- measureRepeated statsEnabled "hash_finish_scc_impl" reps $ do
      nameImplHashes' <- readIORef nameImplHashesRef
      implLocalDeps' <- readIORef implLocalDepsRef
      let summary@(nodeCount, edgeCount, cyclicCount, maxCyclic) = sccSummary nameImplHashes' implLocalDeps'
      E.evaluate (rnf summary)
      return (nodeCount + edgeCount + cyclicCount + maxCyclic)

    _ <- measureRepeated statsEnabled "hash_finish_compute_pub" reps $ do
      selfPubHashes' <- readIORef selfPubHashesRef
      pubSigLocalDeps' <- readIORef pubSigLocalDepsRef
      pubSigExtHashes' <- readIORef pubSigExtHashesRef
      let hashes = Hashing.computeHashesSortedDeps selfPubHashes' pubSigLocalDeps' pubSigExtHashes'
      E.evaluate (rnf hashes)
      return (Map.size hashes)

    _ <- measureRepeated statsEnabled "hash_finish_compute_impl" reps $ do
      nameImplHashes' <- readIORef nameImplHashesRef
      implLocalDeps' <- readIORef implLocalDepsRef
      implExtHashes' <- readIORef implExtHashesRef
      let hashes = Hashing.computeHashesSortedDeps nameImplHashes' implLocalDeps' implExtHashes'
      E.evaluate (rnf hashes)
      return (Map.size hashes)

    _ <- measureRepeated statsEnabled "hash_finish_assemble" reps $ do
      nameKeys' <- readIORef nameKeysRef
      nameSrcHashes' <- readIORef nameSrcHashesRef
      pubHashes' <- readIORef pubHashesRef
      implHashes' <- readIORef implHashesRef
      pubLocalDeps' <- readIORef pubLocalDepsRef
      implLocalDeps' <- readIORef implLocalDepsRef
      pubExtHashes' <- readIORef pubExtHashesRef
      implExtHashes' <- readIORef implExtHashesRef
      let hashes =
            Hashing.assembleNameHashes
              nameKeys'
              nameSrcHashes'
              pubHashes'
              implHashes'
              pubLocalDeps'
              implLocalDeps'
              pubExtHashes'
              implExtHashes'
      E.evaluate (rnf hashes)
      return (length hashes)

    _ <- measureRepeated statsEnabled "hash_finish_module_hashes" reps $ do
      nameHashes' <- readIORef nameHashesRef
      let modulePubHash = Hashing.modulePubHashFromIface nmod nameHashes'
          moduleImplHash = Hashing.moduleImplHashFromNameHashes nameHashes'
      E.evaluate (rnf (modulePubHash, moduleImplHash))
      return (B.length modulePubHash + B.length moduleImplHash)

    _ <- measureRepeated statsEnabled "hash_finish_module_hashes_from_maps" reps $ do
      nameKeys' <- readIORef nameKeysRef
      pubHashes' <- readIORef pubHashesRef
      implHashes' <- readIORef implHashesRef
      let moduleHashes = Hashing.moduleHashesFromHashMaps nmod nameKeys' pubHashes' implHashes'
      E.evaluate (rnf moduleHashes)
      return (B.length (fst moduleHashes) + B.length (snd moduleHashes))

    _ <- measureRepeated statsEnabled "hash_finish_no_ast_hash" reps $ do
      implItems' <- readIORef implItemsRef
      nameSrcHashes' <- readIORef nameSrcHashesRef
      nameImplHashes' <- readIORef nameImplHashesRef
      selfPubHashes' <- readIORef selfPubHashesRef
      let result@(_, _, nameHashes) =
            hashBenchFromHashes
              (hbsModName bench)
              (hbsEnv bench)
              nmod
              (hbsExtMaps bench)
              implItems'
              nameSrcHashes'
              nameImplHashes'
              selfPubHashes'
      E.evaluate (rnf result)
      return (length nameHashes)

    _ <- measureRepeated statsEnabled "hash_total" reps $ do
      parsed' <- readIORef parsedRef
      tchecked' <- readIORef tcheckedRef
      let result@(_, _, nameHashes) =
            hashBenchOnce
              (hbsModName bench)
              (hbsEnv bench)
              parsed'
              tchecked'
              nmod
              (hbsExtMaps bench)
      E.evaluate (rnf result)
      return (length nameHashes)

    return ()

runHashMinimal :: Int -> Int -> IO ()
runHashMinimal reps count = do
    statsEnabled <- getRTSStatsEnabled
    let items =
          [ Hashing.TLStmt
              (Syntax.name ("hash_min_" ++ show i))
              Syntax.sPass
          | i <- [1..count]
          ]
    E.evaluate (length items)
    _ <- measureRepeated statsEnabled "hash_minimal_ast" reps $ do
      let hashes = Hashing.nameHashesFromItems items
      E.evaluate (rnf hashes)
      return (Map.size hashes)
    return ()

runDirect :: DirectMode -> FilePath -> FilePath -> IO ()
runDirect mode typesPath sourcePath = do
    statsEnabled <- getRTSStatsEnabled
    src <- readFile sourcePath
    let opts = benchOpts typesPath True
    paths <- Compile.findPaths sourcePath opts
    env0 <- Env.initEnv typesPath False
    let modName = Compile.modName paths

    s0 <- getStats statsEnabled
    t0 <- getCurrentTime
    parsed <- Parser.parseModule modName sourcePath src Nothing
    E.evaluate (rnf parsed)
    t1 <- getCurrentTime
    s1 <- getStats statsEnabled
    elapsed "parse" t0 t1
    printStatsMaybe "parse_stats" s0 s1

    case mode of
      ParseOnly -> return ()
      _ -> do
        env <- Env.mkEnv [typesPath] env0 parsed
        E.evaluate (forceHTEnv (Env.hnames env))
        E.evaluate (forceHTEnv (Env.hmodules env))
        t2 <- getCurrentTime
        s2 <- getStats statsEnabled
        elapsed "env" t1 t2
        printStatsMaybe "env_stats" s1 s2

        kchecked <- Kinds.check env parsed
        E.evaluate (rnf kchecked)
        t3 <- getCurrentTime
        s3 <- getStats statsEnabled
        elapsed "kinds" t2 t3
        printStatsMaybe "kinds_stats" s2 s3

        case mode of
          KindsOnly -> return ()
          _ -> do
            (nmod, tchecked, typeEnv, tests) <- Types.reconstruct Nothing Nothing env kchecked Nothing
            E.evaluate (rnf nmod)
            E.evaluate (rnf tchecked)
            E.evaluate (forceHTEnv (Env.hnames typeEnv))
            E.evaluate (forceHTEnv (Env.hmodules typeEnv))
            E.evaluate (length tests)
            t4 <- getCurrentTime
            s4 <- getStats statsEnabled
            elapsed "types" t3 t4
            printStatsMaybe "types_stats" s3 s4

runCompilerFront :: Bool -> Bool -> FilePath -> FilePath -> IO ()
runCompilerFront buildFront runBack typesPath sourcePath = do
    statsEnabled <- getRTSStatsEnabled
    src <- readFile sourcePath
    srcBytes <- B.readFile sourcePath
    let opts = benchOpts typesPath (not buildFront)
    paths <- Compile.findPaths sourcePath opts
    env0 <- Env.initEnv typesPath False
    let modName = Compile.modName paths

    s0 <- getStats statsEnabled
    t0 <- getCurrentTime
    parsed <- Parser.parseModule modName sourcePath src Nothing
    E.evaluate (rnf parsed)
    t1 <- getCurrentTime
    s1 <- getStats statsEnabled
    elapsed "parse" t0 t1
    printStatsMaybe "parse_stats" s0 s1

    res <- Compile.runFrontPasses
      benchGopts
      opts
      False
      paths
      env0
      parsed
      src
      srcBytes
      Nothing
      (Compile.getPubHashCached paths)
      (Compile.getImplHashCached paths)
      (resolveNameHashMap paths)
      (\_ -> return ())
      (\_ _ -> return ())
      (\_ _ _ -> return ())
      (\_ _ _ -> return ())
      (return True)
      (\_ -> return ())
    fr <- case res of
            Left diags -> error ("front pass failed with " ++ show (length diags) ++ " diagnostics")
            Right fr -> return fr
    if runBack then return () else Compile.waitFrontOutputJobs fr
    E.evaluate (length (Compile.frIfaceTE fr) + length (Compile.frImps fr) + length (Compile.frNameHashes fr))
    t2 <- getCurrentTime
    s2 <- getStats statsEnabled
    elapsed "front" t1 t2
    printStatsMaybe "front_stats" s1 s2
    forM_ (Compile.frFrontTiming fr) printFrontTiming

    case (runBack, Compile.frBackJob fr) of
      (True, Just job) -> do
        (do
            s3 <- getStats statsEnabled
            t3 <- getCurrentTime
            (_mtime, mtiming) <- Compile.runBackPasses benchGopts (Compile.bjOpts job) (Compile.bjPaths job) (Compile.bjInput job) (return True)
            t4 <- getCurrentTime
            s4 <- getStats statsEnabled
            elapsed "back" t3 t4
            printStatsMaybe "back_stats" s3 s4
            forM_ mtiming printBackTiming)
          `E.finally` Compile.waitFrontOutputJobs fr
      (True, Nothing) -> do
        Compile.waitFrontOutputJobs fr
        error "front pass did not return a back job"
      (False, _) -> return ()

main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
      ["--parse", typesPath, sourcePath] ->
        runDirect ParseOnly typesPath sourcePath
      ["--kinds", typesPath, sourcePath] ->
        runDirect KindsOnly typesPath sourcePath
      ["--types", typesPath, sourcePath] ->
        runDirect TypesOnly typesPath sourcePath
      ["--hash", reps, typesPath, sourcePath] ->
        runHashBench (read reps) typesPath sourcePath
      ["--hash-breakdown", reps, typesPath, sourcePath] ->
        runHashBreakdown (read reps) typesPath sourcePath
      ["--hash-minimal", reps, count] ->
        runHashMinimal (read reps) (read count)
      ["--front", typesPath, sourcePath] ->
        runCompilerFront False False typesPath sourcePath
      ["--front-docs", typesPath, sourcePath] ->
        runCompilerFront True False typesPath sourcePath
      ["--pipeline", typesPath, sourcePath] ->
        runCompilerFront True True typesPath sourcePath
      _ ->
        error "usage: compiler-bench (--parse|--kinds|--types|--front|--front-docs|--pipeline) TYPES_PATH SOURCE.act | (--hash|--hash-breakdown) REPS TYPES_PATH SOURCE.act | --hash-minimal REPS N"
