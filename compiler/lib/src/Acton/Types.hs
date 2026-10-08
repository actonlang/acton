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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Acton.Types(reconstruct, showTyFile, prettySigs, TypeError(..), TypeErrors(..), TypeProgressCallback, TypeInferredCallback) where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (runState)
import Data.Maybe (isJust)
import Data.List (nub, nubBy, intersect, sort)
import Pretty
import qualified Control.Exception
import Debug.Trace
import Utils
import Acton.Syntax
import Acton.Names
import Acton.Builtin
import Acton.Prim
import Acton.NameInfo
import Acton.Env
import Acton.Solver
import Acton.Subst
import Acton.Transform
import Acton.Converter
import Acton.TypeEnv
import Acton.WitKnots
import qualified InterfaceFiles
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Set as Set
import Data.List (foldl', intersperse, isPrefixOf, partition)
import Data.Maybe (mapMaybe, fromMaybe)
import GHC.Conc (getNumCapabilities)

type TypeProgressCallback = Int -> Int -> Maybe String -> [String] -> Int -> IO ()
type TypeInferredCallback = [String] -> String -> IO ()

emitTypeProgressIO :: Maybe TypeProgressCallback -> Int -> Int -> Maybe String -> [String] -> Int -> IO ()
emitTypeProgressIO Nothing _ _ _ _ _ = return ()
emitTypeProgressIO (Just cb) total completed current names weight = cb total completed current names weight

emitTypeInferredIO :: Maybe TypeInferredCallback -> Env -> TEnv -> IO ()
emitTypeInferredIO Nothing _ _        = return ()
emitTypeInferredIO (Just cb) env te   = cb (map nstr (dom te)) (render $ pretty (simp env [(n, stripDocsNI i) | (n,i) <- te]))

data TypeErrors = TypeErrors [TypeError]
                  deriving (Show)

instance Control.Exception.Exception TypeErrors

-- | Type-check a module and return its module NameInfo, typed module, env, and discovered tests.
reconstruct                             :: Maybe TypeProgressCallback -> Maybe TypeInferredCallback -> Env0 -> Module -> Maybe String -> IO (NModule, Module, Env0, [String])
reconstruct progressCb inferredCb env0 (Module m i mdoc ss) bareMod = do --traceM ("#################### original env0 for " ++ prstr m ++ ":")
                                             --traceM (render (pretty env0))
                                             (te,ss1) <- infTop progressCb inferredCb env1 ss
                                             let env2 = defineClosed te (setMod m env0)
                                                 (teT,ssT,tests) =
                                                   if hasTesting i
                                                     then let (testSs, discovered) = testStmts env2 (fromMaybe (modNameStr m) bareMod) ss1
                                                          in (te ++ testEnv, ss1 ++ testSs, discovered)
                                                     else (te, ss1, [])
                                                 -- Defaults are evaluated at call sites, so every free
                                                 -- name they export must retain its definition-site
                                                 -- meaning.  Unlike ordinary type names, expressions in
                                                 -- DefaultSpec are not handled by Unalias; qualify them
                                                 -- once the module name is available.
                                                 iface = qualifyInterfaceDefaults env2 (unalias env2 teT)
                                                 nmod = NModule (getImports env2) iface mdoc
                                             --traceM ("#################### converted env0:")
                                             --traceM (render (pretty env0'))
                                             return (nmod, Module m i mdoc ssT, env0', tests)

  where env1                            = reserveClosed (assigned ss) (initTypeEnv env0)
        env0'                           = convEnvProtos env0
        hasTesting i                    = Import NoLoc [ModuleItem (ModName [name "testing"]) Nothing] `elem` i
        rmTests (Assign _ [PVar _ n _] _ : ss)
          | nstr n `elem` ["__unit_tests","__simple_sync_tests","__sync_tests","__async_tests","__env_tests"]
                                        = rmTests ss
        rmTests (Decl _ [Actor _ n _ _ _ _ _] : ss)
          | nstr n == "test_main"       = rmTests ss
        rmTests (s : ss)                = s : rmTests ss
        rmTests []                      = []

        -- Convert the module name (ModName) to a string, e.g. "foo.bar"
        modNameStr (ModName ns) = concat (intersperse "." (map nstr ns))

qualifyInterfaceDefaults env          = map qualifyBinding
  where qualifyBinding (n,i)           = (n, qualifyInfo i)

        qualifyInfo (NVar t)           = NVar (qualifyType t)
        qualifyInfo (NSVar t)          = NSVar (qualifyType t)
        qualifyInfo (NDef sc d doc)    = NDef (qualifySchema sc) d doc
        qualifyInfo (NSig sc d doc)    = NSig (qualifySchema sc) d doc
        qualifyInfo (NAct q p k te doc)= NAct (map qualifyQBind q) (qualifyTypeIn q p) (qualifyTypeIn q k)
                                               (qualifyInterfaceDefaults env te) doc
        qualifyInfo (NClass q cs te doc)
                                        = NClass (map qualifyQBind q) (map qualifyWTCon cs)
                                                 (qualifyInterfaceDefaults env te) doc
        qualifyInfo (NProto q ps te doc)
                                        = NProto (map qualifyQBind q) (map qualifyWTCon ps)
                                                 (qualifyInterfaceDefaults env te) doc
        qualifyInfo (NType q t doc)     = NType (map qualifyQBind q) (qualifyTypeIn q t) doc
        qualifyInfo (NExt q c ps te os doc)
                                        = NExt (map qualifyQBind q) (qualifyTCon c) (map qualifyWTCon ps)
                                               (qualifyInterfaceDefaults env te) os doc
        qualifyInfo (NTVar k c ps)      = NTVar k (qualifyTCon c) (map qualifyTCon ps)
        qualifyInfo i                   = i

        qualifySchema (TSchema l q t)   = TSchema l (map qualifyQBind q) (qualifyTypeIn q t)
        qualifyQBind (QBind v cs)       = QBind v (map qualifyTCon cs)
        qualifyTCon (TC q ts)           = TC q (map qualifyType ts)
        qualifyWTCon (path,c)           = (path, qualifyTCon c)

        qualifyType                     = qualifyTypeWith []
        qualifyTypeIn q                 = qualifyTypeWith [ w | (w,_) <- qualWits env q ]
        qualifyTypeWith ws (TCon l (TC q ts))
                                        = TCon l (TC q (map (qualifyTypeWith ws) ts))
        qualifyTypeWith ws (TFun l fx p k r)
                                        = TFun l (qualifyTypeWith ws fx) (qualifyTypeWith ws p)
                                                 (qualifyTypeWith ws k) (qualifyTypeWith ws r)
        qualifyTypeWith ws (TTuple l p k)
                                        = TTuple l (qualifyTypeWith ws p) (qualifyTypeWith ws k)
        qualifyTypeWith ws (TOpt l t)   = TOpt l (qualifyTypeWith ws t)
        qualifyTypeWith ws (TRow l rk n t d r)
                                        = TRow l rk n (qualifyTypeWith ws t) (fmap (qualifyDefault ws) d)
                                                        (qualifyTypeWith ws r)
        qualifyTypeWith ws (TStar l rk r)
                                        = TStar l rk (qualifyTypeWith ws r)
        qualifyTypeWith ws (TUnboxed l t)
                                        = TUnboxed l (qualifyTypeWith ws t)
        qualifyTypeWith _ t             = t

        -- Keep the presentation expression exactly as written.  Only the
        -- checked expansion needs definition-site qualification.
        qualifyDefault ws (DfltExpr e v ref)
                                        = DfltExpr e (qualifyExpr ws v) (exportRef ref)
        qualifyExpr ws                  = qualifyDefaultExprExcept env ws
        exportRef (Just q)
          | Internal Tempvar _ _ <- noq q
                                        = Nothing
        exportRef ref                   = ref

-- Instantiating a polymorphic signature creates fresh caller-side protocol
-- witnesses.  Checked defaults refer to stable witness placeholders derived
-- from the signature's quantified variables; rebind those placeholders to the
-- freshly created witnesses before Solver copies a default into the call.
instantiateDefaults env sc@(TSchema _ q _)
                                        = do (cs,tvs,t) <- instantiate env sc
                                             let s = [ (w,e) | Eqn _ w _ e <- witSubst env q cs ]
                                             return (cs,tvs,substDefaultTerms s t)

substDefaultTerms s (TCon l (TC q ts))= TCon l (TC q (map (substDefaultTerms s) ts))
substDefaultTerms s (TFun l fx p k r) = TFun l (substDefaultTerms s fx)
                                               (substDefaultTerms s p)
                                               (substDefaultTerms s k)
                                               (substDefaultTerms s r)
substDefaultTerms s (TTuple l p k)    = TTuple l (substDefaultTerms s p) (substDefaultTerms s k)
substDefaultTerms s (TOpt l t)        = TOpt l (substDefaultTerms s t)
substDefaultTerms s (TRow l rk n t d r)
                                        = TRow l rk n (substDefaultTerms s t)
                                                  (fmap substDefault d) (substDefaultTerms s r)
  where substDefault (DfltExpr e v ref)= DfltExpr e (termsubst s v) ref
substDefaultTerms s (TStar l rk r)    = TStar l rk (substDefaultTerms s r)
substDefaultTerms s (TUnboxed l t)    = TUnboxed l (substDefaultTerms s t)
substDefaultTerms _ t                 = t


-- | Print a .tydb header and interface; include name hashes when verbose.
showTyFile env0 m fname verbose = do
                                     (_,nmod,_,sourceMeta,srcH,pubH,implH,imps,depModules,nameHashes,roots,tests,mdocH) <- InterfaceFiles.readFile fname
                                     putStrLn ("\n############### Header ###############")
                                     putStrLn ("Imports: " ++ (show [ (prstr mn, take 16 (B.unpack $ Base16.encode h)) | (mn,h) <- imps ]))
                                     putStrLn ("Deps   : " ++ (show [ (prstr mn, take 16 (B.unpack $ Base16.encode pubH'), take 16 (B.unpack $ Base16.encode implH')) | InterfaceFiles.DepModuleInfo mn pubH' implH' <- depModules ]))
                                     putStrLn ("Roots  : " ++ (show (map prstr roots)))
                                     putStrLn ("Tests  : " ++ (show tests))
                                     case mdocH of
                                       Just ds -> putStrLn ("Doc    : \"\"\"" ++ ds ++ "\"\"\"")
                                       Nothing -> return ()
                                     putStrLn ("ModuleSrcBytesHash: 0x" ++ (B.unpack $ Base16.encode srcH))
                                     putStrLn ("ModulePubHash     : 0x" ++ (B.unpack $ Base16.encode pubH))
                                     putStrLn ("ModuleImplHash    : 0x" ++ (B.unpack $ Base16.encode implH))

                                     when verbose $ do
                                       putStrLn ("\n############### Source Meta ############")
                                       case sourceMeta of
                                         Nothing -> putStrLn "None"
                                         Just meta -> do
                                           putStrLn ("mtimeNs: " ++ show (InterfaceFiles.sfmMTimeNs meta))
                                           putStrLn ("ctimeNs: " ++ show (InterfaceFiles.sfmCTimeNs meta))
                                           putStrLn ("size   : " ++ show (InterfaceFiles.sfmSize meta))
                                           putStrLn ("device : " ++ maybe "-" show (InterfaceFiles.sfmDevice meta))
                                           putStrLn ("inode  : " ++ maybe "-" show (InterfaceFiles.sfmInode meta))
                                       putStrLn ("\n############### Name Hashes ############")
                                       forM_ nameHashes $ \nh -> do
                                         let formatHash h =
                                               if B.null h
                                                 then ""
                                                 else "0x" ++ B.unpack (Base16.encode h)
                                             srcHex = formatHash (InterfaceFiles.nhSrcHash nh)
                                             pubHex = formatHash (InterfaceFiles.nhPubHash nh)
                                             implHex = formatHash (InterfaceFiles.nhImplHash nh)
                                             showDep (qn,h) = (prstr qn, take 16 (B.unpack $ Base16.encode h))
                                         putStrLn ("Name   : " ++ prstr (InterfaceFiles.nhName nh))
                                         putStrLn ("  src  : " ++ srcHex)
                                         putStrLn ("  pub  : " ++ pubHex)
                                         putStrLn ("  impl : " ++ implHex)
                                         when (not (null (InterfaceFiles.nhPubLocalDeps nh))) $
                                           putStrLn ("  pubLocalDeps : " ++ show (map prstr (InterfaceFiles.nhPubLocalDeps nh)))
                                         when (not (null (InterfaceFiles.nhImplLocalDeps nh))) $
                                           putStrLn ("  implLocalDeps: " ++ show (map prstr (InterfaceFiles.nhImplLocalDeps nh)))
                                         when (not (null (InterfaceFiles.nhPubDeps nh))) $
                                           putStrLn ("  pubDeps : " ++ show (map showDep (InterfaceFiles.nhPubDeps nh)))
                                         when (not (null (InterfaceFiles.nhImplDeps nh))) $
                                           putStrLn ("  implDeps: " ++ show (map showDep (InterfaceFiles.nhImplDeps nh)))

                                     putStrLn ("\n############### Interface ############")
                                     let NModule imps te mdoc = nmod
                                     forM_ mdoc $ \docstring ->
                                       putStrLn $ "\"\"\"" ++ docstring ++ "\"\"\""

                                     putStrLn $ prettySigs env0 m imps te

prettySigs env m imps te        = render $ vcat [ text "import" <+> pretty m | m <- nub imps ] $++$
                                           vpretty (simp env1 te)
  where env1                    = defineClosed te $ setMod m env

nodup x
  | not $ null vs               = err2 vs "Duplicate names:"
  | otherwise                   = True
  where vs                      = duplicates (bound x)


addTyping env n s t c                   = c {info = addT n (simp env s) t (info c){errloc = loc n}}
    where addT n s t (DfltInfo l m mbe ts)
                                        = DfltInfo l m mbe ((n,s,t):ts)
          addT _ _ _ info               = info

------------------------------

-- | One source-level progress unit: the names in the top-level statement and
-- its weight.  A recursive group can bind several names, so the weight is kept
-- separate from the label.
type TopProgressItem                    = ([String], Int)

-- | Progress events are sent from the scanner thread and from background
-- checker workers to the single progress reporter thread.
data TopProgressEvent                   = TopProgressStarted TopProgressItem
                                        | TopProgressFinished TopProgressItem
                                        | TopProgressStop

-- Concurrent type checker
infTop                                  :: Maybe TypeProgressCallback -> Maybe TypeInferredCallback -> Env -> Suite -> IO (TEnv,Suite)
infTop _ _ _ []                         = return ([], [])
infTop progressCb inferredCb env ss     = do -- The scanner itself is sequential, but total statements, i.e.
                                             -- with a complete type signature can be independently type checked
                                             -- by a fixed worker queue in the background.
                                             ncap <- getNumCapabilities
                                             -- nworkers is the active checkTopStmt parallelism. window is bounded
                                             -- scanner lookahead, so generated modules cannot allocate one queued
                                             -- result and closure per total top-level statement.
                                             let nworkers = max 1 (min ncap (length ss))
                                                 window = max 1 (min (10 * nworkers) (length ss))
                                             -- slots is acquired before enqueueing and released by the worker
                                             -- after that queued total statement has finished checking.
                                             slots <- newQSem window
                                             workQ <- newChan
                                             -- Progress reporting is serialized through one channel so concurrent
                                             -- workers do not call the UI callback directly.
                                             progressQ <- newChan
                                             workers <- replicateM nworkers (async $ worker slots workQ progressQ)
                                             -- The progress reporter is independent from scanning/checking; it
                                             -- consumes start/finish events until it receives TopProgressStop.
                                             progressA <- async $ topProgress progressCb total progressQ
                                             -- Stop the reporter after all work has been collected, and wait so
                                             -- the final progress update is emitted before infTop returns.
                                             let stopProgress = writeChan progressQ TopProgressStop >> wait progressA
                                                 -- Workers exit on Nothing, written only after collect has consumed
                                                 -- all real jobs.
                                                 stopWorkers = replicateM_ nworkers (writeChan workQ Nothing) >> mapM_ wait workers
                                                 -- run is the real top-level flow: scan left-to-right, collect
                                                 -- all completed worker results, then validate signatures once
                                                 -- the whole top-level environment is known.
                                                 run = do
                                                   (te,ss,errs) <- go slots workQ progressQ env [] [] ss
                                                   stopWorkers
                                                   stopProgress
                                                   if null errs
                                                     then do runType "" (checkSigs env te)
                                                             return (te, ss)
                                                     else throwTopErrors errs
                                             -- If scan/check throws before stopProgress runs, make sure the
                                             -- background threads do not stay blocked on readChan.
                                             run `Control.Exception.finally` (mapM_ cancel workers >> cancel progressA)
  where -- Total source progress is known syntactically before any type checking.
        total                           = sum (map stmtProgressWeight ss)

        -- Each worker runs total statements from workQ. Nothing is the stop
        -- marker; Just jobs must always publish their result MVar and release
        -- their scanner slot before the worker moves on.
        worker slots workQ progressQ    = do job <- readChan workQ
                                             case job of
                                               Nothing -> return ()
                                               Just (env,te,s,item,result) -> do
                                                 writeChan progressQ (TopProgressStarted item)
                                                 Control.Exception.finally
                                                   (runWorker env te s result)
                                                   (writeChan progressQ (TopProgressFinished item) >> signalQSem slots)
                                                 worker slots workQ progressQ

        -- End of input: every statement has either produced an immediate
        -- result or a worker wait action, so now wait for them and assemble the
        -- final top-level environment and typed suite in source order.
        go slots workQ q env tes rs []  = collect tes rs
        -- Main scanner loop. This is deliberately sequential: every new scan
        -- sees a type environment containing all previous top-level declarations.
        go slots workQ q env tes rs (s:ss)
                                        = do let item = topProgressItem s
                                             -- scanOrCheck always scans in this thread.  If the statement is not
                                             -- total, scanOrCheck also runs checkTopStmt here and blocks progress.
                                             r <- scanOrCheck env s item q
                                             case r of
                                               -- A scan error stops the source-order scan.
                                               -- Existing worker results are still collected so their errors can
                                               -- be reported together with this one.
                                               Left err ->
                                                 collect tes (return (Left err) : rs)
                                               -- A total statement has a complete boundary after scanning.  Its
                                               -- declarations can be put in env immediately, while full checking
                                               -- continues in the background.
                                               Right (True,te1,s1,_) -> do
                                                 -- Acquire a queue slot before enqueueing so scanner lookahead
                                                 -- is bounded, then let a fixed worker run checkTopStmt.
                                                 waitQSem slots
                                                 result <- newEmptyMVar
                                                 writeChan workQ (Just (env,te1,s1,item,result))
                                                 -- Continue scanning with the scanned declarations visible.
                                                 -- Keep te1 in the scanning environment, but collect the checked
                                                 -- environment returned by the worker. Checked defaults can differ
                                                 -- from their provisional scan representations.
                                                 go slots workQ q (tydefineClosed te1 env) (te1:tes) (readMVar result:rs) ss
                                               -- A non-total statement was already fully checked by scanOrCheck,
                                               -- so becomes complete / total before we get here and the scanner may
                                               -- continue.
                                               Right (_,te2,_,ss1) ->
                                                 go slots workQ q (tydefineClosed te2 env) (te2:tes)
                                                    (return (Right (te2,ss1)):rs) ss
        -- Wait for all statement results in source order, concatenate their
        -- checked environments, keep typed statements whose checks succeeded,
        -- and keep every error for aggregate reporting.
        collect tes rs                  = do xs <- sequence (reverse rs)
                                             let te0 = concat [ te1 | Right (te1,_) <- xs ]
                                                 ss0 = [ s | Right (_,ss1) <- xs, s <- ss1 ]
                                                 refs = defaultRefSubst te0
                                             return (clearDefaultRefs refs te0, termsubst refs ss0,
                                                     [ e | Left e <- xs ])
        -- Scanner/checker front door for one statement. Ordinary exceptions
        -- become Left values so infTop can collect multiple errors; async
        -- exceptions still escape through tryTop.
        scanOrCheck env s item q        = tryTop $ do
                                             let p = nstr $ uniqPrefix s
                                             ((total,te1,s1),st) <- runTypeFromState p (initTypeState p) $ do
                                               pushFX fxPure tNone
                                               -- The scanner returns the syntactic totality bit, the scanned
                                               -- top-level env, and the rewritten statement.
                                               scanTopStmt env s
                                             if total
                                               -- Total means the signature boundary is complete, so the
                                               -- expensive check can be deferred to a worker.
                                               then do te1 <- Control.Exception.evaluate (force te1)
                                                       return (True,te1,s1,[])
                                               -- Non-total means inference/checking is needed now. This blocks
                                               -- the scanner because later statements need this completed env.
                                               else do
                                                 writeChan q (TopProgressStarted item)
                                                 Control.Exception.finally
                                                   (do ((te2,ss1),_) <- runTypeFromState p st (checkTopStmt env te1 s1)
                                                       (te2,ss1) <- forceChecked te2 ss1
                                                       emitTypeInferredIO inferredCb env te2
                                                       return (False,te2,s1,ss1))
                                                   (writeChan q (TopProgressFinished item))
        -- Background worker body for a total statement.  It receives exactly
        -- the scanned env and scanned statement returned by scanTopStmt.
        checkTotal env te s             = tryTop $ do
                                             (te1,ss1) <- runType (nstr $ uniqPrefix s) $ do
                                               pushFX fxPure tNone
                                               checkTopStmt env te s
                                             forceSuite te1 ss1
        -- Once a worker has taken a job, collect may wait on result. Mask the
        -- publish step so every claimed job fills the MVar; async exceptions are
        -- rethrown afterwards so cancellation still propagates normally.
        runWorker env te s result       = Control.Exception.mask $ \restore -> do
                                             r <- Control.Exception.try (restore (checkTotal env te s))
                                             case r of
                                               Left err -> do putMVar result (Left err)
                                                              when (isAsyncException err) (Control.Exception.throwIO err)
                                               Right x -> putMVar result x
        -- Run a TypeM action and convert TypeM's Either error into the IO
        -- exception path used by tryTop.
        runType p m                     = do r <- Control.Exception.evaluate (runTypeMState p m)
                                             case r of
                                               Left err -> Control.Exception.throwIO err
                                               Right (x,_) -> return x
        runTypeFromState p st m         = do r <- Control.Exception.evaluate (runState (runExceptT m) st)
                                             case r of
                                               (Left err, _) -> Control.Exception.throwIO err
                                               (Right x, st') -> return (x, st')
        -- Return both checked declarations and statements: default expressions
        -- in a total declaration are finalized by the worker even though the
        -- scanner has already continued with its provisional environment.
        forceSuite                      = forceChecked
        forceChecked te ss              = do te <- Control.Exception.evaluate (force te)
                                             ss <- Control.Exception.evaluate (force ss)
                                             return (te,ss)

topProgress                             :: Maybe TypeProgressCallback -> Int -> Chan TopProgressEvent -> IO ()
topProgress progressCb total q          = do when (total > 0) $
                                               emit [] 0
                                             loop [] 0
  where loop active done                = do event <- readChan q
                                             case event of
                                               TopProgressStarted item -> do
                                                 let active' = active ++ [item]
                                                 emit active' done
                                                 loop active' done
                                               TopProgressFinished item -> do
                                                 let active' = removeProgressItem item active
                                                     done' = done + snd item
                                                 emit active' done'
                                                 loop active' done'
                                               TopProgressStop ->
                                                 emit [] done
        emit active done                = emitTypeProgressIO progressCb total done (activeProgressLabel active) (concatMap fst active) 0

topProgressItem                         :: Stmt -> TopProgressItem
topProgressItem s                       = (stmtProgressNames s, stmtProgressWeight s)

removeProgressItem                      :: TopProgressItem -> [TopProgressItem] -> [TopProgressItem]
removeProgressItem item []              = []
removeProgressItem item (x:xs)
  | item == x                           = xs
  | otherwise                           = x : removeProgressItem item xs

activeProgressLabel                     :: [TopProgressItem] -> Maybe String
activeProgressLabel []                  = Nothing
activeProgressLabel active              = Just (formatActive active)
  where formatActive active             = show nstmt ++ " " ++ plural "stmt" nstmt ++ ", "
                                       ++ show nnames ++ " " ++ plural "name" nnames ++ ": "
                                       ++ formatNames names
          where nstmt                   = length active
                names                   = concatMap fst active
                nnames                  = length names
        plural s n                      = s ++ if n == 1 then "" else "s"
        formatNames [n]                 = n
        formatNames [n1,n2]             = n1 ++ ", " ++ n2
        formatNames (n1:n2:rest)        = n1 ++ ", " ++ n2 ++ " (+" ++ show (length rest) ++ " others)"
        formatNames []                  = ""

tryTop                                  :: IO a -> IO (Either Control.Exception.SomeException a)
tryTop m                                = do r <- Control.Exception.try m
                                             case r of
                                               Left err | isAsyncException err -> Control.Exception.throwIO err
                                               _ -> return r

isAsyncException                        :: Control.Exception.SomeException -> Bool
isAsyncException err                    = isJust (Control.Exception.fromException err :: Maybe Control.Exception.SomeAsyncException)

throwTopErrors                          :: [Control.Exception.SomeException] -> IO a
throwTopErrors errs                     = case others of
                                           err:_ -> Control.Exception.throwIO err
                                           [] -> case typeErrs of
                                                   [] -> error "Internal error: empty type error list"
                                                   [err] -> Control.Exception.throwIO err
                                                   _ -> Control.Exception.throwIO (TypeErrors typeErrs)
  where (typeErrs, others)              = foldl' collect ([], []) errs
        collect (ts, os) err            = case topTypeErrors err of
                                            Just ts' -> (ts ++ ts', os)
                                            Nothing -> (ts, os ++ [err])

topTypeErrors                           :: Control.Exception.SomeException -> Maybe [TypeError]
topTypeErrors err                       = case Control.Exception.fromException err of
                                            Just (TypeErrors errs) -> Just errs
                                            Nothing -> case Control.Exception.fromException err of
                                                         Just typeErr -> Just [typeErr]
                                                         Nothing -> Nothing

-- | Progress weight for one top-level statement, based on bound names.
-- This makes large recursive groups count proportionally.
stmtProgressWeight :: Stmt -> Int
stmtProgressWeight s = length (stmtProgressNames s)

-- | Top-level bound names used for progress reporting and weighting.
stmtProgressNames :: Stmt -> [String]
stmtProgressNames (Decl _ ds)          = [ nstr (dname' d) | d <- ds ]
stmtProgressNames s@Assign{}           = map nstr (bound s)
stmtProgressNames s@Signature{}        = map nstr (bound s)
stmtProgressNames s                    = error ("Unexpected top-level stmt: " ++ prstr s)

uniqPrefix :: Stmt -> Name
uniqPrefix (Decl _ (d : _))            = dname' d
uniqPrefix s@Assign{}                  = head (bound s)
uniqPrefix s@Signature{}               = head (bound s)
uniqPrefix s                           = error ("Unexpected top-level stmt: " ++ prstr s)


infTopStmt                              :: Env -> Stmt -> TypeM (TEnv, [Stmt])
infTopStmt env s                        = do (total,te1,s) <- scanTopStmt env s
                                             -- NOTE: when total is True, te2 below is guaranteed to be identical to te1
                                             --when total $ traceM ("## Scanned total env for " ++ prstrs (dom te1))
                                             (te2,s) <- checkTopStmt env te1 s
                                             return (te2, s)


scanTopStmt                             :: Env -> Stmt -> TypeM (Bool, TEnv, Stmt)
scanTopStmt env (Decl l ds)             = do (_,te,ds) <- infEnv (setInDecl env) ds
                                             return (null $ ufree te, te, Decl l ds)
scanTopStmt env (Signature l ns sc d)   = return (True, [ (n, NSig sc d Nothing) | n <- ns ], Signature l ns sc d)
scanTopStmt env (Assign l pats e)       = do (te,_,pats) <- infEnvT env pats
                                             return (null $ ufree te, te, Assign l pats e)


checkTopStmt                            :: Env -> TEnv -> Stmt -> TypeM (TEnv, [Stmt])
checkTopStmt env te (Decl l ds)         = do (cs,ds) <- checkEnv (tydefine te env) ds

                                             --traceM ("****************************************** infer (" ++ show (length cs) ++ ") " ++ prstrs (bound ds))
                                             --traceM ("\n\n\n############\n" ++ render (nest 4 $ vcat $ map pretty te))
                                             --traceM ("------------\n" ++ render (nest 4 $ vcat $ map pretty ds))
                                             --traceM ("\\\\\\\\\\\\\n" ++ render (nest 4 $ vcat $ map pretty cs))

                                             (te,eq,ds) <- genEnv env cs te ds
                                             --traceM ("============ push\n" ++ render (nest 4 $ vcat $ map pretty eq))
                                             --traceM ("------------ onto\n" ++ render (nest 4 $ vcat $ map pretty ds))
                                             finishToStmt env te eq (Decl l ds)
checkTopStmt env te (Signature l ns sc d)
                                        = return ([ (n, NSig sc d Nothing) | n <- ns ], [Signature l ns sc d])
checkTopStmt env te (Assign l pats e)   = do (_,t,pats) <- infEnvT env pats
                                             (cs,e) <- inferSub env t e
                                             eq <- solveAll env te cs
                                             finishToStmt env te eq (Assign l pats e)

finishToStmt env te eq s                = do te <- defaultX env te
                                             --traceM ("===========\n" ++ render (nest 4 $ vcat $ map pretty te))
                                             --traceM ("-----------\n" ++ render (nest 4 $ pretty s))
                                             eq <- usubst eq
                                             let s0 = qualifyDefaultNames env s
                                             let (eq0, eq1) = spliteqns eq
                                             --traceM ("~~~~~~~~~~~~ top:\n" ++ render (nest 4 $ vcat $ map pretty eq0))
                                             --traceM ("============ scoped:\n" ++ render (nest 4 $ vcat $ map pretty eq1))
                                             s <- defaultX env =<< termred eq1 <$> usubst (pushEqns env eq0 s0)
                                             let s1 = qualifyDefaultNames env (inlineDefaultWitnesses eq s)
                                             --traceM (".........................................."  ++ prstrs (bound s) ++ "\n")
                                             let s' = fixupSelf s1
                                                 te1 = refreshDefaults te s'
                                                 refs = defaultRefSubst te1
                                                 s'' = termsubst refs s'
                                                 te2 = substituteDefaultRefs refs te1
                                             tieWitKnots te2 [s'']

-- During the scan phase, recursive declarations must publish their callable
-- types before their defaults have been checked.  Calls in the same recursive
-- group must therefore not copy the provisional source expression directly:
-- doing so bypasses elaboration of literals, coercions, and protocol
-- witnesses.  Give every provisional default a temporary reference instead;
-- finishToStmt replaces those references with the checked expressions after
-- solving the group.
deferDefaultExpansions (TFun l fx p k r)= TFun l fx <$> deferDefaultRow p <*> deferDefaultRow k <*> pure r
deferDefaultExpansions t                = return t

deferDefaultRow (TRow l rk n t d rest)
                                        = do d' <- mapM deferSpec d
                                             rest' <- deferDefaultRow rest
                                             return (TRow l rk n t d' rest')
  where deferSpec (DfltExpr source _ Nothing)
                                        = do refName <- newTmp
                                             let ref = NoQ refName
                                             return (DfltExpr source (eQVar ref) (Just ref))
        deferSpec spec                 = return spec
deferDefaultRow (TStar l rk rest)      = TStar l rk <$> deferDefaultRow rest
deferDefaultRow row                    = return row

defaultRefSubst                         = concatMap refsBinding
  where refsBinding (_,i)               = refsInfo i
        refsInfo (NVar t)               = refsType t
        refsInfo (NSVar t)              = refsType t
        refsInfo (NDef (TSchema _ _ t) _ _)
                                            = refsType t
        refsInfo (NSig (TSchema _ _ t) _ _)
                                            = refsType t
        refsInfo (NAct _ p k te _)      = refsType p ++ refsType k ++ defaultRefSubst te
        refsInfo (NClass _ _ te _)      = defaultRefSubst te
        refsInfo (NProto _ _ te _)      = defaultRefSubst te
        refsInfo (NType _ t _)          = refsType t
        refsInfo (NExt _ c ps te _ _)   = concatMap refsType (tcargs c) ++
                                              concatMap (concatMap refsType . tcargs . snd) ps ++
                                              defaultRefSubst te
        refsInfo _                      = []

        refsType (TCon _ (TC _ ts))    = concatMap refsType ts
        refsType (TFun _ fx p k r)     = concatMap refsType [fx,p,k,r]
        refsType (TTuple _ p k)         = refsType p ++ refsType k
        refsType (TOpt _ t)             = refsType t
        refsType (TRow _ _ _ t d r)     = maybe [] ref d ++ refsType t ++ refsType r
        refsType (TStar _ _ r)          = refsType r
        refsType (TUnboxed _ t)         = refsType t
        refsType _                      = []

        ref (DfltExpr _ (Var _ (NoQ valueName)) (Just (NoQ n)))
          | valueName == n                 = []
        ref (DfltExpr _ value (Just (NoQ n)))
                                            = [(n,value)]
        ref _                           = []

substituteDefaultRefs                  = updateDefaultRefs False

clearDefaultRefs                       = updateDefaultRefs True

updateDefaultRefs clear refs           = map clearBinding
  where clearBinding (n,i)              = (n, clearInfo i)
        clearInfo (NVar t)              = NVar (clearType t)
        clearInfo (NSVar t)             = NSVar (clearType t)
        clearInfo (NDef sc d doc)       = NDef (clearSchema sc) d doc
        clearInfo (NSig sc d doc)       = NSig (clearSchema sc) d doc
        clearInfo (NAct q p k te doc)   = NAct q (clearType p) (clearType k) (updateDefaultRefs clear refs te) doc
        clearInfo (NClass q cs te doc)  = NClass q cs (updateDefaultRefs clear refs te) doc
        clearInfo (NProto q ps te doc)  = NProto q ps (updateDefaultRefs clear refs te) doc
        clearInfo (NType q t doc)       = NType q (clearType t) doc
        clearInfo (NExt q c ps te os doc)
                                            = NExt q c ps (updateDefaultRefs clear refs te) os doc
        clearInfo i                     = i

        clearSchema (TSchema l q t)     = TSchema l q (clearType t)
        clearType (TCon l (TC n ts))    = TCon l (TC n (map clearType ts))
        clearType (TFun l fx p k r)     = TFun l (clearType fx) (clearType p) (clearType k) (clearType r)
        clearType (TTuple l p k)        = TTuple l (clearType p) (clearType k)
        clearType (TOpt l t)            = TOpt l (clearType t)
        clearType (TRow l rk n t d r)   = TRow l rk n (clearType t) (fmap clearRef d) (clearType r)
        clearType (TStar l rk r)        = TStar l rk (clearType r)
        clearType (TUnboxed l t)        = TUnboxed l (clearType t)
        clearType t                     = t

        clearRef (DfltExpr source value ref)
                                            = DfltExpr source (termsubst refs value) ref'
          where ref'                    = case ref of
                                               Just q | clear, Internal Tempvar _ _ <- noq q -> Nothing
                                               _ -> ref

-- The scan phase must publish a function type before its body is checked, so
-- its default rows initially contain parsed expressions. Replace those with
-- the checked and witness-reduced expressions before exporting the interface.
refreshDefaults te stmt                = map refresh te
  where ds                             = topDecls stmt
        refresh entry@(n,i)             = case [ d | d <- ds, dname' d == n ] of
                                             d:_ -> (n, refreshInfo d i)
                                             []  -> entry
        refreshInfo Def{pos=p,kwd=k} (NDef sc dec doc)
                                            = NDef (refreshSchema p k sc) dec doc
        refreshInfo Def{pos=p,kwd=k} (NSig sc dec doc)
                                            = NSig (refreshSchema p k sc) dec doc
        refreshInfo Actor{pos=p,kwd=k,dbody=b} (NAct q pr kr members doc)
                                            = NAct q (refreshPosRow p k pr' kr')
                                                     kr'
                                                     (refreshMembers members b) doc
          where defs                    = parameterDefaults p k
                pr'                     = refreshRow defs pr
                kr'                     = refreshRow defs kr
        refreshInfo Class{dbody=b} (NClass q us members doc)
                                            = NClass q us (refreshMembers members b) doc
        refreshInfo Protocol{dbody=b} (NProto q us members doc)
                                            = NProto q us (refreshMembers members b) doc
        refreshInfo Extension{dbody=b} (NExt q c us members os doc)
                                            = NExt q c us (refreshMembers members b) os doc
        refreshInfo _ i                 = i

        refreshMembers members body     = map (refreshMember ms) members
          where ms                      = concatMap topDecls body
        refreshMember ms entry@(n,i)    = case [ d | d <- ms, dname' d == n ] of
                                             d:_ -> (n, refreshInfo d i)
                                             []  -> entry

        refreshSchema p k (TSchema l q (TFun lt fx pr kr result))
                                            = TSchema l q (TFun lt fx
                                                     (refreshPosRow p k pr' kr') kr' result)
          where defs                    = parameterDefaults p k
                pr'                     = refreshRow defs pr
                kr'                     = refreshRow defs kr
        refreshSchema _ _ sc            = sc

        refreshRow defs (TRow l rk n t old rest)
                                            = TRow l rk n t spec (refreshRow defs rest)
          where spec                    = case lookup n defs of
                                               Just e  -> Just $ case old of
                                                                    Just d  -> DfltExpr (defaultSource d) e (defaultRef d)
                                                                    Nothing -> DfltExpr e e Nothing
                                               Nothing -> old
        refreshRow defs (TStar l rk rest)= TStar l rk (refreshRow defs rest)
        refreshRow _ row                 = row

        -- Override constraints can erase both the names and default markers
        -- from the positional row. Reapply the checked declaration defaults
        -- by callable position. The inferred function rows may omit an
        -- implicit method `self`, so first align the declaration against the
        -- complete fixed-parameter shape, then select its positional prefix.
        refreshPosRow pospars kwdpars prow krow
                                            = refreshPos slots row
          where declared                = posDefaultSlots pospars ++ kwdDefaultSlots kwdpars
                callableCount           = rowEntryCount prow + rowEntryCount krow
                callable                = drop (max 0 (length declared - callableCount)) declared
                slots                   = take (rowEntryCount prow) callable
                row                     = prow

        refreshPos (mb:more) (TRow l rk n t old rest)
                                            = TRow l rk n t spec (refreshPos more rest)
          where spec                    = case mb of
                                               Just e  -> Just $ case old of
                                                                    Just d  -> DfltExpr (defaultSource d) e (defaultRef d)
                                                                    Nothing -> DfltExpr e e Nothing
                                               Nothing -> Nothing
        refreshPos _ row                = row

        posDefaultSlots (PosPar _ _ d rest)
                                            = d : posDefaultSlots rest
        posDefaultSlots PosSTAR{}        = []
        posDefaultSlots PosNIL           = []

        kwdDefaultSlots (KwdPar _ _ d rest)
                                            = d : kwdDefaultSlots rest
        kwdDefaultSlots KwdSTAR{}        = []
        kwdDefaultSlots KwdNIL           = []

        rowEntryCount TRow{rtail=rest}   = 1 + rowEntryCount rest
        rowEntryCount _                  = 0

        defaultSource (DfltExpr e _ _)   = e
        defaultRef (DfltExpr _ _ ref)    = ref

        topDecls (Decl _ declarations)  = declarations
        topDecls (With _ _ body)        = concatMap topDecls body
        topDecls _                      = []

parameterDefaults p k                  = posDefaults p ++ kwdDefaults k
  where posDefaults (PosPar n _ (Just e) rest)
                                            = (n,e) : posDefaults rest
        posDefaults (PosPar _ _ Nothing rest)
                                            = posDefaults rest
        posDefaults _                    = []
        kwdDefaults (KwdPar n _ (Just e) rest)
                                            = (n,e) : kwdDefaults rest
        kwdDefaults (KwdPar _ _ Nothing rest)
                                            = kwdDefaults rest
        kwdDefaults _                    = []

-- A checked default can contain coercion/protocol witnesses introduced while
-- checking the declaration. Since the expression is copied to another call
-- site, inline its witness equations instead of leaving definition-local
-- witness names in the exported type.
inlineDefaultWitnesses eq               = defaultsStmt
  where defaultsStmt (Decl l ds)        = Decl l (map defaultsDecl ds)
        defaultsStmt (If l bs els)      = If l [ Branch e (map defaultsStmt b) | Branch e b <- bs ] (map defaultsStmt els)
        defaultsStmt (While l e b els)  = While l e (map defaultsStmt b) (map defaultsStmt els)
        defaultsStmt (For l p e b els)  = For l p e (map defaultsStmt b) (map defaultsStmt els)
        defaultsStmt (Try l b hs els fin)
                                            = Try l (map defaultsStmt b) (map defaultsHandler hs)
                                                    (map defaultsStmt els) (map defaultsStmt fin)
        defaultsStmt (With l items b)   = With l items (map defaultsStmt b)
        defaultsStmt (Data l p b)       = Data l p (map defaultsStmt b)
        defaultsStmt stmt               = stmt

        defaultsHandler (Handler ex b)  = Handler ex (map defaultsStmt b)

        defaultsDecl d@Def{}            = d{ pos = inlineDefaultsP eq (pos d), kwd = inlineDefaultsK eq (kwd d),
                                               dbody = map defaultsStmt (dbody d) }
        defaultsDecl d@Actor{}          = d{ pos = inlineDefaultsP eq (pos d), kwd = inlineDefaultsK eq (kwd d),
                                               dbody = map defaultsStmt (dbody d) }
        defaultsDecl d@Class{}          = d{ dbody = map defaultsStmt (dbody d) }
        defaultsDecl d@Protocol{}       = d{ dbody = map defaultsStmt (dbody d) }
        defaultsDecl d@Extension{}      = d{ dbody = map defaultsStmt (dbody d) }
        defaultsDecl d                  = d


inlineDefaultsP eq (PosPar n t d rest) = PosPar n t (fmap (inlineWitnessExpr eq) d) (inlineDefaultsP eq rest)
inlineDefaultsP _ p                    = p

inlineDefaultsK eq (KwdPar n t d rest) = KwdPar n t (fmap (inlineWitnessExpr eq) d) (inlineDefaultsK eq rest)
inlineDefaultsK _ k                    = k

qualifyDefaultNames env                = defaultsStmt
  where defaultsStmt (Decl l ds)        = Decl l (map defaultsDecl ds)
        defaultsStmt (If l bs els)      = If l [ Branch e (map defaultsStmt b) | Branch e b <- bs ] (map defaultsStmt els)
        defaultsStmt (While l e b els)  = While l e (map defaultsStmt b) (map defaultsStmt els)
        defaultsStmt (For l p e b els)  = For l p e (map defaultsStmt b) (map defaultsStmt els)
        defaultsStmt (Try l b hs els fin)
                                            = Try l (map defaultsStmt b) (map defaultsHandler hs)
                                                    (map defaultsStmt els) (map defaultsStmt fin)
        defaultsStmt (With l items b)   = With l items (map defaultsStmt b)
        defaultsStmt (Data l p b)       = Data l p (map defaultsStmt b)
        defaultsStmt stmt               = stmt

        defaultsHandler (Handler ex b)  = Handler ex (map defaultsStmt b)

        defaultsDecl d@Def{}            = d{ pos = qualifyP ws (pos d), kwd = qualifyK ws (kwd d),
                                               dbody = map defaultsStmt (dbody d) }
          where ws                      = [ w | (w,_) <- qualWits env (qbinds d) ]
        defaultsDecl d@Actor{}          = d{ pos = qualifyP ws (pos d), kwd = qualifyK ws (kwd d),
                                               dbody = map defaultsStmt (dbody d) }
          where ws                      = [ w | (w,_) <- qualWits env (qbinds d) ]
        defaultsDecl d@Class{}          = d{ dbody = map defaultsStmt (dbody d) }
        defaultsDecl d@Protocol{}       = d{ dbody = map defaultsStmt (dbody d) }
        defaultsDecl d@Extension{}      = d{ dbody = map defaultsStmt (dbody d) }
        defaultsDecl d                  = d

        qualifyP ws (PosPar n t d rest) = PosPar n t (fmap (qualify ws) d) (qualifyP ws rest)
        qualifyP _ p                    = p
        qualifyK ws (KwdPar n t d rest) = KwdPar n t (fmap (qualify ws) d) (qualifyK ws rest)
        qualifyK _ k                    = k
        qualify ws                      = qualifyDefaultExprExcept env ws

-- Canonicalize every free name captured by a default.  Unqualified names need
-- scope-aware substitution so lambda/comprehension locals are left alone;
-- qualified names cannot be locally bound and can be rewritten directly.
qualifyDefaultExpr env                 = qualifyDefaultExprExcept env []

qualifyDefaultExprExcept env excluded e
                                        = qexpr $ termsubst subst e
  where subst                           = [ (n, eQVar $ unalias env (NoQ n))
                                          | NoQ n <- nub (freeQ e), n `notElem` excluded,
                                            not (temporaryDefaultRef n) ]

        temporaryDefaultRef (Internal Tempvar _ _) = True
        temporaryDefaultRef _                      = False

        qexpr (Var l q@QName{})         = Var l (unalias env q)
        qexpr (Call l f p k)            = Call l (qexpr f) (qposarg p) (qkwdarg k)
        qexpr (Let l ss x)              = Let l (map qstmt ss) (qexpr x)
        qexpr (TApp l f ts)             = TApp l (qexpr f) (unalias env ts)
        qexpr (Async l x)               = Async l (qexpr x)
        qexpr (Await l x)               = Await l (qexpr x)
        qexpr (Index l x i)             = Index l (qexpr x) (qexpr i)
        qexpr (Slice l x s)             = Slice l (qexpr x) (qsliz s)
        qexpr (Cond l x c y)            = Cond l (qexpr x) (qexpr c) (qexpr y)
        qexpr (IsInstance l x c)        = IsInstance l (qexpr x) (unalias env c)
        qexpr (BinOp l x op y)          = BinOp l (qexpr x) op (qexpr y)
        qexpr (CompOp l x ops)          = CompOp l (qexpr x) (map qoparg ops)
        qexpr (UnOp l op x)             = UnOp l op (qexpr x)
        qexpr (Dot l x n)               = Dot l (qexpr x) n
        qexpr (Rest l x n)              = Rest l (qexpr x) n
        qexpr (DotI l x i)              = DotI l (qexpr x) i
        qexpr (RestI l x i)             = RestI l (qexpr x) i
        qexpr (Opt l x b)               = Opt l (qexpr x) b
        qexpr (OptChain l x)            = OptChain l (qexpr x)
        qexpr (Lambda l p k x fx)       = Lambda l (qpospar p) (qkwdpar k) (qexpr x) (unalias env fx)
        qexpr (Yield l x)                = Yield l (fmap qexpr x)
        qexpr (YieldFrom l x)            = YieldFrom l (qexpr x)
        qexpr (Tuple l p k)              = Tuple l (qposarg p) (qkwdarg k)
        qexpr (List l xs)                = List l (map qelem xs)
        qexpr (ListComp l x c)           = ListComp l (qelem x) (qcomp c)
        qexpr (Dict l xs)                = Dict l (map qassoc xs)
        qexpr (DictComp l x c)           = DictComp l (qassoc x) (qcomp c)
        qexpr (Set l xs)                 = Set l (map qelem xs)
        qexpr (SetComp l x c)            = SetComp l (qelem x) (qcomp c)
        qexpr (Paren l x)                = Paren l (qexpr x)
        qexpr (Box t x)                  = Box (unalias env t) (qexpr x)
        qexpr (UnBox t x)                = UnBox (unalias env t) (qexpr x)
        qexpr x                          = x

        qposarg (PosArg x p)             = PosArg (qexpr x) (qposarg p)
        qposarg (PosStar x)              = PosStar (qexpr x)
        qposarg PosNil                   = PosNil
        qkwdarg (KwdArg n x k)           = KwdArg n (qexpr x) (qkwdarg k)
        qkwdarg (KwdStar x)              = KwdStar (qexpr x)
        qkwdarg KwdNil                   = KwdNil
        qelem (Elem x)                   = Elem (qexpr x)
        qelem (Star x)                   = Star (qexpr x)
        qassoc (Assoc k v)               = Assoc (qexpr k) (qexpr v)
        qassoc (StarStar x)              = StarStar (qexpr x)
        qcomp (CompFor l p x c)          = CompFor l (qpat p) (qexpr x) (qcomp c)
        qcomp (CompIf l x c)             = CompIf l (qexpr x) (qcomp c)
        qcomp NoComp                     = NoComp
        qsliz (Sliz l x y z)             = Sliz l (fmap qexpr x) (fmap qexpr y) (fmap qexpr z)
        qoparg (OpArg op x)              = OpArg op (qexpr x)

        qpospar (PosPar n t d p)         = PosPar n (unalias env t) (fmap qexpr d) (qpospar p)
        qpospar (PosSTAR n t)            = PosSTAR n (unalias env t)
        qpospar PosNIL                   = PosNIL
        qkwdpar (KwdPar n t d k)         = KwdPar n (unalias env t) (fmap qexpr d) (qkwdpar k)
        qkwdpar (KwdSTAR n t)            = KwdSTAR n (unalias env t)
        qkwdpar KwdNIL                   = KwdNIL

        qstmt (Expr l x)                 = Expr l (qexpr x)
        qstmt (Assign l ps x)            = Assign l (map qpat ps) (qexpr x)
        qstmt (MutAssign l x y)          = MutAssign l (qexpr x) (qexpr y)
        qstmt (AugAssign l x op y)       = AugAssign l (qexpr x) op (qexpr y)
        qstmt (Assert l x msg)           = Assert l (qexpr x) (fmap qexpr msg)
        qstmt (Delete l x)               = Delete l (qexpr x)
        qstmt (Return l x)               = Return l (fmap qexpr x)
        qstmt (Raise l x)                = Raise l (qexpr x)
        qstmt (If l bs els)              = If l (map qbranch bs) (map qstmt els)
        qstmt (While l x ss els)         = While l (qexpr x) (map qstmt ss) (map qstmt els)
        qstmt (For l p x ss els)         = For l (qpat p) (qexpr x) (map qstmt ss) (map qstmt els)
        qstmt (Try l ss hs els fin)      = Try l (map qstmt ss) (map qhandler hs)
                                                (map qstmt els) (map qstmt fin)
        qstmt (With l items ss)          = With l (map qitem items) (map qstmt ss)
        qstmt (Data l p ss)              = Data l (fmap qpat p) (map qstmt ss)
        qstmt (VarAssign l ps x)         = VarAssign l (map qpat ps) (qexpr x)
        qstmt (After l now x y)          = After l now (qexpr x) (qexpr y)
        qstmt (Signature l ns sc d)      = Signature l ns (unalias env sc) d
        qstmt (Decl l ds)                = Decl l (map qdecl ds)
        qstmt s                          = s

        qbranch (Branch x ss)            = Branch (qexpr x) (map qstmt ss)
        qhandler (Handler ex ss)         = Handler ex (map qstmt ss)
        qitem (WithItem x p)             = WithItem (qexpr x) (fmap qpat p)
        qpat (PWild l t)                  = PWild l (unalias env t)
        qpat (PVar l n t)                 = PVar l n (unalias env t)
        qpat (PParen l p)                 = PParen l (qpat p)
        qpat (PTuple l p k)               = PTuple l (qpospat p) (qkwdpat k)
        qpat (PList l ps p)               = PList l (map qpat ps) (fmap qpat p)
        qpat (PData l n is)               = PData l n (map qexpr is)
        qpospat (PosPat p ps)             = PosPat (qpat p) (qpospat ps)
        qpospat (PosPatStar p)            = PosPatStar (qpat p)
        qpospat PosPatNil                 = PosPatNil
        qkwdpat (KwdPat n p ps)           = KwdPat n (qpat p) (qkwdpat ps)
        qkwdpat (KwdPatStar p)            = KwdPatStar (qpat p)
        qkwdpat KwdPatNil                 = KwdPatNil
        qdecl d@Def{}                    = d{ pos = qpospar (pos d), kwd = qkwdpar (kwd d),
                                               ann = unalias env (ann d), dbody = map qstmt (dbody d) }
        qdecl d@Actor{}                  = d{ pos = qpospar (pos d), kwd = qkwdpar (kwd d),
                                               dbody = map qstmt (dbody d) }
        qdecl d@Class{}                  = d{ bounds = unalias env (bounds d), dbody = map qstmt (dbody d) }
        qdecl d@Protocol{}               = d{ bounds = unalias env (bounds d), dbody = map qstmt (dbody d) }
        qdecl d@Typedef{}                = d{ texp = unalias env (texp d) }
        qdecl d@Extension{}              = d{ tycon = unalias env (tycon d), bounds = unalias env (bounds d),
                                               dbody = map qstmt (dbody d) }

inlineWitnessExpr eq e                 = case allEqs of
                                             []  -> e
                                             _   -> case kept of
                                                       [] -> e'
                                                       _  -> Let NoLoc (bindWits kept) e'
  where
        -- Function-valued Sub witnesses are normally inlined by Transform.
        -- Do that here as well, but retain protocol objects as local bindings:
        -- Boxing can then recognize their static implementations and lower
        -- numeric defaults without allocation.
        e'                              = inlineAll e
        kept                            = [ Eqn level w t (inlineAll rhs)
                                          | Eqn level w t rhs <- allEqs, not (inlineEq t rhs) ]
        allEqs                          = witnessEquations (filter isWitness $ free e) []
        inlineEqs                       = [ q | q@(Eqn _ _ t rhs) <- allEqs, inlineEq t rhs ]
        inlineEq TFun{} _               = True
        inlineEq _ rhs                  = wrapperRoot rhs `elem`
                                              [primWrapProc, primWrapAction, primWrapMut, primWrapPure]
        wrapperRoot (Var _ n)           = n
        wrapperRoot (TApp _ f _)        = wrapperRoot f
        wrapperRoot _                   = NoQ nWild
        inlineAll x                     = foldl subst x (reverse inlineEqs)
        subst x (Eqn _ w _ rhs)         = termsubst [(w,rhs)] x

        witnessEquations [] _           = []
        witnessEquations ws seen        = witnessEquations deps seen' ++ matches
          where matches                 = [ q | q@(Eqn _ w _ _) <- eq, w `elem` ws, w `notElem` seen ]
                deps                    = nub [ n | Eqn _ _ _ rhs <- matches, n <- free rhs,
                                                   isWitness n, n `notElem` seen, n `notElem` map eqName matches ]
                seen'                   = seen ++ map eqName matches
        eqName (Eqn _ w _ _)            = w

defaultX                                :: (UFree a, USubst a) => Env -> a -> TypeM a
defaultX env x                          = do defaultVars (ufree x)
                                             usubst x
  where defaultVars tvs                 = do tvs' <- ufree <$> usubst (map tUni tvs)
                                             sequence [ usubstitute tv (dflt (uvkind tv)) | tv <- tvs' ]
        dflt KType                      = tNone
        dflt KFX                        = fxPure
        dflt PRow                       = posNil
        dflt KRow                       = kwdNil

pushEqns                                :: Env -> Equations -> Stmt -> Stmt
pushEqns env [] s                       = s
pushEqns env eqs s
  | null pre                            = inject env inj s
  | otherwise                           = withLocal (bindWits pre) $ inject env inj s
  where backward                        = free s `intersect` bound eqs
        (pre,inj)                       = split [] [] (bound s) eqs
        split pre inj bvs []            = (reverse pre, reverse inj)
        split pre inj bvs (eq:eqs)
          | null forward                = split (eq:pre) inj bvs eqs
          | otherwise                   = split pre (eq:inj) (bound eq ++ bvs) eqs
          where forward                 = free eq `intersect` bvs

inject env [] s                         = s
inject env eqs (Decl l ds)              = Decl l (map injectDecl ds)
  where reveqs                          = reverse eqs
        injectDecl d@Typedef{}          = d -- A typedef can never refer to any witness names
        injectDecl d                    = d{ dbody = prune [] (free d) reveqs ++ dbody d }
        prune inj fvs []                = --trace ("### Injecting " ++ prstrs (bound inj) ++ " into " ++ prstr n) $
                                          bindWits inj
        prune inj fvs (eq:eqs)
          | null needed                 = prune inj fvs eqs
          | otherwise                   = prune (eq:inj) (free eq ++ fvs) eqs
          where needed                  = bound eq `intersect` fvs
inject env eqs (With l [] ss)           = With l [] (injlast eqs ss)
  where injlast eqs [s]                 = [inject env eqs s]
        injlast eqs (s:ss)              = s : injlast eqs ss
inject env eqs s                        = error ("# Internal error: cyclic witnesses " ++ prstrs eqs ++ "\n# and statement\n" ++ prstr s)


genEnv                                  :: Env -> Constraints -> TEnv -> [Decl] -> TypeM (TEnv,Equations,[Decl])
genEnv env cs te ds
  | any typeDecl te                     = do te <- usubst te
                                             --traceM ("## genEnv types 1\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("   where\n" ++ render (nest 6 $ vcat $ map pretty cs))
                                             eq <- solveAll (posdefine (filter typeDecl te) env) te cs
                                             te <- usubst te
                                             --traceM ("## genEnv types 2\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("   where\n" ++ render (nest 6 $ vcat $ map pretty cs))
                                             return (te, eq, ds)
  | otherwise                           = do te <- usubst te
                                             --traceM ("## genEnv defs 1\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("   where\n" ++ render (nest 6 $ vcat $ map pretty cs))
                                             (cs,eq) <- newSimplify env te cs
                                             te <- usubst te
                                             (gen_us, gen_cs, te, eq) <- refine env cs te eq
                                             let gen_vs = take (length gen_us) tvarSupply
                                             sequence [ usubstitute uv (tVar tv) | (uv,tv) <- gen_us `zip` gen_vs ]
                                             te <- usubst te
                                             gen_cs <- usubst gen_cs
                                             --traceM ("## genEnv defs 2 [" ++ prstrs gen_vs ++ "]\n" ++ render (nest 6 $ pretty te))
                                             --traceM ("   where\n" ++ render (nest 6 $ vcat $ map pretty gen_cs))
                                             let (q,ws) = qualify gen_vs gen_cs
                                                 te1 = map (generalize q) te
                                                 (eq1,eq2) = splitEqs (dom ws) eq
                                                 ds1 = map (abstract q ds ws eq1) ds
                                             --traceM ("## genEnv defs 3 [" ++ prstrs q ++ "]\n" ++ render (nest 6 $ pretty te1))
                                             return (te1, eq2, ds1)
  where
    qualify vs cs                       = (q, concat wss)
      where (q,wss)                     = unzip $ map qbind vs
            qbind v                     = (QBind v bounds, wits)
              where bounds              = [ p | Proto _ _ w (TVar _ v') p <- cs, v == v' ]
                    wits                = [ (w, proto2type t p) | Proto _ _ w t@(TVar _ v') p <- cs, v == v' ]

    generalize q (n, NDef (TSchema l [] t) d doc)
                                        = (n, NDef (TSchema l q t) d doc)
    generalize q (n, i)                 = (n, i)


    abstract q ds ws eq d@Def{}
      | null $ qbinds d                 = d{ qbinds = noqual env q,
                                             pos = wit2par ws (defaultWitsP $ pos d),
                                             kwd = defaultWitsK $ kwd d,
                                             dbody = bindWits eq ++ wsubst ds q ws (dbody d) }
      | otherwise                       = d{ pos = defaultWitsP $ pos d,
                                             kwd = defaultWitsK $ kwd d,
                                             dbody = bindWits eq ++ wsubst ds q ws (dbody d) }
      where defaultWitsP                = termsubst witnessSubst
            defaultWitsK                = termsubst witnessSubst
            witnessSubst                = [ (w,eVar formal)
                                          | ((w,_),(formal,_)) <- ws `zip` qualWits env q ]

    wsubst ds [] []                     = id
    wsubst ds q ws                      = termsubst s
      where s                           = [ (n, Lambda l0 p k (Call l0 (tApp (eVar n) tvs) (wit2arg ws (pArg p)) (kArg k)) fx)
                                            | Def _ n [] p k _ _ _ fx _ <- ds ]
            tvs                         = map tVar $ qbound q

    splitEqs ws eq
      | null eq1                        = ([], eq)
      | otherwise                       = (eq1++eq1', eq2')
      where (eq1,eq2)                   = partition (any (`elem` ws) . free) eq
            (eq1',eq2')                 = splitEqs (bound eq1 ++ ws) eq2

    newRefine env cs te eq              = do (eq,cs) <- newsolve env te eq cs
                                             te <- usubst te
                                             eq <- usubst eq
                                             return (ufree te, cs, te, eq)

    refine env cs te eq
      | run_new_solver                  = newRefine env cs te eq
      | not $ null solve_cs             = do --traceM ("  #solving: " ++ prstrs solve_cs)
                                             (cs',eq') <- solve env noQual te eq cs
                                             refineAgain cs' eq'
      | not $ null ambig_vs             = do --traceM ("  #defaulting: " ++ prstrs ambig_vs)
                                             (cs',eq') <- solve env isAmbig te eq cs
                                             refineAgain cs' eq'
      | not $ null tail_vs              = do sequence [ tryUnify env (Simple NoLoc "internal") (tUni v) (tNil $ uvkind v) | v <- tail_vs ]
                                             refineAgain cs eq
      | otherwise                       = do eq <- usubst eq
                                             return (gen_vs, cs, te, eq)
      where ambig_vs                    = ufree cs \\ closeDepVars (safe_vs) cs
            tail_vs                     = gen_vs `intersect` (tailvars te ++ tailvars cs)

            safe_vs                     = if null def_vss then [] else nub $ foldr1 intersect def_vss
            def_vss                     = [ nub $ filter canGen $ ufree sc | (_, NDef sc _ _) <- te, null $ scbind sc ]
            gen_vs                      = nub (foldr union (ufree cs) def_vss)

            isAmbig c                   = any (`elem` ambig_vs) (ufree c)

            refineAgain cs eq           = do (cs1,eq1) <- newSimplify env te cs
                                             te <- usubst te
                                             refine env cs1 te (eq1++eq)

            solve_cs                    = [ c | c <- cs, noQual c ]

            noQual (Proto _ _ _ (TUni _ u) p)
                                        = False
            noQual c                    = True

            canGen tv                   = uvkind tv /= KFX


markScoped env n q te []                = return ([], [])
-- Should remove this simplify call too, but doing so destroys performance of our current inferior constraint-solver (see module yang.schema in acton-yang).
markScoped env n [] te cs               = newSimplify env te cs
-- Return the marks in terms of NotImplemented equations for now, so that we can coexist with the need to also run simplify (see above)
markScoped env n q te cs                = return (cs, eq)
  where eq                              = [ mkEqn env w tWild eNotImpl | w <- ws ]
        ws                              = scopedWits env q cs

tempGoal t                              = [(nWild, NVar t)]

newSolveAll env te cs                   = do (eq,cs) <- newsolve env te [] cs
                                             (eq,_) <- newsolve env [] eq cs
                                             return eq

solveAll env te []                      = return []
solveAll env te cs
  | run_new_solver                      = newSolveAll env te cs
  | otherwise                           = do --traceM ("\n\n### solveAll " ++ prstrs cs)
                                             (cs,eq) <- newSimplify env te cs
                                             (cs,eq) <- solve env (const True) te eq cs
                                             return eq


--------------------------------------------------------------------------------------------------------------------------

class Infer a where
    infer                               :: Env -> a -> TypeM (Constraints,Type,a)

class InfEnv a where
    infEnv                              :: Env -> a -> TypeM (Constraints,TEnv,a)

class InfEnvT a where
    infEnvT                             :: Env -> a -> TypeM (TEnv,Type,a)


--------------------------------------------------------------------------------------------------------------------------

commonTEnv                              :: Env -> [TEnv] -> TypeM (Constraints,TEnv)
commonTEnv env []                       = return ([], [])
commonTEnv env (te:tes)                 = unifEnv tes (restrict te vs)
  where vs                              = foldr intersect (dom te) $ map dom tes
        l                               = length tes
        unifEnv tes []                  = return ([], [])
        unifEnv tes ((n,i):te)          = do t <- newUnivar env
                                             let (cs1,i') = unif n t i
                                             (cs2,te') <- unifEnv tes te
                                             return (cs1++cs2, (n,i'):te')
        unif n t0 (NVar t)
          | length ts == l              = ([ Cast (locinfo t 26) env t1 t0 | t1 <- t:ts ], NVar t0)
          where ts                      = [ t | te <- tes, Just (NVar t) <- [lookup n te] ]
        unif n t0 (NSVar t)
          | length ts == l              = ([ Cast (locinfo t 27) env t1 t0 | t1 <- t:ts ], NSVar t0)
          where ts                      = [ t | te <- tes, Just (NSVar t) <- [lookup n te] ]
{-
        unif n t0 (NDef sc d)
          | null (scbind sc) &&
            length ts == l              = ([ Cast t t0 | t <- ts ], NDef (monotype t0) d)
          where ts                      = [ sctype sc | te <- tes, Just (NDef sc d') <- [lookup n te], null (scbind sc), d==d' ]
        unif n t0 (NDef _ _)
          | length scs == l             = case findName n env of
                                             NReserved -> err1 n "Expected a common signature for"
                                             NSig sc d -> ([], NDef sc d)
          where scs                     = [ sc | te <- tes, Just (NDef sc d) <- [lookup n te] ]
-}
        unif n _ _                      = err1 n "Conflicting bindings for"



infSuiteEnv env ss                      = do (cs,te,ss') <- infEnv env ss
                                             checkSigs env te
                                             return (cs, te, ss')

checkSigs env te
  | null ns                            = return ()
  | otherwise                           = err2 ns "Signature lacks subsequent binding"
  where (sigs,terms)                    = sigTerms te
        termNames                       = Set.fromList (dom terms)
        ns                              = [ n | n <- dom sigs, Set.notMember n termNames ]

infLiveEnv env x
  | fallsthru x                         = do (cs,te,x') <- infSuiteEnv env x
                                             return (cs, Just te, x')
  | otherwise                           = do (cs,te,x') <- infSuiteEnv env x
                                             return (cs, Nothing, x')

liveCombine te Nothing                  = Nothing
liveCombine Nothing te'                 = Nothing
liveCombine (Just te) (Just te')        = Just $ te++te'

fxUnwrapSc env sc                       = sc{ sctype = fxUnwrap env $ sctype sc }

fxUnwrap env (TFun l fx p k t)          = TFun l (fxUnwrap env fx) p k t
fxUnwrap env (TFX l FXAction)
  | inAct env                           = TFX l FXProc
fxUnwrap env t                          = t

wrap env t@TFun{}                       = do tvx <- newUnivarOfKind KFX env
                                             tvy <- newUnivarOfKind KFX env
                                             w <- newWitness
                                             return (Proto (locinfo t 28) env w tvx (pWrapped (effect t) tvy), t{ effect = tvx })

wrapped l kw env cs ts args             = do tvx <- newUnivarOfKind KFX env
                                             tvy <- newUnivarOfKind KFX env
                                             let p = pWrapped tvx tvy
                                                 Just (_, sc, Just Static) = findAttr env p kw
                                             (_,tvs,t0) <- instantiateDefaults env sc
                                             fx <- newUnivarOfKind KFX env
                                             t' <- newUnivar env
                                             let t1 = vsubst [(fxSelf,fx)] t0
                                                 t2 = tFun fxPure (foldr posRow posNil ts) kwdNil t'
                                             w <- newWitness
                                             unify env (locinfo l 30) t1 t2
                                             t' <- usubst t'
                                             cs' <- usubst (Proto (locinfo l 29) env w fx p : cs)
                                             return (cs', t', eCall (tApp (Dot l0 (eVar w) kw) tvs) args)

--------------------------------------------------------------------------------------------------------------------------

instance (InfEnv a) => InfEnv [a] where
    infEnv env []                       = return ([], [], [])
    infEnv env (s : ss)                 = do (cs1,te1,s1) <- infEnv env s
                                             let te1' = if inDecl env then noDefs te1 else te1      -- TODO: also stop class instantiation!
                                                 env' = tydefine te1' env
                                             (cs2,te2,ss2) <- infEnv env' ss
                                             return (cs1++cs2, te1++te2, s1:ss2)

instance InfEnv Stmt where
    infEnv env (Expr l e)
      | e == eNotImpl                   = return ([], [], Expr l e)
      | otherwise                       = do (cs,_,e') <- infer env e
                                             return (cs, [], Expr l e')

    infEnv env (Assign l pats e)
      | nodup pats, e == eNotImpl       = do (te,t,pats') <- infEnvT env pats
                                             return ([], te, Assign l pats' e)
      | otherwise                       = do (te,t,pats') <- infEnvT env pats
                                             (cs,e') <- inferSub env t e
                                             return (cs, te, Assign l pats' e')

    infEnv env (Assert l e1 e2)         = do (cs1,_,_,_,e1') <- inferTest env e1
                                             (cs2,e2') <- inferSub env tStr e2
                                             return (cs1++cs2, [], Assert l e1' e2')
    infEnv env s@(Pass l)               = return ([], [], s)

    infEnv env s@(Return l Nothing)     = do t <- currRet
                                             return ([Cast (locinfo l 31) env tNone t], [], Return l Nothing)
    infEnv env (Return l (Just e))      = do t <- currRet
                                             (cs,e') <- inferSub env t e
                                             return (cs, [], Return l (Just e'))
    infEnv env (Raise l e)              = do (cs,t,e') <- infer env e
                                             return (Cast (locinfo2 32 e) env t tException : cs, [], Raise l e')
    infEnv env s@(Break _)              = return ([], [], s)
    infEnv env s@(Continue _)           = return ([], [], s)
    infEnv env (If l bs els)            = do (css,tes,bs') <- fmap unzip3 $ mapM (infLiveEnv env) bs
                                             (cs0,te,els') <- infLiveEnv env els
                                             (cs1,te1) <- commonTEnv env $ catMaybes (te:tes)
                                             return (cs0++cs1++concat css, te1, If l bs' els')
    infEnv env (While l e b els)        = do (cs1,env',s,_,e') <- inferTest env e
                                             (cs2,te1,b') <- infSuiteEnv env' b
                                             (cs3,te2,els') <- infSuiteEnv env els
                                             return (cs1++cs2++cs3, [], While l e' (termsubst s b') els')
    infEnv env (For l p e b els)
      | nodup p                         = do (te,t1,p') <- infEnvT env p
                                             t2 <- newUnivar env
                                             (cs2,e') <- inferSub env t2 e
                                             (cs3,te1,b') <- infSuiteEnv (define te env) b
                                             (cs4,te2,els') <- infSuiteEnv env els
                                             w <- newWitness
                                             return (Proto (locinfo2 33 e) env w t2 (pIterable t1) :
                                                     cs2++cs3++cs4, [], For l p' (eCall (eDot (eVar w) iterKW) [e']) b' els')
    infEnv env (Try l b hs els fin)     = do (cs1,te,b') <- infLiveEnv env b
                                             (cs2,te',els') <- infLiveEnv (maybe id define te $ env) els
                                             (css,tes,hs') <- fmap unzip3 $ mapM (infLiveEnv env) hs
                                             (cs3,te1) <- commonTEnv env $ catMaybes $ (liveCombine te te'):tes
                                             (cs4,te2,fin') <- infSuiteEnv env fin
                                             fx <- currFX
                                             return (--Cast fxProc fx :
                                                     cs1++cs2++cs3++cs4++concat css, te1++te2, Try l b' hs' els' fin')
    infEnv env (With l items b)
      | nodup items                     = do (cs1,te,items') <- infEnv env items
                                             (cs2,te1,b') <- infSuiteEnv (define te env) b
                                             return $ (cs1++cs2, exclude te1 (dom te), With l items' b')

    infEnv env (VarAssign l pats e)
      | nodup pats                      = do (te,t,pats') <- infEnvT env pats
                                             --traceM ("## VarAssign " ++ prstrs te)
                                             (cs,e') <- inferSub env t e
                                             let te' = [ (n, NSVar t) | n <- bound pats, let NVar t = findName n (define te env) ]
                                             return (cs, te', VarAssign l pats' e')

    infEnv env (After l now e1 e2)      = do (cs1,e1') <- inferSub env tFloat e1
                                             (cs2,t,e2') <- infer env e2
                                             -- TODO: constrain t
                                             fx <- currFX
                                             return (Cast (locinfo l 34) env fxProc fx :
                                                     cs1++cs2, [], After l now e1' e2')

    infEnv env (Signature l ns sc@(TSchema _ q t) dec)
      | not $ null bad                  = illegalSigOverride (head bad)
      | otherwise                       = return ([], [(n, NSig sc dec' Nothing) | n <- ns], Signature l ns sc dec)
      where
        redefs                          = [ (n,i) | n <- ns, let i = findName n env, i /= NReserved ]
        bad                             = [ n | (n,i) <- redefs, not $ ok i ]
        ok (NSig (TSchema _ [] t') d _) = null q && castable env t t' && dec == d
        ok _                            = False
        dec'                            = if inClass env && isProp dec sc then Property else dec

    infEnv env (Data l _ _)             = notYet l "data syntax"

    infEnv env (Decl l ds)
      | inDecl env && nodup ds          = do (cs1,te1,ds1) <- infEnv env ds
                                             return (cs1, te1, Decl l ds1)
      | nodup ds                        = do --traceM ("######## decls: " ++ prstrs (declnames ds))
                                             (_,te1,ds1) <- infEnv (setInDecl env) ds
                                             (cs2,ds2) <- checkEnv (tydefine te1 env) ds1
                                             --traceM ("-------- done: " ++ prstrs (declnames ds))
                                             let stmt = Decl l ds2
                                                 te2 = refreshDefaults te1 stmt
                                                 refs = defaultRefSubst te2
                                             return (cs2, clearDefaultRefs refs te2, termsubst refs stmt)

    infEnv env (Delete l targ)          = do (cs0,t0,e0,tg) <- infTarg env targ
                                             (cs1,stmt) <- del t0 e0 tg
                                             return (cs0++cs1, [], stmt)
      where del t0 e0 (TgVar n)         = do return ( Cast (locinfo l 35) env tNone t0 : [], sAssign (pVar' n) eNone)
            del t0 e0 (TgIndex ix)      = do ti <- newUnivar env
                                             (cs,ix) <- inferSub env ti ix
                                             t <- newUnivar env
                                             w <- newWitness
                                             return ( Proto (locinfo l 36) env w t0 (pIndexed ti t) : cs, sExpr $ dotCall w delitemKW [e0, ix] )
            del t0 e0 (TgSlice sl)      = do (cs,sl) <- inferSlice env sl
                                             t <- newUnivar env
                                             w <- newWitness
                                             return ( Proto (locinfo l 37) env w t0 (pSliceable t) : cs, sExpr $ dotCall w delsliceKW [e0, sliz2exp sl] )
            del t0 e0 (TgDot n)         = do t <- newUnivar env
                                             return ( Mut (locinfo l 38) env t0 n t : Cast (locinfo l 39) env tNone t : [], sMutAssign (eDot e0 n) eNone )

    infEnv env (MutAssign l targ e)     = do (cs0,t0,e0,tg) <- infTarg env targ
                                             t <- newUnivar env
                                             (cs1,e) <- inferSub env t e
                                             (cs2,stmt) <- asgn t0 t e0 e tg
                                             return (cs0++cs1++cs2, [], stmt)
      where asgn t0 t e0 e (TgVar n)    = do tryUnify env (locinfo l 40) t0 t
                                             return ( [], sAssign (pVar' n) e )
            asgn t0 t e0 e (TgIndex ix) = do ti <- newUnivar env
                                             (cs,ix) <- inferSub env ti ix
                                             w <- newWitness
                                             return ( Proto (locinfo l 41) env w t0 (pMutIndexed ti t) : cs, sExpr $ dotCall w setitemKW [e0, ix, e] )
            asgn t0 t e0 e (TgSlice sl) = do (cs,sl) <- inferSlice env sl
                                             t' <- newUnivar env
                                             w <- newWitness
                                             w' <- newWitness
                                             return ( Proto (locinfo l 42) env w t0 (pSliceable t') :
                                                      Proto (locinfo l 43) env w' t (pIterable t') :
                                                      cs, sExpr $ eCall (tApp (eDot (eVar w) setsliceKW) [t]) [e0, eVar w', sliz2exp sl, e] )
            asgn t0 t e0 e (TgDot n)    = do return ( Mut (locinfo l 44) env t0 n t : [], sMutAssign (eDot e0 n) e )

    infEnv env (AugAssign l targ o e)   = do (cs0,t0,e0,tg) <- infTarg env targ
                                             t1 <- newUnivar env
                                             (cs1,e) <- inferSub env (rtype o t1) e
                                             let (proto,kw) = oper t1 o
                                             t <- if o `elem` [MultA,DivA] then newUnivar env else pure t1
                                             w <- newWitness
                                             (ss,x) <- mkvar t0 e0
                                             (cs2,stmt) <- aug t0 t x (dotCall w kw) e tg
                                             return ( Proto (locinfo l 45) env w t proto : cs0++cs1++cs2, [], withLocal ss stmt )
      where oper t MultA                = (pTimes t,  imulKW)
            oper t DivA                 = (pDiv t,    itruedivKW)
            oper _ PlusA                = (pPlus,     iaddKW)
            oper _ MinusA               = (pMinus,    isubKW)
            oper _ PowA                 = (pNumber,   ipowKW)
            oper _ ModA                 = (pIntegral, imodKW)
            oper _ EuDivA               = (pIntegral, ifloordivKW)
            oper _ ShiftLA              = (pIntegral, ilshiftKW)
            oper _ ShiftRA              = (pIntegral, irshiftKW)
            oper _ BOrA                 = (pLogical,  iorKW)
            oper _ BXorA                = (pLogical,  ixorKW)
            oper _ BAndA                = (pLogical,  iandKW)
            rtype ShiftLA t             = tInt
            rtype ShiftRA t             = tInt
            rtype _ t                   = t

            aug t0 t x f e (TgVar _)    = do tryUnify env (locinfo l 46) t0 t
                                             return ( [], sAssign (pVar' x) $ f [eVar x, e] )
            aug t0 t x f e (TgIndex ix) = do ti <- newUnivar env
                                             (cs,ix) <- inferSub env ti ix
                                             w <- newWitness
                                             return ( Proto (locinfo l 47) env w t0 (pMutIndexed ti t) :
                                                      cs, sExpr $ dotCall w setitemKW [eVar x, ix, f [dotCall w getitemKW [eVar x, ix], e]])
            aug t0 t x f e (TgSlice sl) = do tryUnify env (locinfo l 1115) t0 t
                                             (cs,sl) <- inferSlice env sl
                                             t' <- newUnivar env
                                             w <- newWitness
                                             w' <- newWitness
                                             let e1 = f [dotCall w getsliceKW [eVar x, sliz2exp sl], e]
                                             return ( Proto (locinfo l 48) env w t (pSliceable t') :
                                                      Proto (locinfo l 49) env w' t (pIterable t') :
                                                      cs, sExpr $ eCall (tApp (eDot (eVar w) setsliceKW) [t]) [eVar x, eVar w', sliz2exp sl, e1] )
            aug t0 t x f e (TgDot n)    = do return ( Mut (locinfo l 50) env t0 n t : [], sMutAssign (eDot (eVar x) n) $ f [eDot (eVar x) n, e])


dotCall w kw                            = eCall (eDot (eVar w) kw)

mkvar t (Var _ (NoQ x))                 = return ([], x)
mkvar t e                               = do x <- newTmp
                                             return ([sAssign (pVar x t) e], x)

data Tg                                 = TgVar Name | TgIndex Expr | TgSlice Sliz | TgDot Name

infTarg env e@(Var l (NoQ n))           = case findName n env of
                                             NReserved ->
                                                 err1 n "Variable not yet assigned"
                                             NSig{} ->
                                                 err1 n "Variable not yet assigned"
                                             NVar t ->
                                                 return ([], t, e, TgVar n)
                                             NSVar t -> do
                                                 fx <- currFX
                                                 return ([Cast (locinfo l 51) env fxProc fx], t, e, TgVar n)
                                             _ ->
                                                 err1 n "Variable not assignable:"
infTarg env (Index l e ix)              = do (cs,t,e) <- infer env e
                                             fx <- currFX
                                             return (Cast (locinfo l 52) env fxMut fx : Cast (locinfo' l 53 e) env t tObject : cs, t, e, TgIndex ix)
infTarg env (Slice l e sl)              = do (cs,t,e) <- infer env e
                                             fx <- currFX
                                             return (Cast (locinfo l 54) env fxMut fx : Cast (locinfo' l 55 e) env t tObject : cs, t, e, TgSlice sl)
infTarg env (Dot l e n)                 = do (cs,t,e) <- infer env e
                                             fx <- currFX
                                             return (Cast (locinfo l 56) env fxMut fx : Cast (locinfo' l 57 e) env t tObject : cs, t, e, TgDot n)

sliz2exp (Sliz _ e1 e2 e3)              = eCall (eQVar qnSlice) $ map (maybe eNone id) [e1,e2,e3]

withLocal [] s                          = s
withLocal ss s                          = With l0 [] (ss ++ [s])

--------------------------------------------------------------------------------------------------------------------------

matchingDec n sc dec NoDec              = True
matchingDec n sc dec dec'
  | dec == dec'                         = True
  | otherwise                           = decorationMismatch n sc dec

matchDefAssumption env cs1 def@Def{dname=n, qbinds=q1}
  | q0 == q1                            = match cs1 [] def
  | null q1                             = do let uvs = nub (ufree def ++ ufree cs1) \\ ufree env
                                             --traceM ("### matching " ++ prstr def)
                                             --traceM ("### with\n" ++ render (nest 4 $ vcat $ map pretty cs1))
                                             --traceM ("### against " ++ prstr (n, findName n env) ++ "\n")
                                             sequence [ usubstitute uv =<< newUnivarOfKind (uvkind uv) env0 | uv <- uvs ]
                                             def <- usubst def
                                             cs1 <- usubst (requantize env0 cs1)
                                             match cs1 [] def
  | otherwise                           = do (cs, uvs) <- instQBinds env0 q1
                                             let eq1 = witSubst env0 q1 cs
                                                 s = qbound q1 `zip` uvs
                                                 cs1' = vsubst s (requantize env0 cs1)
                                                 def' = vsubst s def
                                             match (cs ++ cs1') eq1 def'
  where NDef (TSchema _ q0 t) dec _     = findName n env
        t0 | inClass env                = addSelf t (Just dec)
           | otherwise                  = t
        env0                            = tydefineVars q0 env
        fx | inAct env                  = dfx def
           | otherwise                  = effect t0

        match cs eq def                 = do --traceM ("## matchDefAssumption " ++ prstr n ++ ": [" ++ prstrs q0 ++ "] => ")
                                             --traceM (render (nest 4 $ vcat $ map pretty $ Cast info env0 t1 t0 : cs))
                                             (cs',eq') <- markScoped env n q0 (tempGoal t1) (Cast info env0 t1 t0 : cs)
                                             cs' <- usubst cs'
                                             return (cs', def{ qbinds = noqual env0 q0, pos = pos0, kwd = kwd0,
                                                               dbody = bindWits (eq++eq') ++ dbody def, dfx = fx })
           where t1                     = tFun (dfx def) (prowOf $ pos def) (krowOf $ kwd def) (fromJust $ ann def)
                 (pos0,kwd0)            = qualDef env dec (pos def) (kwd def) (qualWPar env q0)
                 sc1                    = TSchema NoLoc q1 t1
                 mbl                    = findSigLoc n env
                 msg                    = "Type incompatibility between signature for and definition of "++Pretty.print n
                 info                   = maybe (locinfo def 58) (\l -> DeclInfo l (loc def) n sc1 msg) mbl

qualDef env dec p k qf | not (inClass env) = (qf p, k)
qualDef env Static p k qf                  = (qf p, k)
qualDef env dec PosNIL (KwdPar n t e k) qf = (PosPar n t e (qf PosNIL), k)
qualDef env dec (PosPar n t e p) k qf      = (PosPar n t e (qf p), k)


initComplement env n as body
-- | True                                = body
  | inBuiltin env || null as            = body
  | otherwise                           = defAltInit : body
  where defAltInit
          | baseHasAltInit              = --trace ("### Connecting altInit chain to " ++ prstr base) $
                                          mkInit (sExpr (eCall (eDot (eQVar basename) altInit) [eVar selfKW]))
          | otherwise                   = --trace ("### Stopping altInit chain before " ++ prstr base) $
                                          mkInit sPass
        base                            = snd $ head as
        basename                        = tcname base
        baseHasAltInit                  = isJust $ findAttr env base altInit
        mkInit stmt                     = sDef altInit (pospar [(selfKW,tSelf)]) tNone [stmt] fxPure

lockSelf env l p k dec                  = tryUnify env (Simple l "Type of first parameter of class method does not unify with Self") tSelf $ selfType p k dec

--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Decl where
    infEnv env d@(Def l n q p k a _ dec' fx ddoc)
      | nodup (p,k)                     = case findName n env of
                                             NSig sc dec _ | t@TFun{} <- sctype sc, matchingDec n sc dec dec' -> do
                                                 --traceM ("\n## infEnv (sig) def " ++ prstr (n, NDef sc dec Nothing))
                                                 when (inClass env) $ lockSelf env l p k dec
                                                 return ([], [(n, NDef (fxUnwrapSc env sc) dec ddoc)], d{deco = dec})
                                             NReserved -> do
                                                 when (inClass env) $ lockSelf env l p k dec'
                                                 t0 <- tFun (fxUnwrap env fx) (prowOf p) (krowOf k) <$> maybe (newUnivar env) return a
                                                 t <- deferDefaultExpansions t0
                                                 let sc = tSchema q (if inClass env then dropSelf t dec' else t)
                                                 --traceM ("\n## infEnv def " ++ prstr (n, NDef sc dec' Nothing))
                                                 return ([], [(n, NDef sc dec' ddoc)], d)
                                             _ ->
                                                 illegalRedef n


    infEnv env d@(Actor _ n q p k b ddoc)
      | nodup (p,k)                     = case findName n env of
                                             NReserved -> do
                                                 te <- infActorEnv env b
                                                 prow <- deferDefaultRow (prowOf p)
                                                 krow <- deferDefaultRow (krowOf k)
                                                 --traceM ("\n## infEnv actor " ++ prstr (n, NAct q prow krow te ddoc))
                                                 return ([], [(n, NAct q prow krow te ddoc)], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env (Class l n q us b ddoc)
      | not $ null ps                   = notYet (loc n) "Classes with direct extensions"
      | otherwise                       = case findName n env of
                                             NReserved -> do
                                                 --traceM ("\n## infEnv class " ++ prstr n)
                                                 when (not (inBuiltin env) && getAttrKW `elem` userMethods) $
                                                     err2 (filter (== getAttrKW) userMethods) "Cannot define reserved compiler-synthesized method"
                                                 pushFX fxPure tNone
                                                 te0 <- infProperties env1 as' b
                                                 (cs,te,b1) <- infEnv env1 b0
                                                 popFX
                                                 when (not $ null cs) $ err (loc n) "Deprecated class syntax"
                                                 checkClassAttributesInitialized n l env as' b
                                                 (nterms,asigs,_,_) <- checkAttributes [] te' te
                                                 let (te2,b2) = if notImplBody b then let te1 = unSig asigs in (te++te1, addImpl te1 b1)
                                                                else if null asigs && initKW `notElem` dom te then relayInit te b1
                                                                else (te,b1)
                                                 return ([], [(n, NClass q as' (te0++te2) ddoc)], Class l n q us (props te0 ++ b2) ddoc)
                                             _ -> illegalRedef n
      where env1                        = define (toSigs te' `exclude` [initKW]) $ reserve (assigned b0) $ tydefineVars (stripQual q') $ setInClass env
            (as,ps)                     = mro2 env us
            as'                         = if null as && not (inBuiltin env && n == nValue) then leftpath [cValue] else as
            te'                         = parentTEnv env as'
            q'                          = selfQuant (NoQ n) q
            props te0                   = [ Signature l0 [n] sc Property | (n,NSig sc Property _) <- te0 ]
            tc                          = TC (NoQ n) (map tVar $ qbound q)
            b0                          = addGetAttr (initComplement env n as' b)
            -- Synthesize an empty-bodied reflective attribute getter
            --   pure def __get_attr__(self, name: str) -> ?value: return None
            -- on every user class. The body stays empty through type checking and
            -- hashing, so it references no fields and pins nothing in the
            -- dependency graph; CodeGen fills it from the class property set.
            -- __get_attr__ is a reserved name: a user definition is rejected above,
            -- which keeps CodeGen's unconditional replacement of the body sound.
            -- Skipped for builtins (C-defined structs); the userMethods guard only
            -- avoids synthesizing a duplicate on the rejected path.
            addGetAttr body
              | inBuiltin env           = body
              | getAttrKW `elem` userMethods
                                        = body
              | otherwise               = getAttrDef : body
            userMethods                 = [ dname d | Decl _ ds <- b, d@Def{} <- ds ]
            getAttrDef                  = sDef getAttrKW (pospar [(selfKW,tSelf),(nameKW,tStr)]) (tOpt tValue) [sReturn eNone] fxPure
            relayInit te b              = --trace ("####### Creating relayInit for class " ++ prstr n) $
                                          case lookup initKW (reverse te') of
                                            Just ni@(NDef sc _ _) ->
                                                ((initKW,ni):te, sDecl [Def NoLoc initKW [] pp kp Nothing body NoDec fx Nothing]:b)
                                              where t  = addSelf (sctype sc) (Just NoDec)
                                                    pp = pPar pNames $ posrow t
                                                    kp = kPar attrKW $ kwdrow t
                                                    fx = effect t
                                                    body = [sExpr $ Call NoLoc (eDot (eQVar $ tcname $ head us) initKW) (pArg pp) (kArg kp)]

    infEnv env (Protocol l n q us b ddoc)
                                        = case findName n env of
                                             NReserved -> do
                                                 --traceM ("\n## infEnv protocol " ++ prstr n)
                                                 pushFX fxPure tNone
                                                 (cs,te,b') <- infEnv env1 b
                                                 popFX
                                                 when (not $ null cs) $ err (loc n) "Deprecated protocol syntax"
                                                 (nterms,_,_,sigs) <- checkAttributes [] te' te
                                                 let noself = [ n | (n, NSig sc Static _) <- te, tvSelf `notElem` vfree sc ]
                                                 when (notImplBody b) $ err0 (notImpls b) "A protocol body cannot be NotImplemented"
                                                 when (not $ null nterms) $ err2 (dom nterms) "Method/attribute lacks signature:"
                                                 when (initKW `elem` sigs) $ err2 (filter (==initKW) sigs) "A protocol cannot define __init__"
                                                 when (not $ null noself) $ err2 noself "A static protocol signature must mention Self"
                                                 return ([], [(n, NProto q ps te ddoc)], Protocol l n q us b' ddoc)
                                             _ -> illegalRedef n
      where env1                        = define (toSigs te') $ reserve (assigned b) $ tydefineVars (stripQual q') $ setInClass env
            ps                          = mro1 env us
            te'                         = parentTEnv env ps
            q'                          = selfQuant (NoQ n) q

    infEnv env d@(Typedef l n q t ddoc)
                                        = case findName n env of
                                             NReserved ->
                                                 return ([], [(n, NType q t ddoc)], d)
                                             _ ->
                                                 illegalRedef n

    infEnv env (Extension l q c us b ddoc)
      | length us == 0                  = err (loc n) "Extension lacks a protocol"
--      | length us > 1                   = notYet (loc n) "Extensions with multiple protocols"
      | not $ null witsearch            = err (loc n) ("Extension already exists: " ++ prstr (head witsearch))
      | otherwise                       = do --traceM ("\n## infEnv extension " ++ prstr (extensionName us c))
                                             pushFX fxPure tNone
                                             (cs,te,b1) <- infEnv env1 b
                                             popFX
                                             when (not $ null cs) $ err (loc n) "Deprecated extension syntax"
                                             (nterms,asigs,fsigs,sigs) <- checkAttributes (dom finals) te' te
                                             when (not $ null nterms) $ err2 (dom nterms) "Method/attribute not in listed protocols:"
                                             when (not $ null sigs) $ err2 sigs "Extension with new methods/attributes not supported"
                                             when (not (null asigs || notImplBody b)) $ err3 l (dom asigs) "Protocol method/attribute lacks implementation:"
                                             let te1 = unSig $ selfSubst n q asigs
                                                 fwds = selfSubst n q $ nubBy (\a b -> fst a == fst b) $ reverse fsigs
                                                 te2 = te ++ te1 ++ unSig fwds
                                                 b2 = addImpl te1 b1 ++ map fwdDef fwds
                                             return ([], [(extensionName us c, NExt q c ps te2 [] ddoc)], Extension l q c us b2 ddoc)
      where TC n ts                     = c
            env1                        = define (toSigs te') $ reserve (assigned b) $ tydefineVars (stripQual q') $ setInClass env
            witsearch                   = findWitness env (tCon c) u
            u                           = head us
            ps                          = selfSubst n q $ mro1 env us -- TODO: check that ps doesn't contradict any previous extension mro for c
            finals                      = [ (a, p) | (_,p) <- tail ps, hasWitness env (tCon c) p, a <- conAttrs env (tcname p) ]
            te'                         = parentTEnv env ps
            q'                          = selfQuant n q
            -- A final slot is already implemented by an earlier witness for its protocol.
            -- It becomes a method that calls the slot through that protocol, which the type
            -- checker resolves to the covering witness like any other call.
            fwdDef (a, NSig sc dec _)   = sDecl [Def NoLoc a (scbind sc) pp kp (Just $ restype t) [sReturn call] dec (effect t) Nothing]
              where t                   = addSelf (sctype sc) (Just dec)
                    pp                  = pPar pNames $ posrow t
                    kp                  = kPar attrKW $ kwdrow t
                    Just owner          = lookup a finals
                    call                = Call NoLoc (eDot (eQVar $ tcname owner) a) (pArg pp) (kArg kp)

--------------------------------------------------------------------------------------------------------------------------

checkAttributes final te' te
  | not $ null dupsigs                  = err2 dupsigs "Duplicate signatures for"
  | not $ null props                    = err2 props "Property attributes cannot have class-level definitions:"
  | not $ null nodef                    = err2 nodef "Methods finalized in a previous extension cannot be overridden:"
  | not $ null clashes                  = err2 clashes "Conflicting inherited signatures for"
  | otherwise                           = return (nterms, abssigs, finalsigs, dom sigs)
  where (sigs,terms)                    = sigTerms te
        (sigs',terms')                  = sigTerms te'
        (allsigs,allterms)              = (sigs ++ sigs', terms ++ terms')
        dupsigs                         = duplicates (dom sigs)
        nterms                          = terms `exclude` dom allsigs
        misssigs                        = allsigs `exclude` dom allterms
        abssigs                         = misssigs `exclude` final
        finalsigs                       = misssigs `restrict` final
        clashes                         = nub [ n | (n, NSig sc dec _) <- sigs', n /= initKW, (n', NSig sc' dec' _) <- sigs',
                                                    n == n', sctype sc' /= sctype sc || dec' /= dec ]
        props                           = dom terms `intersect` dom (propSigs allsigs)
        nodef                           = dom terms `intersect` final

addImpl [] ss                           = ss
addImpl asigs (s : ss)
  | isNotImpl s                         = fromTEnv (unSig asigs) ++ s : ss
  | otherwise                           = s : addImpl asigs ss

toSigs te                               = map makeSig te
  where makeSig (n, NDef sc dec doc)    = (n, NSig sc dec doc)
        makeSig (n, NVar t)             = (n, NSig (monotype t) Static Nothing)
        makeSig (n, i)                  = (n,i)


--------------------------------------------------------------------------------------------------------------------------

checkClassAttributesInitialized         :: Name -> SrcLoc -> Env -> [WTCon] -> Suite -> TypeM ()
checkClassAttributesInitialized className classLoc env ancestors b
                                        = do -- Only check if the class defines its own __init__. If it doesn't, it uses the parent's
                                             -- __init__ which already initialized everything (and we check the parent separately)
                                             case findInitMethod b of
                                                 Nothing -> return ()  -- No __init__, uses parent's
                                                 Just (self, initBody, initLoc) ->
                                                     -- Check if __init__ is implemented in C (body is NotImplemented)
                                                     if hasNotImpl initBody
                                                       then return ()  -- Assume all is OK - we can't analyze C implementation
                                                       else do
                                                         let inherited = concatMap (getPropertiesFromClass env . tcname . snd) ancestors
                                                             explicit  = concat [ ns | Signature _ ns sc dec <- b, isProp dec sc ]
                                                             inferred  = inferClassAttributes self initBody
                                                             expected  = nub $ inherited ++ explicit ++ inferred
                                                             initialized = scanSelfAssigns env self b initBody
                                                             -- Track which parent __init__ methods are called
                                                             calledParentInits = getCalledParentInits env initBody
                                                             -- Get attributes initialized by called parent __init__ methods
                                                             parentInitialized = nub $ concatMap (getInitializedByParent env) calledParentInits
                                                             uninitialized = expected \\ (initialized ++ parentInitialized)

                                                         forM_ uninitialized $ \prop ->
                                                             let parentInfo = findAttributeParent prop
                                                                 isInferred = prop `elem` inferred && prop `notElem` explicit && prop `notElem` inherited
                                                             in Control.Exception.throw $ UninitializedAttribute (loc prop) prop isInferred initLoc classLoc className parentInfo
  where getPropertiesFromClass env qn   = let (_,_,te) = findConName qn env
                                          in [ n | (n, NSig _ Property _) <- te ]

        -- Helper to look up class location in environment
        getClassLoc env qname           = case findClass (activeNames env) of
                                            Just l  -> l
                                            Nothing -> maybe NoLoc id (findClass (closedNames env))
          where findClass ((n, NClass{}):te)
                  | n == noq qname      = Just (loc n)
                findClass (_:te)        = findClass te
                findClass []            = Nothing

        -- Find which parent class (if any) defines the given attribute
        findAttributeParent attrName    = case [ n | Signature _ ns _ _ <- b, n <- ns, n == attrName ] of
                                              (_:_) -> Nothing  -- Defined in current class
                                              []    -> -- Look for it in ancestors
                                                     case mapMaybe (findInAncestor attrName) ancestors of
                                                         (result:_) -> Just result
                                                         []         -> Nothing

        -- Check if a specific ancestor defines the attribute
        findInAncestor attrName (_, anc)= let ancName = tcname anc
                                          in if attrName `elem` getPropertiesFromClass env ancName
                                             then Just (noq ancName, getClassLoc env ancName)
                                             else Nothing


wellformed                              :: (WellFormed a) => Env -> a -> TypeM ()
wellformed env x                        = do _ <- solveAll env [] cs
                                             return ()
  where cs                              = wf env x

wellformedProtos                        :: Env -> [PCon] -> TypeM (Constraints, [(QName,[Expr])])
wellformedProtos env ps                 = do (css0, css1) <- unzip <$> mapM (wfProto env) ps
                                             _ <- solveAll env [] (concat css0)
                                             return (concat css1, [ (tcname p, protoWitsOf cs) | (p,cs) <- ps `zip` css1 ])


--------------------------------------------------------------------------------------------------------------------------

class Check a where
    checkEnv                            :: Env -> a -> TypeM (Constraints,a)
    checkEnv'                           :: Env -> a -> TypeM (Constraints,[a])
    checkEnv env x                      = undefined
    checkEnv' env x                     = do (cs,x') <- checkEnv env x
                                             return (cs, [x'])

instance (Check a) => Check [a] where
    checkEnv env []                     = return ([], [])
    checkEnv env (d:ds)                 = do (cs1,d') <- checkEnv' env d
                                             (cs2,ds') <- checkEnv env ds
                                             return (cs1++cs2, d'++ds')

------------------

infActorEnv env ss                      = do dsigs <- mapM mkNDef ddefs                                 -- exposed defs without sigs
                                             bsigs <- mapM mkNVar pvars                                 -- exposed assigns without sigs
                                             return (abssigs ++ unSig concsigs ++ dsigs ++ bsigs)       -- abstract sigs ++ exposed sigs + the above
  where sigs                            = [ (n, NSig sc dec Nothing) | Signature _ ns sc dec <- ss, n <- ns ]
        (concsigs, abssigs)             = partition ((`elem`(dvars++pvars)) . fst) sigs
        dvars                           = methods ss \\ dom sigs
        ddefs                           = [ d | Decl _ ds <- ss, d@Def{dname=n} <- ds, n `elem` dvars ]
        -- Calls through an actor interface can occur while the actor's
        -- recursive declaration group is still being checked.  Methods with
        -- defaults need their known callable shape at scan time; a single
        -- unconstrained type variable loses the default markers when an early
        -- call first fixes its row.  Explicitly generic methods likewise need
        -- their quantified scope here.  Keep the old monomorphic assumption
        -- for all other methods, since it deliberately lets recursive actor
        -- inference determine the entire function type as one unit.
        mkNDef (Def _ n q p k a _ dec fx doc)
          | not (null q) || hasDefaultsP p || hasDefaultsK k
                                        = do result <- maybe (newUnivar env) return a
                                             effect <- newUnivarOfKind KFX env
                                             pr <- freshRow PRow (prowOf p)
                                             kr <- freshRow KRow (krowOf k)
                                             t <- deferDefaultExpansions $ tFun effect pr kr result
                                             -- prowOf/krowOf represent missing annotations
                                             -- with wildcards.  The old monomorphic recursion
                                             -- assumption supplied inference variables for those
                                             -- positions, so uses of an unannotated actor method
                                             -- could still determine its parameter types.  Keep
                                             -- that property while retaining the row shape and its
                                             -- default markers.  Freshen only those wildcards: the
                                             -- general instwild operation also expands named types,
                                             -- including the actor currently reserved by the scan.
                                             return (n, NDef (tSchema q t) dec doc)
          | otherwise                   = do t <- newUnivar env
                                             return (n, NDef (monotype t) dec doc)
        hasDefaultsP (PosPar _ _ d p)   = isJust d || hasDefaultsP p
        hasDefaultsP PosSTAR{}          = False
        hasDefaultsP PosNIL             = False
        hasDefaultsK (KwdPar _ _ d k)   = isJust d || hasDefaultsK k
        hasDefaultsK KwdSTAR{}          = False
        hasDefaultsK KwdNIL             = False
        freshRow k (TWild _)            = newUnivarOfKind k env
        freshRow k (TRow l rk n t d r)  = do t' <- freshEntry t
                                             r' <- freshRow rk r
                                             return (TRow l rk n t' d r')
        freshRow k (TStar l rk r)       = TStar l rk <$> freshRow rk r
        freshRow _ r                    = return r
        freshEntry (TWild _)            = newUnivar env
        freshEntry t                    = return t
        svars                           = statevars ss
        pvars                           = pvarsF ss \\ dom (sigs) \\ dvars
        pvarsF ss                       = nub $ concat $ map pvs ss
          where pvs (Assign _ ps _)     = bound ps                   -- svars only excluded until we move stateful actor cmds to __init__
                pvs (VarAssign _ ps _)  = bound ps
                pvs (If _ bs els)       = foldr intersect (pvarsF els) [ pvarsF ss | Branch _ ss <- bs ]
                pvs _                   = []
        mkNVar n                        = do t <- newUnivar env
                                             return (n, if n `elem` svars then NSVar t else NVar t)

maybeSeal env n ts
  | isHidden n                          = []
  | otherwise                           = [ Seal (locinfo n 114) env t | t <- ts ]

matchActorAssumption env n0 p k te      = do --traceM ("## matchActorAssumption " ++ prstrs te)
                                             (css,eqs) <- unzip <$> mapM check1 te0
                                             let cs = [Cast (locinfo p 60) env (tTuple p0 k0) (tTuple (prowOf p) (krowOf k)),
                                                       Seal (locinfo p 112) env p0, Seal (locinfo k 113) env k0]
                                             (cs,eq) <- oldSimplify env obs (cs ++ concat css)
                                             return (cs, eq ++ concat eqs)
  where NAct q p0 k0 te0 _              = findName n0 env
        ns                              = dom te0
        obs                             = te0 ++ te
        te1                             = nTerms $ te `restrict` ns
        check1 (n, NSig _ _ _)          = return ([], [])
        check1 (n, NVar t0)             = do --traceM ("## matchActorAssumption for attribute " ++ prstr n)
                                             return (Cast (locinfo n 62) env t t0 : maybeSeal env n [t0], [])
          where Just (NVar t)           = lookup n te1
        check1 (n, NSVar t0)            = do --traceM ("## matchActorAssumption for state var " ++ prstr n)
                                             return ([Cast (locinfo n 62) env t t0], [])
          where Just (NSVar t)          = lookup n te1
        check1 (n, NDef sc0 _ _)
          | TUni{} <- sctype sc0        = do (cs0,_,t0) <- instantiateDefaults env sc
                                             (c0,t') <- wrap env t0
                                             let c1 = Cast (locinfo n 63) env t' (sctype sc0)
                                                 cs1 = maybeSeal env n (leaves sc0)
                                             (cs2,eq) <- markScoped env n0 (scbind sc0) obs
                                                                  (c0:c1:cs0++cs1)
                                             return (cs2, eq)
          | otherwise                   = do (c0,t') <- wrap env0 t
                                             let c1 = Cast (locinfo n 63) env0 t' (sctype sc0)
                                             --traceM ("## matchActorAssumption for method " ++ prstr n ++ ": " ++ prstr c1)
                                             -- A structured provisional interface retains
                                             -- defaults or an explicit quantified scope.  Do
                                             -- not seal each still-unknown leaf here: method-body
                                             -- constraints are combined only by the enclosing
                                             -- actor solve, and early sealing prevents those
                                             -- constraints from completing inference.
                                             (cs2,eq) <- markScoped env n0 q0 obs [c0,c1]
                                             return (cs2, eq)
          where Just (NDef sc _ _)      = lookup n te1
                q0                      = scbind sc0
                env0                    = tydefineVars q0 env
                -- Both schemas originate in the same method declaration.  Match
                -- them under one rigid quantified scope; instantiating only the
                -- checked schema would let its fresh variables escape that scope.
                t                       = vsubst (qbound (scbind sc) `zip` map tVar (qbound q0)) (sctype sc)
        check1 (n, i)                   = return ([], [])


-- Find __init__ method in class body, return self parameter, body and location
findInitMethod :: Suite -> Maybe (Name, Suite, SrcLoc)
findInitMethod b                        = listToMaybe [ (x, dbody d, loc d) | Decl _ ds <- b, d <- ds, dname d == initKW, Just x <- [selfPar d] ]

-- Scan __init__ for "self.x = ..." to find which attributes are definitely
-- assigned before any references to `self` escape externally. We allow most
-- statements in the constructor as long as `self` does not escape although
-- there are a few statements that will also be considered the end of the
-- constructor (this isn't well documented). We are handling if/elif/else as
-- well as some variations of exception handling and raising. Loops are allowed
-- but any assignments in a loop does not count to the set of initialized
-- attributes since we cannot statically determine if the loop executes. The
-- loop is scanned to ensure `self` does not escape.
scanSelfAssigns :: Env -> Name -> Suite -> Suite -> [Name]
scanSelfAssigns env self classBody stmts = scanSuite [] stmts
  where
    -- Check if a method in the class has NotImplemented body
    isNotImplMethod :: Name -> Bool
    isNotImplMethod methodName          = case [ d | Decl _ ds <- classBody, d@Def{dname=n} <- ds, n == methodName ] of
                                                (d:_) -> hasNotImpl (dbody d)
                                                []    -> False
    -- Check if a statement list ends with an early exit (only raise - not return!)
    -- Return would give back an uninitialized object, so it doesn't excuse initialization
    branchExitsEarly :: Suite -> Bool
    branchExitsEarly []                 = False
    branchExitsEarly stmts              = case last stmts of
                                              Raise _ _ -> True  -- Only raise truly prevents object creation
                                              -- Recursively check nested if/else
                                              If _ branches elseBranch ->
                                                  all (\(Branch _ body) -> branchExitsEarly body) branches &&
                                                  (not (null elseBranch) && branchExitsEarly elseBranch)
                                              -- Recursively check try/except
                                              Try _ tryBody handlers elseBranch finallyBlock ->
                                                  -- If finally block raises, that's an early exit
                                                  if not (null finallyBlock) && branchExitsEarly finallyBlock
                                                    then True
                                                    else -- Otherwise, check if all normal paths exit early
                                                         branchExitsEarly tryBody &&
                                                         all (\(Handler _ hbody) -> branchExitsEarly hbody) handlers &&
                                                         (null elseBranch || branchExitsEarly elseBranch)
                                              _ -> False
    scanSuite seen []                   = []
    -- Handle assignments
    scanSuite seen (MutAssign _ (Dot _ (Var _ qn) n) e : rest)
        | noq qn == self                = if checkNoSelfReference self seen e
                                            then n : scanSuite (n:seen) rest
                                            else []  -- Stop at self reference
    scanSuite seen (MutAssign _ _ _ : rest)
                                        =     -- Continue: local assignment is OK
                                          scanSuite seen rest
    scanSuite seen (Assign _ _ e : rest)
                                        =     -- Continue past local assignments, unless RHS contains self reference
                                          if checkNoSelfReference self seen e
                                            then scanSuite seen rest  -- Continue past assignment
                                            else []  -- Stop at self reference
    scanSuite seen (AugAssign _ (Dot _ (Var _ qn) n) _ _ : rest)
        | noq qn == self && n `elem` seen  -- OK: augmenting already-initialized attribute
                                        = scanSuite seen rest
        | noq qn == self                = []  -- STOP: can't augment uninitialized attribute
    scanSuite seen (AugAssign _ _ _ _ : rest)
                                        =     -- Continue: local augmented assignment is OK
                                          scanSuite seen rest
    -- Handle if/elif/else statements. Count assignments that happen in all
    -- branches as unconditional. Note how we must have an else branch in order
    -- to consider it exhaustive. Each branch either:
    --   1. Completes normally and returns an object (must have initialized attributes), or
    --   2. Exits early via raise (never returns an object, so doesn't constrain initialization)
    --
    -- We need the intersection of assignments from all branches. Branches that exit early
    -- are "compatible" with any assignment set (they contribute the universal set to the
    -- intersection), so in practice we only intersect the branches that complete normally.
    --
    -- Example: if cond:          if cond:          if cond:
    --            self.x = 1        self.x = 1        raise Error()
    --          else:             else:             else:
    --            self.x = 2        raise Error()     self.x = 1
    --          Result: {x}       Result: {x}       Result: {x}
    --
    -- Without an else branch, the if/elif is non-exhaustive - we might skip all branches,
    -- so we can't guarantee any assignments. (Note: We don't attempt to analyze whether
    -- the predicates are exhaustive through logical analysis - that's undecidable in general
    -- and NP-complete even for boolean satisfiability. The else branch is our simple,
    -- syntactic criterion for exhaustiveness.)
    scanSuite seen (If _ branches elseBranch : rest)
                                        =     -- Scan all branches for assignments
                                          let branchResults = map (\(Branch _ body) ->
                                                  (scanSuite seen body, branchExitsEarly body)) branches
                                              elseResult = (scanSuite seen elseBranch, branchExitsEarly elseBranch)

                                              -- Branches that complete normally must be considered
                                              normalBranches = [assigns | (assigns, exits) <- branchResults, not exits]
                                              -- Else branch if it completes normally
                                              normalElse = case elseResult of
                                                             (assigns, False) -> [assigns]  -- Else completes normally
                                                             (_, True) -> []  -- Else exits early

                                              -- All normal branches that do not exit early
                                              allNormalBranches = normalBranches ++ normalElse

                                              -- if we have else, it's exhaustive, otherwise it's not
                                              newAssigns = case not (null elseBranch) of
                                                  False -> []  -- No else: not exhaustive, nothing counts
                                                  True -> case allNormalBranches of
                                                            [] -> []  -- All branches exit: no object returned
                                                            branches -> foldl1 intersect branches  -- Require intersection
                                          in newAssigns ++ scanSuite (newAssigns ++ seen) rest
    -- Only continue past simple statements
    scanSuite seen (Pass _ : rest)      = scanSuite seen rest
    -- Parent init calls are special - they initialize parent attributes
    scanSuite seen (Expr _ (Call _ (Dot _ (Var _ c) n) _ _) : rest)
        | isClass env c, n == initKW    = scanSuite seen rest
    -- Allow calling self methods that have NotImplemented body (C implementations)
    scanSuite seen (Expr _ (Call _ (Dot _ (Var _ (NoQ x)) methodName) _ _) : rest)
        | x == self && isNotImplMethod methodName
                                        = scanSuite seen rest  -- Continue: NotImplemented method on self
    -- Stop at other self method calls
    scanSuite seen (Expr _ (Call _ (Dot _ _ _) _ _) : _)
                                        = []  -- STOP: method call
    -- Allow expressions without references to `self`
    scanSuite seen (Expr _ e : rest)
        | checkNoSelfReference self seen e
                                        = scanSuite seen rest
    -- Handle try/except/else/finally
    --
    -- The possible execution paths are:
    --   1. try → else (no exception raised)
    --   2. handler (exception raised and caught)
    --   3. exception propagates (not caught - early exit, no object returned)
    -- Finally always executes regardless of path taken.
    --
    -- Since we don't know WHERE in try an exception might occur, we can't count ANY
    -- assignments from try when considering the exception path. But if try completes
    -- normally (no exception), we know ALL its assignments happened.
    --
    -- Note: else is a CONTINUATION of try (only runs if try completes without exception),
    -- not an alternative branch like in if/else.
    --
    -- Example: try:                  try:                try:
    --            self.x = 1             self.x = 1           raise Error()
    --            self.y = 2             raise Error()      except:
    --          except:               except:                self.x = 1
    --            self.x = 1             self.x = 1         else:
    --          else:                 else:                  self.y = 2
    --            self.z = 3             self.z = 3
    --
    --          Normal paths:         Normal paths:       Normal paths:
    --          - try+else: {x,y,z}   - except: {x}       - except: {x}
    --          - except: {x}         (try+else exits)    (try exits, else never runs)
    --          Result: {x}           Result: {x}         Result: {x}
    --
    -- If try always exits (raises), we only consider handlers. If a handler exits,
    -- it doesn't contribute to the intersection (like if/else branches that exit).
    -- Finally assignments always count since finally always executes.
    scanSuite seen (Try _ tryBody handlers elseBranch finallyBlock : rest)
                                        = let -- Finally always executes
                                              finallyAssigns = scanSuite seen finallyBlock
                                              seenAfterFinally = finallyAssigns ++ seen

                                              -- Scan all code paths
                                              tryAssigns = scanSuite seenAfterFinally tryBody
                                              elseAssigns = scanSuite seenAfterFinally elseBranch
                                              handlerAssigns = map (\(Handler _ hbody) ->
                                                  scanSuite seenAfterFinally hbody) handlers

                                              -- Check which paths exit early
                                              tryExits = branchExitsEarly tryBody
                                              elseExits = branchExitsEarly elseBranch
                                              handlerExits = map (\(Handler _ hbody) ->
                                                  branchExitsEarly hbody) handlers

                                              -- Build list of paths that complete normally:
                                              -- 1. try+else path (if try doesn't exit)
                                              -- 2. each handler that doesn't exit
                                              tryElsePath = if not tryExits
                                                           then [tryAssigns ++ if elseExits then [] else elseAssigns]
                                                           else []
                                              handlerPaths = [assigns | (assigns, exits) <- zip handlerAssigns handlerExits, not exits]
                                              normalPaths = tryElsePath ++ handlerPaths

                                              -- Intersect all normal paths (or empty if all exit)
                                              guaranteedFromPaths = case normalPaths of
                                                  [] -> []
                                                  paths -> foldl1 intersect paths

                                          in finallyAssigns ++ guaranteedFromPaths ++
                                             scanSuite (finallyAssigns ++ guaranteedFromPaths ++ seen) rest
    -- Handle loops - skip over them if they don't leak self references
    scanSuite seen (While _ cond body elseBranch : rest)
        | checkNoSelfReference self seen cond &&
          checkNoSelfReferenceInSuite self seen body &&
          checkNoSelfReferenceInSuite self seen elseBranch
                                        = scanSuite seen rest  -- Continue past safe loop
        | otherwise                     = []  -- STOP: loop references self
    scanSuite seen (For _ pat expr body elseBranch : rest)
        | checkNoSelfReference self seen expr &&
          checkNoSelfReferenceInSuite self seen body &&
          checkNoSelfReferenceInSuite self seen elseBranch
                                        = scanSuite seen rest  -- Continue past safe loop
        | otherwise                     = []  -- STOP: loop references self
    scanSuite _ (With _ _ _ : _)        = []  -- STOP: with statements not supported
    -- Handle assert like if & raise - continue if test doesn't reference self
    scanSuite seen (Assert _ test msg : rest)
        | checkNoSelfReference self seen test &&
          maybe True (checkNoSelfReference self seen) msg
                                        = scanSuite seen rest  -- Continue: assert doesn't leak self
        | otherwise                     = []  -- STOP: assert references self
    scanSuite _ (Return _ _ : _)        = []  -- STOP: early exit
    scanSuite _ (Raise _ _ : _)         = []  -- STOP: raises exception
    -- Skip past break and continue - they are loop control flow, and we skip loops anyway
    scanSuite seen (Break _ : rest)     = scanSuite seen rest
    scanSuite seen (Continue _ : rest)  = scanSuite seen rest
    scanSuite seen (Delete _ _ : rest)  = scanSuite seen rest
    scanSuite _ (After{} : _)           = []  -- STOP: after statement (async)
    -- Check nested function declarations for self references
    scanSuite seen (Decl _ decls : rest)
        | all (checkDeclNoSelfReference self seen) decls
                                        = scanSuite seen rest  -- Continue: nested functions don't capture self
        | otherwise                     = []  -- STOP: nested function captures self
    scanSuite _ (_ : _)                 = []  -- STOP: unhandled statement type (safe default)


-- Check if a suite (list of statements) contains disallowed references to self
checkNoSelfReferenceInSuite :: Name -> [Name] -> Suite -> Bool
checkNoSelfReferenceInSuite self seen stmts = all checkStmt stmts
  where
    checkStmt (Expr _ e)                = checkNoSelfReference self seen e
    checkStmt (Assign _ _ e)            = checkNoSelfReference self seen e
    checkStmt (MutAssign _ target e)    = checkNoSelfReference self seen e && checkNoSelfReference self seen target
    checkStmt (AugAssign _ target _ e)  = checkNoSelfReference self seen e && checkNoSelfReference self seen target
    checkStmt (Return _ Nothing)        = True
    checkStmt (Return _ (Just e))       = checkNoSelfReference self seen e
    checkStmt (If _ branches elseBranch) = all (\(Branch cond body) -> checkNoSelfReference self seen cond && checkNoSelfReferenceInSuite self seen body) branches &&
                                           checkNoSelfReferenceInSuite self seen elseBranch
    checkStmt (While _ cond body elseBranch) = checkNoSelfReference self seen cond &&
                                                checkNoSelfReferenceInSuite self seen body &&
                                                checkNoSelfReferenceInSuite self seen elseBranch
    checkStmt (For _ _ expr body elseBranch) = checkNoSelfReference self seen expr &&
                                                checkNoSelfReferenceInSuite self seen body &&
                                                checkNoSelfReferenceInSuite self seen elseBranch
    checkStmt (Try _ tryBody handlers elseBranch finallyBlock) =
                                           checkNoSelfReferenceInSuite self seen tryBody &&
                                           all (\(Handler _ hbody) -> checkNoSelfReferenceInSuite self seen hbody) handlers &&
                                           checkNoSelfReferenceInSuite self seen elseBranch &&
                                           checkNoSelfReferenceInSuite self seen finallyBlock
    checkStmt _                          = True  -- Conservative: allow other statements

-- Check if an expression contains disallowed references to self
-- We allow self.x since it can be seen as just a lone variable, which is valid
-- as long as it has been previously assigned, but we cannot pass a reference to
-- the whole `self`
checkNoSelfReference :: Name -> [Name] -> Expr -> Bool
checkNoSelfReference self seen expr = checkExpr expr
  where
    -- Check if an expression is allowed
    checkExpr :: Expr -> Bool
    checkExpr (Await _ _)               = False  -- Await is not allowed
    checkExpr (Var _ qn) | noq qn == self
                                        = False  -- Direct reference to self not allowed
    checkExpr e@(Dot _ (Var _ qn) attr)
        | noq qn == self                = attr `elem` seen  -- self.attr is OK only if attr is initialized
    -- For other expressions, recursively check subexpressions
    checkExpr (Call _ func args kwds)   = checkExpr func && checkPosArgs args && checkKwdArgs kwds
    checkExpr (BinOp _ e1 _ e2)         = checkExpr e1 && checkExpr e2
    checkExpr (CompOp _ e1 ops)         = checkExpr e1 && all checkOpArg ops
      where checkOpArg (OpArg _ e)      = checkExpr e
    checkExpr (UnOp _ _ e)              = checkExpr e
    checkExpr (List _ elems)            = all checkElem elems
    checkExpr (Tuple _ args kwds)       = checkPosArgs args && checkKwdArgs kwds
    checkExpr (Paren _ e)               = checkExpr e
    checkExpr (Cond _ cond thenE elseE) = checkExpr cond && checkExpr thenE && checkExpr elseE
    checkExpr (Index _ base idx)        = checkExpr base && checkExpr idx
    checkExpr (Slice _ base (Sliz _ start stop step))
                                        = checkExpr base &&
                                          maybe True checkExpr start &&
                                          maybe True checkExpr stop &&
                                          maybe True checkExpr step
    checkExpr (Dict _ items)            = all checkItem items
    checkExpr (Set _ elems)             = all checkElem elems
    checkExpr (ListComp _ elem comp)    = checkElem elem && checkComp comp
    checkExpr (DictComp _ (Assoc k v) comp)
                                        = checkExpr k && checkExpr v && checkComp comp
    checkExpr (SetComp _ elem comp)     = checkElem elem && checkComp comp
    checkExpr (GeneratorExpr _ elem comp)
                                        = checkElem elem && checkComp comp
    checkExpr (Lambda _ _ _ body _)     = checkExpr body
    checkExpr (Yield _ e)               = maybe True checkExpr e
    checkExpr (YieldFrom _ e)           = checkExpr e
    checkExpr (Dot _ e _)               = checkExpr e  -- Check base expression
    checkExpr (Int _ _ _)               = True  -- Integer literals are OK
    checkExpr (Float _ _ _)             = True  -- Float literals are OK
    checkExpr (Strings _ _)             = True  -- String literals are OK
    checkExpr (BStrings _ _)            = True  -- Byte string literals are OK
    checkExpr (Bool _ _)                = True  -- Boolean literals are OK
    checkExpr None{}                    = True  -- None is OK
    checkExpr _                         = True  -- Other expressions are OK (for now)

    checkPosArgs (PosArg e rest)        = checkExpr e && checkPosArgs rest
    checkPosArgs (PosStar e)            = checkExpr e
    checkPosArgs PosNil                 = True

    checkKwdArgs (KwdArg _ e rest)      = checkExpr e && checkKwdArgs rest
    checkKwdArgs (KwdStar e)            = checkExpr e
    checkKwdArgs KwdNil                 = True

    checkElem (Elem e)                  = checkExpr e
    checkElem (Star e)                  = checkExpr e

    checkItem (Assoc k v)               = checkExpr k && checkExpr v

    checkComp (CompFor _ _ iter c)      = checkExpr iter && checkComp c
    checkComp (CompIf _ test c)         = checkExpr test && checkComp c
    checkComp NoComp                    = True


-- Check if a declaration (nested function) references self
-- We conservatively stop if ANY nested function references self at all,
-- even if it only accesses already-initialized attributes
checkDeclNoSelfReference :: Name -> [Name] -> Decl -> Bool
checkDeclNoSelfReference self seen decl = case decl of
    Def _ _ _ _ _ _ body _ _ _ -> checkNoSelfReferenceInSuite self [] body  -- Pass empty list to disallow ANY self reference
    Actor _ _ _ _ _ body _     -> checkNoSelfReferenceInSuite self [] body  -- Pass empty list to disallow ANY self reference
    Class _ _ _ _ body _       -> True  -- Nested classes don't capture self by default
    Protocol _ _ _ _ body _    -> True  -- Nested protocols don't capture self
    Typedef _ _ _ _ _          -> True  -- Type aliases don't capture self
    Extension _ _ _ _ body _   -> True  -- Extensions don't capture self

-- Infer all class attributes by scanning the entire __init__ method for any
-- self.x assignments, regardless of control flow. This is used for attribute
-- discovery/inference, not for initialization checking.
inferClassAttributes :: Name -> Suite -> [Name]
inferClassAttributes self stmts = nub $ scanAll stmts
  where
    scanAll []                          = []
    -- Direct assignment to self.attribute
    scanAll (MutAssign _ (Dot _ (Var _ qn) n) _ : rest)
        | noq qn == self                = n : scanAll rest
    -- Scan inside control structures
    scanAll (If _ branches elseBranch : rest)
                                        = concatMap (\(Branch _ body) -> scanAll body) branches
                                          ++ scanAll elseBranch
                                          ++ scanAll rest
    scanAll (While _ _ body elseBranch : rest)
                                        = scanAll body ++ scanAll elseBranch ++ scanAll rest
    scanAll (For _ _ _ body elseBranch : rest)
                                        = scanAll body ++ scanAll elseBranch ++ scanAll rest
    scanAll (Try _ tryBody handlers elseBranch finallyBlock : rest)
                                        = scanAll tryBody ++
                                          concatMap (\(Handler _ hbody) -> scanAll hbody) handlers ++
                                          scanAll elseBranch ++
                                          scanAll finallyBlock ++
                                          scanAll rest
    -- TODO: uh, do what with "with"??
    scanAll (With _ witems body : rest) = scanAll body ++ scanAll rest
    scanAll (After{} : rest)            = scanAll rest  -- After has expressions, not suite
    -- Skip declarations - we don't scan nested function bodies
    scanAll (Decl _ _ : rest)           = scanAll rest
    -- Continue past other statements
    scanAll (_ : rest)                  = scanAll rest

-- Get all parent classes whose __init__ methods are called
getCalledParentInits :: Env -> Suite -> [QName]
getCalledParentInits _ []               = []
getCalledParentInits env (Expr _ (Call _ (Dot _ (Var _ c) n) _ _) : rest)
    | isClass env c, n == initKW        = c : getCalledParentInits env rest
getCalledParentInits env (_ : rest)     = getCalledParentInits env rest

-- Get attributes that would be initialized by calling a parent's __init__
-- This assumes the parent's __init__ properly initializes all attributes it's
-- responsible for - we can rely on this since the parent's __init__ in turn
-- will be checked
getInitializedByParent :: Env -> QName -> [Name]
getInitializedByParent env qn           = -- When calling ParentClass.__init__(self), we assume it initializes:
                                          -- 1. All attributes declared in ParentClass
                                          -- 2. All attributes ParentClass inherited (since it should call its parent's __init__)
                                          let (_,ancestors,te) = findConName qn env
                                              inherited = concatMap (getPropertiesFromClass env . tcname . snd) ancestors
                                              declared = [ n | (n, NSig _ Property _) <- te ]
                                          in nub $ inherited ++ declared
  where getPropertiesFromClass env qn   = let (_,_,te) = findConName qn env
                                          in [ n | (n, NSig _ Property _) <- te ]



infProperties env as b
  | Just (self,ss) <- inits             = forM newProps $ \n -> do
                                             t <- newUnivarOfKind KType env
                                             return (n, NSig (monotype t) Property Nothing)
  | otherwise                           = return []
  where inherited                       = concat $ map (conAttrs env . tcname . snd) as
        explicit                        = concat [ ns | Signature _ ns sc dec <- b, isProp dec sc ]
        inits                           = case findInitMethod b of
                                              Just (self, body, _) -> Just (self, body)
                                              Nothing -> Nothing
        assigned                        = maybe [] (\(self,ss) -> inferClassAttributes self ss) inits
        newProps                        = assigned \\ (inherited ++ explicit)


infDefBody env n p@(PosPar x _ _ _) k b
  | inClass env && n == initKW          = infInitEnv env' x b
  where env'                            = withDefaultLocalNames (bound (p,k) ++ assigned b) $ setInDef env
infDefBody env n p k@(KwdPar x _ _ _) b
  | inClass env && n == initKW          = infInitEnv env' x b
  where env'                            = withDefaultLocalNames (bound (p,k) ++ assigned b) $ setInDef env
infDefBody env _ p k b                  = infSuiteEnv env' b
  where env'                            = withDefaultLocalNames (bound (p,k) ++ assigned b) $ setInDef env

infInitEnv env self (MutAssign l (Dot l' e1@(Var _ (NoQ x)) n) e2 : b)
  | x == self                           = do (cs1,t1,e1') <- infer env e1
                                             t2 <- newUnivar env
                                             (cs2,e2') <- inferSub env t2 e2
                                             (cs3,te,b') <- infInitEnv env self b
                                             return (Mut (locinfo l 64) env t1 n t2 :
                                                     cs1++cs2++cs3, te, MutAssign l (Dot l' e1' n) e2' : b')
infInitEnv env self (Expr l e : b)
  | Call{fun=Dot _ (Var _ c) n} <- e,
    isClass env c, n == initKW          = do (cs1,_,e') <- infer env e
                                             (cs2,te,b') <- infInitEnv env self b
                                             return (cs1++cs2, te, Expr l e' : b')
infInitEnv env self b                   = infSuiteEnv env b

abstractDefs env q b                    = qsigs ++ map absDef b
  where qsigs                           = [ Signature NoLoc [n] (monotype $ proto2type (tVar v) p) Property | (v,p) <- quals env q, let n = tvarWit v p ]
        absDef (Decl l ds)              = Decl l (map absDef' ds)
        absDef (If l bs els)            = If l [ Branch e (map absDef ss) | Branch e ss <- bs ] (map absDef els)
        absDef stmt                     = stmt
        absDef' d@Def{}
          | deco d == Static            = d{ pos = pos1 }
          | dname d == initKW           = d{ pos = pos1, dbody = qcopies ++ dbody d }
          | otherwise                   = d{ dbody = bindWits qcopies' ++ dbody d }
          where nSelf                   = case pos d of PosPar nSelf _ _ _ -> nSelf
                pos1                    = case pos d of
                                            PosPar nSelf t e p | deco d /= Static ->
                                                PosPar nSelf t e $ qualWPar env q p
                                            p -> qualWPar env q p
                qcopies                 = [ MutAssign NoLoc (eDot (eVar nSelf) n) (eVar n) | (v,p) <- quals env q, let n = tvarWit v p ]
                qcopies'                = [ mkEqn env n (proto2type (tVar v) p) (eDot (eVar nSelf) n) | (v,p) <- quals env q, let n = tvarWit v p ]
        absDef' d                       = d


instance Check Decl where
    checkEnv env (Def l n q p k a b dec fx ddoc)
                                        = do --traceM ("## checkEnv def " ++ prstr n ++ " FX " ++ prstr fx')
                                             checkIndependentDefaults env p k
                                             t <- maybe (newUnivar env) return a
                                             pushFX fx' t
                                             st <- newUnivar env
                                             wellformed env1 q
                                             wellformed env1 a
                                             when (inClass env) $
                                                 tryUnify env (Simple l "Type of first parameter of class method does not unify with Self") tSelf $ selfType p k dec
                                             (csp,te0,p') <- infEnv env1 p
                                             (csk,te1,k') <- infEnv (define te0 env1) k
                                             (csb,_,b') <- infDefBody (define te1 (define te0 env1)) n p' k' b
                                             popFX
                                             let cst = if fallsthru b then [Cast (locinfo l 65) env1 tNone t] else []
                                                 t1 = tFun fx' (prowOf p') (krowOf k') t
                                             (cs0,eq1) <- oldSimplify env1 (tempGoal t1) (csp++csk++csb++cst)
                                             -- At this point, n has the type given by its def annotations.
                                             -- Now check that this type is no less general than its recursion assumption in env.
                                             let body = bindWits eq1 ++ b'
                                                 p'' = inlineDefaultsP eq1 p'
                                                 k'' = inlineDefaultsK eq1 k'
                                             (cs1,def) <- matchDefAssumption env cs0 (Def l n q p'' k'' (Just t) body dec fx' ddoc)
                                             return (cs1, def)
      where env1                        = reserve (bound (p,k) ++ assigned b \\ stateScope env) $ tydefineVars q env
            fx'                         = fxUnwrap env fx

    checkEnv env (Actor l n q p k b ddoc)
                                        = do --traceM ("## checkEnv actor " ++ prstr n)
                                             checkIndependentDefaults (withDefaultLocalNames (assigned b) env) p k
                                             pushFX fxProc tNone
                                             wellformed env1 q
                                             (csp,te1,p') <- infEnv env1 p
                                             (csk,te2,k') <- infEnv (define te1 env1) k
                                             (csb,te,b') <- infSuiteEnv (define te2 $ define te1 env1) b
                                             -- At this point, each name defined in b has the type given by its annotations
                                             -- and possible type signatures. Now check that these types are no less general
                                             -- than the recursion assumption on actor n itself (which is distinct from any
                                             -- direct assumptions on its methods because actor interfaces are sealed).
                                             (cs0,eq0) <- matchActorAssumption env1 n p' k' te
                                             popFX
                                             (cs1,eq1) <- markScoped env n q te (csp++csk++csb++cs0)
                                             let body = bindWits (eq1++eq0) ++ b'
                                                 p'' = inlineDefaultsP (eq1++eq0) p'
                                                 k'' = inlineDefaultsK (eq1++eq0) k'
                                                 act = Actor l n (noqual env q) (qualWPar env q p'') k'' body ddoc
                                             return (cs1, act)
      where env1                        = withDefaultLocalNames (bound (p,k) ++ assigned b) $
                                          reserve (bound (p,k) ++ assigned b) $ setInAct $
                                          define [(selfKW, NVar (tCon tc))] $ tydefineVars q env
            tc                          = TC (NoQ n) (map tVar $ qbound q)

    checkEnv env (Typedef l n q t ddoc)
                                        = do wellformed env1 q
                                             wellformed env1 t
                                             return ([], Typedef l n (noqual env q) t ddoc)
      where env1                        = tydefineVars q env

    checkEnv' env (Class l n q us b ddoc)
                                        = do --traceM ("## checkEnv class " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             wellformed env1 us
                                             (csb,b') <- checkEnv (define te' env1) b
                                             popFX
                                             (cs1,eq1) <- markScoped env n q' te csb
                                             return (cs1, [Class l n (noqual env q) (map snd as) (bindWits eq1 ++ abstractDefs env q b') ddoc])
      where env1                        = withDefaultLocalNames (dom te') $ tydefineVars q' $ setInClass env
            NClass _ as te _            = findName n env
            te'                         = selfSubst n' q te
            q'                          = selfQuant n' q
            n'                          = NoQ n

    checkEnv' env (Protocol l n q us b ddoc)
                                        = do --traceM ("## checkEnv protocol " ++ prstr n)
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             (csu,wmap) <- wellformedProtos env1 us
                                             (csb,b') <- checkEnv (define te env1) b
                                             popFX
                                             (cs1,eq1) <- markScoped env n q' te (csu++csb)
                                             b' <- usubst b'
                                             return (cs1, convProtocol env n q ps eq1 wmap b')
      where env1                        = withDefaultLocalNames (dom te) $ tydefineVars q' $ setInClass env
            NProto _ ps te _            = findName n env
            te'                         = selfSubst n' q te
            q'                          = selfQuant n' q
            n'                          = NoQ n

    checkEnv' env (Extension l q c us b ddoc)
      | isActor env n                   = notYet (loc n) "Extension of an actor"
      | isProto env n                   = notYet (loc n) "Extension of a protocol"
      | otherwise                       = do --traceM ("## checkEnv extension " ++ prstr n' ++ "(" ++ prstrs us ++ ")")
                                             pushFX fxPure tNone
                                             wellformed env1 q
                                             (csu,wmap) <- wellformedProtos env1 us
                                             (csb,b') <- checkEnv (define te' env1) b
                                             popFX
                                             (cs1,eq1) <- markScoped env n' q' te (csu++csb)
                                             b' <- usubst b'
                                             return (cs1, convExtension env n' c q ps eq1 wmap b' [])
      where env1                        = withDefaultLocalNames (dom te') $ tydefineInst c ps thisKW' $ tydefineVars q' $ setInClass env
            n                           = tcname c
            n'                          = extensionName us c
            NExt _ _ ps te _ _          = findName n' env
            te'                         = selfSubst n q te
            q'                          = selfQuant n q
            tc                          = TC n (map tVar $ qbound q)

    checkEnv' env x                     = do (cs,x') <- checkEnv env x
                                             return (cs, [x'])

instance Check Stmt where
    checkEnv env (If l bs els)          = do (cs1,bs') <- checkEnv env bs
                                             (cs2,els') <- checkEnv env els
                                             return (cs1++cs2, If l bs' els')
    checkEnv env (Decl l ds)            = do (cs,ds') <- checkEnv env ds
                                             return (cs, Decl l ds')
    checkEnv env (Signature l ns sc dec)
                                        = do wellformed env1 q
                                             wellformed env1 t
                                             return ([], Signature l ns sc' dec')
      where TSchema l q t               = sc
            sc' | null q                = sc
                | otherwise             = let TFun l' x p k t' = t in TSchema l (noqual env q) (TFun l' x (qualWRow env q p) k t')
            dec'                        = if inClass env && isProp dec sc then Property else dec
            env1                        = tydefineVars q env
    checkEnv env s                      = return ([], s)

instance Check Branch where
    checkEnv env (Branch e b)           = do (cs,b') <- checkEnv env b
                                             return (cs, Branch e b')

--------------------------------------------------------------------------------------------------------------------------



-- Defaults are expanded at call sites, where parameters and enclosing lexical
-- bindings of the callee are not in scope.  Supporting either kind of
-- dependency requires a provider/closure design; reject them for this first
-- stage instead of allowing a later pass to resolve them in the wrong scope.
checkIndependentDefaults env p k        = checkP [] p
  where checkP seen (PosPar n _ d rest) = check seen d >> checkP (n:seen) rest
        checkP seen (PosSTAR n _)        = checkK (n:seen) k
        checkP seen PosNIL               = checkK seen k

        checkK seen (KwdPar n _ d rest) = check seen d >> checkK (n:seen) rest
        checkK seen (KwdSTAR _ _)        = return ()
        checkK seen KwdNIL               = return ()

        check _ Nothing                  = return ()
        check seen (Just e)
          | n:_ <- free e `intersect` seen
                                            = err (loc e) ("Default value may not depend on parameter " ++ prstr n)
          | n:_ <- free e `intersect` defaultLocalNames env
                                            = err (loc e) ("Default value may not capture enclosing name " ++ prstr n)
          | not (liftableDefault env e)     = err (loc e) "Default value must be a literal, constructor expression, or module-level name"
          | otherwise                       = return ()

-- A default is copied, verbatim in source terms, from its definition into
-- each call that omits the parameter.  Keep that operation easy to explain:
-- permit literals (including recursively literal containers), references to
-- stable module-level values, and constructor applications whose arguments
-- are themselves liftable.  In particular, an arbitrary function or method
-- call is not made into an implicit call-site computation.
liftableDefault env                       = lift
  where lift Int{}                        = True
        lift Float{}                      = True
        lift Bool{}                       = True
        lift None{}                       = True
        lift Strings{}                    = True
        lift BStrings{}                   = True
        lift e@Var{}                      = moduleValue e
        lift (Paren _ e)                  = lift e
        lift (UnOp _ op e)                = op `elem` [UPlus, UMinus, BNot] && lift e
        lift (Tuple _ p k)                = liftPos p && liftKwd k
        lift (List _ es)                  = all liftElem es
        lift (Dict _ as)                  = all liftAssoc as
        lift (Set _ es)                   = all liftElem es
        lift (Call _ f p k)               = constructor f && liftPos p && liftKwd k
        lift _                             = False

        liftPos (PosArg e p)              = lift e && liftPos p
        liftPos (PosStar e)               = lift e
        liftPos PosNil                    = True
        liftKwd (KwdArg _ e k)            = lift e && liftKwd k
        liftKwd (KwdStar e)               = lift e
        liftKwd KwdNil                    = True
        liftElem (Elem e)                 = lift e
        liftElem (Star e)                 = lift e
        liftAssoc (Assoc k v)             = lift k && lift v
        liftAssoc (StarStar e)            = lift e

        constructor (TApp _ f _)          = constructor f
        constructor e                     = maybe False (\q -> isClass env q || isActor env q) (exprQName e)

        moduleValue e                     = case exprQName e >>= (`tryQName` env) of
                                                 Just NVar{} -> True
                                                 Just NDef{} -> True
                                                 _           -> False

        exprQName (Var _ q)               = Just q
        exprQName (Dot _ prefix n)         = do m <- isModName env prefix
                                                return (QName m n)
        exprQName _                       = Nothing

noDefaultsP (PosPar n t _ p)            = PosPar n t Nothing (noDefaultsP p)
noDefaultsP k                           = k

noDefaultsK (KwdPar n t _ k)            = KwdPar n t Nothing (noDefaultsK k)
noDefaultsK k                           = k


--------------------------------------------------------------------------------------------------------------------------

instance InfEnv Branch where
    infEnv env (Branch e b)             = do (cs1,env',s,_,e') <- inferTest env e
                                             (cs2,te,b') <- infEnv env' b
                                             return (cs1++cs2, te, Branch e' (termsubst s b'))

instance InfEnv WithItem where
    infEnv env (WithItem e Nothing)     = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Proto (locinfo2  66 e) env w t pContextManager :
                                                     cs, [], WithItem e' Nothing)           -- TODO: translate using w
    infEnv env (WithItem e (Just p))    = do (cs1,t1,e') <- infer env e
                                             (te,t2,p') <- infEnvT env p
                                             w <- newWitness
                                             return (Cast (locinfo2 67 e) env t1 t2 :
                                                     Proto (locinfo2 68 e) env w t1 pContextManager :
                                                     cs1, te, WithItem e' (Just p'))         -- TODO: translate using w

instance InfEnv Handler where
    infEnv env (Handler ex b)           = do (cs1,te,ex') <- infEnv env ex
                                             (cs2,te1,b') <- infEnv (define te env) b
                                             return (cs1++cs2, exclude te1 (dom te), Handler ex' b')

instance InfEnv Except where
    infEnv env (ExceptAll l)            = return ([], [], ExceptAll l)
    infEnv env (Except l x)             = return ([Cast (locinfo l 69) env t tException], [], Except l x)
      where t                           = tCon (TC (unalias env x) [])
    infEnv env (ExceptAs l x n)         = return ([Cast (locinfo l 70) env t tException], [(n, NVar t)], ExceptAs l x n)
      where t                           = tCon (TC (unalias env x) [])

instance Infer Expr where
    infer env x@(Var l n)               = case findQName n env of
                                            NVar t -> return ([], t, x)
                                            NSVar t -> do
                                                fx <- currFX
                                                return ([Cast info env fxProc fx], t, x)
                                              where info = Simple l ("State variable may only be accessed in a proc")
                                            NDef sc d _ -> do
                                                (cs,tvs,t) <- instantiateDefaults env sc
                                                let e = app t (tApp x tvs) $ protoWitsOf cs
                                                    cs1 = map (addTyping env n sc t) cs
                                                --traceM ("## type of " ++ prstr n ++ " = " ++ prstr t ++ ", cs = " ++ render(commaList cs))
                                                if actorSelf env
                                                    then wrapped l attrWrap env cs1 [tActor,t] [eVar selfKW,e]
                                                    else return (cs1, t, e)
                                            NClass q _ _ _ -> do
                                                (cs0,ts) <- instQBinds env q
                                                --traceM ("## Instantiating " ++ prstr n)
                                                let ns = abstractAttrs env n
                                                when (not $ null ns) (err3 (loc n) ns "Abstract attributes prevent instantiation:")
                                                case findAttr env (TC n ts) initKW of
                                                    Just (_,sc,_) -> do
                                                        (cs1,tvs,t) <- instantiateDefaults env sc
                                                        let t0 = tCon $ TC (unalias env n) ts
                                                            t' = vsubst [(tvSelf,t0)] t{ restype = tSelf }
                                                        return (cs0++cs1, t', app t' (tApp x (ts++tvs)) $ protoWitsOf (cs0++cs1))
                                            NAct q p k _ _ -> do
--                                                when (abstractActor env n) (err1 n "Abstract actor cannot be instantiated:")
                                                (cs,tvs,t) <- instantiateDefaults env (tSchema q (tFun fxProc p k (tCon0 (unalias env n) q)))
                                                return (cs, t, app t (tApp x tvs) $ protoWitsOf cs)
                                            NSig _ _ _ -> nameReserved n
                                            NReserved -> nameReserved n
                                            _ -> nameUnexpected n

    infer env e@(Int _ val s)
       | val < (-9223372036854775808)   = return ([], tBigint, e) -- below i64 range ⇒ bigint
       | val > 18446744073709551615     = return ([], tBigint, e) -- above u64 range ⇒ bigint
       | val > 9223372036854775807      = return ([], tU64, e)    -- between i64 max and u64 max ⇒ u64
       | otherwise                      = do t <- newUnivar env
                                             w <- newWitness
                                             return ([Proto (locinfo2 72 e) env w t pNumber], t, eCall (eDot (eVar w) fromatomKW) [e])
    infer env e@(Float _ val s)         = do t <- newUnivar env
                                             w <- newWitness
                                             return ([Proto (locinfo2 73 e) env w t pRealFloat], t, eCall (eDot (eVar w) fromatomKW) [e])
    infer env e@Imaginary{}             = notYetExpr e
    infer env e@(Bool _ val)            = return ([], tBool, e)
    infer env e@(None _)                = return ([], tNone, e)
    infer env e@(NotImplemented _)      = notYetExpr e
    infer env e@(Ellipsis _)            = notYetExpr e
    infer env e@(Strings _ ss)          = return ([], tStr, e)
    infer env e@(BStrings _ ss)         = return ([], tBytes, e)
    infer env (Call l e ps ks)          = inferCall env True l e ps ks
    infer env (TApp l e ts)             = internal l "Unexpected TApp in infer"
    infer env (Let l ss e)              = do (cs1, te, ss') <- infEnv env ss
                                             (cs2, t, e') <- infer (define te env) e
                                             return (cs1++cs2, t, Let l ss' e')
    infer env (Async l e)               = do (cs,t,e) <- infer env e                        -- expect an action returning t'
                                             prow <- newUnivarOfKind PRow env
                                             krow <- newUnivarOfKind KRow env
                                             t' <- newUnivar env
                                             let tf fx = tFun fx prow krow
                                             return (Cast (locinfo2 74 e) env t (tf fxAction t') :
                                                     cs, tf fxProc (tMsg t'), Async l e)    -- produce a proc returning Msg[t']
    infer env (Await l e)               = do t0 <- newUnivar env
                                             (cs1,e') <- inferSub env (tMsg t0) e
                                             fx <- currFX
                                             return (Cast (locinfo2 75 e) env fxProc fx :
                                                     cs1, t0, Await l e')
    infer env (Index l e ix)            = do (cs2,t,e') <- infer env e
                                             ti <- newUnivar env
                                             (cs1,ix') <- inferSub env ti ix
                                             t0 <- newUnivar env
                                             w <- newWitness
                                             return (Proto (locinfo2 76 e) env w t (pIIndexed ti t0) :
                                                     cs1++cs2, t0, eCall (eDot (eVar w) getitemKW) [e', ix'])
    infer env (Slice l e sl)            = do (cs1,sl') <- inferSlice env sl
                                             (cs2,t,e') <- infer env e
                                             t0 <- newUnivar env
                                             w <- newWitness
                                             return (Proto (locinfo2 77 e) env w t (pISliceable t0) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) getsliceKW) [e', sliz2exp sl'])
    infer env (Cond l e1 e e2)          = do t0 <- newUnivar env
                                             (cs0,env',s,_,e') <- inferTest env e
                                             (cs1,e1') <- inferSub env' t0 e1
                                             (cs2,e2') <- inferSub env t0 e2
                                             return (cs0++cs1++cs2, t0, Cond l (termsubst s e1') e' e2')
    infer env (IsInstance l e c)        = case findQName c env of
                                             NClass q _ _ _ -> do
                                                (cs,t,e') <- infer env e
                                                ts <- newUnivars env [ tvkind v | v <- qbound q ]
                                                return (cs, tBool, IsInstance l e' c)
                                             _ -> nameUnexpected c
    infer env (BinOp l s@Strings{} Mod e)
      | TRow _ _ _ t _ TNil{} <- prow   = do (cs,e') <- inferSub env t e
                                             return (cs, tStr, eCall formatF [s,eTuple [e']])
      | otherwise                       = do (cs,e') <- inferSub env tup e
                                             return (cs, tStr, eCall formatF [s,e'])
      where formatF                     = tApp (eQVar primFORMAT) [prow]
            tup                         = tTuple prow kwdNil
            prow                        = format $ concat $ sval s
            format []                   = posNil
            format ('%':s)              = nokey s
            format (c:s)                = format s
            nokey ('(':s)               = err l ("Mapping keys not supported in format strings")
            nokey s                     = flags s
            flags (f:s)
              | f `elem` "#0- +"        = flags s
            flags s                     = width s
            width ('*':s)               = posRow tInt (dot s)
            width (n:s)
              | n `elem` "123456789"    = dot (dropWhile (`elem` "0123456789") s)
            width s                     = dot s
            dot ('.':s)                 = prec s
            dot s                       = len s
            prec ('*':s)                = posRow tInt (len s)
            prec (n:s)
              | n `elem` "0123456789"   = len (dropWhile (`elem` "0123456789") s)
            prec s                      = len s
            len (l:s)
              | l `elem` "hlL"          = conv s
            len s                       = conv s
            conv (t:s)
              | t `elem` "diouxXc"      = posRow tInt (format s)
              | t `elem` "eEfFgG"       = posRow tFloat (format s)
              | t `elem` "rsa"          = posRow tStr (format s)
              | t == '%'                = format s
            conv (c:s)                  = err l ("Bad conversion character: " ++ [c])
            conv []                     = err l ("Bad conversion string")
    infer env e@(BinOp l e1 op e2)
      | op `elem` [Or,And]              = do (cs,_,_,t,e') <- inferTest env e
                                             return (cs, t, e')
      | op == Mult                      = do t <- newUnivar env
                                             t' <- newUnivar env
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t' e2
                                             w <- newWitness
                                             return (Proto (locinfo' l 79 e) env w t (pTimes t') :
                                                     cs1++cs2, t, eCall (eDot (eVar w) mulKW) [e1',e2'])
      | op == Div                       = do t <- newUnivar env
                                             t' <- newUnivar env
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t e2
                                             w <- newWitness
                                             return (Proto (locinfo' l 80 e) env w t (pDiv t') :
                                                     cs1++cs2, t', eCall (eDot (eVar w) truedivKW) [e1',e2'])
      | otherwise                       = do t <- newUnivar env
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env (rtype op t) e2
                                             w <- newWitness
                                             return (Proto (locinfo2 81 e) env w t (protocol op) :
                                                     cs1++cs2, t, eCall (eDot (eVar w) (method op)) [e1',e2'])
      where protocol Plus               = pPlus
            protocol Minus              = pMinus
            protocol Pow                = pNumber
            protocol Mod                = pIntegral
            protocol EuDiv              = pIntegral
            protocol ShiftL             = pIntegral
            protocol ShiftR             = pIntegral
            protocol BOr                = pLogical
            protocol BXor               = pLogical
            protocol BAnd               = pLogical
            method Plus                 = addKW
            method Minus                = subKW
            method Pow                  = powKW
            method Mod                  = modKW
            method EuDiv                = floordivKW
            method ShiftL               = lshiftKW
            method ShiftR               = rshiftKW
            method BOr                  = orKW
            method BXor                 = xorKW
            method BAnd                 = andKW
            rtype ShiftL t              = tInt
            rtype ShiftR t              = tInt
            rtype _ t                   = t
    infer env (UnOp l op e)
      | op == Not                       = do (cs,_,_,_,e') <- inferTest env e
                                             return (cs, tBool, UnOp l op e')
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             return (Proto (locinfo2 82 e) env w t (protocol op) :
                                                     cs, t, eCall (eDot (eVar w) (method op)) [e'])
      where protocol UPlus              = pNumber
            protocol UMinus             = pNumber
            protocol BNot               = pIntegral
            method UPlus                = posKW
            method UMinus               = negKW
            method BNot                 = invertKW
    infer env e@(CompOp l e1 [OpArg op e2])
      | op `elem` [In,NotIn]            = do t1 <- newUnivar env
                                             (cs1,e1') <- inferSub env t1 e1
                                             t2 <- newUnivar env
                                             (cs2,e2') <- inferSub env t2 e2
                                             w <- newWitness
                                             return (Proto (locinfo2 83 e) env w t2 (pContainer t1) :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w) (method op)) [e2', e1'])
      | op `elem` [Is,IsNot], e2==eNone = do (cs,_,_,t,e') <- inferTest env e
                                             return (cs, t, e')
      | otherwise                       = do t <- newUnivar env
                                             (cs1,e1') <- inferSub env t e1
                                             (cs2,e2') <- inferSub env t e2
                                             w <- newWitness
                                             return (Proto (locinfo' l 84 e) env w t (protocol op) :
                                                     cs1++cs2, tBool, eCall (eDot (eVar w) (method op)) [e1',e2'])
                                             -- TODO: This gives misleading error msg; it says that "e1 op e2 must implement protocol op"
      where protocol Eq                 = pEq
            protocol NEq                = pEq
            protocol LtGt               = pEq
            protocol Lt                 = pOrd
            protocol Gt                 = pOrd
            protocol LE                 = pOrd
            protocol GE                 = pOrd
            protocol Is                 = pIdentity
            protocol IsNot              = pIdentity
            method Eq                   = eqKW
            method NEq                  = neKW
            method LtGt                 = neKW
            method Lt                   = ltKW
            method Gt                   = gtKW
            method LE                   = leKW
            method GE                   = geKW
            method Is                   = isKW
            method IsNot                = isnotKW
            method In                   = containsKW
            method NotIn                = containsnotKW
    infer env (CompOp l e1 ops)         = notYet l "Comparison chaining"

    infer env (Dot l x@(Var _ c) n)
      | NClass q us te _ <- cinfo       = do (cs0,ts) <- instQBinds env q
                                             let tc = TC c' ts
                                             case findAttr env tc n of
                                                Just (_,sc,dec)
                                                  | dec == Just Property -> err l "Property attribute not selectable by class"
                                                  | abstractAttr env tc n -> err l "Abstract attribute not selectable by class"
                                                  | otherwise -> do
                                                      (cs1,tvs,t) <- instantiateDefaults env sc
                                                      let t' = vsubst [(tvSelf,tCon tc)] $ addSelf t dec
                                                          csq = if dec == Just Static || n == initKW then cs0 else []
                                                      return (csq++cs1, t', app2nd dec t' (tApp (Dot l x n) (ts++tvs)) $ protoWitsOf (csq++cs1))
                                                Nothing ->
                                                    case findProtoByAttr env c' n of
                                                        Just p -> do
                                                            p <- instwildcon env p
                                                            we <- eVar <$> newWitness
                                                            let Just (wf,sc,dec) = findAttr env p n
                                                            (cs2,tvs,t) <- instantiateDefaults env sc
                                                            let t' = vsubst [(tvSelf,tCon tc)] $ addSelf t dec
                                                            return (cs2, t', app t' (tApp (eDot (wf we) n) tvs) $ protoWitsOf cs2)
                                                        Nothing -> err1 l "Attribute not found"
      | NProto q us te _ <- cinfo       = do (_,ts) <- instQBinds env q
                                             let tc = TC c' ts
                                             case findAttr env tc n of
                                                Just (wf,sc,dec) -> do
                                                    (cs1,tvs,t) <- instantiateDefaults env sc
                                                    t0 <- newUnivar env
                                                    let t' = vsubst [(tvSelf,t0)] $ addSelf t dec
                                                    w <- newWitness
                                                    return (Proto (locinfo l 85) env w t0 tc :
                                                            cs1, t', app t' (tApp (Dot l (wf $ eVar w) n) tvs) $ protoWitsOf cs1)
                                                Nothing -> err1 l "Attribute not found"
      where c'                          = unalias env c
            cinfo                       = findQName c' env

    infer env (Dot l e n)
      | n == initKW                     = err1 n "__init__ cannot be selected by instance"
      | otherwise                       = do (cs,t,e') <- infer env e
                                             w <- newWitness
                                             t0 <- newUnivar env
                                             let con = case t of
                                                          TOpt _ _ -> Sel info env w t n t0
                                                          _ ->  Sel (locinfo' l 86 e) env w t n t0
                                                 info = Simple l (Pretty.print t ++ " does not have an attribute "++ Pretty.print n ++
                                                                  "\nHint: you may need to test if " ++ Pretty.print e ++ " is not None")
                                             return  (con : cs, t0, eCall (eVar w) [e'])

                                         -- The parser inserts Opt nodes only within OptChain nodes (which each contains exactly one Opt node).
                                         -- The Opt nodes are handled and eliminated by infer on an OptChain node.
                                         -- e is an atomic expression (atom_expr in the parser) which contains exactly one ? in its sequence of trailers
    infer env (OptChain l e)           = do x <- newTmp                                                                                      -- Example: a s1 s2 ? t1 t2
                                            let (b,e1,e2) = split x e                                                                          -- e1 = a s1 s2, e2 = x t1 t2
                                            te <- newUnivar env
                                            (cs1,e1') <- inferSub env (tOpt te) e1
                                            let env1 = define [(x,NVar te)] env
                                            (cs2,t,e2') <- infer env1 e2
                                            y <- newTmp
                                            (cs3,t',e3) <- alt t b
                                            return (cs1++cs2++cs3,
                                                     t', eLet [sAssign (pVar y (tOpt te)) e1']
                                                                           (eCond (termsubst [(x,eCAST (tOpt te) te (eVar y))] e2')
                                                                                  (eCall (tApp (eQVar primISNOTNONE) [te]) [eVar y])
                                                                                  e3))
      where split x (Opt l e b)         = (b, e, eVar x)
            split x (Dot l e n)         = (b, e1,Dot l e2 n) where (b,e1,e2) = split x e
            split x (DotI l e i)        = (b, e1,DotI l e2 i) where (b,e1,e2) = split x e
            split x (Rest l e n)        = (b, e1,Rest l e2 n) where (b,e1,e2) = split x e
            split x (RestI l e i)       = (b, e1,RestI l e2 i) where (b,e1,e2) = split x e
            split x (Call l f ps ks)    = (b, e1,Call l e2 ps ks) where (b,e1,e2) = split x f
            split x (Index l e ix)      = (b, e1,Index l e2 ix) where (b,e1,e2) = split x e
            split x (Slice l e sz)      = (b, e1,Slice l e2 sz) where (b,e1,e2) = split x e
            alt t True                  = do w <- newWitness
                                             w1 <- newWitness
                                             t1 <- newUnivar env
                                             return ([Sub (noinfo 444) env w tNone t1, Sub (noinfo 555) env w1 t t1],t1,eNone)
            alt t False                 = return ([],t,eCall (tApp (eQVar primRaiseValueError) [t]) [Strings NoLoc ["Forced unwrapping applied to None"]] )

    infer env e@(Rest _ _ _)            = notYetExpr e
--    infer env (Rest l e n)              = do p <- newUnivarOfKind PRow env
--                                             k <- newUnivarOfKind KRow env
--                                             t0 <- newUnivar env
--                                             (cs,e') <- inferSub env (tTuple p (kwdRow n t0 k)) e
--                                             return (cs, tTuple p k, Rest l e' n)

    infer env (DotI l e i)              = do (tup,ti,_) <- tupleTemplate env i
                                             (cs,e') <- inferSub env tup e
                                             return (cs, ti, DotI l e' i)

    infer env e@(RestI _ _ _)           = notYetExpr e
--    infer env (RestI l e i)             = do (tup,_,rest) <- tupleTemplate env i
--                                             (cs,e') <- inferSub env tup e
--                                             return (cs, rest, RestI l e' i)

    infer env (Lambda l p k e fx)
      | nodup (p,k)                     = do checkIndependentDefaults env p k
                                             pushFX fx tNone
                                             (cs0,te0,p') <- infEnv env1 p
                                             (cs1,te1,k') <- infEnv (define te0 env1) k
                                             let env2 = define te1 $ define te0 env1
                                             (cs2,t,e') <- case e of
                                                             Call l' e' ps ks -> inferCall env2 False l' e' ps ks
                                                             _ -> infer env2 e
                                             popFX
                                             return (cs0++cs1++cs2, tFun fx (prowOf p') (krowOf k') t, Lambda l (noDefaultsP p') (noDefaultsK k') e' fx)
                                                     -- TODO: replace defaulted params with Conds
      where env1                        = reserve (bound (p,k)) env
    infer env e@Yield{}                 = notYetExpr e
    infer env e@YieldFrom{}             = notYetExpr e
    infer env (Tuple l pargs kargs)     = do (cs1,prow,pargs') <- infer env pargs
                                             (cs2,krow,kargs') <- infer env kargs
                                             return (cs1++cs2, TTuple l prow krow, Tuple l pargs' kargs')
    infer env (List l es)               = do t0 <- newUnivar env
                                             (cs,es') <- infElems env es t0
                                             return (cs, tList t0, List l es')
    infer env (ListComp l e co)
      | nodup co                        = do (cs1,env',s,co') <- infComp env co
                                             t0 <- newUnivar env
                                             (cs2,es) <- infElems env' [e] t0
                                             let [e'] = es
                                             return (cs1++cs2, tList t0, ListComp l (termsubst s e') co')
    infer env (Set l es)                = do t0 <- newUnivar env
                                             (cs,es')  <- infElems env es t0
                                             w <- newWitness
                                             return (Proto (locinfo l 87) env w t0 pHashable : cs, tSet t0, eCall (tApp (eQVar primMkSet) [t0]) [eVar w,Set l es'])
    infer env (SetComp l e co)
      | nodup co                        = do (cs1,env',s,co') <- infComp env co
                                             t0 <- newUnivar env
                                             (cs2,es) <- infElems env' [e] t0
                                             w <- newWitness
                                             let Elem v = head es
                                                 e' = Elem (annot (tHashableW t0) (eVar w) t0 v)
                                             return (Proto (locinfo l 89) env w t0 pHashable : cs1++cs2, tSet t0, SetComp l (termsubst s e') co')
    infer env (Dict l as)               = do tk <- newUnivar env
                                             tv <- newUnivar env
                                             (cs,as') <- infAssocs env as tk tv
                                             w <- newWitness
                                             return (Proto (locinfo l 88) env w tk pHashable : cs, tDict tk tv, eCall (tApp (eQVar primMkDict) [tk, tv]) [eVar w,Dict l as'])
    infer env (DictComp l a co)
      | nodup co                        = do (cs1,env',s,co') <- infComp env co
                                             tk <- newUnivar env
                                             tv <- newUnivar env
                                             (cs2,as) <- infAssocs env' [a] tk tv
                                             w <- newWitness
                                             let Assoc k v = head as
                                                 a' = Assoc (annot (tHashableW tk) (eVar w) tk k) v
                                             return (Proto (locinfo l 90) env w tk pHashable : cs1++cs2, tDict tk tv, DictComp l (termsubst s a') co')
    infer env (GeneratorExpr l e co)
      | nodup co                        = do (cs1,env',s,co') <- infGenComp env co
                                             t0 <- newUnivar env
                                             pushFX fxPure tNone
                                             (cs2,es) <- infElems env' [e] t0
                                             popFX
                                             let [e'] = es
                                             return (cs1++cs2, tIterator t0, GeneratorExpr l (termsubst s e') co')

    infer env (Paren l e)               = do (cs,t,e') <- infer env e
                                             return (cs, t, Paren l e')

inferCall env unwrap l e ps ks          = do (cs1,t,e') <- infer env e{eloc = l}
                                             (cs1,t,e') <- if unwrap && actorSelf env then wrapped l attrUnwrap env cs1 [t] [e'] else pure (cs1,t,e')
                                             (cs2,prow,ps') <- infer env ps
                                             (cs3,krow,ks') <- infer env ks
                                             t0 <- newUnivar env
                                             fx <- currFX
                                             w <- newWitness
                                             let i = case e of
                                                        Var _ n@(NoQ n')
                                                          | NDef sc _ _ <- findQName n env,
                                                            Just l2 <- findDefLoc n' env ->
                                                                   DeclInfo l l2 n' sc ("Type incompatibility between definition of and call of "++Pretty.print n')
                                                        _ -> DfltInfo l 837 (Just (Call l e ps ks)) []
                                             return (Sub i env w t (tFun fx prow krow t0)  :
                                            -- return (Sub (DfltInfo l 837 (Just (Call l e ps ks)) []) w [] t (tFun fx prow krow t0) :
                                                     cs1++cs2++cs3, t0, Call l (eCall (eVar w) [e']) ps' ks')



tupleTemplate env i                     = do ts <- mapM (const $ newUnivar env) [0..i]        -- Handle DotI or RestI...
                                             p <- newUnivarOfKind PRow env
                                             k <- newUnivarOfKind KRow env
                                             let p0 = foldl (flip posRow) p ts
                                                 p1 = foldl (flip posRow) p (tail ts)
                                             return (TTuple NoLoc p0 k, head ts, TTuple NoLoc p1 k)


infElems env [] t0                      = return ([], [])
infElems env (Elem e : es) t0           = do (cs1,e') <- inferSub env t0 e
                                             (cs2,es') <- infElems env es t0
                                             return (cs1++cs2, Elem e' : es')
infElems env (Star e : es) t0           = do t1 <- newUnivar env
                                             (cs1,e') <- inferSub env t1 e
                                             (cs2,es') <- infElems env es t0
                                             w <- newWitness
                                             return (Proto (locinfo2 89 e) env w t1 (pIterable t0) :
                                                     cs1++cs2, Star e' : es')


infAssocs env [] tk tv                  = return ([], [])
infAssocs env (Assoc k v : as) tk tv    = do (cs1,k') <- inferSub env tk k
                                             (cs2,v') <- inferSub env tv v
                                             (cs3,as') <- infAssocs env as tk tv
--                                             return (cs1++cs2++cs3, Elem (eTuple [k',v']) : as')
                                             return (cs1++cs2++cs3, Assoc k' v' : as')
infAssocs env (StarStar e : as) tk tv   = do t1 <- newUnivar env
                                             (cs1,e') <- inferSub env t1 e
                                             (cs2,as') <- infAssocs env as tk tv
                                             w <- newWitness
                                             return (Proto (locinfo2 90 e) env w t1 (pIterable $ tTupleP $ posRow tk $ posRow tv posNil) :
--                                                     cs1++cs2, Star e' : as')
                                                     cs1++cs2, StarStar e' : as')


inferTest env (BinOp l e1 And e2)       = do (cs1,env1,s1,t1,e1') <- inferTest env e1
                                             (cs2,env2,s2,t2,e2') <- inferTest env1 e2
                                             t <- newUnivar env
                                             w1 <- newWitness
                                             w2 <- newWitness
                                             return (Sub (locinfo' l 91 e1) env w1 t1 t : Sub (locinfo' l 92 e2) env w2 t2 t :
                                                     cs1++cs2, env2, s1++s2, t, BinOp l (eCall (eVar w1) [e1']) And (eCall (eVar w2) [termsubst s1 e2']))
inferTest env (BinOp l e1 Or e2)        = do (cs1,_,_,t1,e1') <- inferTest env e1
                                             (cs2,_,_,t2,e2') <- inferTest env e2
                                             t <- newUnivar env
                                             w1 <- newWitness
                                             w2 <- newWitness
                                             return (Sub (locinfo2 93 e1) env w1 t1 (tOpt t) : Sub (locinfo2 94 e2) env w2 t2 t :
                                                     cs1++cs2, env, [], t, BinOp l (eCall (eVar w1) [e1']) Or (eCall (eVar w2) [e2']))
inferTest env (UnOp l Not e)            = do (cs,_,_,_,e') <- inferTest env e
                                             return (cs, env, [], tBool, UnOp l Not e')
inferTest env (CompOp l e [OpArg IsNot None{}])
                                        = do t <- newUnivar env
                                             (cs1,e1) <- inferSub env (tOpt t) e
                                             let e' = eCall (tApp (eQVar primISNOTNONE) [t]) [e1]
                                             case e of
                                               Var _ (NoQ n) ->
                                                  return (cs1, define [(n,NVar t)] env, sCast n (tOpt t) t, tBool, e')
                                               _ ->
                                                 return (cs1, env, [], tBool, e')
inferTest env (CompOp l e [OpArg Is None{}])
                                        = do t <- newUnivar env
                                             (cs1,e') <- inferSub env (tOpt t) e
                                             return (cs1, env, [], tBool, eCall (tApp (eQVar primISNONE) [t]) [e'])
inferTest env (IsInstance l e@(Var _ (NoQ n)) c)
                                        = case findQName c env of
                                             NClass q _ _ _ -> do
                                                (cs,t,e') <- infer env e
                                                ts <- newUnivars env [ tvkind v | v <- qbound q ]
                                                let tc = tCon (TC c ts)
                                                return (cs, define [(n,NVar tc)] env, sCast n t tc, tBool, IsInstance l e' c)
                                             _ -> nameUnexpected c
inferTest env (Paren l e)               = do (cs,env',s,t,e') <- inferTest env e
                                             return (cs, env', s, t, Paren l e')
inferTest env e                         = do (cs,t,e') <- infer env e
                                             return (cs, env, [], t, e')


sCast n t t'                            = [(n, eCAST t t' (eVar n))]

inferSlice env (Sliz l e1 e2 e3)        = do (cs1,e1') <- inferSub env tInt e1
                                             (cs2,e2') <- inferSub env tInt e2
                                             (cs3,e3') <- inferSub env tInt e3
                                             return (cs1++cs2++cs3, Sliz l e1' e2' e3')


class InferSub a where
    inferSub                            :: Env -> Type -> a -> TypeM (Constraints,a)

instance InferSub Expr where
    inferSub env t e                    = do (cs,t',e') <- infer env e
                                             w <- newWitness
                                             return (Sub (locinfo2 96 e) env w t' t : cs, eCall (eVar w) [e'])

instance InferSub (Maybe Expr) where
    inferSub env t Nothing              = return ([], Nothing)
    inferSub env t (Just e)             = do (cs,e') <- inferSub env t e
                                             return (cs, Just e')


instance (Infer a) => Infer (Maybe a) where
    infer env Nothing                   = do t <- newUnivar env
                                             return ([], t, Nothing)
    infer env (Just x)                  = do (cs,t,e') <- infer env x
                                             return (cs, t, Just e')

instance InfEnv PosPar where
    infEnv env (PosPar n a Nothing p)   = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             let t' = t -- {tloc = loc n}
                                             (cs,te,p') <- infEnv (define [(n, NVar t')] env) p
                                             return (cs, (n, NVar t'):te, PosPar n (Just t') Nothing p')
    infEnv env (PosPar n a (Just e) p)  = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,p') <- infEnv (define [(n, NVar t)] env) p
                                             return (cs1++cs2, (n, NVar t):te, PosPar n (Just t) (Just e') p')
    infEnv env (PosSTAR n a)            = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             r <- newUnivarOfKind PRow env
                                             return ([Cast (locinfo n 97) env t (tTupleP r)], [(n, NVar t)], PosSTAR n (Just $ tTupleP r))
    infEnv env PosNIL                   = return ([], [], PosNIL)

instance InfEnv KwdPar where
    infEnv env (KwdPar n a Nothing k)   = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             (cs,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (cs, (n, NVar t):te, KwdPar n (Just t) Nothing k')
    infEnv env (KwdPar n a (Just e) k)  = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             (cs1,e') <- inferSub env t e
                                             (cs2,te,k') <- infEnv (define [(n, NVar t)] env) k
                                             return (cs1++cs2, (n, NVar t):te, KwdPar n (Just t) (Just e') k')
    infEnv env (KwdSTAR n a)            = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             r <- newUnivarOfKind KRow env
                                             return ([Cast (locinfo n 98) env t (tTupleK r)], [(n, NVar t)], KwdSTAR n (Just $ tTupleK r))
    infEnv env KwdNIL                   = return ([], [], KwdNIL)

---------

instance Infer PosArg where
    infer env (PosArg e p)              = do (cs1,t,e') <- infer env e
                                             (cs2,prow,p') <- infer env p
                                             return (cs1++cs2, posRow t prow, PosArg e' p')
    infer env (PosStar e)               = do prow <- newUnivarOfKind PRow env
                                             (cs,e') <- inferSub env (tTupleP prow) e
                                             return (cs, posStar prow, PosStar e')
    infer env PosNil                    = return ([], posNil, PosNil)

instance Infer KwdArg where
    infer env (KwdArg n e k)            = do (cs1,t,e') <- infer env e
                                             (cs2,krow,k') <- infer env k
                                             return (cs1++cs2, kwdRow n t krow, KwdArg n e' k')
    infer env (KwdStar e)               = do krow <- newUnivarOfKind KRow env
                                             (cs,e') <- inferSub env (tTupleK krow) e
                                             return (cs, kwdStar krow, KwdStar e')
    infer env KwdNil                    = return ([], kwdNil, KwdNil)


infComp env NoComp                      = return ([], env, [], NoComp)
infComp env (CompIf l e c)              = do (cs1,env1,s,_,e') <- inferTest env e
                                             (cs2,env2,s',c') <- infComp env1 c
                                             return (cs1++cs2, env2, s++s', CompIf l e' (termsubst s c'))
infComp env (CompFor l p e c)           = do (te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             t2 <- newUnivar env
                                             (cs2,e') <- inferSub env t2 e
                                             (cs3,env',s,c') <- infComp (define te1 env) c
                                             w <- newWitness
                                             return (Proto (locinfo2 101 e) env w t2 (pIterable t1) :
                                                     cs2++cs3, env', s, CompFor l p' (eCall (eDot (eVar w) iterKW) [e']) c')

-- A generator constructs its first iterator immediately, but evaluates all
-- following clauses and its result expression from Iterator.__next__.  The
-- latter must therefore be pure, as required by Iterator and by the public
-- map/filter/flatmap combinators used during normalization.
infGenComp env (CompFor l p e c)        = do (te1,t1,p') <- infEnvT (reserve (bound p) env) p
                                             t2 <- newUnivar env
                                             (cs2,e') <- inferSub env t2 e
                                             pushFX fxPure tNone
                                             (cs3,env',s,c') <- infComp (define te1 env) c
                                             popFX
                                             w <- newWitness
                                             return (Proto (locinfo2 109 e) env w t2 (pIterable t1) :
                                                     cs2++cs3, env', s, CompFor l p' (eCall (eDot (eVar w) iterKW) [e']) c')
infGenComp env co                        = infComp env co

instance InfEnvT PosPat where
    infEnvT env (PosPat p ps)           = do (te1,t,p') <- infEnvT env p
                                             (te2,r,ps') <- infEnvT env ps
                                             return (te1++te2, posRow t r, PosPat p' ps')
    infEnvT env (PosPatStar p)          = do (te,t,p') <- infEnvT env p
                                             r <- newUnivarOfKind PRow env
                                             tryUnify env (locinfo p 102) t (tTupleP r)
                                             return (te, posStar r, PosPatStar p')
    infEnvT env PosPatNil               = return ([], posNil, PosPatNil)


instance InfEnvT KwdPat where
    infEnvT env (KwdPat n p ps)         = do (te1,t,p') <- infEnvT env p
                                             (te2,r,ps') <- infEnvT env ps
                                             return (te1++te2, kwdRow n t r, KwdPat n p' ps')
    infEnvT env (KwdPatStar p)          = do (te,t,p') <- infEnvT env p
                                             r <- newUnivarOfKind KRow env
                                             tryUnify env (locinfo p 103) t (tTupleK r)
                                             return (te, kwdStar r, KwdPatStar p')
    infEnvT env KwdPatNil               = return ([], kwdNil, KwdPatNil)



instance InfEnvT Pattern where
    infEnvT env (PWild l a)             = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             return ([], t, PWild l (Just t))
    infEnvT env (PVar l n a)            = do t <- maybe (newUnivar env) return a
                                             wellformed env t
                                             case findName n env of
                                                 NReserved -> do
                                                     --traceM ("## infEnvT " ++ prstr n ++ " : " ++ prstr t)
                                                     return ([(n, NVar t)], t, PVar l n (Just t))
                                                 NSig (TSchema _ [] t') _ _
                                                   | TFun{} <- t' -> notYet l "Pattern variable with previous function signature"
                                                   | otherwise -> do
                                                     --traceM ("## infEnvT (sig) " ++ prstr n ++ " : " ++ prstr t' ++ " < " ++ prstr t)
                                                     let te = [(n, NVar t')]
                                                     solveAll env te [Cast (locinfo l 104) env t' t]
                                                     return (te, t', PVar l n (Just t'))
                                                 NVar t'
                                                   | isJust a -> do
                                                     return ([], t, PVar l n (Just t))
                                                   | otherwise ->
                                                     return ([], t', PVar l n Nothing)
                                                 NSVar t' -> do
                                                     fx <- currFX
                                                     solveAll env [(n,NVar t)] [Cast (locinfo l 106) env fxProc fx, Cast (locinfo l 107) env t t']
                                                     return ([], t', PVar l n Nothing)
                                                 _ ->
                                                     err1 n "Variable not assignable:"
    infEnvT env (PTuple l ps ks)        = do (te1,prow,ps') <- infEnvT env ps
                                             (te2,krow,ks') <- infEnvT env ks
                                             return (te1++te2, TTuple NoLoc prow krow, PTuple l ps' ks')
    infEnvT env (PList l ps p)          = do (te1,t1,ps') <- infEnvT env ps
                                             (te2,t2,p') <- infEnvT (define te1 env) p
                                             tryUnify env (locinfo l 108) t2 (tList t1)
                                             return (te1++te2, t2, PList l ps' p')
    infEnvT env (PParen l p)            = do (te,t,p') <- infEnvT env p
                                             return (te, t, PParen l p')
    infEnvT env (PData l n es)          = notYet l "data syntax"


instance InfEnvT (Maybe Pattern) where
    infEnvT env Nothing                 = do t <- newUnivar env
                                             return ([], t, Nothing)
    infEnvT env (Just p)                = do (te,t,p') <- infEnvT env p
                                             return (te, t, Just p')

instance InfEnvT [Pattern] where
    infEnvT env [p]                     = do (te1,t1,p') <- infEnvT env p
                                             return (te1,t1,[p'])
    infEnvT env (p:ps)                  = do (te1,t1,p') <- infEnvT env p
                                             (te2,t2,ps') <- infEnvT env ps
                                             tryUnify env (locinfo p 109) t1 t2
                                             return (te1++te2, t1, p':ps')



-- Test discovery --------------------------------------------------------------

tEnv                                    = tCon (TC (gname [name "__builtin__"] (name "Env")) [])
emptyDict                               = Dict NoLoc []

testDicts                               = [ ("__unit_tests",        "UnitTest"),
                                            ("__simple_sync_tests", "SimpleSyncTest"),
                                            ("__sync_tests",        "SyncTest"),
                                            ("__async_tests",       "AsyncTest"),
                                            ("__env_tests",         "EnvTest") ]

testStmts env m ss                      = (stmts, tests)
  where assocs                          = testFuns (define te env) m (ss++ss')
        (te, ss')                       = genTestActorWrappers ss
        stmts                           = ss' ++
                                          [ dictAssign n cl assoc | ((n,cl), assoc) <- testDicts `zip` assocs ] ++
                                          [ testActor ]
        tests                           = sort (nub (concatMap assocNames assocs))
        assocNames assocList            = mapMaybe assocName assocList
        assocName (Assoc (Strings _ ssParts) _) = Just (concat ssParts)
        assocName _                     = Nothing

testEnv                                 = [ (name n, NVar (tDict tStr (testing cl))) | (n,cl) <- testDicts ] ++
                                          [ (name "test_main", NAct [] posNil (kwdRow (name "env") tEnv kwdNil) [] Nothing) ]

gname ns n                              = GName (ModName ns) n
dername a b                             = Derived (name a) (name b)

dictAssign dictname cl dict             = sAssign (pVar (name dictname) (tDict tStr (testing cl))) (mkDict cl dict)

testing tstr                            = tCon (TC (gname [name "testing"] (name tstr)) [])

mkDict cl as                            = eCall (tApp (eQVar primMkDict) [tStr, testing cl]) [w,Dict NoLoc as]
    where w                             = eCall (eQVar (gname [name "__builtin__"] (dername "Hashable" "str"))) []

testActor                               = sDecl [Actor NoLoc (name "test_main") []
                                                 PosNIL (KwdPar (name "env")  (Just tEnv) Nothing KwdNIL)
                                             [sExpr (eCall (eQVar (gname [name "testing"] (name "test_runner")))
                                                           (map (eVar . name) ["env","__unit_tests","__simple_sync_tests","__sync_tests","__async_tests","__env_tests"]))] Nothing]

row2list (TRow _ _ _ t _ r)             = t : row2list r
row2list (TNil _ _)                     = []

mkAssoc d testType modName =
    Assoc (Strings NoLoc [nstr (dname d)])
          (eCall (eQVar (gname [name "testing"] testType))
                 [ eVar (dname d)
                 , Strings NoLoc [nstr (dname d)]
                 , comment (dbody d)
                 , Strings NoLoc [modName]
                 ])
  where comment (Expr _ s@(Strings _ ss) : _) = s
        comment _ = Strings NoLoc [""]

mkAssocActor (Actor _ n _ _ _ body _) testType modName =
    Assoc (Strings NoLoc [nstr n])
          (eCall (eQVar (gname [name "testing"] testType))
                 [ eVar n
                 , Strings NoLoc [nstr n]
                 , comment body
                 , Strings NoLoc [modName]
                 ])
  where comment (Expr _ s@(Strings _ ss) : _) = s
        comment _ = Strings NoLoc [""]


testFuns :: Env0 -> String -> Suite -> [[Assoc]]
testFuns env modName ss = tF ss [] [] [] [] []
  where
    tF (With _ _ ss' : ss) uts ssts sts ats ets = tF (ss' ++ ss) uts ssts sts ats ets
    tF (Decl l (d@Def{}:ds) : ss) uts ssts sts ats ets
      | isTestName (dname d) =
          case testType (findQName (NoQ (dname d)) env) of
            Just UnitTest ->
              tF (Decl l ds : ss) (mkAssoc d (name "UnitTest") modName : uts) ssts sts ats ets
            Just SimpleSyncTest ->
              tF (Decl l ds : ss) uts (mkAssoc d (name "SimpleSyncTest") modName : ssts) sts ats ets
            Just SyncTest ->
              tF (Decl l ds : ss) uts ssts (mkAssoc d (name "SyncTest") modName : sts) ats ets
            Just AsyncTest ->
              tF (Decl l ds : ss) uts ssts sts (mkAssoc d (name "AsyncTest") modName : ats) ets
            Just EnvTest ->
              tF (Decl l ds : ss) uts ssts sts ats (mkAssoc d (name "EnvTest") modName : ets)
            Nothing -> tF (Decl l ds : ss) uts ssts sts ats ets
    -- Don't discover actors here - they're handled via wrapper generation
    tF (Decl l (_:ds) : ss) uts ssts sts ats ets = tF (Decl l ds : ss) uts ssts sts ats ets
    tF (Decl _ [] : ss) uts ssts sts ats ets = tF ss uts ssts sts ats ets
    tF (_ : ss) uts ssts sts ats ets = tF ss uts ssts sts ats ets
    tF [] uts ssts sts ats ets = [reverse uts, reverse ssts, reverse sts, reverse ats, reverse ets]

isTestName n                             = take 6 (nstr n) == "_test_"

-- Generate wrapper functions for test actors
genTestActorWrappers :: Suite -> (TEnv, Suite)
genTestActorWrappers ss =
    let testActors = findTestActors ss
        existingFunctions = collectFunctionNames ss
        wrappers = mapMaybe (genWrapper existingFunctions) testActors
    in unzip wrappers
  where
    -- Find actors that are test actors (either with testing params or _test_ prefix)
    findTestActors :: Suite -> [Decl]
    findTestActors = go []
      where
        go actors [] = actors
        go actors (Decl _ ds : rest) =
            go (actors ++ filter isTestActor ds) rest
        go actors (With _ [] ss : rest) =
            go actors (ss ++ rest)
        go actors (_ : rest) = go actors rest

    isTestActor (Actor _ n _ ppar kpar _ _) =
        checkTestActorParams ppar kpar || (isTestName n && ppar == PosNIL && kpar == KwdNIL)
    isTestActor _ = False

    checkTestActorParams PosNIL (KwdPar _ (Just t) _ KwdNIL) =
        t == tCon (TC (gname [name "testing"] (name "SyncT")) []) ||
        t == tCon (TC (gname [name "testing"] (name "AsyncT")) []) ||
        t == tCon (TC (gname [name "testing"] (name "EnvT")) [])
    checkTestActorParams (PosPar _ (Just t) _ PosNIL) KwdNIL =
        t == tCon (TC (gname [name "testing"] (name "SyncT")) []) ||
        t == tCon (TC (gname [name "testing"] (name "AsyncT")) []) ||
        t == tCon (TC (gname [name "testing"] (name "EnvT")) [])
    checkTestActorParams _ _ = False

    -- Collect all function names to check for conflicts
    collectFunctionNames :: Suite -> [Name]
    collectFunctionNames = go []
      where
        go names [] = names
        go names (Decl _ ds : rest) = go (names ++ mapMaybe getFuncName ds) rest
        go names (_ : rest) = go names rest
        getFuncName (Def _ n _ _ _ _ _ _ _ _) = Just n
        getFuncName _ = Nothing

    -- Generate a wrapper function for a test actor if needed
    genWrapper :: [Name] -> Decl -> Maybe ((Name,NameInfo), Stmt)
    genWrapper existingFuncs (Actor _ actorName _ ppar kpar _ _) =
        let tParam = name "t"
            paramType = getActorParamType ppar kpar
            -- actor Foo(t: testing.AsyncT)           -> _test_Foo
            -- actor _test_Foo(t: testing.AsyncT)     -> _test_Foo_wrapper
            wrapperName = if "_test_" `isPrefixOf` nstr actorName
                          then name (nstr actorName ++ "_wrapper")
                          else name ("_test_" ++ nstr actorName)
            checkName = case paramType of
                          Nothing | isTestName actorName && ppar == PosNIL && kpar == KwdNIL ->
                            name (nstr actorName ++ "_wrapper")
                          _ -> wrapperName
        in if checkName `elem` existingFuncs
           then Nothing  -- Wrapper / test function already exists, don't generate
           else case paramType of
                Just pType ->
                    Just $ ((wrapperName, NDef (monotype $ tFun fxProc (posRow pType posNil) kwdNil tNone) NoDec Nothing),
                            sDecl [Def NoLoc wrapperName []  -- Wrapper function with positional param
                                        (PosPar tParam (Just pType) Nothing PosNIL)
                                        KwdNIL
                                        (Just (TNone NoLoc))
                                        [Expr NoLoc (eCall (eVar actorName) [eVar tParam])]  -- Call original actor, passing parameters
                                        NoDec
                                        fxProc
                                        Nothing])
                Nothing ->
                    -- SimpleSyncTest actor - no parameters
                    if isTestName actorName && ppar == PosNIL && kpar == KwdNIL
                    then
                        Just $ ((wrapperName, NDef (monotype $ tFun fxProc posNil kwdNil tNone) NoDec Nothing),
                                sDecl [Def NoLoc wrapperName []
                                        PosNIL
                                        KwdNIL
                                        (Just (TNone NoLoc))
                                        [Expr NoLoc (eCall (eVar actorName) [])]  -- Call original actor
                                        NoDec
                                        fxProc
                                        Nothing])
                    else Nothing
    genWrapper _ _ = Nothing

    -- Get the parameter type from actor parameters
    getActorParamType PosNIL (KwdPar _ (Just t) _ KwdNIL) = Just t
    getActorParamType (PosPar _ (Just t) _ PosNIL) KwdNIL = Just t
    getActorParamType _ _ = Nothing



data TestType = UnitTest | SimpleSyncTest | SyncTest | AsyncTest | EnvTest
                deriving (Eq,Show,Read)

-- Determine test type based on function signature
testType (NDef (TSchema _ [] (TFun _ fx ppar kpar res)) _ _)
                                        = case (res, fx, row2list ppar, row2list kpar) of
                                             -- Functions with no parameters
                                             (r, fx', [], [])  | validReturn r && (fx' == fxPure || fx' == fxMut) -> Just UnitTest
                                             (r, fx', [], [])  | validReturn r && fx' == fxProc                   -> Just SimpleSyncTest
                                             -- Functions with positional test parameters
                                             (r, fx', [t], []) | t == syncT  && validReturn r                     -> Just SyncTest
                                             (r, fx', [t], []) | t == asyncT && validReturn r                     -> Just AsyncTest
                                             (r, fx', [t], []) | t == envT   && validReturn r                     -> Just EnvTest
                                             -- Functions with keyword test parameters
                                             (r, fx', [], [t]) | t == syncT  && validReturn r                     -> Just SyncTest
                                             (r, fx', [], [t]) | t == asyncT && validReturn r                     -> Just AsyncTest
                                             (r, fx', [], [t]) | t == envT   && validReturn r                     -> Just EnvTest
                                             _                                                                    -> Nothing
    where validReturn r                 = r == tNone || r == TNone NoLoc || r == tStr
          syncT                         = tCon (TC (gname [name "testing"] (name "SyncT")) [])
          asyncT                        = tCon (TC (gname [name "testing"] (name "AsyncT")) [])
          envT                          = tCon (TC (gname [name "testing"] (name "EnvT")) [])
testType _                              = Nothing
