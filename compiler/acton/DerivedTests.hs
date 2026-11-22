-- | Derived modules: the modules that the derived_modules table of
-- Build.act names, whose source a function returns during a build.  The
-- runner of an entry is the program that a build makes to call the function
-- of the entry.
module DerivedTests (derivedModuleTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, finally, try)
import Control.Monad
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (isJust)
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, ownerModes, setFileSize)
import System.Process

import qualified Acton.Fingerprint as Fingerprint
import Acton.Project (withProjectLock)
import EmbedTests (edited, opened, withServerWithin)
import Test.Tasty
import Test.Tasty.HUnit
import WatchTests (awaitGenerationWithin, awaitWithin, withActon)

derivedModuleTests :: TestTree
derivedModuleTests = testGroup "derived modules"
  [ testCase "a function runs when its module is out of date and not otherwise" $
      withProject "demo" demoFiles $ \acton proj -> do
        let build label expectRun = do
              out <- succeeds label acton proj ["build"]
              assertEqual (label ++ ": whether the function ran\n" ++ out) expectRun ("Derived by gen.schema" `isInfixOf` out)
            prints label expected = assertEqual label expected =<< runBinary (proj </> "out/bin/demo")
            -- A build writes this file each time it has built the runner of
            -- the entry.
            runnerBuilt = getModificationTime (proj </> "out/derived/runner/schema.built")
        build "first build" True
        prints "first build" "hello, world\n"
        assertEqual "the source of the derived module" "greeting = \"hello, world\"\n" =<< readFile (proj </> "out/derived/src/schema.act")
        assertBool "the documentation index lists the derived module" . ("demo.schema" `isInfixOf`) =<< readFileStrict (proj </> "out/doc/index.html")
        firstRunner <- runnerBuilt
        build "nothing changed" False
        writeFile (proj </> "data/greeting.txt") "hi\n"
        build "an input file changed" True
        prints "an input file changed" "hi, world\n"
        assertEqual "an input file changed: the runner is not built again" firstRunner =<< runnerBuilt
        appendFile (proj </> "src/gen.act") "\ndef unused() -> int:\n    return 1\n"
        build "a name that the function does not use was added to its module" False
        writeFile (proj </> "src/gen.act") (deriveImport ++ unlines
          [ "separator = \"; \""
          , ""
          , "def schema(inputs: dict[str, derive.InputFile], name: str) -> str:"
          , "    text = inputs[\"data/greeting.txt\"].read().decode().strip()"
          , "    return 'greeting = \"' + text + separator + name + '\"\\n'"
          ])
        build "the function changed" True
        prints "the function changed" "hi; world\n"
        assertBool "the function changed: the runner is built again" . (/= firstRunner) =<< runnerBuilt
        writeFile (proj </> "src/gen.act") . replace "\"; \"" "\" - \"" =<< readFileStrict (proj </> "src/gen.act")
        build "a constant that the function reads changed" True
        prints "a constant that the function reads changed" "hi - world\n"
        writeFile (proj </> "Build.act") . replace "\"world\"" "\"there\"" =<< readFileStrict (proj </> "Build.act")
        build "an argument changed" True
        prints "an argument changed" "hi - there\n"
        removeFile (proj </> "out/derived/src/schema.act")
        build "the source of the derived module was removed" True
        -- Every build of a test runs the same compiler, so the test writes
        -- another compiler version into the record of the entry.
        writeFile (proj </> "out/derived/schema.json") . replace "\"compiler\":\"" "\"compiler\":\"another " =<< readFileStrict (proj </> "out/derived/schema.json")
        out <- succeeds "the record holds another compiler version" acton proj ["build", "--verbose"]
        assertBool ("the function runs when the record holds another compiler version\n" ++ out)
                   ("out of date, the compiler version changed" `isInfixOf` out && "Derived by gen.schema" `isInfixOf` out)
        build "nothing changed at the end" False

  , testCase "a function returns several modules for several names" $
      withProject "several" severalFiles $ \acton proj -> do
        out <- succeeds "first build" acton proj ["build"]
        assertBool ("the line of the run\n" ++ out) ("Derived 3 modules by gen.modules" `isInfixOf` out)
        assertEqual "output" "a b\n1 2\n" =<< runBinary (proj </> "out/bin/several")
        -- The order of the names of an entry has no meaning: the smallest
        -- name gives the record its file name, and the names in another
        -- order are the same entry.
        assertBool "the record is named by the smallest name" =<< doesFileExist (proj </> "out/derived/names.json")
        writeFile (proj </> "Build.act") . replace "{part.b, part.a, names}" "{names, part.a, part.b}" =<< readFileStrict (proj </> "Build.act")
        reordered <- succeeds "the names in another order" acton proj ["build"]
        assertBool ("no function runs when the names are written in another order\n" ++ reordered) (not ("Derived" `isInfixOf` reordered))
        -- The names of an entry are exactly the modules that its function
        -- returns.  A module that the function no longer returns is taken
        -- out of the entry too, and is then removed with its outputs.  The
        -- modules that are returned unchanged stay as they are.
        removeFile (proj </> "parts/b.txt")
        missing <- fails "a named module that the function no longer returns" acton proj ["build"]
        assertBool missing ("Named and not returned: part.b" `isInfixOf` missing)
        writeFile (proj </> "Build.act") . replace "{names, part.a, part.b}" "{names, part.a}" =<< readFileStrict (proj </> "Build.act")
        writeFile (proj </> "src/several.act") (unlines
          [ "import names"
          , "import part.a"
          , ""
          , "actor main(env):"
          , "    proc def __startup__():"
          , "        print(\" \".join(names.names))"
          , "        print(part.a.size)"
          , "        env.exit(0)"
          ])
        _ <- succeeds "second build" acton proj ["build"]
        assertEqual "output" "a\n1\n" =<< runBinary (proj </> "out/bin/several")
        assertBool "the module that the function no longer returns is removed" . not =<< doesFileExist (proj </> "out/derived/src/part/b.act")
        assertBool "the interface of the removed module is removed" . not =<< doesPathExist (proj </> "out/types/several/part/b.tydb")
        -- acton spec update writes Build.act from its JSON form, with the
        -- names of the entry one per line.  That is the same entry.
        writeFile (proj </> "spec.json") =<< succeeds "the JSON form of Build.act" acton proj ["spec", "dump"]
        _ <- succeeds "Build.act from its JSON form" acton proj ["spec", "update", "spec.json"]
        written <- readFileStrict (proj </> "Build.act")
        assertBool ("the names of the entry one per line\n" ++ written)
                   ("    {\n        names,\n        part.a\n    }: (\n        function=gen.modules,\n        inputs=[\"parts/\"]\n    )\n}\n" `isInfixOf` written)
        rewritten <- succeeds "the names one per line" acton proj ["build"]
        assertBool ("no function runs when the names are written one per line\n" ++ rewritten) (not ("Derived" `isInfixOf` rewritten))

  , testCase "a function gets the path, the size and the SHA-256 of each input file and reads none" $
      withProject "described" describedFiles $ \acton proj -> do
        let build label expectRun = do
              out <- succeeds label acton proj ["build"]
              assertEqual (label ++ ": whether the function ran\n" ++ out) expectRun ("Derived by gen.facts" `isInfixOf` out)
            prints label expected = assertEqual label (unlines expected) =<< runBinary (proj </> "out/bin/described")
            one   = "data/one.txt 6 "
            two   = "data/t{w}o \"x\".bin 2 fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"
            three = "data/three.txt 0 e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            hello = "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"
            hallo = "622cb3371c1a08096eaac564fb59acccda1fcdbe13a9dd10b486e6463c8c2525"
        build "first build" True
        prints "first build" [one ++ hello, two]
        build "nothing changed" False
        -- A description holds the SHA-256 of the content, so the function
        -- runs for new content of a file that it does not read.
        writeFile (proj </> "data/one.txt") "hallo\n"
        build "a file has new content of the same size" True
        prints "a file has new content of the same size" [one ++ hallo, two]
        -- The paths are sorted by code point: h comes before {.
        writeFile (proj </> "data/three.txt") ""
        build "a file was added" True
        prints "a file was added" [one ++ hallo, three, two]
        removeFile (proj </> "data/one.txt")
        build "a file was removed" True
        prints "a file was removed" [three, two]

  , testCase "a file that an input path does not stand for is no input file" $
      -- An input path that names a directory stands for the files below it,
      -- except a file whose name starts with a dot, a file below a
      -- directory whose name starts with a dot, and a --tydb copy.  A path
      -- that names such a file or such a directory itself is taken as it is
      -- written.
      withProject "described" (overrideFiles describedFiles
          [ ("Build.act", described ["data/", "data/one.txt", "data/.well-known/", "data/.config"])
          , ("data/Zed.txt", "z")
          , ("data/.hidden", "hidden\n")
          , ("data/.cache/kept.txt", "cache\n")
          , ("data/part.tydb/data.mdb", "copy\n")
          , ("data/.well-known/acme.txt", "token\n")
          , ("data/.config", "c\n")
          ]) $ \acton proj -> do
        _ <- succeeds "first build" acton proj ["build"]
        printed <- lines <$> runBinary (proj </> "out/bin/described")
        -- The paths are sorted by code point: a dot comes before a letter,
        -- and Z before o.  The file data/one.txt, which two input paths
        -- name, is one input file.
        let expected = ["data/.config 2 ", "data/.well-known/acme.txt 6 ", "data/Zed.txt 1 ", "data/one.txt 6 ", "data/t{w}o \"x\".bin 2 "]
        assertBool ("the input files\n" ++ unlines printed) (length printed == length expected && and (zipWith isPrefixOf expected printed))
        writeFiles proj [("data/.hidden", "changed\n"), ("data/.cache/new.txt", "new\n"), ("data/part.tydb/lock.mdb", "lock\n")]
        out <- succeeds "files that no input path stands for changed" acton proj ["build"]
        assertBool ("no function runs\n" ++ out) (not ("Derived" `isInfixOf` out))
        writeFile (proj </> "data/.well-known/acme.txt") "other\n"
        again <- succeeds "a file below a path that is written in inputs changed" acton proj ["build"]
        assertBool ("the function runs\n" ++ again) ("Derived by gen.facts" `isInfixOf` again)

  , testCase "an input file that changes while its function runs stops the build" $ do
      -- The function of the project described reads no input file, so the
      -- look of the build at its input files after the run finds the change.
      withProject "described" describedFiles $ \acton proj -> do
        (code, out) <- buildWithChange acton proj "facts" "data/one.txt" "hallo\n"
        assertEqual out (ExitFailure 1) code
        assertBool out ("data/one.txt changed while the build ran. Run the build again" `isInfixOf` out)
        assertBool "no module is moved into place" . not =<< doesPathExist (proj </> "out/derived/src/facts.act")
        _ <- succeeds "the build again" acton proj ["build"]
        printed <- runBinary (proj </> "out/bin/described")
        assertBool ("the SHA-256 of the new content\n" ++ printed)
                   ("data/one.txt 6 622cb3371c1a08096eaac564fb59acccda1fcdbe13a9dd10b486e6463c8c2525" `isInfixOf` printed)
      -- The function of the demo project reads its input file, and the read
      -- finds content that the description is not of.
      withProject "demo" demoFiles $ \acton proj -> do
        (code, out) <- buildWithChange acton proj "schema" "data/greeting.txt" "hallo\n"
        assertEqual out (ExitFailure 1) code
        assertBool out ("The function gen.schema of the derived module schema in " `isInfixOf` out
                        && "the input file data/greeting.txt changed while the function ran" `isInfixOf` out)
        _ <- succeeds "the build again" acton proj ["build"]
        assertEqual "output" "hallo, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "an input path is spelled as its directories list it" $
      -- A file system that takes one spelling of a name for another finds
      -- the directory data under the name Data.  The build stops on every
      -- file system.
      forM_ [ ("Data/", "the directory data", "Data")
            , ("data/One.txt", "the file data/one.txt", "data/One.txt")
            , ("Data/sub/x.txt", "the file data/sub/x.txt", "Data/sub/x.txt") ] $ \(input, named, written) ->
        withProject "described" (overrideFiles describedFiles
            [ ("Build.act", described [input])
            , ("data/sub/x.txt", "x") ]) $ \acton proj -> do
          folds <- doesDirectoryExist (proj </> "Data")
          out <- fails "build" acton proj ["build"]
          assertBool out $ if folds
            then ("The input path \"" ++ input ++ "\" of the derived module facts names " ++ named ++ " in ") `isInfixOf` out
                 && (", and the path is written " ++ written ++ ".") `isInfixOf` out
            else ("The input path \"" ++ input ++ "\" of the derived module facts names no file and no directory in ") `isInfixOf` out
          assertBool "no function ran" . not =<< doesPathExist (proj </> "out/derived/src/facts.act")

  , testCase "an input file that cannot be read, is no regular file or has a name that is not text" $ do
      let stops label setup expected =
            withProject "described" describedFiles $ \acton proj -> do
              arises <- setup proj
              when arises $ do
                out <- fails label acton proj ["build"]
                assertBool out ("The derived module facts in " `isInfixOf` out && ("Build.act is not brought up to date: " ++ expected) `isInfixOf` out)
      -- A user who can read every file, such as root, can read this file
      -- too, and then the case does not arise.
      stops "a file without permissions"
        (\proj -> do setPermissions (proj </> "data/one.txt") emptyPermissions
                     isLeft <$> (try (readFileStrict (proj </> "data/one.txt")) :: IO (Either IOException String)))
        "data/one.txt cannot be read: "
      stops "a named pipe" (\proj -> True <$ createNamedPipe (proj </> "data/pipe") ownerModes) "data/pipe is not a regular file."
      -- A symbolic link below an input directory that names nothing is an
      -- input file that cannot be read.
      stops "a symbolic link that names nothing" (\proj -> True <$ createFileLink "none.txt" (proj </> "data/dangling")) "data/dangling cannot be read: "
      -- The character stands for the byte 255, which is text in no encoding
      -- that file names are read in.  A file system that takes only names
      -- in UTF-8 refuses the name, and then the case does not arise.
      stops "a name with a byte that is not text"
        (\proj -> isRight <$> (try (writeFile (proj </> "data/\56575.txt") "x") :: IO (Either IOException ())))
        "data/?.txt has a name with bytes that are not text in the encoding of the locale, shown as ?."

  , testCase "an input file that a module embeds and that embed cannot take gets the message of embed" $
      -- A module of the project embeds a file below the input directory.
      -- The file has 2^31 bytes, one more than embed takes.  The first
      -- build reads it for its SHA-256, and the second build does not.
      withProject "described" (overrideFiles describedFiles
          [("src/big.act", "content: bytes = embed(\"data/big.bin\")\n")]) $ \acton proj -> do
        writeFile (proj </> "data/big.bin") ""
        setFileSize (proj </> "data/big.bin") 2147483648
        forM_ ["first build", "second build"] $ \label -> do
          out <- fails label acton proj ["build"]
          assertBool out ("data/big.bin has 2147483648 bytes, and embed takes a file of fewer than 2147483648 bytes" `isInfixOf` out)
          assertBool out (not ("changed while the build ran" `isInfixOf` out))

  , testCase "arguments of several types for a derived module in a package" $
      withProject "typed" typedFiles $ \acton proj -> do
        _ <- succeeds "build" acton proj ["build"]
        assertEqual "output" (unlines
          [ "[97, 34, 98, 92, 99, 123, 100, 125, 233, 10]"
          , "-4 2.5 3.5 True True 7"
          , "y 2 3 1 0"
          ]) =<< runBinary (proj </> "out/bin/typed")
        -- The label of the entry is model.schema.  A second build finds the
        -- record of the entry and the files of its runner under that label.
        again <- succeeds "second build" acton proj ["build"]
        assertBool ("no function runs in the second build\n" ++ again) (not ("Derived" `isInfixOf` again))
        forM_ [ "out/derived/src/model/schema.act", "out/derived/model.schema.json", "out/derived/runner/src/runner/model/schema.act"
              , "out/derived/runner/model.schema.built", "out/derived/runner/out/bin/runner.model.schema" ] $ \file ->
          assertBool (file ++ " is in place after the second build") =<< doesFileExist (proj </> file)

  , testCase "a function that uses an actor" $
      withProject "demo" (overrideFiles demoFiles [("src/gen.act", genActor)]) $ \acton proj -> do
        _ <- succeeds "first build" acton proj ["build"]
        assertEqual "first build" "hello, world\n" =<< runBinary (proj </> "out/bin/demo")
        writeFile (proj </> "src/gen.act") (replace "\", \"" "\"; \"" genActor)
        out <- succeeds "the actor changed" acton proj ["build"]
        assertBool ("the function runs when an actor that it uses changed\n" ++ out) ("Derived by gen.schema" `isInfixOf` out)
        assertEqual "the actor changed" "hello; world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "a runner is built again when its function changed back after a failed run" $
      withProject "demo" demoFiles $ \acton proj -> do
        -- The build that fails leaves a runner that was built from the
        -- function that raises, and the record of the first build.  After
        -- that the function is the one of the record again, and the runner
        -- in place was built from another function.
        _ <- succeeds "first build" acton proj ["build"]
        writeFile (proj </> "src/gen.act") (schemaOf "str" ["raise ValueError(\"no greeting today\")"])
        out <- fails "the function raises" acton proj ["build"]
        assertBool out ("no greeting today" `isInfixOf` out)
        writeFile (proj </> "src/gen.act") genSchema
        writeFile (proj </> "data/greeting.txt") "hi\n"
        _ <- succeeds "the function is as it was and an input file changed" acton proj ["build"]
        assertEqual "output" "hi, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "a file with the stat of the last run is not read" $
      withProject "demo" demoFiles $ \acton proj -> do
        -- The files are the input file and the source of the derived
        -- module.  A build says under --verbose how many of them it read.
        let build label expectRun expectRead = do
              out <- succeeds label acton proj ["build", "--verbose"]
              assertEqual (label ++ ": whether the function ran\n" ++ out) expectRun ("Derived by gen.schema" `isInfixOf` out)
              forM_ expectRead $ \count ->
                assertBool (label ++ ": expected " ++ count ++ " files read\n" ++ out) (("up to date, " ++ count ++ " files read") `isInfixOf` out)
            settle label = do
              -- A file that was written in the same instant as the record
              -- is read once more, and the build after that reads none.
              build (label ++ ", the build after") False Nothing
              build (label ++ ", the second build after") False (Just "0 of 2")
        build "first build" True Nothing
        settle "first build"
        writeFile (proj </> "data/greeting.txt") "hello\n"
        build "an input file was written again with the same content" False (Just "1 of 2")
        build "the build after that" False (Just "0 of 2")
        writeFile (proj </> "data/greeting.txt") "hi\n"
        build "an input file changed" True Nothing
        settle "an input file changed"
        appendFile (proj </> "out/derived/src/schema.act") "# edited\n"
        build "the source of the derived module was edited" True Nothing
        assertEqual "the source of the derived module" "greeting = \"hi, world\"\n" =<< readFile (proj </> "out/derived/src/schema.act")
        settle "the source of the derived module was edited"

  , testCase "a function and a derived module of a dependency" $
      withSystemTempDirectory "acton-derived-dep" $ \tmp -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        writeFiles (tmp </> "lib") (libFiles "lib")
        writeFiles (tmp </> "app") (appFiles "app")
        _ <- succeeds "build" acton (tmp </> "app") ["build"]
        assertEqual "output" "lib:from lib\napp:from app\nown:from a function of app that read from lib\nmade:from a derived module of lib\n"
          =<< runBinary (tmp </> "app/out/bin/app")
        assertBool "the derived module of the dependency is in the dependency" =<< doesFileExist (tmp </> "lib/out/derived/src/table.act")
        assertBool "the derived module of the project is in the project" =<< doesFileExist (tmp </> "app/out/derived/src/local.act")
        again <- succeeds "second build" acton (tmp </> "app") ["build"]
        assertBool ("no function runs in the second build\n" ++ again) (not ("Derived" `isInfixOf` again))
        -- The function gen.table of lib is the function of table and of
        -- local, and the functions of wrapped and of made call it.  When
        -- gen.table changes, a build runs the functions of these four
        -- derived modules again, and not the function of maker, which does
        -- not use gen.table.  Each expected text starts with the label of
        -- its entry, since the name gen.table is part of lib.gen.table.
        writeFile (tmp </> "lib/src/gen.act") . replace "\"from " "\"by " =<< readFileStrict (tmp </> "lib/src/gen.act")
        changed <- succeeds "a function of the dependency changed" acton (tmp </> "app") ["build"]
        forM_ ["table  Derived by gen.table", "local  Derived by lib.gen.table", "wrapped  Derived by appgen.wrapped", "made  Derived by lib.maker.made"] $ \line ->
          assertBool ("expected " ++ show line ++ " in\n" ++ changed) (line `isInfixOf` changed)
        assertBool ("the function of maker does not run\n" ++ changed) (not ("Derived by gen.maker" `isInfixOf` changed))
        assertEqual "output" "lib:by lib\napp:by app\nown:by a function of app that read by lib\nmade:by a derived module of lib\n"
          =<< runBinary (tmp </> "app/out/bin/app")

  , testCase "a function in a project that the project of the entry does not declare as a dependency" $
      withSystemTempDirectory "acton-derived-undeclared" $ \tmp -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        -- The project demo depends on lib, and lib depends on other.  The
        -- function of the entry of demo is in other.
        writeFiles (tmp </> "other") [("Build.act", buildAct "other" []), ("src/gen.act", genSchema)]
        writeFiles (tmp </> "lib") [("Build.act", buildAct "lib" ["dependencies = {", "    \"other\": (path=\"../other\")", "}"]), ("src/unused.act", "size = 1\n")]
        writeFiles (tmp </> "demo") (overrideFiles demoFiles [("Build.act", buildAct "demo"
          [ "dependencies = {", "    \"lib\": (path=\"../lib\")", "}"
          , "derived_modules = {"
          , "    schema: (function=other.gen.schema, inputs=[\"data/greeting.txt\"], args=(name=\"world\"))"
          , "}"
          ])])
        out <- fails "build" acton (tmp </> "demo") ["build"]
        assertBool out ("names the module other.gen of the project other, which is not a dependency in that Build.act.\nAdd other to its dependencies."
                        `isInfixOf` out)

  , testCase "a derived module under the name of a dependency is refused before its function runs" $
      withProject "demo" (overrideFiles demoFiles
          [ ("Build.act", buildAct "demo"
              [ "dependencies = {", "    \"lib\": (path=\"lib\")", "}"
              , "derived_modules = {"
              , "    lib.extra: (function=gen.schema, inputs=[\"data/greeting.txt\"], args=(name=\"world\"))"
              , "}"
              ])
          , ("lib/Build.act", buildAct "lib" [])
          , ("lib/src/unused.act", "size = 1\n")
          ]) $ \acton proj -> do
        out <- fails "build" acton proj ["build"]
        assertBool out ("and lib is a dependency of the project.\nThe name lib.extra names a module of the dependency lib. Give the derived module another name."
                        `isInfixOf` out)
        assertBool "no function ran" . not =<< doesPathExist (proj </> "out/derived/src/lib/extra.act")

  , testCase "a project in a directory whose name has a character outside ASCII" $ do
      -- The Build.act of the runner project holds the directory of the
      -- project.  The case runs where file names are encoded as UTF-8.
      encoding <- getFileSystemEncoding
      when (show encoding == "UTF-8") $
        withProject "dérivé" demoFiles $ \acton proj -> do
          _ <- succeeds "build" acton proj ["build"]
          assertEqual "output" "hello, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "an input path with an empty component is taken as the path without that component" $
      withProject "demo" (overrideFiles demoFiles
          [("Build.act", replace "data/greeting.txt" "data//greeting.txt" (demoBuildAct "gen.schema"))]) $ \acton proj -> do
        -- The function reads the file under the path data/greeting.txt.
        _ <- succeeds "build" acton proj ["build"]
        assertEqual "output" "hello, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "the module files of an entry stay when its smallest name changes" $
      withProject "relabel" relabelFiles $ \acton proj -> do
        _ <- succeeds "first build" acton proj ["build"]
        assertBool "the record is named by the smallest name" =<< doesFileExist (proj </> "out/derived/b.json")
        before <- getModificationTime (proj </> "out/derived/src/b.act")
        writeFile (proj </> "Build.act") . replace "{b, c}" "{a, b, c}" . replace "[\"b\", \"c\"]" "[\"a\", \"b\", \"c\"]"
          =<< readFileStrict (proj </> "Build.act")
        out <- succeeds "a smaller name was added" acton proj ["build"]
        assertBool ("the line of the run\n" ++ out) ("Derived 3 modules by gen.modules" `isInfixOf` out)
        assertBool "the record has the new smallest name" =<< doesFileExist (proj </> "out/derived/a.json")
        assertBool "the record of the former smallest name is removed" . not =<< doesPathExist (proj </> "out/derived/b.json")
        -- The function returned the same source for b, so its file is not
        -- written again.
        assertEqual "the modification time of the module file of b" before =<< getModificationTime (proj </> "out/derived/src/b.act")
        assertEqual "output" "b c\n" =<< runBinary (proj </> "out/bin/relabel")

  , testCase "the module of a function is checked without preparing its generated code" $
      withProject "demo" demoFiles $ \acton proj -> do
        -- The check of the function's module comes before the line of the
        -- entry, and the build of the project after it.  Under --verbose
        -- the check prints a Fresh line for the cached module, and a DBP
        -- line reports a back job that was prepared.
        _ <- succeeds "first build" acton proj ["build"]
        out <- succeeds "nothing changed" acton proj ["build", "--verbose"]
        let (before, entry) = break ("Derived module schema" `isInfixOf`) (lines out)
        assertBool ("the line of the entry\n" ++ out) (not (null entry))
        assertBool ("the function's module is checked before the line of the entry\n" ++ out) (any ("  Fresh gen: " `isInfixOf`) before)
        assertBool ("no back job is prepared before the line of the entry\n" ++ out) (not (any ("  DBP " `isInfixOf`) before))

  , testCase "an entry that is removed takes its modules and its runner with it" $
      withProject "demo" (overrideFiles demoFiles [("Build.act", buildAct "demo"
          [ "derived_modules = {"
          , "    schema: (function=gen.schema, inputs=[\"data/greeting.txt\"], args=(name=\"world\")),"
          , "    other: (function=gen.schema, inputs=[\"data/greeting.txt\"], args=(name=\"there\"))"
          , "}"
          ])]) $ \acton proj -> do
        _ <- succeeds "first build" acton proj ["build"]
        -- One of two entries is removed.  What the build keeps for it is
        -- removed, and what it keeps for the other entry stays.
        let kept label = [ "out/derived/src" </> label <.> "act", "out/derived" </> label <.> "json", "out/derived/runner/src/runner" </> label <.> "act"
                         , "out/derived/runner" </> label <.> "built", "out/derived/runner/out/bin" </> ("runner." ++ label) ]
        forM_ (kept "schema" ++ kept "other") $ \file ->
          assertBool (file ++ " is in place after the first build") =<< doesFileExist (proj </> file)
        writeFile (proj </> "Build.act") (demoBuildAct "gen.schema")
        -- A file under out/derived/src that no entry names is removed too,
        -- whatever its name.
        writeFile (proj </> "out/derived/src/stray.txt") "stray\n"
        _ <- succeeds "the entry other is removed" acton proj ["build"]
        forM_ ("out/derived/src/stray.txt" : kept "other") $ \file -> assertBool (file ++ " is removed") . not =<< doesPathExist (proj </> file)
        forM_ (kept "schema") $ \file -> assertBool (file ++ " stays") =<< doesFileExist (proj </> file)
        -- The last entry is removed.
        writeFile (proj </> "Build.act") (buildAct "demo" [])
        writeFile (proj </> "src/demo.act") "actor main(env):\n    proc def __startup__():\n        print(\"alone\")\n        env.exit(0)\n"
        _ <- succeeds "second build" acton proj ["build"]
        assertEqual "output" "alone\n" =<< runBinary (proj </> "out/bin/demo")
        assertBool "out/derived is removed" . not =<< doesPathExist (proj </> "out/derived")
        assertBool "the interface of the derived module is removed" . not =<< doesPathExist (proj </> "out/types/demo/schema.tydb")

  , testCase "a build leaves the --tydb copy of a derived module in place" $
      withProject "demo" demoFiles $ \acton proj -> do
        -- A build with --tydb writes a copy of the interface of a module
        -- beside its source file, also for a derived module.  No entry
        -- names the files of the copy.
        let copy = proj </> "out/derived/src/schema.tydb/data.mdb"
        _ <- succeeds "a build with --tydb" acton proj ["build", "--tydb"]
        assertBool "the copy is written" =<< doesFileExist copy
        _ <- succeeds "a build without --tydb" acton proj ["build"]
        assertBool "the copy stays" =<< doesFileExist copy

  , testCase "--skip-build and acton test list run no function" $
      withProject "demo" demoFiles $ \acton proj -> do
        forM_ [["build", "--skip-build"], ["test", "list"]] $ \args -> do
          out <- fails "out of date" acton proj args
          assertBool out ("is out of date, and this build runs no function" `isInfixOf` out)
        _ <- succeeds "build" acton proj ["build"]
        forM_ [["build", "--skip-build"], ["test", "list"]] $ \args ->
          void (succeeds "up to date" acton proj args)

  , testCase "--only-build runs no function" $
      withProject "demo" demoFiles $ \acton proj -> do
        _ <- succeeds "first build" acton proj ["build"]
        writeFile (proj </> "data/greeting.txt") "hi\n"
        out <- succeeds "an input file changed" acton proj ["build", "--only-build"]
        assertBool ("no function runs\n" ++ out) (not ("Derived" `isInfixOf` out))
        assertEqual "output" "hello, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "acton test and a build of one file use derived modules" $
      withProject "demo" (demoFiles ++ [testSchema]) $ \acton proj -> do
        out <- succeeds "acton test" acton proj ["test"]
        assertBool out ("Derived by gen.schema" `isInfixOf` out)
        removePathForcibly (proj </> "out")
        _ <- succeeds "acton build FILE" acton proj ["build", "src/demo.act"]
        assertEqual "output" "hello, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "acton build --watch builds a derived module that a failed generation wrote" $
      -- The module tool is imported by no module.
      withProject "build-watch" (demoFiles ++ [("src/tool.act", "size = 1\n")]) $ \acton proj ->
        withActon acton proj ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          let refused = "Do not use prefix 'demo' when referring to modules in that project"
          awaitLines output "the first build" "Final compilation done" 1
          awaitGeneration proj
          assertBool "the documentation index lists the derived module" . ("demo.schema" `isInfixOf`) =<< readFileStrict (proj </> "out/doc/index.html")
          -- An import that the planning of a build refuses.
          writeFile (proj </> "src/tool.act") "import demo.gen\n"
          awaitLines output "the import in tool is refused" refused 1
          awaitGeneration proj
          -- The generation that the module of the function starts runs the
          -- function and writes the source of schema.  Its planning fails,
          -- so it compiles no module.  A saved source file is a changed
          -- path, so the generations from here on compile what the changed
          -- paths affect, not the whole project.
          writeFile (proj </> "src/gen.act") (replace "\", \"" "\"; \"" genSchema)
          awaitLines output "the run of the function" "Derived by gen.schema" 2
          awaitLines output "the import in tool is refused again" refused 2
          awaitGeneration proj
          -- The generation that the corrected tool starts runs no function.
          -- No module imports gen or tool.  It compiles schema and demo
          -- because the source file of schema is among the changed paths
          -- since the failed generation wrote it.
          writeFile (proj </> "src/tool.act") "size = 2\n"
          awaitLines output "the build with the corrected tool" "Final compilation done" 2
          awaitGeneration proj
          assertEqual "output" "hello; world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "acton build --watch builds when a file is added below an input directory" $
      withProject "watch-added" describedFiles $ \acton proj ->
        withActon acton proj ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          let builds = length . filter ("Building project in " `isPrefixOf`) . lines <$> output
          awaitLines output "the first build" "Final compilation done" 1
          awaitGeneration proj
          -- A file that the input path does not stand for starts no build
          -- in the two seconds after it is written.
          before <- builds
          writeFile (proj </> "data/.hidden") "hidden\n"
          threadDelay 2000000
          assertEqual "the builds that a file whose name starts with a dot starts" before =<< builds
          writeFile (proj </> "data/three.txt") ""
          await "the build that the added file starts" ((\now -> if now > before then Just () else Nothing) <$> builds)
          awaitLines output "the run of the function" "Derived by gen.facts" 2
          awaitLines output "the build after the file was added" "Final compilation done" 2
          awaitGeneration proj
          printed <- runBinary (proj </> "out/bin/described")
          assertBool ("the added file is an input file\n" ++ printed) ("data/three.txt 0 " `isInfixOf` printed)
          -- The watch reads the input paths again when Build.act changes.
          -- A file below a directory that no input path names starts no
          -- build, and a file below that directory starts one once
          -- Build.act names the directory.
          let done = length . filter ("Final compilation done" `isInfixOf`) . lines <$> output
          settled <- builds
          writeFiles proj [("more/early.txt", "x")]
          threadDelay 2000000
          assertEqual "the builds that a file below a directory that no input path names starts" settled =<< builds
          finished <- done
          writeFile (proj </> "Build.act") (described ["data/", "more/"])
          awaitLines output "the build after Build.act changed" "Final compilation done" (finished + 1)
          awaitGeneration proj
          finishedAgain <- done
          writeFile (proj </> "more/four.txt") "yy"
          awaitLines output "the run of the function for the file below the added input path" "Derived by gen.facts" 4
          awaitLines output "the build after that file was added" "Final compilation done" (finishedAgain + 1)
          awaitGeneration proj
          printedAgain <- runBinary (proj </> "out/bin/described")
          assertBool ("the file below the added input path is an input file\n" ++ printedAgain) ("more/four.txt 2 " `isInfixOf` printedAgain)
          -- One save of an input file starts one build, and the function
          -- runs once in it.  The file is put in place by a rename, which
          -- is one change for the system.
          started <- builds
          finishedThird <- done
          putFile proj "data/three.txt" "333"
          awaitLines output "the run of the function for the saved file" "Derived by gen.facts" 5
          awaitLines output "the build after the file was saved" "Final compilation done" (finishedThird + 1)
          awaitGeneration proj
          logText <- output
          assertEqual ("the builds that one save of an input file starts\n" ++ logText) (started + 1) =<< builds
          assertEqual ("the runs of the function\n" ++ logText) 5 (length (filter ("Derived by gen.facts" `isInfixOf`) (lines logText)))

  , testCase "acton build --watch builds when an input file changes after a build that failed in its function" $
      -- The first build of the watch ends in the function, before the
      -- build has a plan.
      withProject "watch-failed" (overrideFiles demoFiles
          [ ("data/greeting.txt", "fail\n")
          , ("src/gen.act", schemaOf "str"
              [ "text = inputs[\"data/greeting.txt\"].read().decode().strip()"
              , "if text == \"fail\":"
              , "    raise ValueError(\"no greeting today\")"
              , "return 'greeting = \"' + text + \", \" + name + '\"\\n'"
              ])
          ]) $ \acton proj ->
        withActon acton proj ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          awaitLines output "the function raises" "no greeting today" 1
          awaitGeneration proj
          writeFile (proj </> "data/greeting.txt") "hi\n"
          awaitLines output "the build after the input file changed" "Final compilation done" 1
          awaitGeneration proj
          assertEqual "output" "hi, world\n" =<< runBinary (proj </> "out/bin/demo")

  , testCase "acton build --watch watches an input directory that appears after the watch started" $
      -- The input path parts/ names nothing when the watch starts.  The
      -- function returns a module for each file below it, and the entry
      -- names the modules of a.txt and b.txt, so the builds fail until
      -- both files are there.
      withProject "watch-appears" [ file | file@(path, _) <- severalFiles, not ("parts/" `isPrefixOf` path) ] $ \acton proj ->
        withActon acton proj ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          awaitLines output "the input path names nothing" "names no file and no directory" 1
          awaitGeneration proj
          createDirectory (proj </> "parts")
          awaitLines output "the function returns no module of part" "Named and not returned: part.a, part.b" 1
          awaitGeneration proj
          writeFile (proj </> "parts/a.txt") "1"
          awaitLines output "the function returns one module of part" "Named and not returned: part.b" 1
          awaitGeneration proj
          writeFile (proj </> "parts/b.txt") "22"
          awaitLines output "the build with both files" "Final compilation done" 1
          awaitGeneration proj
          assertEqual "output" "a b\n1 2\n" =<< runBinary (proj </> "out/bin/several")
          -- The directory of the input path is the project root, which is
          -- also the directory of Build.act.  A change of Build.act still
          -- starts a build.
          appendFile (proj </> "Build.act") "# saved\n"
          awaitLines output "the build after Build.act changed" "Final compilation done" 2
          awaitGeneration proj

  , testCase "acton build --watch builds for an input file after Build.act takes out another input file of its directory" $
      withProject "watch-two" (overrideFiles describedFiles
          [ ("Build.act", described ["data/one.txt", "data/two.txt"])
          , ("data/two.txt", "ab")
          ]) $ \acton proj ->
        withActon acton proj ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          awaitLines output "the first build" "Final compilation done" 1
          awaitGeneration proj
          writeFile (proj </> "Build.act") (described ["data/one.txt"])
          awaitLines output "the run of the function after Build.act changed" "Derived by gen.facts" 2
          awaitLines output "the build after Build.act changed" "Final compilation done" 2
          awaitGeneration proj
          putFile proj "data/one.txt" "hallo, hallo\n"
          awaitLines output "the run of the function after the input file was saved" "Derived by gen.facts" 3
          awaitLines output "the build after the input file was saved" "Final compilation done" 3
          awaitGeneration proj
          printed <- runBinary (proj </> "out/bin/described")
          assertBool ("the saved file is the one input file\n" ++ printed) (["data/one.txt 13 "] == map (take 16) (lines printed))

  , testCase "acton build --watch builds for an input file and an embedded file of a project with 600 input paths" $
      -- macOS gives a process events from about 500 watches, and a watch
      -- beyond them tells of nothing.  The entry names 600 files and the
      -- directory more, and the module described embeds a file.
      let files = [ "many/f" ++ show n ++ ".txt" | n <- [100 .. 699 :: Int] ]
      in withProject "watch-many" (overrideFiles describedFiles
          ([ ("Build.act", described (files ++ ["more/"]))
           , ("more/one.txt", "1")
           , ("embedded/text.txt", "one\n")
           , ("src/described.act", unlines
               [ "import facts"
               , ""
               , "text: bytes = embed(\"embedded/text.txt\")"
               , ""
               , "actor main(env):"
               , "    proc def __startup__():"
               , "        print(len(facts.files), len(text))"
               , "        env.exit(0)"
               ])
           ] ++ [ (file, "x") | file <- files ])) $ \acton proj ->
        withActon acton proj ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          let count text = length . filter (text `isInfixOf`) . lines <$> output
              -- Wait for the build that an action starts.  The function
              -- runs in it the given number of times, and the program then
              -- prints the given line.
              builds label runs expected action = do
                done <- count "Final compilation done"
                ran  <- count "Derived by gen.facts"
                () <- action
                awaitLines output label "Final compilation done" (done + 1)
                awaitGeneration proj
                logText <- output
                assertEqual (label ++ ": the runs of the function\n" ++ logText) (ran + runs) =<< count "Derived by gen.facts"
                assertEqual label expected =<< runBinary (proj </> "out/bin/described")
          builds "the first build" 1 "601 4\n" (return ())
          builds "a file is added below the input directory" 1 "602 4\n" (putFile proj "more/two.txt" "22")
          builds "the embedded file is saved" 0 "602 8\n" (putFile proj "embedded/text.txt" "two two\n")
          -- A directory that is created below the input directory has no
          -- file, so the function does not run.  A file that is added to it
          -- is an input file.
          builds "a directory is created below the input directory" 0 "602 8\n" (createDirectory (proj </> "more/sub"))
          builds "a file is added to the created directory" 1 "603 8\n" (putFile proj "more/sub/three.txt" "333")

  , testCase "acton build --watch builds when an input file of a local dependency changes" $
      withSystemTempDirectory "acton-derived-watch-dep" $ \tmp -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        -- The function gen.table of lib reads no input file.  It runs again
        -- for new content of a file below the input path of its entry.
        writeFiles (tmp </> "lib") (overrideFiles (libFiles "lib")
          [ ("Build.act", buildAct "lib"
              [ "derived_modules = {"
              , "    table: (function=gen.table, inputs=[\"data/\"], args=(owner=\"lib\")),"
              , "    maker: (function=gen.maker)"
              , "}"
              ])
          , ("data/owner.txt", "one\n")
          ])
        writeFiles (tmp </> "app")
          [ ("Build.act", buildAct "app" ["dependencies = {", "    \"lib\": (path=\"../lib\")", "}"])
          , ("src/app.act", "import lib.table\n\nactor main(env):\n    proc def __startup__():\n        print(lib.table.owner)\n        env.exit(0)\n")
          ]
        withActon acton (tmp </> "app") ["build", "--watch", "--color", "never"] $ \_ _ output -> do
          awaitLines output "the first build" "Final compilation done" 1
          awaitGeneration (tmp </> "app")
          writeFile (tmp </> "lib/data/owner.txt") "two\n"
          awaitLines output "the run of the function of lib" "Derived by gen.table" 2
          awaitLines output "the build after the input file of lib changed" "Final compilation done" 2

  , testCase "acton test --watch runs the tests that import a derived module when an input file changes" $
      withProject "test-watch" (demoFiles ++ [testSchema]) $ \acton proj ->
        withActon acton proj ["test", "--watch", "--no-cache", "--color", "never"] $ \_ _ output -> do
          awaitLines output "the first report" "All 1 tests passed" 1
          awaitGeneration proj
          -- The generation that an input file starts runs the function and
          -- then the test, which fails with the new source of schema.
          writeFile (proj </> "data/greeting.txt") "hi\n"
          awaitLines output "the run of the function" "Derived by gen.schema" 2
          awaitGeneration proj
          logText <- output
          assertBool ("the test of test_schema ran with the new source of schema\n" ++ logText) ("A: hi, world B: hello, world" `isInfixOf` logText)

  , testCase "acton test --watch runs the tests that import a changed module, directly or through a derived module" $
      -- The derived module schema imports shared.  The test module
      -- test_schema imports schema, and the test module test_plain imports
      -- shared and plain.
      withProject "test-select" (overrideFiles demoFiles [("src/gen.act", replace "'greeting = " "'import shared\\n\\ngreeting = " genSchema)]
          ++ [ testSchema, ("src/shared.act", "size = 1\n"), ("src/plain.act", "size = 1\n")
             , ("src/test_plain.act", "import testing\nimport shared\nimport plain\n\ndef _test_sizes():\n    testing.assertEqual(shared.size, plain.size)\n")
             ]) $ \acton proj ->
        withActon acton proj ["test", "--watch", "--no-cache", "--color", "never"] $ \_ _ output -> do
          -- Wait for the report with the given number and return its line.
          let report label number = do
                awaitLines output label "tests passed" number
                awaitGeneration proj
                (!! (number - 1)) . filter ("tests passed" `isInfixOf`) . lines <$> output
              ran label tests line = assertBool (label ++ ": " ++ line) (("All " ++ tests ++ " tests passed") `isInfixOf` line)
          ran "the first build runs every test" "2" =<< report "the first report" 1
          -- The watch runs every test when it does not know the changed
          -- paths, as in its first build.  It can get this save before it
          -- has noted the end of the first build, and then it does not know
          -- them for this build either.  So the number of tests in this
          -- report is not checked.
          appendFile (proj </> "src/plain.act") "# saved\n"
          _ <- report "the report after plain was saved" 2
          appendFile (proj </> "src/plain.act") "# saved again\n"
          -- Only test_plain imports plain.
          ran "the tests that run after plain was saved" "1" =<< report "the report after plain was saved again" 3
          appendFile (proj </> "src/shared.act") "# saved\n"
          -- Both test_plain and schema import shared.
          ran "the tests that run after shared was saved" "2" =<< report "the report after shared was saved" 4
          -- No module imports gen, the module of the function.  Its save
          -- changes the source of schema, which only test_schema imports.
          writeFile (proj </> "src/gen.act") (replace "'greeting = " "'import shared\\n\\nsalute = 1\\ngreeting = " genSchema)
          ran "the tests that run after the module of the function was saved" "1" =<< report "the report after gen was saved" 5

  , testCase "the language server runs no function and uses a derived module as the last build left it" $
      -- The derived module has one name for each line of the input file.
      withProject "demo" (overrideFiles demoFiles
          [ ("data/greeting.txt", "greeting\n")
          , ("src/gen.act", schemaOf "str"
              [ "names = inputs[\"data/greeting.txt\"].read().decode().splitlines()"
              , "return \"\".join([n + ' = \"' + name + '\"\\n' for n in names])"
              ])
          ]) $ \acton tmp -> do
        proj <- canonicalizePath tmp
        let demo = "src" </> "demo.act"
            text name = replace "schema.greeting" ("schema." ++ name) (maybe "" id (lookup demo demoFiles))
            reports label expected diags =
              assertBool (label ++ ": " ++ show (map Ae.encode diags)) (any ((expected `isInfixOf`) . LBS.unpack . Ae.encode) diags)
        -- The two builds run while the server does, so the limit is the one
        -- of the waits of these tests.
        withServerWithin 900 proj $ \notify published -> do
          opened notify proj demo (text "greeting")
          reports "before a build" "Type interface file not found for schema" =<< published demo
          assertBool "the server ran no function" . not =<< doesPathExist (proj </> "out/derived")
          _ <- succeeds "first build" acton proj ["build"]
          edited notify proj demo 2 (text "greeting")
          published demo >>= (@?= [])
          edited notify proj demo 3 (text "salute")
          reports "a name that the derived module does not have" "Module schema does not export salute" =<< published demo
          -- New content of the input file changes nothing for the server.
          writeFile (proj </> "data/greeting.txt") "greeting\nsalute\n"
          edited notify proj demo 4 (text "salute")
          reports "an input file changed and no build ran" "Module schema does not export salute" =<< published demo
          _ <- succeeds "second build" acton proj ["build"]
          edited notify proj demo 5 (text "salute")
          published demo >>= (@?= [])
          -- A build writes source of the derived module that does not
          -- parse.  The names of the derived modules stay, and the server
          -- reports the error in the source at the next edit.
          writeFile (proj </> "data/greeting.txt") "greeting\nsalute\n1 +\n"
          _ <- fails "third build" acton proj ["build"]
          edited notify proj demo 6 (text "salute")
          reported <- published ("out/derived/src" </> "schema.act")
          assertBool "the error in the source of the derived module" (not (null reported))

  , testCase "the derived modules of a dependency are brought up to date under the lock of the dependency" $
      withSystemTempDirectory "acton-derived-lock" $ \tmp -> do
        acton <- canonicalizePath "../../dist/bin/acton"
        writeFiles (tmp </> "lib") (libFiles "lib")
        writeFiles (tmp </> "app")
          [ ("Build.act", buildAct "app" ["dependencies = {", "    \"lib\": (path=\"../lib\")", "}"])
          , ("src/app.act", "import lib.table\n\nactor main(env):\n    proc def __startup__():\n        print(lib.table.owner)\n        env.exit(0)\n")
          ]
        -- The test holds the lock of lib while the build of app starts.
        held <- newEmptyMVar
        release <- newEmptyMVar
        _ <- forkIO (withProjectLock (tmp </> "lib") (putMVar held () >> takeMVar release))
        takeMVar held
        (withActon acton (tmp </> "app") ["build", "--color", "never"] $ \ph _ output -> do
          waits <- await "the build of app waits or ends" $ do
            logText <- output
            code <- getProcessExitCode ph
            let notice line = "Waiting for compiler lock in " `isPrefixOf` line && "/lib" `isSuffixOf` line
            return (if any notice (lines logText) then Just True else if isJust code then Just False else Nothing)
          assertBool "the build of app waits for the lock of lib" waits
          putMVar release ()
          code <- await "the end of the build of app" (getProcessExitCode ph)
          logText <- output
          assertEqual logText ExitSuccess code
          assertEqual "output" "from lib\n" =<< runBinary (tmp </> "app/out/bin/app"))
          `finally` tryPutMVar release ()

  , testGroup "errors" (map errorCase errorCases)
  ]

-- | A project that cannot be built: the files that differ from the demo
-- project, the files that are written after a first build that succeeds,
-- and the texts that the output of the failed build contains.  An error
-- case without files of the second kind has no first build.
errorCase :: (String, [(FilePath, String)], [(FilePath, String)], [String]) -> TestTree
errorCase (name, files, later, expected) = testCase name $
    withProject "demo" (overrideFiles demoFiles files) $ \acton proj -> do
      unless (null later) $ do
        _ <- succeeds "the build before the error" acton proj ["build"]
        writeFiles proj later
      out <- fails name acton proj ["build"]
      forM_ expected $ \text -> assertBool ("expected " ++ show text ++ " in\n" ++ out) (text `isInfixOf` out)

-- | What a build prints when the compiler does not accept the call that
-- an entry of the demo project gives: the diagnostic of the compiler at the
-- call, which is on line 6 of the source of the runner, and the message
-- that shows the call as the entry gives it.
refusedCall :: String -> [String]
refusedCall call =
  [ "--> runner/schema.act@6:"
  , "was not built.\nIts entry gives the call\n  " ++ call ++ "\nThe function of a derived module takes"
  ]

errorCases :: [(String, [(FilePath, String)], [(FilePath, String)], [String])]
errorCases =
  [ ( "the module name is a string"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    \"schema\": (function=gen.schema)", "}"])]
    , [], ["to the left of its colon an entry has a module name, or several module names in braces"] )
  , ( "several names in parentheses"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    (schema, other): (function=gen.schema)", "}"])]
    , [], ["to the left of its colon an entry has a module name, or several module names in braces"] )
  , ( "the function is a string"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=\"gen.schema\")", "}"])]
    , [], ["function of the entry schema is a module name followed by the function's name, written without quotes"] )
  , ( "an unknown field"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema, output=\"x\")", "}"])]
    , [], ["has the unknown field output"] )
  , ( "a module in two entries"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema),", "    {other, schema}: (function=gen.schema)", "}"])]
    , [], ["the module schema is named by two entries"] )
  , ( "an input path outside the project"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema, inputs=[\"../other\"])", "}"])]
    , [], ["must be a relative path inside the project"] )
  , ( "an input path in out"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema, inputs=[\"out/x\"])", "}"])]
    , [], ["the input path \"out/x\" of the entry schema is in out, which a build writes and which is not committed"] )
  , ( "a derived module with a source file"
    , [("src/schema.act", "greeting = \"written by hand\"\n")]
    , [], ["and has the source file"] )
  , ( "an input path that names nothing"
    , [("data/greeting.txt", "")]
    , [], ["names no file and no directory"] )
  , ( "a function in a module that does not exist"
    , [("Build.act", demoBuildAct "nowhere.schema")]
    , [], ["the module nowhere is not found"] )
  , ( "a function that does not exist"
    , [("Build.act", demoBuildAct "gen.nothing")]
    , [], ["the module gen does not define nothing"] )
  , ( "a name that is not a function"
    , [("src/gen.act", "schema = 3\n")]
    , [], ["cannot be used: it is not a function.\nThe function of a derived module takes"] )
  , ( "a function whose module has an error"
    , [("src/gen.act", schemaOf "str" ["return 1"])]
      -- The diagnostic of the function's module comes before the message.
    , [], ["--> gen.act@4:12", "are not brought up to date: the modules of their functions have errors"] )
  , ( "the name of the project in front of the module of a function"
    , [("Build.act", demoBuildAct "demo.gen.schema")]
    , [], ["names the module demo.gen with the name of its own project in front.\nWrite function=gen.schema."] )
  , ( "a function in a module whose source file was removed after a build"
    , []
      -- The first build leaves the interface file of gen in out/types.
    , [("Build.act", demoBuildAct "demo.gen.schema"), ("src/gen.act", "")]
    , ["the module demo.gen is not found"] )
  , ( "a function in a derived module of its own project"
    , [("Build.act", buildAct "demo"
          [ "derived_modules = {"
          , "    schema: (function=gen.schema, inputs=[\"data/greeting.txt\"], args=(name=\"world\")),"
          , "    other: (function=schema.make)"
          , "}"
          ])]
    , [], ["names the module schema, which is a derived module of the same project.\nPut the function in a module of the project that has a file under src."] )
  , ( "args written as a dict"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema, args={\"name\": \"world\"})", "}"])]
    , [], ["are the keyword arguments of its function"] )
  , ( "an argument that is not a literal"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema, args=(name=world))", "}"])]
    , [], ["the argument name of the entry schema must be a literal"] )
  , ( "a function with another result type"
    , [("src/gen.act", schemaOf "int" ["return 1"])]
    , [], ["It returns str, the source of one module, or dict[str, str]"] )
  , ( "a function without a parameter for the input files"
    , [("src/gen.act", "def schema(name: str) -> str:\n    return \"greeting = 'x'\\n\"\n")]
    , [], refusedCall "gen.schema(inputs, name=\"world\")" )
  , ( "a function whose first parameter is a dict[str, bytes]"
    , [("src/gen.act", "def schema(inputs: dict[str, bytes], name: str) -> str:\n    return \"greeting = 'x'\\n\"\n")]
    , [], refusedCall "gen.schema(inputs, name=\"world\")"
          ++ ["takes the descriptions of the input files, a dict[str, std.build.derive.InputFile], and then the arguments of its entry."] )
  , ( "an argument of another type than the parameter"
    , [("Build.act", demoBuildActArgs "gen.schema" "name=3")]
    , [], refusedCall "gen.schema(inputs, name=3)" )
  , ( "an argument that the function has no parameter for"
    , [("Build.act", demoBuildActArgs "gen.schema" "name=\"world\", other=[1, 2]")]
    , [], refusedCall "gen.schema(inputs, name=\"world\", other=[1, 2])" )
  , ( "a parameter without an argument"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    schema: (function=gen.schema, inputs=[\"data/greeting.txt\"])", "}"])]
    , [], refusedCall "gen.schema(inputs)" )
  , ( "a function that takes an Env"
    , [("src/gen.act", deriveImport ++ "def schema(inputs: dict[str, derive.InputFile], name: str, env: Env) -> str:\n    return \"greeting = 'x'\\n\"\n")]
    , [], refusedCall "gen.schema(inputs, name=\"world\")" )
  , ( "a function that raises"
    , [("src/gen.act", schemaOf "str" ["raise ValueError(\"no greeting today\")"])]
    , [], ["no greeting today"] )
  , ( "a str result for several names"
    , [("Build.act", buildAct "demo" ["derived_modules = {", "    {schema, other}: (function=gen.schema, inputs=[\"data/greeting.txt\"], args=(name=\"world\"))", "}"])]
    , [], ["it returns str, the source of one module, and the entry names 2 modules"] )
  , ( "returned modules that the entry does not name"
    , [("src/gen.act", schemaOf "dict[str, str]" ["return {\"schema\": \"greeting = 'x'\\n\", \"schema.inner\": \"\", \"elsewhere\": \"\"}"])]
    , [], ["returned other modules than its entry names.\nReturned and not named: elsewhere, schema.inner\nAn entry names exactly"] )
  , ( "a named module that the function does not return"
    , [("src/gen.act", schemaOf "dict[str, str]" ["return {\"elsewhere\": \"greeting = 'x'\\n\"}"])]
    , [], ["Returned and not named: elsewhere\nNamed and not returned: schema\nAn entry names exactly"] )
  , ( "a function that imports a derived module of its own project"
    , []
    , [("src/gen.act", "import schema\n\n" ++ genSchema)], ["imports a derived module of its own project: gen imports schema"] )
  , ( "a derived module with a type error"
    , [("src/gen.act", schemaOf "str" ["return \"greeting: int = 'x'\\n\""])]
      -- The diagnostic names the source of the derived module, and a note
      -- after it says where that source comes from.
    , [], ["--> out/derived/src/schema.act@1:17", "Note: schema is a derived module. The function gen.schema returned its source"] )
  , ( "a derived module whose actor main cannot be the root actor"
    , [ ("src/gen.act", schemaOf "str" ["return \"actor main(n: int):\\n    pass\\n\""])
      , ("src/demo.act", "import schema\n") ]
      -- The error is at the definition of the actor in the source of the
      -- derived module.
    , [], ["--> out/derived/src/schema.act@1:7", "Cannot use main as the root actor"] )
  ]


-- Projects -----------------------------------------------------------------------------------------

-- | A project with the derived module schema, whose function joins the
-- text of an input file and an argument.
demoFiles :: [(FilePath, String)]
demoFiles =
  [ ("Build.act", demoBuildAct "gen.schema")
  , ("data/greeting.txt", "hello\n")
  , ("src/gen.act", genSchema)
  , ("src/demo.act", unlines
      [ "import schema"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(schema.greeting)"
      , "        env.exit(0)"
      ])
  ]

-- | A test module of the demo project that imports the derived module.
testSchema :: (FilePath, String)
testSchema = ("src/test_schema.act", unlines
  [ "import testing"
  , "import schema"
  , ""
  , "def _test_greeting():"
  , "    testing.assertEqual(schema.greeting, \"hello, world\")"
  ])

demoBuildAct :: String -> String
demoBuildAct function = demoBuildActArgs function "name=\"world\""

-- | The Build.act of the demo project with a function and the arguments of
-- its entry as they are written between the parentheses of args.
demoBuildActArgs :: String -> String -> String
demoBuildActArgs function args = buildAct "demo"
  [ "derived_modules = {"
  , "    schema: ("
  , "        function=" ++ function ++ ","
  , "        inputs=[\"data/greeting.txt\"],"
  , "        args=(" ++ args ++ ")"
  , "    )"
  , "}"
  ]

-- | The first lines of a module of functions for derived modules: the
-- import of the module that has the class InputFile, the description of an
-- input file.
deriveImport :: String
deriveImport = "import std.build.derive as derive\n\n"

-- | A module with a function schema for the demo project that has the
-- given result type and the given lines as its body.
schemaOf :: String -> [String] -> String
schemaOf result body = deriveImport ++ unlines (("def schema(inputs: dict[str, derive.InputFile], name: str) -> " ++ result ++ ":") : map ("    " ++) body)

-- | The function of the demo project, which reads its one input file.
genSchema :: String
genSchema = schemaOf "str"
  [ "text = inputs[\"data/greeting.txt\"].read().decode().strip()"
  , "return 'greeting = \"' + text + \", \" + name + '\"\\n'"
  ]

-- | The function of the demo project with an actor that joins the two
-- texts.
genActor :: String
genActor = deriveImport ++ unlines
  [ "actor Greeter(name: str):"
  , "    def greet(text: str) -> str:"
  , "        return text + \", \" + name"
  , ""
  , "def schema(inputs: dict[str, derive.InputFile], name: str) -> str:"
  , "    text = inputs[\"data/greeting.txt\"].read().decode().strip()"
  , "    return 'greeting = \"' + Greeter(name).greet(text) + '\"\\n'"
  ]

-- | A project whose function reads no input file: its derived module holds
-- the path, the size and the SHA-256 of each file below data, from the
-- description of the file.  A path is written as the list of its code
-- points.  The name of one file has braces and a double quote, which a
-- path of embed cannot have.
describedFiles :: [(FilePath, String)]
describedFiles =
  [ ("Build.act", described ["data/"])
  , ("data/one.txt", "hello\n")
  , ("data/t{w}o \"x\".bin", "ab")
  , ("src/gen.act", deriveImport ++ unlines
      [ "def facts(inputs: dict[str, derive.InputFile]) -> str:"
      , "    lines = []"
      , "    for f in inputs.values():"
      , "        lines.append(\"    (\" + str([ord(c) for c in f.path]) + \", \" + str(f.size) + ', \"' + f.sha256 + '\"),')"
      , "    return \"files = [\\n\" + \"\\n\".join(lines) + \"\\n]\\n\""
      ])
  , ("src/described.act", unlines
      [ "import facts"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        for codes, size, sha256 in facts.files:"
      , "            print(\"\".join([chr(c) for c in codes]), size, sha256)"
      , "        env.exit(0)"
      ])
  ]

-- | Put content at a path of a project in one step: the file is written
-- at the project root, where no input path names it, and moved into place.
-- So no build sees a file that is written in part, and the save is one
-- change for the system.
putFile :: FilePath -> FilePath -> String -> IO ()
putFile proj path content = do
    writeFile (proj </> "put.new") content
    renameFile (proj </> "put.new") (proj </> path)

-- | The Build.act of the project described with the given input paths.
described :: [String] -> String
described inputs = buildAct "described" ["derived_modules = {", "    facts: (function=gen.facts, inputs=" ++ show inputs ++ ")", "}"]

-- | A project whose function takes an argument of each kind of literal and
-- has a parameter with a default.  The string holds the characters that a
-- literal has to escape and a character outside ASCII, written as the bytes
-- of its UTF-8 form.  Its derived module model.schema is in a package, so
-- the label of its entry has a dot.
typedFiles :: [(FilePath, String)]
typedFiles =
  [ ("Build.act", buildAct "typed"
      [ "derived_modules = {"
      , "    model.schema: ("
      , "        function=gen.schema,"
      , "        args=(prefix=\"a\\\"b\\\\c{{d}}\\xc3\\xa9\\n\", depth=-4, ratio=2.5, whole=3, flag=True, note=None,"
      , "              names=[\"x\", \"y\"], sizes={\"k\": 1, \"l\": 2}, nested=[[1], []])"
      , "    )"
      , "}"
      ])
  , ("src/gen.act", deriveImport ++ unlines
      [ "def schema(inputs: dict[str, derive.InputFile], prefix: str, depth: int, ratio: float, whole: float, flag: bool, note: ?str,"
      , "           names: list[str], sizes: dict[str, int], nested: list[list[int]], level: int = 7) -> str:"
      , "    return \"\\n\".join(["
      , "        \"codes = \" + str([ord(c) for c in prefix]),"
      , "        \"depth = \" + str(depth),"
      , "        \"ratio = \" + str(ratio),"
      , "        \"whole = \" + str(whole + 0.5),"
      , "        \"flag = \" + str(flag),"
      , "        \"no_note = \" + str(note is None),"
      , "        \"level = \" + str(level),"
      , "        \"name = '\" + names[1] + \"'\","
      , "        \"names = \" + str(len(names)),"
      , "        \"sizes = \" + str(len(sizes)) + \" + \" + str(sizes[\"l\"]) + \" - \" + str(sizes[\"k\"]),"
      , "        \"first = \" + str(nested[0][0]),"
      , "        \"second = \" + str(len(nested[1])),"
      , "    ]) + \"\\n\""
      ])
  , ("src/typed.act", unlines
      [ "import model.schema as schema"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(schema.codes)"
      , "        print(schema.depth, schema.ratio, schema.whole, schema.flag, schema.no_note, schema.level)"
      , "        print(schema.name, schema.names, schema.sizes, schema.first, schema.second)"
      , "        env.exit(0)"
      ])
  ]

-- | A project whose function returns the module names and one module in
-- the package part for every file under parts.  The function reads no
-- input file: a module of part holds the size of its file.
severalFiles :: [(FilePath, String)]
severalFiles =
  [ ("Build.act", buildAct "several"
      [ "derived_modules = {"
      , "    {part.b, part.a, names}: ("
      , "        function=gen.modules,"
      , "        inputs=[\"parts/\"]"
      , "    )"
      , "}"
      ])
  , ("parts/a.txt", "1")
  , ("parts/b.txt", "22")
  , ("src/gen.act", deriveImport ++ unlines
      [ "def modules(inputs: dict[str, derive.InputFile]) -> dict[str, str]:"
      , "    res = {}"
      , "    names = []"
      , "    for path, f in inputs.items():"
      , "        name = path.split(\"/\")[-1].split(\".\")[0]"
      , "        names.append('\"' + name + '\"')"
      , "        res[\"part.\" + name] = \"size = \" + str(f.size) + \"\\n\""
      , "    res[\"names\"] = \"names = [\" + \", \".join(names) + \"]\\n\""
      , "    return res"
      ])
  , ("src/several.act", unlines
      [ "import names"
      , "import part.a"
      , "import part.b"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(\" \".join(names.names))"
      , "        print(part.a.size, part.b.size)"
      , "        env.exit(0)"
      ])
  ]

-- | A dependency with a function for derived modules and two derived
-- modules of its own: table, and maker, which has a function for derived
-- modules.
libFiles :: String -> [(FilePath, String)]
libFiles name =
  [ ("Build.act", buildAct name
      [ "derived_modules = {"
      , "    table: (function=gen.table, args=(owner=\"lib\")),"
      , "    maker: (function=gen.maker)"
      , "}"
      ])
  , ("src/gen.act", deriveImport ++ unlines
      [ "def table(inputs: dict[str, derive.InputFile], owner: str) -> str:"
      , "    return 'owner = \"from ' + owner + '\"\\n'"
      , ""
      , "def maker(inputs: dict[str, derive.InputFile]) -> str:"
      , "    return ('import gen\\nimport std.build.derive as derive\\n\\ndef made(inputs: dict[str, derive.InputFile]) -> str:\\n'"
      , "            + '    return gen.table(inputs, owner=\"a derived module of lib\")\\n')"
      ])
  ]

-- | A project that imports the derived module table of its dependency lib,
-- and has three derived modules: one whose function is in a module of the
-- dependency, one whose function is in a module of the project that
-- imports a module and a derived module of the dependency, and one whose
-- function is in a derived module of the dependency.
appFiles :: String -> [(FilePath, String)]
appFiles name =
  [ ("Build.act", buildAct name
      [ "dependencies = {"
      , "    \"lib\": (path=\"../lib\")"
      , "}"
      , "derived_modules = {"
      , "    local: (function=lib.gen.table, args=(owner=\"app\")),"
      , "    wrapped: (function=appgen.wrapped),"
      , "    made: (function=lib.maker.made)"
      , "}"
      ])
  , ("src/appgen.act", unlines
      [ "import lib.gen"
      , "import lib.table"
      , "import std.build.derive as derive"
      , ""
      , "def wrapped(inputs: dict[str, derive.InputFile]) -> str:"
      , "    return lib.gen.table(inputs, owner=\"a function of app that read \" + lib.table.owner)"
      ])
  , ("src/app.act", unlines
      [ "import lib.table"
      , "import local"
      , "import made"
      , "import wrapped"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(\"lib:\" + lib.table.owner)"
      , "        print(\"app:\" + local.owner)"
      , "        print(\"own:\" + wrapped.owner)"
      , "        print(\"made:\" + made.owner)"
      , "        env.exit(0)"
      ])
  ]

-- | A project whose function returns one module for each name in its
-- argument names.
relabelFiles :: [(FilePath, String)]
relabelFiles =
  [ ("Build.act", buildAct "relabel"
      [ "derived_modules = {"
      , "    {b, c}: (function=gen.modules, args=(names=[\"b\", \"c\"]))"
      , "}"
      ])
  , ("src/gen.act", deriveImport ++ unlines
      [ "def modules(inputs: dict[str, derive.InputFile], names: list[str]) -> dict[str, str]:"
      , "    res = {}"
      , "    for name in names:"
      , "        res[name] = \"name = '\" + name + \"'\\n\""
      , "    return res"
      ])
  , ("src/relabel.act", unlines
      [ "import b"
      , "import c"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(b.name, c.name)"
      , "        env.exit(0)"
      ])
  ]


-- Helpers ------------------------------------------------------------------------------------------

-- | A Build.act with a name, the fingerprint of the name, and more lines.
buildAct :: String -> [String] -> String
buildAct name rest = unlines (
    [ "name = \"" ++ name ++ "\""
    , "fingerprint = " ++ Fingerprint.formatFingerprint (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
    ] ++ rest)

-- | The files with those of the same path replaced, and the rest added.
overrideFiles :: [(FilePath, String)] -> [(FilePath, String)] -> [(FilePath, String)]
overrideFiles base files = [ f | f@(path, _) <- base, path `notElem` map fst files ] ++ files

-- | Write files below a directory.  An empty content removes the file.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles root files = forM_ files $ \(path, content) -> do
    createDirectoryIfMissing True (takeDirectory (root </> path))
    if null content then removePathForcibly (root </> path) else writeFile (root </> path) content

-- | Run a test with the compiler and a project of the given files in a
-- temporary directory.
withProject :: String -> [(FilePath, String)] -> (FilePath -> FilePath -> IO a) -> IO a
withProject name files test =
    withSystemTempDirectory ("acton-derived-" ++ name) $ \tmp -> do
      acton <- canonicalizePath "../../dist/bin/acton"
      let proj = tmp </> name
      writeFiles proj files
      test acton proj

-- | Run acton in a project and return its exit code and what it printed.
runActon :: FilePath -> FilePath -> [String] -> IO (ExitCode, String)
runActon acton proj args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc acton (args ++ ["--color", "never"])) { cwd = Just proj } ""
    return (code, out ++ err)

succeeds :: String -> FilePath -> FilePath -> [String] -> IO String
succeeds label acton proj args = do
    (code, out) <- runActon acton proj args
    assertEqual (label ++ ": acton " ++ unwords args ++ "\n" ++ out) ExitSuccess code
    return out

fails :: String -> FilePath -> FilePath -> [String] -> IO String
fails label acton proj args = do
    (code, out) <- runActon acton proj args
    assertBool (label ++ ": acton " ++ unwords args ++ " should fail\n" ++ out) (code /= ExitSuccess)
    return out

runBinary :: FilePath -> IO String
runBinary binary = do
    (code, out, err) <- readCreateProcessWithExitCode (proc binary []) ""
    assertEqual (binary ++ "\n" ++ err) ExitSuccess code
    return out

-- | Run acton build in a project that has no runner yet, write the given
-- content to a file as soon as the build has written the source of the
-- runner of the entry with the given label, and return the exit code of the
-- build and what it printed.  The build has taken the SHA-256 of the input
-- files by then, and it builds the runner next, which takes seconds.  So
-- the write lands before the function of the entry has run.
buildWithChange :: FilePath -> FilePath -> String -> FilePath -> String -> IO (ExitCode, String)
buildWithChange acton proj label file content =
    withActon acton proj ["build", "--color", "never"] $ \ph _ output -> do
      await "the source of the runner" $ do
        written <- doesFileExist (proj </> "out/derived/runner/src/runner" </> label <.> "act")
        return (if written then Just () else Nothing)
      writeFile (proj </> file) content
      code <- await "the end of the build" (getProcessExitCode ph)
      (,) code <$> output

-- | The waits of these tests last at most 15 minutes.  A wait covers up to
-- three Zig builds, and a Zig build of a new project can take minutes on
-- the macOS workers of CI.  The tests of WatchTests wait 120 seconds: their
-- projects have a script in place of zig.
await :: String -> IO (Maybe a) -> IO a
await = awaitWithin 900

awaitGeneration :: FilePath -> IO ()
awaitGeneration = awaitGenerationWithin 900

-- | Wait until the output of a process has 'count' lines that contain
-- 'text'.
awaitLines :: IO String -> String -> String -> Int -> IO ()
awaitLines output label text count = await label $ do
    logText <- output
    return (if length (filter (text `isInfixOf`) (lines logText)) >= count then Just () else Nothing)

readFileStrict :: FilePath -> IO String
readFileStrict file = do
    content <- readFile file
    length content `seq` return content

-- | Replace every occurrence of a text.
replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go text@(c : rest)
      | take (length old) text == old = new ++ go (drop (length old) text)
      | otherwise                     = c : go rest
