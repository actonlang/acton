{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The expression embed("path").  The messages for a path that names no
-- file as embed takes it: as the command line compiler prints them, and as
-- the language server publishes them for the same project.  Programs that
-- embed files, built through zig and run.  And builds in which an embedded
-- file is not the one that its module was compiled with when zig has run.
module EmbedTests (embedErrorTests, embedProgramTests, embedChangedTests) where

import Control.Exception (IOException, try)
import Control.Monad
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createNamedPipe, createSymbolicLink, nullFileMode, ownerModes, setFileMode, setFileSize)
import System.Process
import qualified System.Timeout as Timeout

import qualified Acton.Fingerprint as Fingerprint
import WatchTests (writeExecutable)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

-- | One message: the name of its snapshots, the path that the module embeds
-- and what the project root holds before the build.
data Case = Case String FilePath (FilePath -> IO ())

-- | A snapshot of each message from both front ends.  Two messages need what
-- not every machine has: a file system that takes one spelling of a name for
-- another, and a user who cannot read a file without permissions.
embedErrorTests :: IO TestTree
embedErrorTests = withSystemTempDirectory "acton-embed-probe" $ \dir -> do
    writeFile (dir </> "Probe") ""
    folds  <- doesFileExist (dir </> "probe")
    setFileMode (dir </> "Probe") nullFileMode
    denied <- either (\(_ :: IOException) -> True) (const False) <$> try (readFile (dir </> "Probe") >>= \s -> length s `seq` return ())
    return $ testGroup "embed errors" $ concatMap snapshots $
         [ Case "no_file"     "web/index.html" (\_ -> return ())
         , Case "directory"   "web"            (\root -> createDirectory (root </> "web"))
         , Case "not_regular" "web/pipe"       (\root -> createDirectory (root </> "web") >> createNamedPipe (root </> "web" </> "pipe") ownerModes)
         , Case "too_large"   "web/big.bin"    (\root -> webFile root "big.bin" "" >> setFileSize (root </> "web" </> "big.bin") 3221225472)
         ]
      ++ [ Case "other_spelling" "web/Index.html" (\root -> webFile root "index.html" "<html></html>\n") | folds ]
      ++ [ Case "unreadable"     "web/app.js"     (\root -> webFile root "app.js" "let a = 1\n" >> setFileMode (root </> "web" </> "app.js") nullFileMode) | denied ]
  where
    webFile root name content = createDirectoryIfMissing True (root </> "web") >> writeFile (root </> "web" </> name) content

    snapshots c@(Case name _ _) =
      [ goldenVsString (name ++ " from the command line compiler") ("test" </> "embed" </> name <.> "golden") (withProject c compilerOutput)
      , goldenVsString (name ++ " from the language server") ("test" </> "embed" </> name <.> "lsp" <.> "golden") (withProject c serverDiagnostics)
      ]

-- | Run an action in a project "shop" with the module src/page.act of a
-- case.  The path of the project root in the result is replaced by
-- /home/user/shop.
withProject :: Case -> (FilePath -> IO String) -> IO LBS.ByteString
withProject (Case _ path setup) run = withSystemTempDirectory "acton-embed" $ \tmp -> do
    root <- (</> "shop") <$> canonicalizePath tmp
    createDirectoryIfMissing True (root </> "src")
    writeFile (root </> "Build.act") (buildAct "shop" [])
    writeFile (root </> "src" </> "page.act") $ unlines
      [ "# The page that the shop serves"
      , "page: bytes = embed(\"" ++ path ++ "\")"
      ]
    setup root
    LBS.pack . T.unpack . T.replace (T.pack root) "/home/user/shop" . T.pack <$> run root

-- | The Build.act of the project 'name': its name, its fingerprint and the
-- given lines.
buildAct :: String -> [String] -> String
buildAct name rest = unlines $
    [ "name = \"" ++ name ++ "\""
    , "fingerprint = " ++ Fingerprint.formatFingerprint (Fingerprint.updateFingerprintPrefix (Fingerprint.fingerprintPrefixForName name) 1)
    ] ++ rest

-- | What a build of the project prints.
compilerOutput :: FilePath -> IO String
compilerOutput root = do
    acton <- canonicalizePath "../../dist/bin/acton"
    (code, out, err) <- readCreateProcessWithExitCode (proc acton ["build", "--color", "never", "--quiet"]) { cwd = Just root } ""
    assertEqual ("the build should fail\nstdout:\n" ++ out ++ "\nstderr:\n" ++ err) (ExitFailure 1) code
    return out

-- | The diagnostics that the language server publishes for src/page.act
-- after the file is opened, one per line: the range and the message.
serverDiagnostics :: FilePath -> IO String
serverDiagnostics root = do
    server <- canonicalizePath "../../dist/bin/lsp-server-acton"
    text   <- readFile file
    withFile "/dev/null" WriteMode $ \devNull ->
      withCreateProcess (proc server []) { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle devNull } $ \(Just hin) (Just hout) _ process -> do
        let send :: Ae.Value -> IO ()
            send message = do
              let body = Ae.encode message
              LBS.hPut hin (LBS.pack ("Content-Length: " ++ show (LBS.length body) ++ "\r\n\r\n") <> body)
              hFlush hin
            message method params = Ae.object ["jsonrpc" Ae..= ("2.0" :: String), "method" Ae..= (method :: String), "params" Ae..= (params :: Ae.Value)]
        send (Ae.object ["jsonrpc" Ae..= ("2.0" :: String), "id" Ae..= (1 :: Int), "method" Ae..= ("initialize" :: String)
                        , "params" Ae..= Ae.object ["processId" Ae..= Ae.Null, "rootUri" Ae..= ("file://" ++ root), "capabilities" Ae..= Ae.object []]])
        send (message "initialized" (Ae.object []))
        send (message "textDocument/didOpen" (Ae.object ["textDocument" Ae..= Ae.object
               ["uri" Ae..= ("file://" ++ file), "languageId" Ae..= ("acton" :: String), "version" Ae..= (1 :: Int), "text" Ae..= text]]))
        published <- Timeout.timeout (120 * 1000000) (diagnosticsOf hout)
        -- The server is gone before the project is removed.
        terminateProcess process
        _ <- waitForProcess process
        case published of
          Nothing    -> assertFailure "the language server published no diagnostics for src/page.act within 120 seconds"
          Just diags -> return (unlines (map line diags))
  where
    file = root </> "src" </> "page.act"

    -- The first diagnostics that are published for the file and are not empty.
    diagnosticsOf hout = do
      message <- received hout
      case Ae.parseMaybe published message of
        Just (uri, diags) | "/src/page.act" `isSuffixOf` uri, not (null diags) -> return diags
        _                                                                    -> diagnosticsOf hout

    published = Ae.withObject "message" $ \o -> do
      method <- o Ae..: "method"
      params <- o Ae..: "params"
      guard (method == ("textDocument/publishDiagnostics" :: String))
      (,) <$> params Ae..: "uri" <*> params Ae..: "diagnostics"

    line :: Ae.Value -> String
    line diag = maybe ("unexpected diagnostic: " ++ LBS.unpack (Ae.encode diag)) id (Ae.parseMaybe text diag)
      where text = Ae.withObject "diagnostic" $ \o -> do
              range   <- o Ae..: "range"
              start   <- range Ae..: "start"
              end     <- range Ae..: "end"
              message <- o Ae..: "message"
              from    <- position start
              to      <- position end
              return ("src/page.act " ++ from ++ "-" ++ to ++ ": " ++ message)
            position p = do
              l <- p Ae..: "line"
              c <- p Ae..: "character"
              return (show (l :: Int) ++ ":" ++ show (c :: Int))

    -- One message of the server: a header with the length of the body, an
    -- empty line and the body.
    received hout = do
      header <- B.hGetLine hout
      case B.stripPrefix "Content-Length: " header of
        Nothing -> received hout
        Just n  -> do
          _    <- B.hGetLine hout
          body <- B.hGet hout (read (B.unpack (B.filter (/= '\r') n)))
          maybe (assertFailure ("the language server sent no JSON: " ++ B.unpack body)) return (Ae.decodeStrict body)


-- Programs -----------------------------------------------------------------------------------------

-- | Programs that embed files, each built through zig and run.
embedProgramTests :: TestTree
embedProgramTests = testGroup "embed programs"
    [ testCase "the files of a project, a dependency and two libraries" projectProgram
    , testCase "a project whose out is a symbolic link" linkedOutProgram
    , testCase "a file that is compiled without a project" looseProgram
    , testCase "a release build for Linux has the data C file as an object file" linuxDataObjects
    ]

-- | Run acton in a directory and return what it prints.  The test fails
-- unless acton succeeds.
runActon :: FilePath -> [String] -> IO String
runActon dir args = do
    exe <- canonicalizePath "../../dist/bin/acton"
    (code, out, err) <- readCreateProcessWithExitCode (proc exe args) { cwd = Just dir } ""
    assertEqual ("acton " ++ unwords args ++ " in " ++ dir ++ "\nstdout:\n" ++ out ++ "\nstderr:\n" ++ err) ExitSuccess code
    return (out ++ err)

-- | What a built program prints.  A program that raises an exception in its
-- root actor does not end, so the run has a time limit.
programOutput :: FilePath -> IO String
programOutput program = do
    result <- Timeout.timeout (120 * 1000000) (readCreateProcessWithExitCode (proc program []) "")
    case result of
      Nothing               -> assertFailure (program ++ " did not end within 120 seconds")
      Just (code, out, err) -> do
        assertEqual (program ++ "\nstdout:\n" ++ out ++ "\nstderr:\n" ++ err) ExitSuccess code
        return out

-- | Write text as UTF-8, whatever the locale is.
writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path = BS.writeFile path . TE.encodeUtf8 . T.pack

-- | The #embed lines of a data C file.
embedLines :: FilePath -> IO [String]
embedLines file = filter ("#embed" `isPrefixOf`) . lines . T.unpack . TE.decodeUtf8 <$> BS.readFile file

-- | A project that embeds a file of every byte value, a file without
-- content, a file of 16 MiB and files whose names have a space, the
-- characters */ and, where file names are UTF-8, a letter outside ASCII.  A
-- function embeds a file and is called more than once.  A dependency embeds
-- the same path as the project, and a static and a dynamic library of the
-- project embed a file each.  A module below a directory of src embeds a
-- file too.  Each #embed line names its file with ../ once per level from
-- the data C file up to the project root.  A debug build and a release
-- build give the same program.
projectProgram :: IO ()
projectProgram = withSystemTempDirectory "acton-embed-program" $ \tmp -> do
    root <- canonicalizePath tmp
    utf8 <- (== "UTF-8") . show <$> getFileSystemEncoding
    let shop      = root </> "shop"
        lib       = root </> "lib"
        named     = "web/a b/x*/index.html"
        letter    = "web/" ++ (if utf8 then "\252" else "u") ++ ".txt"
        bigSize   = 16 * 1024 * 1024 :: Int
        bigByte i = fromIntegral ((i * 7 + (i `shiftR` 8) * 13 + (i `shiftR` 16)) .&. 255)
    forM_ [shop </> "src" </> "pages", shop </> "data", shop </> takeDirectory named, lib </> "src", lib </> "data"] (createDirectoryIfMissing True)
    writeFile (lib </> "Build.act") (buildAct "lib" [])
    writeFile (shop </> "Build.act") $ buildAct "shop"
      [ "dependencies = {"
      , "    \"lib\": (path=\"../lib\")"
      , "}"
      , "libraries = {"
      , "    \"s\": (modules=[\"stat\"], linkage=\"static\"),"
      , "    \"d\": (modules=[\"dyn\"], linkage=\"dynamic\")"
      , "}"
      ]
    BS.writeFile (lib </> "data" </> "a.txt") "from the library\n"
    BS.writeFile (shop </> "data" </> "a.txt") "from the project\n"
    BS.writeFile (shop </> "data" </> "s.txt") "static\n"
    BS.writeFile (shop </> "data" </> "d.txt") "dynamic\n"
    BS.writeFile (shop </> "data" </> "bytes.bin") (BS.pack [0 .. 255])
    BS.writeFile (shop </> "data" </> "empty.bin") ""
    BS.writeFile (shop </> "data" </> "big.bin") (fst (BS.unfoldrN bigSize (\i -> Just (bigByte i, i + 1)) (0 :: Int)))
    BS.writeFile (shop </> named) "<html></html>\n"
    BS.writeFile (shop </> letter) "a letter\n"
    writeFile (lib </> "src" </> "text.act") "data = embed(\"data/a.txt\")\n"
    writeFile (shop </> "src" </> "stat.act") "data = embed(\"data/s.txt\")\n"
    writeFile (shop </> "src" </> "pages" </> "home.act") "data = embed(\"data/a.txt\")\n"
    writeFile (shop </> "src" </> "dyn.act") $ unlines
      [ "def data() -> bytes:"
      , "    return embed(\"data/d.txt\")"
      ]
    writeUtf8 (shop </> "src" </> "main.act") $ unlines
      [ "import lib.text"
      , "import stat"
      , "import dyn"
      , "import pages.home"
      , ""
      , "data = embed(\"data/a.txt\")"
      , "every = embed(\"data/bytes.bin\")"
      , "empty = embed(\"data/empty.bin\")"
      , "big = embed(\"data/big.bin\")"
      , "named = [embed(\"" ++ named ++ "\"), embed(\"" ++ letter ++ "\")]"
      , ""
      , "def page() -> bytes:"
      , "    return embed(\"" ++ named ++ "\")"
      , ""
      , "def every_ok() -> bool:"
      , "    for i in range(256):"
      , "        if every[i] != i:"
      , "            return False"
      , "    return True"
      , ""
      , "def big_ok() -> bool:"
      , "    for i in list(range(0, len(big), 4099)) + [len(big) - 1]:"
      , "        if big[i] != (i * 7 + (i >> 8) * 13 + (i >> 16)) & 255:"
      , "            return False"
      , "    return True"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(data, lib.text.data, stat.data, dyn.data(), pages.home.data)"
      , "        print(len(every), every_ok(), len(empty), empty == b\"\")"
      , "        print(len(big), big_ok())"
      , "        print(named)"
      , "        for i in range(3):"
      , "            print(page())"
      , "        env.exit(0)"
      ]
    forM_ [[], ["--release"]] $ \mode -> do
      _   <- runActon shop (["build", "--color", "never"] ++ mode)
      forM_ [ (shop </> "out" </> "types" </> "shop" </> "main.embed.c", "../../../", ["data/a.txt", "data/big.bin", "data/bytes.bin", named, letter])
            , (shop </> "out" </> "types" </> "shop" </> "pages" </> "home.embed.c", "../../../../", ["data/a.txt"])
            , (lib </> "out" </> "types" </> "lib" </> "text.embed.c", "../../../", ["data/a.txt"])
            ] $ \(file, prefix, embedded) -> do
        found <- embedLines file
        assertEqual ("the #embed lines of " ++ file) [ "#embed \"" ++ prefix ++ path ++ "\"" | path <- embedded ] found
      out <- programOutput (shop </> "out" </> "bin" </> "main")
      assertEqual ("what the program of acton build " ++ unwords mode ++ " prints") expected out
  where
    expected = unlines
      [ "b'from the project\\n' b'from the library\\n' b'static\\n' b'dynamic\\n' b'from the project\\n'"
      , "256 True 0 True"
      , "16777216 True"
      , "[b'<html></html>\\n', b'a letter\\n']"
      , "b'<html></html>\\n'"
      , "b'<html></html>\\n'"
      , "b'<html></html>\\n'"
      ]

-- | A project whose out is a symbolic link to a directory at another depth.
-- The data C file names the embedded file by its absolute path.  From the
-- real directory of the data C file, the path that leads to the project root
-- of a project without such a link names another file of the same size,
-- which must not be embedded.
linkedOutProgram :: IO ()
linkedOutProgram = withSystemTempDirectory "acton-embed-linked-out" $ \tmp -> do
    root <- canonicalizePath tmp
    let shop = root </> "deep" </> "er" </> "shop"
        real = root </> "elsewhere" </> "out"
    forM_ [shop </> "src", shop </> "web", real, root </> "elsewhere" </> "web"] (createDirectoryIfMissing True)
    createDirectoryLink real (shop </> "out")
    writeFile (shop </> "Build.act") (buildAct "shop" [])
    BS.writeFile (shop </> "web" </> "app.js") "the file of the project\n"
    BS.writeFile (root </> "elsewhere" </> "web" </> "app.js") "the file of another dir\n"
    writeFile (shop </> "src" </> "main.act") $ unlines
      [ "app = embed(\"web/app.js\")"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(app)"
      , "        env.exit(0)"
      ]
    _ <- runActon shop ["build", "--color", "never"]
    embedded <- embedLines (real </> "types" </> "shop" </> "main.embed.c")
    embedded @?= ["#embed \"" ++ shop </> "web" </> "app.js" ++ "\""]
    out <- programOutput (real </> "bin" </> "main")
    out @?= "b'the file of the project\\n'\n"

-- | A file that is compiled without a project embeds a file beside itself.
-- Two such files with the same name, in two directories, are built with one
-- --tempdir and with --dbg-no-lines, under which the generated code of the
-- module does not name its source file.  Each program gets the file of its
-- own directory, also when the two sources and the two embedded files have
-- the same content, so that only the data C file tells the builds apart.
looseProgram :: IO ()
looseProgram = withSystemTempDirectory "acton-embed-loose" $ \tmp -> do
    root <- canonicalizePath tmp
    let scratch     = root </> "scratch"
        build dir   = runActon root [dir </> "prog.act", "--color", "never", "--tempdir", scratch, "--dbg-no-lines"]
        text dir    = root </> dir </> "text.txt"
        printed dir = programOutput (root </> dir </> "prog")
    createDirectory scratch
    forM_ ["one", "two"] $ \dir -> do
      createDirectory (root </> dir)
      BS.writeFile (text dir) (B.pack ("in " ++ dir ++ "\n"))
      writeFile (root </> dir </> "prog.act") $ unlines
        [ "text = embed(\"text.txt\")"
        , ""
        , "actor main(env):"
        , "    proc def __startup__():"
        , "        print(text)"
        , "        env.exit(0)"
        ]
    forM_ ["one", "two", "one"] $ \dir -> do
      _ <- build dir
      printed dir >>= (@?= "b'in " ++ dir ++ "\\n'\n")
    forM_ ["one", "two"] $ \dir -> BS.writeFile (text dir) "the same\n"
    forM_ ["one", "two", "one"] $ \dir -> do
      _ <- build dir
      embedded <- embedLines (scratch </> "out" </> "types" </> "prog.embed.c")
      embedded @?= ["#embed \"" ++ text dir ++ "\""]
      printed dir >>= (@?= "b'the same\\n'\n")
    -- Without --tempdir, and from another working directory.
    BS.writeFile (text "two") "from another directory\n"
    _ <- runActon scratch [root </> "two" </> "prog.act", "--color", "never"]
    printed "two" >>= (@?= "b'from another directory\\n'\n")

-- | A release build for Linux compiles C to bitcode for link-time
-- optimization.  The data C file of a module is compiled to an object file
-- all the same, in the library of the project and in a library that the
-- project declares.
linuxDataObjects :: IO ()
linuxDataObjects = withSystemTempDirectory "acton-embed-linux" $ \tmp -> do
    root <- canonicalizePath tmp
    zig  <- canonicalizePath "../../dist/zig/zig"
    let shop = root </> "shop"
        members archive = do
          let dir = root </> takeBaseName archive
          createDirectory dir
          _     <- readCreateProcess (proc zig ["ar", "x", shop </> "out" </> "lib" </> archive]) { cwd = Just dir } ""
          names <- sort <$> listDirectory dir
          forM names $ \name -> (,) name . BS.take 4 <$> BS.readFile (dir </> name)
    forM_ [shop </> "src", shop </> "data"] (createDirectoryIfMissing True)
    writeFile (shop </> "Build.act") $ buildAct "shop"
      [ "libraries = {"
      , "    \"s\": (modules=[\"stat\"], linkage=\"static\")"
      , "}"
      ]
    BS.writeFile (shop </> "data" </> "a.txt") "a\n"
    writeFile (shop </> "src" </> "stat.act") "data = embed(\"data/a.txt\")\n"
    writeFile (shop </> "src" </> "main.act") $ unlines
      [ "import stat"
      , ""
      , "data = embed(\"data/a.txt\")"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(data, stat.data)"
      , "        env.exit(0)"
      ]
    _ <- runActon shop ["build", "--color", "never", "--release", "--target", "x86_64-linux-gnu.2.27"]
    project  <- members "libActonProject.a"
    declared <- members "libs.a"
    project  @?= [("main.embed.o", elf), ("main.o", bitcode)]
    declared @?= [("stat.embed.o", elf), ("stat.o", bitcode)]
  where
    elf     = "\DELELF"
    bitcode = "BC\192\222"


-- After zig ----------------------------------------------------------------------------------------

-- | The look at the embedded files after zig.  The C compiler reads an
-- embedded file when zig runs, which is after the build looked at the file.
-- A file that is then not the one that the build took it for stops the build
-- with a message that names it, whatever zig made of it.
embedChangedTests :: TestTree
embedChangedTests = testGroup "embedded files after zig"
    [ testCase "other content of the same size while zig runs" changedWhileZigRuns
    , testCase "a file that is removed while zig runs" removedWhileZigRuns
    , testCase "the file of a source file without a project" looseChangedWhileZigRuns
    , testCase "a file that changed before a build with --only-build" changedBeforeOnlyBuild
    ]

-- | The message for a file that is not the one that the plan of the build
-- saw, as the command line compiler prints it.
whileBuilding :: String
whileBuilding = "acton: data/value.txt changed while the build ran. Run the build again\n"

-- | Run acton in a directory and return its exit code and what it prints to
-- standard output and to standard error.
actonResult :: FilePath -> [String] -> IO (ExitCode, String, String)
actonResult dir args = do
    exe <- canonicalizePath "../../dist/bin/acton"
    readCreateProcessWithExitCode (proc exe args) { cwd = Just dir } ""

-- | Run an action on a project "shop" below a new directory.  Its module
-- main embeds data/value.txt, which holds "first\n", tests the size of the
-- content and prints the content.  The action gets the project root and a
-- function that makes a system directory for --syspath: the distribution
-- with a zig that is a shell script of the given lines.  The script runs in
-- the working directory of the build, which is the project root, and
-- compiles nothing.  So its lines change a file at the time zig runs, after
-- the front passes looked at the file, and its exit code is that of zig.
withShop :: (FilePath -> ([String] -> IO FilePath) -> IO ()) -> IO ()
withShop action = withSystemTempDirectory "acton-embed-changed" $ \tmp -> do
    root <- canonicalizePath tmp
    dist <- canonicalizePath "../../dist"
    let shop   = root </> "shop"
        system = root </> "system"
        zig    = system </> "zig" </> "zig"
    forM_ [shop </> "src", shop </> "data", system </> "zig"] (createDirectoryIfMissing True)
    entries <- listDirectory dist
    forM_ (filter (/= "zig") entries) $ \entry -> createSymbolicLink (dist </> entry) (system </> entry)
    writeFile (shop </> "Build.act") (buildAct "shop" [])
    BS.writeFile (shop </> "data" </> "value.txt") "first\n"
    writeFile (shop </> "src" </> "main.act") $ unlines
      [ "import testing"
      , ""
      , "text = embed(\"data/value.txt\")"
      , ""
      , "def _test_size():"
      , "    testing.assertEqual(len(text), 6)"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(text)"
      , "        env.exit(0)"
      ]
    action shop $ \script -> do
      writeExecutable zig script
      return system

-- | Other content of the same size is written to an embedded file while zig
-- runs, and zig succeeds, as it does for a file of the size that the data C
-- file asserts.  The build fails all the same, and so does the build of
-- acton test.  The next build takes the file as it is.  The file that a
-- build starts with was last modified some seconds ago, so that the file
-- that zig leaves has another stat on a file system with coarse timestamps
-- too.
changedWhileZigRuns :: IO ()
changedWhileZigRuns = withShop $ \shop systemWith -> do
    system <- systemWith ["printf 'other\\n' > data/value.txt"]
    now    <- getCurrentTime
    forM_ ["build", "test"] $ \command -> do
      BS.writeFile (shop </> "data" </> "value.txt") "first\n"
      setModificationTime (shop </> "data" </> "value.txt") (addUTCTime (-30) now)
      (code, out, err) <- actonResult shop [command, "--color", "never", "--syspath", system]
      let printed = "acton " ++ command ++ "\nstdout:\n" ++ out ++ "\nstderr:\n" ++ err
      assertEqual printed (ExitFailure 1) code
      assertEqual printed whileBuilding err
    _ <- runActon shop ["build", "--color", "never"]
    programOutput (shop </> "out" </> "bin" </> "main") >>= (@?= "b'other\\n'\n")

-- | An embedded file is removed while zig runs, and zig fails, as it does
-- when the C compiler does not find the file.  The build names the file,
-- and does not report the failure of zig as an error of the compiler.
removedWhileZigRuns :: IO ()
removedWhileZigRuns = withShop $ \shop systemWith -> do
    system <- systemWith ["rm data/value.txt", "echo 'the C compiler found no file' >&2", "exit 1"]
    (code, out, err) <- actonResult shop ["build", "--color", "never", "--syspath", system]
    let printed = "stdout:\n" ++ out ++ "\nstderr:\n" ++ err
    assertEqual printed (ExitFailure 1) code
    assertEqual printed whileBuilding err
    assertBool ("did not expect the report of a failed zig\n" ++ printed)
      (not (any (`isInfixOf` out) ["internal compiler error", "the C compiler found no file"]))

-- | A file that is compiled without a project embeds a file beside itself.
-- Content of another size is written to that file while zig runs, and zig
-- fails, as it does for a file of another size than the data C file asserts.
-- The build names the file by the path that the source file embeds.
looseChangedWhileZigRuns :: IO ()
looseChangedWhileZigRuns = withShop $ \shop systemWith -> do
    let root    = takeDirectory shop
        loose   = root </> "loose"
        scratch = root </> "scratch"
    forM_ [loose, scratch] createDirectory
    BS.writeFile (loose </> "text.txt") "first\n"
    writeFile (loose </> "prog.act") $ unlines
      [ "text = embed(\"text.txt\")"
      , ""
      , "actor main(env):"
      , "    proc def __startup__():"
      , "        print(text)"
      , "        env.exit(0)"
      ]
    system <- systemWith ["echo 'another version' > \"" ++ loose </> "text.txt" ++ "\"", "exit 1"]
    (code, out, err) <- actonResult loose ["prog.act", "--color", "never", "--syspath", system, "--tempdir", scratch]
    let printed = "stdout:\n" ++ out ++ "\nstderr:\n" ++ err
    assertEqual printed (ExitFailure 1) code
    assertEqual printed "acton: text.txt changed while the build ran. Run the build again\n" err

-- | A build with --only-build runs no front pass, so the module is not
-- compiled again for new content of a file that it embeds, or for a file
-- that is gone.  The build fails after zig.  A build without --only-build
-- then compiles the module with the new content.
changedBeforeOnlyBuild :: IO ()
changedBeforeOnlyBuild = withShop $ \shop _ -> do
    let file = shop </> "data" </> "value.txt"
        onlyBuild = do
          (code, out, err) <- actonResult shop ["build", "--color", "never", "--only-build"]
          let printed = "stdout:\n" ++ out ++ "\nstderr:\n" ++ err
          assertEqual printed (ExitFailure 1) code
          assertEqual printed "acton: data/value.txt changed since its module was compiled. Run the build without --only-build\n" err
    _ <- runActon shop ["build", "--color", "never"]
    BS.writeFile file "other\n"
    onlyBuild
    removeFile file
    onlyBuild
    BS.writeFile file "third\n"
    _ <- runActon shop ["build", "--color", "never"]
    programOutput (shop </> "out" </> "bin" </> "main") >>= (@?= "b'third\\n'\n")
