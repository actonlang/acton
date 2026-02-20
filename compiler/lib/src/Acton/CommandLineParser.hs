{-# LANGUAGE CPP #-}
module Acton.CommandLineParser where

import Options.Applicative

#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
defTarget = "aarch64-macos-none"
#elif defined(darwin_HOST_OS) && defined(x86_64_HOST_ARCH)
defTarget = "x86_64-macos-none"
#elif defined(linux_HOST_OS) && defined(aarch64_HOST_ARCH)
defTarget = "aarch64-linux-gnu.2.27"
#elif defined(linux_HOST_OS) && defined(x86_64_HOST_ARCH)
defTarget = "x86_64-linux-gnu.2.27"
#else
#error "Unsupported platform"
#endif

parseCmdLine        :: IO CmdLineOptions
parseCmdLine        = execParser (info (cmdLineParser <**> helper) descr)

data CmdLineOptions = CompileOpt [String] GlobalOptions CompileOptions
                    | CmdOpt GlobalOptions Command
                    deriving Show

data ColorWhen = Auto | Always | Never deriving (Show, Eq)

data OptimizeMode = Debug | ReleaseSafe | ReleaseSmall | ReleaseFast deriving (Show, Eq)

data GlobalOptions = GlobalOptions {
                        color        :: ColorWhen,
                        quiet        :: Bool,
                        timing       :: Bool,
                        tty          :: Bool,
                        verbose      :: Bool,
                        verboseZig   :: Bool,
                        jobs         :: Int
                     } deriving Show

data Command        = New NewOptions
                    | Build BuildOptions
                    | Test TestCommand
                    | Fetch
                    | PkgShow
                    | PkgAdd PkgAddOptions
                    | PkgRemove PkgRemoveOptions
                    | PkgUpgrade PkgUpgradeOptions
                    | PkgUpdate
                    | PkgSearch PkgSearchOptions
                    | BuildSpecCmd BuildSpecCommand
                    | Cloud CloudOptions
                    | Doc DocOptions
                    | ZigPkgAdd ZigPkgAddOptions
                    | ZigPkgRemove ZigPkgRemoveOptions
                    | Version
                    deriving Show


data NewOptions     = NewOptions {
                         file      :: String
                    }  deriving Show

data CompileOptions   = CompileOptions {
                         alwaysbuild :: Bool,
                         ignore_compiler_version :: Bool,
                         db          :: Bool,
                         parse       :: Bool,
                         parse_ast   :: Bool,
                         kinds       :: Bool,
                         types       :: Bool,
                         sigs        :: Bool,
                         norm        :: Bool,
                         deact       :: Bool,
                         cps         :: Bool,
                         llift       :: Bool,
                         box         :: Bool,
                         hgen        :: Bool,
                         cgen        :: Bool,
                         ccmd        :: Bool,
                         ty          :: Bool,
                         cpedantic   :: Bool,
                         dbg_no_lines:: Bool,
                         optimize    :: OptimizeMode,
                         only_build  :: Bool,
                         skip_build  :: Bool,
                         watch       :: Bool,
                         no_threads  :: Bool,
                         root        :: String,
                         tempdir     :: String,
                         syspath     :: String,
                         target      :: String,
                         cpu         :: String,
                         test        :: Bool,
                         searchpath  :: [String],
                         dep_overrides :: [(String,String)]
                     } deriving Show

data BuildOptions = BuildOptions
    { buildCompile :: CompileOptions
    , buildFiles   :: [String]
    } deriving Show



data CloudOptions   = CloudOptions {
                         run  :: Bool,
                         list :: Bool,
                         show :: Bool,
                         stop :: Bool
                    }  deriving Show


data DocOptions     = DocOptions {
                         inputFile :: String,
                         outputFormat :: Maybe DocFormat,
                         outputFile :: Maybe String
                    }  deriving Show

data DocFormat = AsciiFormat | MarkdownFormat | HtmlFormat deriving (Show, Eq)

data TestCommand
    = TestRun TestOptions
    | TestList TestOptions
    | TestPerf TestOptions
    deriving Show

data TestOptions = TestOptions
    { testCompile      :: CompileOptions
    , testShowLog      :: Bool
    , testShowCached   :: Bool
    , testNoCache      :: Bool
    , testJson         :: Bool
    , testRecord       :: Bool
    , testSnapshotUpdate :: Bool
    , testIter         :: Int
    , testMaxIter      :: Int
    , testMinIter      :: Int
    , testMaxTime      :: Int
    , testMinTime      :: Int
    , testTags         :: [String]
    , testModules      :: [String]
    , testNames        :: [String]
    } deriving Show

data PkgAddOptions = PkgAddOptions
    { pkgAddName     :: String
    , pkgAddUrl      :: String
    , pkgAddRepoUrl  :: String
    , pkgAddRepoRef  :: String
    , pkgAddPkgName  :: String
    , pkgAddHash     :: String
    , pkgAddGithubToken :: String
    } deriving Show

data PkgRemoveOptions = PkgRemoveOptions
    { pkgRemoveName :: String
    } deriving Show

data PkgUpgradeOptions = PkgUpgradeOptions
    { pkgUpgradeGithubToken :: String
    } deriving Show

data PkgSearchOptions = PkgSearchOptions
    { pkgSearchTerms :: [String]
    } deriving Show

data ZigPkgAddOptions = ZigPkgAddOptions
    { zigPkgAddUrl       :: String
    , zigPkgAddName      :: String
    , zigPkgAddArtifacts :: [String]
    } deriving Show

data ZigPkgRemoveOptions = ZigPkgRemoveOptions
    { zigPkgRemoveName :: String
    } deriving Show

--------------------------------------------------------------------
-- Internal stuff

cmdLineParser       :: Parser CmdLineOptions
cmdLineParser       = hsubparser
                        (  command "new"     (info (CmdOpt <$> globalOptions <*> (New <$> newOptions)) (progDesc "Create a new Acton project"))
                        <> command "build"   (info (CmdOpt <$> globalOptions <*> (Build <$> buildOptions)) (progDesc "Build an Acton project"))
                        <> command "test"    (info (CmdOpt <$> globalOptions <*> (Test <$> testCommand)) (progDesc "Build and run project tests"))
                        <> command "fetch"   (info (CmdOpt <$> globalOptions <*> pure Fetch) (progDesc "Fetch project dependencies (offline prep)"))
                        <> command "pkg"     (info (CmdOpt <$> globalOptions <*> pkgSubcommands) (progDesc "Package/dependency commands"))
                        <> command "zig-pkg" (info (CmdOpt <$> globalOptions <*> zigPkgSubcommands) (progDesc "Zig package dependency commands"))
                        <> command "spec"    (info (CmdOpt <$> globalOptions <*> (BuildSpecCmd <$> buildSpecCommand)) (progDesc "Inspect or update build specification"))
                        <> command "cloud"   (info (CmdOpt <$> globalOptions <*> (Cloud <$> cloudOptions)) (progDesc "Run an Acton project in the cloud"))
                        <> command "doc"     (info (CmdOpt <$> globalOptions <*> (Doc <$> docOptions)) (progDesc "Show type and docstring info"))
                        <> command "version" (info (CmdOpt <$> globalOptions <*> pure Version) (progDesc "Show version"))
                      )
                     <|> (CompileOpt <$> (fmap (:[]) $ argument str (metavar "ACTONFILE" <> help "Compile Acton file" <> completer (bashCompleter "file -X '!*.act' -o plusdirs"))) <*> globalOptions <*> compileOptions)

globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
    <$> option colorReader
        (long "color"
         <> metavar "WHEN"
         <> value Auto
         <> help "Use colored output (WHEN: auto, always, never)"
        )
    <*> switch (long "quiet"       <> help "Don't print stuff")
    <*> switch (long "timing"      <> help "Print timing information")
    <*> switch (long "tty"         <> help "Act as if run from interactive TTY")
    <*> switch (long "verbose"     <> help "Verbose output")
    <*> switch (long "verbose-zig" <> help "Verbose Zig output")
    <*> option auto (long "jobs"   <> metavar "N" <> value 0 <> help "Max parallel compiler jobs (0 = auto)")
  where
    colorReader :: ReadM ColorWhen
    colorReader = eitherReader $ \s ->
        case s of
            "auto"   -> Right Auto
            "always" -> Right Always
            "never"  -> Right Never
            _        -> Left $ "Invalid color option: " ++ s ++ " (expected: auto, always, never)"

optimizeReader :: ReadM OptimizeMode
optimizeReader = eitherReader $ \s ->
    case s of
        "Debug"        -> Right Debug
        "ReleaseSafe"  -> Right ReleaseSafe
        "ReleaseSmall" -> Right ReleaseSmall
        "ReleaseFast"  -> Right ReleaseFast
        _              -> Left $ "Invalid optimize option: " ++ s ++ " (expected: Debug, ReleaseSafe, ReleaseSmall, ReleaseFast)"


{-
generalOptions         = GeneralOptions <$>
                             strOption (long "tempdir" <> metavar "TEMPDIR" <> value "" <> help "Set temporary directory for build files")
                         <*> strOption (long "syspath" <> metavar "TARGETDIR" <>  value "" <> help "Set syspath")
                         <*> switch (long "dev"        <> help "Development mode; include debug symbols etc")
 -}

newOptions = NewOptions <$> argument (str :: ReadM String) (metavar "PROJECTDIR")

buildOptions :: Parser BuildOptions
buildOptions = BuildOptions
        <$> compileOptions
        <*> many (argument str (metavar "ACTONFILE" <> help "Specific .act file(s) to build"))

compileOptions = CompileOptions
        <$> switch (long "always-build" <> help "Show the result of parsing")
        <*> switch (long "ignore-compiler-version" <> help "Ignore acton version when checking .ty freshness")
        <*> switch (long "db"           <> help "Enable DB backend")
        <*> switch (long "parse"        <> help "Show the result of parsing")
        <*> switch (long "parse-ast"    <> help "Show the raw AST (Haskell Show)")
        <*> switch (long "kinds"        <> help "Show all the result after kind-checking")
        <*> switch (long "types"        <> help "Show all inferred expression types")
        <*> switch (long "sigs"         <> help "Show the inferred type signatures")
        <*> switch (long "norm"         <> help "Show the result after syntactic normalization")
        <*> switch (long "deact"        <> help "Show the result after deactorization")
        <*> switch (long "cps"          <> help "Show the result after CPS conversion")
        <*> switch (long "llift"        <> help "Show the result of lambda-lifting")
        <*> switch (long "box"          <> help "Show the result of (un)boxing")
        <*> switch (long "hgen"         <> help "Show the generated .h header")
        <*> switch (long "cgen"         <> help "Show the generated .c code")
        <*> switch (long "ccmd"         <> help "Show CC / LD commands")
        <*> switch (long "ty"           <> help "Write .ty file to src file directory")
        <*> switch (long "cpedantic"    <> help "Pedantic C compilation with -Werror")
        <*> switch (long "dbg-no-lines" <> help "Disable emission of C #line directives (for debugging codegen)")
        <*> optimizeOption
        <*> switch (long "only-build"   <> help "Only perform final build of .c files, do not compile .act files")
        <*> switch (long "skip-build"   <> help "Skip final bulid of .c files")
        <*> switch (long "watch"        <> help "Rebuild on file changes")
        <*> switch (long "no-threads"   <> help "Don't use threads")
        <*> strOption (long "root"      <> metavar "ROOTACTOR" <> value "" <> help "Set root actor")
        <*> strOption (long "tempdir"   <> metavar "TEMPDIR" <> value "" <> help "Set directory for build files")
        <*> strOption (long "syspath"   <> metavar "TARGETDIR" <> value "" <> help "Set syspath")
        <*> strOption (long "target"    <> metavar "TARGET" <> value defTarget <> help "Target, e.g. x86_64-linux-gnu.2.28")
        <*> strOption (long "cpu"       <> metavar "CPU" <> value "" <> help "CPU, e.g. skylake")
        <*> switch (long "test"         <> help "Build tests")
        <*> many (strOption (long "searchpath" <> metavar "DIR" <> help "Add search path"))
        <*> many (option depOverrideReader
               (long "dep"
                <> metavar "NAME=PATH"
                <> help "Override dependency NAME with local PATH"))

pkgSubcommands :: Parser Command
pkgSubcommands = hsubparser
  (  command "show"    (info (pure PkgShow) (progDesc "Show dependency tree with overrides"))
  <> command "add"     (info (PkgAdd <$> pkgAddOptions) (progDesc "Add package dependency"))
  <> command "remove"  (info (PkgRemove <$> pkgRemoveOptions) (progDesc "Remove package dependency"))
  <> command "upgrade" (info (PkgUpgrade <$> pkgUpgradeOptions) (progDesc "Upgrade (or downgrade) package dependency"))
  <> command "update"  (info (pure PkgUpdate) (progDesc "Update package index"))
  <> command "search"  (info (PkgSearch <$> pkgSearchOptions) (progDesc "Search package index"))
  )

githubTokenOption :: Parser String
githubTokenOption =
    strOption
      (  long "github-token"
      <> metavar "TOKEN"
      <> value ""
      <> help "GitHub token for API requests (env: GITHUB_TOKEN)"
      )

pkgAddOptions :: Parser PkgAddOptions
pkgAddOptions = PkgAddOptions
    <$> argument str (metavar "NAME" <> help "Name of dependency")
    <*> strOption (long "url" <> metavar "URL" <> value "" <> help "URL of dependency")
    <*> strOption (long "repo-url" <> metavar "URL" <> value "" <> help "Git repository URL of dependency")
    <*> strOption (long "repo-ref" <> metavar "REF" <> value "" <> help "Git ref (branch, tag or SHA) to use")
    <*> strOption (long "pkg-name" <> metavar "NAME" <> value "" <> help "Package name in index (defaults to NAME)")
    <*> strOption (long "hash" <> metavar "HASH" <> value "" <> help "Hash of dependency")
    <*> githubTokenOption

pkgRemoveOptions :: Parser PkgRemoveOptions
pkgRemoveOptions =
    PkgRemoveOptions <$> argument str (metavar "NAME" <> help "Name of dependency")

pkgUpgradeOptions :: Parser PkgUpgradeOptions
pkgUpgradeOptions =
    PkgUpgradeOptions <$> githubTokenOption

pkgSearchOptions :: Parser PkgSearchOptions
pkgSearchOptions =
    PkgSearchOptions <$> many (argument str (metavar "TERM" <> help "Search term (regex, ANDed)"))

zigPkgSubcommands :: Parser Command
zigPkgSubcommands = hsubparser
  (  command "add"    (info (ZigPkgAdd <$> zigPkgAddOptions) (progDesc "Add Zig package dependency"))
  <> command "remove" (info (ZigPkgRemove <$> zigPkgRemoveOptions) (progDesc "Remove Zig package dependency"))
  )

zigPkgAddOptions :: Parser ZigPkgAddOptions
zigPkgAddOptions = ZigPkgAddOptions
    <$> argument str (metavar "URL" <> help "URL of dependency")
    <*> argument str (metavar "NAME" <> help "Name of dependency")
    <*> many (strOption (long "artifact" <> metavar "NAME" <> help "Library artifact to link with"))

zigPkgRemoveOptions :: Parser ZigPkgRemoveOptions
zigPkgRemoveOptions =
    ZigPkgRemoveOptions <$> argument str (metavar "NAME" <> help "Name of dependency")

cloudOptions = CloudOptions
        <$> switch (long "run"          <> help "Help run!")
        <*> switch (long "list"         <> help "Help list!")
        <*> switch (long "show"         <> help "Help show!")
        <*> switch (long "stop"         <> help "Help stop!")

docOptions = DocOptions
    <$> (argument str (metavar "FILE" <> help "Input file (.act or .ty) - optional in projects" <> completer (bashCompleter "file -X '!*.act' -X '!*.ty' -o plusdirs")) <|> pure "")
    <*> formatFlags
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file (default: stdout)"))
  where
    -- Parse format flags - simple and explicit
    formatFlags = optional (
              flag' AsciiFormat (long "terminal" <> short 't' <> help "Terminal output (ASCII format)")
          <|> flag' MarkdownFormat (long "md" <> long "markdown" <> help "Output in Markdown format")
          <|> flag' HtmlFormat (long "html" <> help "Output in HTML format")
        )

optimizeOption :: Parser OptimizeMode
optimizeOption = option optimizeReader
    (long "optimize"
     <> metavar "MODE"
     <> value Debug
     <> help "Optimization mode (Debug, ReleaseSafe, ReleaseSmall, ReleaseFast)"
    )

data TestModeTag = ModeList | ModePerf deriving Show

testCommand :: Parser TestCommand
testCommand =
    toCmd
      <$> optional (argument testModeReader (metavar "MODE" <> help "list | perf"))
      <*> testOptions
  where
    toCmd mMode opts =
      case mMode of
        Just ModeList -> TestList opts
        Just ModePerf -> TestPerf opts
        Nothing -> TestRun opts

testModeReader :: ReadM TestModeTag
testModeReader = eitherReader $ \s ->
    case s of
      "list" -> Right ModeList
      "perf" -> Right ModePerf
      _      -> Left "Expected 'list' or 'perf'"

testOptions :: Parser TestOptions
testOptions = TestOptions
    <$> compileOptions
    <*> switch (long "show-log"      <> help "Show test log output")
    <*> switch (long "show-cached"   <> help "Show cached test results")
    <*> switch (long "no-cache"      <> help "Always run tests instead of reusing cached results")
    <*> switch (long "json"          <> help "Output final test results as JSON")
    <*> switch (long "record"        <> help "Record test performance results")
    <*> switch (long "snapshot-update" <> long "golden-update" <> long "accept" <> help "Accept current test output as expected snapshot values")
    <*> option auto (long "iter"     <> metavar "N" <> value (-1) <> help "Number of iterations to run a test")
    <*> option auto (long "max-iter" <> metavar "N" <> value (10^6) <> help "Maximum number of iterations to run a test")
    <*> option auto (long "min-iter" <> metavar "N" <> value 3 <> help "Minimum number of iterations to run a test")
    <*> option auto (long "max-time" <> metavar "MS" <> value 1000 <> help "Maximum time to run a test in milliseconds")
    <*> option auto (long "min-time" <> metavar "MS" <> value 50 <> help "Minimum time to run a test in milliseconds")
    <*> many (strOption (long "tag" <> metavar "TAG" <> help "Enable test capability TAG for testing.require()"))
    <*> many (strOption (long "module" <> metavar "MODULE" <> help "Filter on test module name"))
    <*> many (strOption (long "name" <> metavar "NAME" <> help "Filter on test name (regex, anchored; use .* for substrings)"))

depOverrideReader :: ReadM (String,String)
depOverrideReader = eitherReader $ \s ->
  case break (== '=') s of
    (name, '=':path) | not (null name) && not (null path) -> Right (name, path)
    _ -> Left "Expected NAME=PATH"

descr               = fullDesc <> progDesc "Compilation and management of Acton source code and projects"
                      <> header "acton - the Acton compiler"

-- main = do
--     f <- parseCmdLine
--     print(f)
data BuildSpecCommand = BuildSpecDump
                      | BuildSpecUpdate FilePath
                      deriving Show

buildSpecCommand :: Parser BuildSpecCommand
buildSpecCommand = hsubparser
    (  command "dump"
          (info (pure BuildSpecDump)
                (progDesc "Dump build spec as JSON to stdout (from Build.act)"))
    <> command "update"
          (info (BuildSpecUpdate <$> argument str (metavar "FILE"))
                (progDesc "Update Build.act using JSON file; override entries present in JSON only"))
    )
