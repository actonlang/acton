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
                        sub          :: Bool,
                        timing       :: Bool,
                        tty          :: Bool,
                        verbose      :: Bool,
                        verboseZig   :: Bool,
                        jobs         :: Int
                     } deriving Show

data Command        = New NewOptions
                    | Build CompileOptions
                    | Fetch
                    | PkgShow
                    | BuildSpecCmd BuildSpecCommand
                    | Cloud CloudOptions
                    | Doc DocOptions
                    | Version
                    deriving Show


data NewOptions     = NewOptions {
                         file      :: String
                    }  deriving Show

data CompileOptions   = CompileOptions {
                         alwaysbuild :: Bool,
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
                         listimports :: Bool,
                         only_build  :: Bool,
                         skip_build  :: Bool,
                         no_threads  :: Bool,
                         root        :: String,
                         tempdir     :: String,
                         syspath     :: String,
                         target      :: String,
                         cpu         :: String,
                         test        :: Bool,
                         searchpath  :: [String]
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

--------------------------------------------------------------------
-- Internal stuff

cmdLineParser       :: Parser CmdLineOptions
cmdLineParser       = hsubparser
                        (  command "new"     (info (CmdOpt <$> globalOptions <*> (New <$> newOptions)) (progDesc "Create a new Acton project"))
                        <> command "build"   (info (CmdOpt <$> globalOptions <*> (Build <$> compileOptions)) (progDesc "Build an Acton project"))
                        <> command "fetch"   (info (CmdOpt <$> globalOptions <*> pure Fetch) (progDesc "Fetch project dependencies (offline prep)"))
                        <> command "pkg"     (info (CmdOpt <$> globalOptions <*> pkgSubcommands) (progDesc "Package/dependency commands"))
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
    <*> switch (long "sub")
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

compileOptions = CompileOptions
        <$> switch (long "always-build" <> help "Show the result of parsing")
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
        <*> switch (long "list-imports" <> help "List module imports")
        <*> switch (long "only-build"   <> help "Only perform final build of .c files, do not compile .act files")
        <*> switch (long "skip-build"   <> help "Skip final bulid of .c files")
        <*> switch (long "no-threads"   <> help "Don't use threads")
        <*> strOption (long "root"      <> metavar "ROOTACTOR" <> value "" <> help "Set root actor")
        <*> strOption (long "tempdir"   <> metavar "TEMPDIR" <> value "" <> help "Set directory for build files")
        <*> strOption (long "syspath"   <> metavar "TARGETDIR" <> value "" <> help "Set syspath")
        <*> strOption (long "target"    <> metavar "TARGET" <> value defTarget <> help "Target, e.g. x86_64-linux-gnu.2.28")
        <*> strOption (long "cpu"       <> metavar "CPU" <> value "" <> help "CPU, e.g. skylake")
        <*> switch (long "test"         <> help "Build tests")
        <*> many (strOption (long "searchpath" <> metavar "DIR" <> help "Add search path"))

pkgSubcommands :: Parser Command
pkgSubcommands = hsubparser
  (  command "show" (info (pure PkgShow) (progDesc "Show dependency tree with overrides"))
  )

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

descr               = fullDesc <> progDesc "Compilation and management of Acton source code and projects"
                      <> header "actonc - the Acton compiler"

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
                (progDesc "Dump build spec as JSON to stdout (prefer Build.act; fallback to build.act.json)"))
    <> command "update"
          (info (BuildSpecUpdate <$> argument str (metavar "FILE"))
                (progDesc "Update Build.act using JSON file; override entries present in JSON only"))
    )
