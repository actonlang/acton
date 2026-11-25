-- | The language server and a module whose file is removed while the server
-- runs.
module ServerTests (serverTests) where

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

import EmbedTests (edited, opened, withServer)
import WatchTests (buildAct)
import Test.Tasty
import Test.Tasty.HUnit

serverTests :: TestTree
serverTests = testGroup "language server"
    [ testCase "the import of a module whose file is removed" removedImport
    ]

-- | The module page of a project imports the module base, and both are open
-- in the editor.  The server compiles them and reports nothing for page.
-- Then the file of base is removed and its document is edited.  The server
-- plans from the path of that document, which removes the outputs of base,
-- and reports the import in page.
removedImport :: IO ()
removedImport = withSystemTempDirectory "acton-server-removed" $ \tmp -> do
    root <- (</> "shop") <$> canonicalizePath tmp
    let page     = "src" </> "page.act"
        base     = "src" </> "base.act"
        pageText = unlines ["import base", "", "title: str = base.greeting"]
        baseText = "greeting: str = \"hello\"\n"
        missing  = any ("Type interface file not found or unreadable for shop.base" `isInfixOf`) . map (LBS.unpack . Ae.encode)
    createDirectoryIfMissing True (root </> "src")
    writeFile (root </> "Build.act") (buildAct "shop" [])
    writeFile (root </> page) pageText
    writeFile (root </> base) baseText
    withServer root $ \notify published -> do
      opened notify root page pageText
      published page >>= (@?= [])
      opened notify root base baseText
      published page >>= (@?= [])
      removeFile (root </> base)
      edited notify root base 2 (baseText ++ "# An edit\n")
      after <- published page
      assertBool ("expected the report of the import of base, got " ++ show after) (missing after)
