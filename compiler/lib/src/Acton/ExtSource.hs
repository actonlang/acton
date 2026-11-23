{-# LANGUAGE ScopedTypeVariables #-}
{-|
The hand-written C code of a module's NotImplemented bodies.  It lives in an
.ext.c next to the module's source, which the module's generated .c includes,
and it may include further files of its project.  The compiler folds the
content of all of them into the module's recorded source hash and into the
impl hashes of its NotImplemented names (Acton.Compile, Acton.Hashing), so
that an edit of any of them counts as an edit of the module: its generated .c
gets a new tag, which Zig always notices, and the tests that use those names
run again.  Zig's own cache cannot stand in for this.  The cache of a whole
library learns the files a C file includes only when it compiles that file,
and forgets them when a build reuses the file's object, so an edit of only
an included file would reuse the old library.

The files an .ext.c includes are found by text and resolved the way clang
resolves them for the include path that builder/build.zig sets up: a quoted
name next to the including file, then any name in the project root, which is
the first -I.  A file that clang finds elsewhere belongs to a dependency or
to the compiler, and changes only together with them.  Two kinds of project
file stay out of reach: one that only a header outside the project includes,
through the project root on the include path, and one that a directive names
through a macro in a scratch build, whose source directory may hold
anything.
-}
module Acton.ExtSource
  ( extSourceFile
  , extSourceOwner
  , readExtSourceHash
  , CRef(..)
  , cFileRefs
  , resolveCRef
  , extIncludedFiles
  , extClosureHash
  ) where

import Acton.Builtin (mBuiltin)
import Acton.Project (Paths(..), collapseDots, isActFile, isInterfaceCopy, isScratchFile, isUnder)
import qualified Acton.Syntax as A

import Control.Exception (IOException, throwIO, try)
import Control.Monad (forM)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Binary (encode)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum, isSpace)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix
import System.IO.Error (isDoesNotExistError)

-- | The .ext.c next to a module's source.
extSourceFile :: FilePath -> FilePath
extSourceFile file = dropExtension file <.> "ext.c"

-- | The module source that an .ext.c belongs to, or Nothing for any other
-- file.
extSourceOwner :: FilePath -> Maybe FilePath
extSourceOwner path
  | suffix `isSuffixOf` name && length name > length suffix
              = Just (take (length path - length suffix) path <.> "act")
  | otherwise = Nothing
  where suffix = ".ext.c"
        name   = takeFileName path

-- | The content hash of a module's .ext.c, Nothing when it has none.  It
-- covers the project files that the module's hand-written C includes
-- (extIncludedFiles).  The generated .c of __builtin__ also includes
-- builtin/builtin.c (Acton.CodeGen), which counts as if its .ext.c included
-- it.
readExtSourceHash :: Paths -> A.ModName -> FilePath -> IO (Maybe B.ByteString)
readExtSourceHash paths mn file = do
    res <- try (B.readFile ext)
    case res of
      Right bytes -> Just . extClosureHash (SHA256.hash bytes) <$> extIncludedFiles paths ext bytes extra
      Left err
        | isDoesNotExistError err -> return Nothing
        | otherwise               -> throwIO err
  where ext   = extSourceFile file
        extra = [ CRef False "builtin/builtin.c" True | mn == mBuiltin ]

-- | A reference from C code to another file.  A quoted name is looked up
-- next to the referring file first.  The references of a file that only
-- #embed reads are not followed: it holds data.
data CRef = CRef
  { crQuoted :: Bool
  , crName   :: FilePath
  , crFollow :: Bool
  } deriving (Eq, Show)

-- | The files that C code refers to, found by text: the operand of every
-- #include, #include_next, #import and #embed directive and of every
-- __has_include and __has_include_next.  The lines are read as clang's
-- preprocessor reads them, continued lines joined and comments removed
-- (preprocessorText), but conditionals are not interpreted, so every file
-- that clang includes is among the references.  The flag is set when a
-- directive or a __has_include names its file through a macro, which only
-- the preprocessor can expand.
cFileRefs :: B.ByteString -> ([CRef], Bool)
cFileRefs bytes = foldr addLine ([], False) (B.lines (preprocessorText bytes))
  where
    addLine line (refs, computed) =
      let (direct, macro)  = directive line
          (probes, macro') = hasInclude line
      in (maybe id (:) direct (probes ++ refs), computed || macro || macro')

    directive line = case B.uncons (B.dropWhile isSpace line) of
      Just ('#', rest) ->
        let (word, operand) = B.span isIdent (B.dropWhile isSpace rest)
            arg             = B.dropWhile isSpace operand
        in if word `elem` map B.pack ["include", "include_next", "import", "embed"]
             then case operandOf arg of
                    Just (quoted, name) -> (Just (CRef quoted name (word /= B.pack "embed")), False)
                    Nothing             -> (Nothing, not (B.null arg))
             else (Nothing, False)
      _ -> (Nothing, False)

    hasInclude line = case B.breakSubstring keyword line of
      (_, rest)
        | B.null rest -> ([], False)
        | otherwise   ->
            let after          = B.drop (B.length keyword) rest
                after'         = fromMaybe after (B.stripPrefix (B.pack "_next") after)
                (found, macro) = case B.uncons (B.dropWhile isSpace after') of
                                   Just ('(', inner) -> case operandOf (B.dropWhile isSpace inner) of
                                                          Just (quoted, name) -> ([CRef quoted name True], False)
                                                          Nothing             -> ([], True)
                                   _                 -> ([], False)
                (more, macro') = hasInclude after'
            in (found ++ more, macro || macro')
    keyword = B.pack "__has_include"

    operandOf arg = case B.uncons arg of
      Just ('"', rest) -> close True '"' rest
      Just ('<', rest) -> close False '>' rest
      _                -> Nothing
    close quoted end rest =
      let (name, after) = B.break (== end) rest
      in if B.null after || B.null name then Nothing else Just (quoted, decode name)
    decode = T.unpack . TE.decodeUtf8With TE.lenientDecode

    isIdent c = isAlphaNum c || c == '_'

-- | C code as clang's preprocessor reads its lines: carriage returns
-- dropped, lines that end in a backslash joined with the next, and each
-- comment replaced by a space and the line breaks inside it.  String and
-- character literals are kept as they are, so that a comment marker inside
-- one is no comment; an unterminated one ends with its line.
preprocessorText :: B.ByteString -> B.ByteString
preprocessorText = BL.toStrict . BB.toLazyByteString . code . joinContinued . B.filter (/= '\r')
  where
    joinContinued text = case B.breakSubstring (B.pack "\\\n") text of
      (before, rest)
        | B.null rest -> before
        | otherwise   -> before <> joinContinued (B.drop 2 rest)

    code text = case B.findIndex (`elem` "/\"'") text of
      Nothing -> BB.byteString text
      Just i  -> let (before, rest) = B.splitAt i text in BB.byteString before <> special rest

    special text
      | B.pack "//" `B.isPrefixOf` text = BB.char7 ' ' <> code (B.dropWhile (/= '\n') text)
      | B.pack "/*" `B.isPrefixOf` text =
          let (body, rest) = B.breakSubstring (B.pack "*/") (B.drop 2 text)
              breaks       = BB.byteString (B.filter (== '\n') body)
          in if B.null rest then BB.char7 ' ' <> breaks else BB.char7 ' ' <> breaks <> code (B.drop 2 rest)
      | otherwise = case B.head text of
          '/'   -> BB.char7 '/' <> code (B.drop 1 text)
          quote -> let n = literalLength quote text in BB.byteString (B.take n text) <> code (B.drop n text)

    -- The length of the literal that 'text' starts with, its quotes
    -- included.
    literalLength quote text = go 1
      where go i = case B.findIndex (\c -> c == quote || c == '\\' || c == '\n') (B.drop i text) of
                     Nothing -> B.length text
                     Just j  -> case B.index text (i + j) of
                                  '\\' -> go (i + j + 2)
                                  '\n'  -> i + j
                                  _     -> i + j + 1

-- | The file that clang reads for a reference from the file 'includer',
-- among the places inside the project that the builder lets it search: for
-- a quoted name the includer's directory, then for any name the project
-- root, which builder/build.zig passes as the first -I, before the include
-- trees of dependencies and of the compiler.  Nothing when clang looks
-- further or finds nothing.  As in clang, the includer's directory comes
-- from its path as found, not resolved through symbolic links.
resolveCRef :: FilePath -> FilePath -> CRef -> IO (Maybe FilePath)
resolveCRef root includer ref = firstFile candidates
  where
    name = crName ref
    candidates
      | isAbsolute name = [name]
      | crQuoted ref    = [takeDirectory includer </> name, root </> name]
      | otherwise       = [root </> name]
    firstFile []       = return Nothing
    firstFile (p : ps) = do exists <- doesFileExist p
                            if exists then return (Just p) else firstFile ps

-- | The project files that an .ext.c reaches through its references and
-- theirs, with their content hashes, sorted by their paths relative to the
-- project, or to the source directory in a scratch build, whose project is
-- a temporary directory.  A file is in the project when the path that
-- clang reads it by, or the file it resolves to, lies there, so that a file
-- reached through a linked directory counts.  A file outside belongs to the
-- compiler or a dependency.  A file under out/ is generated: a module's own
-- header carries the tag that this hash decides, so it must not feed back
-- into it, and another module's header changes with that module's code.
-- When a reference names its file through a macro, every other file under
-- the source directory counts, since any of them may be the one, except in
-- a scratch build.
extIncludedFiles :: Paths -> FilePath -> B.ByteString -> [CRef] -> IO [(FilePath, B.ByteString)]
extIncludedFiles paths ext bytes extra = do
    let region = clean (if isTmp paths then srcDir paths else projPath paths)
        out    = clean (projOut paths)
    regionC <- canonical region
    outC    <- canonical out
    selfC   <- canonical ext
    let inside path canon = (clean path `isUnder` region || canon `isUnder` regionC)
                            && not (clean path `isUnder` out || canon `isUnder` outC)
        key path canon
          | clean path `isUnder` region = makeRelative region (clean path)
          | otherwise                   = makeRelative regionC canon
        (refs, computed) = cFileRefs bytes
        go _ found anyComputed [] = return (found, anyComputed)
        go seen found anyComputed ((_, []) : queue) = go seen found anyComputed queue
        go seen found anyComputed ((includer, ref : refs') : queue) = do
          resolved <- resolveCRef (projPath paths) includer ref
          next     <- case resolved of
                        Nothing   -> return Nothing
                        Just path -> do
                          canon <- canonical path
                          if not (inside path canon) || S.member canon seen
                            then return Nothing
                            else fmap ((,,) path canon) <$> readMaybe path
          let rest = (includer, refs') : queue
          case next of
            Nothing                     -> go seen found anyComputed rest
            Just (path, canon, content) -> do
              let (refs'', computed') = cFileRefs content
                  follow              = crFollow ref
              go (S.insert canon seen) (M.insert (key path canon) (SHA256.hash content) found)
                 (anyComputed || (follow && computed'))
                 (if follow then rest ++ [(path, refs'')] else rest)
    (found, anyComputed) <- go (S.singleton selfC) M.empty computed [(ext, refs ++ extra)]
    others <- if anyComputed && not (isTmp paths)
                then do
                  files   <- sourceFiles S.empty (clean (srcDir paths))
                  entries <- forM files $ \file -> do
                               canon <- canonical file
                               if canon == selfC || not (inside file canon)
                                 then return Nothing
                                 else fmap (\content -> (key file canon, SHA256.hash content)) <$> readMaybe file
                  return (M.fromList (catMaybes entries))
                else return M.empty
    return (M.toAscList (M.union found others))
  where
    clean = collapseDots . normalise

    canonical :: FilePath -> IO FilePath
    canonical path = either (\(_ :: IOException) -> clean path) id <$> try (canonicalizePath path)

    readMaybe :: FilePath -> IO (Maybe B.ByteString)
    readMaybe path = either (\(_ :: IOException) -> Nothing) Just <$> try (B.readFile path)

    -- Every file under a directory that may hold C: no Acton source, nothing
    -- that an editor leaves next to the files it saves and no --tydb copy.
    -- A linked directory is followed unless it leads back to a directory on
    -- the way to it, and an entry that cannot be read, or is gone by the
    -- time it is looked at, is left out.
    sourceFiles visited dir = do
      dirC <- canonical dir
      if S.member dirC visited then return [] else do
        names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory dir)
        let kept = [ dir </> name | name <- names, not (isScratchFile name), not (isInterfaceCopy (dir </> name)) ]
        kinds <- forM kept $ \path -> do
          isDir  <- doesDirectoryExist path
          isFile <- doesFileExist path
          return (path, isDir, isFile)
        nested <- concat <$> mapM (sourceFiles (S.insert dirC visited)) [ path | (path, True, _) <- kinds ]
        return ([ path | (path, False, True) <- kinds, not (isActFile path) ] ++ nested)

-- | The hash of an .ext.c, given its own content hash, together with the
-- project files it reaches.  An .ext.c that reaches none keeps its own
-- content hash.
extClosureHash :: B.ByteString -> [(FilePath, B.ByteString)] -> B.ByteString
extClosureHash own []      = own
extClosureHash own entries = SHA256.hash (BL.toStrict (encode ("ext.c includes" :: String, own, entries)))
