{-|
The expression embed("path"), whose value is the content of a file: the rule
for a path as it is written (pathProblem), the name of a path in the generated
C (pathDigits), the embed expressions of a piece of syntax (Embeds), the
file that a path names below a directory (embeddedFileStatus), the files that
the C compiler does not load (startsWithRefusedMark), and where a watch looks
for a change to a file (eventPaths, watchDirectories).
-}
module Acton.Embed
  ( pathProblem
  , pathDigits
  , Embeds(..)
  , embedExprs
  , listedPath
  , embeddedFileStatus
  , cannotBeRead
  , startsWithRefusedMark
  , eventPaths
  , watchesTrees
  , watchDirectories
  , cannotBeWatched
  ) where

import Acton.Syntax

import Control.Exception (IOException, handle, throwIO, try)
import Control.Monad (filterM, zipWithM)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as B
import Data.Char (isControl)
import Data.Functor.Const (Const(..))
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.IO.Exception (IOErrorType(InappropriateType), ioe_description)
import System.Directory (canonicalizePath, doesDirectoryExist, pathIsSymbolicLink)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Info (os)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Posix.Files (FileStatus, deviceID, fileID, fileSize, getFileStatus, getSymbolicLinkStatus, isDirectory, isRegularFile)

-- | Why embed cannot take a path as it is written, or Nothing for a path that
-- it takes: a relative path with / between its components, none of them empty,
-- . or .., without a backslash, a double quote, a brace or a control
-- character, and with a first component other than out.
pathProblem :: String -> Maybe String
pathProblem path
  | any (`elem` ["", ".", ".."]) parts || '\\' `elem` path
              = Just ("the path " ++ quoted ++ " of embed must be a relative path inside the project, using / and without . or .. components")
  | any (\c -> c `elem` "\"{}" || isControl c) path
              = Just ("the path " ++ quoted ++ " of embed has a double quote, a brace or a control character")
  | head parts == "out"
              = Just (quoted ++ " is in out, which a build writes and which is not committed")
  | otherwise = Nothing
  where parts  = components path
        quoted = "\"" ++ path ++ "\""

-- | The components of a path that has / between them.
components :: String -> [String]
components path = case break (== '/') path of
    (c, [])       -> [c]
    (c, _ : rest) -> c : components rest

-- | The first 32 hexadecimal digits of the SHA-256 of a path, which name the
-- content of its file in the generated C.  A path may hold characters that a
-- C name cannot.
pathDigits :: String -> String
pathDigits = take 32 . B.unpack . Base16.encode . SHA256.hash . TE.encodeUtf8 . T.pack


-- Embed expressions --------------------------------------------------------------------------------

-- | The embed expressions inside a piece of syntax, each visited once, in the
-- order of the source.
class Embeds a where
    embeds                          :: Applicative f => (Expr -> f Expr) -> a -> f a

-- | The embed expressions inside a piece of syntax.
embedExprs                          :: Embeds a => a -> [Expr]
embedExprs                          = getConst . embeds (\e -> Const [e])

instance Embeds a => Embeds [a] where
    embeds f                        = traverse (embeds f)

instance Embeds a => Embeds (Maybe a) where
    embeds f                        = traverse (embeds f)

instance Embeds Module where
    embeds f (Module n is doc ss)   = Module n is doc <$> embeds f ss

instance Embeds Stmt where
    embeds f (Expr l e)             = Expr l <$> embeds f e
    embeds f (Assign l ps e)        = Assign l <$> embeds f ps <*> embeds f e
    embeds f (MutAssign l t e)      = MutAssign l <$> embeds f t <*> embeds f e
    embeds f (AugAssign l t op e)   = AugAssign l <$> embeds f t <*> pure op <*> embeds f e
    embeds f (Assert l e x)         = Assert l <$> embeds f e <*> embeds f x
    embeds f (Delete l t)           = Delete l <$> embeds f t
    embeds f (Return l x)           = Return l <$> embeds f x
    embeds f (If l bs els)          = If l <$> embeds f bs <*> embeds f els
    embeds f (While l e b els)      = While l <$> embeds f e <*> embeds f b <*> embeds f els
    embeds f (For l p e b els)      = For l <$> embeds f p <*> embeds f e <*> embeds f b <*> embeds f els
    embeds f (Try l b hs els fin)   = Try l <$> embeds f b <*> embeds f hs <*> embeds f els <*> embeds f fin
    embeds f (With l is b)          = With l <$> embeds f is <*> embeds f b
    embeds f (Data l p b)           = Data l <$> embeds f p <*> embeds f b
    embeds f (VarAssign l ps e)     = VarAssign l <$> embeds f ps <*> embeds f e
    embeds f (After l n e e')       = After l n <$> embeds f e <*> embeds f e'
    embeds f (Decl l ds)            = Decl l <$> embeds f ds
    embeds f s                      = pure s

instance Embeds Decl where
    embeds f (Def l n q p k a b d x doc)
                                    = (\p' k' b' -> Def l n q p' k' a b' d x doc) <$> embeds f p <*> embeds f k <*> embeds f b
    embeds f (Actor l n q p k b doc)
                                    = (\p' k' b' -> Actor l n q p' k' b' doc) <$> embeds f p <*> embeds f k <*> embeds f b
    embeds f (Class l n q as b doc) = (\b' -> Class l n q as b' doc) <$> embeds f b
    embeds f (Protocol l n q as b doc)
                                    = (\b' -> Protocol l n q as b' doc) <$> embeds f b
    embeds f (Extension l q c as b doc)
                                    = (\b' -> Extension l q c as b' doc) <$> embeds f b
    embeds f d@Typedef{}            = pure d

instance Embeds Expr where
    embeds f e@Embed{}              = f e
    embeds f (Call l e p k)         = Call l <$> embeds f e <*> embeds f p <*> embeds f k
    embeds f (Let l ss e)           = Let l <$> embeds f ss <*> embeds f e
    embeds f (TApp l e ts)          = (\e' -> TApp l e' ts) <$> embeds f e
    embeds f (Async l e)            = Async l <$> embeds f e
    embeds f (Await l e)            = Await l <$> embeds f e
    embeds f (Index l e ix)         = Index l <$> embeds f e <*> embeds f ix
    embeds f (Slice l e sl)         = Slice l <$> embeds f e <*> embeds f sl
    embeds f (Cond l e c e')        = Cond l <$> embeds f e <*> embeds f c <*> embeds f e'
    embeds f (IsInstance l e c)     = (\e' -> IsInstance l e' c) <$> embeds f e
    embeds f (BinOp l e op e')      = (\x y -> BinOp l x op y) <$> embeds f e <*> embeds f e'
    embeds f (CompOp l e ops)       = CompOp l <$> embeds f e <*> embeds f ops
    embeds f (UnOp l op e)          = UnOp l op <$> embeds f e
    embeds f (Dot l e n)            = (\e' -> Dot l e' n) <$> embeds f e
    embeds f (Rest l e n)           = (\e' -> Rest l e' n) <$> embeds f e
    embeds f (DotI l e i)           = (\e' -> DotI l e' i) <$> embeds f e
    embeds f (RestI l e i)          = (\e' -> RestI l e' i) <$> embeds f e
    embeds f (Opt l e b)            = (\e' -> Opt l e' b) <$> embeds f e
    embeds f (OptChain l e)         = OptChain l <$> embeds f e
    embeds f (Lambda l p k e fx)    = (\p' k' e' -> Lambda l p' k' e' fx) <$> embeds f p <*> embeds f k <*> embeds f e
    embeds f (Yield l e)            = Yield l <$> embeds f e
    embeds f (YieldFrom l e)        = YieldFrom l <$> embeds f e
    embeds f (Tuple l p k)          = Tuple l <$> embeds f p <*> embeds f k
    embeds f (List l es)            = List l <$> embeds f es
    embeds f (ListComp l e c)       = ListComp l <$> embeds f e <*> embeds f c
    embeds f (Dict l as)            = Dict l <$> embeds f as
    embeds f (DictComp l a c)       = DictComp l <$> embeds f a <*> embeds f c
    embeds f (Set l es)             = Set l <$> embeds f es
    embeds f (SetComp l e c)        = SetComp l <$> embeds f e <*> embeds f c
    embeds f (GeneratorExpr l e c)  = GeneratorExpr l <$> embeds f e <*> embeds f c
    embeds f (Paren l e)            = Paren l <$> embeds f e
    embeds f (Box t e)              = Box t <$> embeds f e
    embeds f (UnBox t e)            = UnBox t <$> embeds f e
    embeds f (Raise l e t)          = (\e' -> Raise l e' t) <$> embeds f e
    embeds f e                      = pure e

instance Embeds Pattern where
    embeds f (PParen l p)           = PParen l <$> embeds f p
    embeds f (PTuple l ps ks)       = PTuple l <$> embeds f ps <*> embeds f ks
    embeds f (PList l ps p)         = PList l <$> embeds f ps <*> embeds f p
    embeds f (PData l n ixs)        = PData l n <$> embeds f ixs
    embeds f p                      = pure p

instance Embeds PosPat where
    embeds f (PosPat p ps)          = PosPat <$> embeds f p <*> embeds f ps
    embeds f (PosPatStar p)         = PosPatStar <$> embeds f p
    embeds f PosPatNil              = pure PosPatNil

instance Embeds KwdPat where
    embeds f (KwdPat n p ps)        = KwdPat n <$> embeds f p <*> embeds f ps
    embeds f (KwdPatStar p)         = KwdPatStar <$> embeds f p
    embeds f KwdPatNil              = pure KwdPatNil

instance Embeds PosPar where
    embeds f (PosPar n t e p)       = PosPar n t <$> embeds f e <*> embeds f p
    embeds f p                      = pure p

instance Embeds KwdPar where
    embeds f (KwdPar n t e k)       = KwdPar n t <$> embeds f e <*> embeds f k
    embeds f k                      = pure k

instance Embeds PosArg where
    embeds f (PosArg e p)           = PosArg <$> embeds f e <*> embeds f p
    embeds f (PosStar e)            = PosStar <$> embeds f e
    embeds f PosNil                 = pure PosNil

instance Embeds KwdArg where
    embeds f (KwdArg n e k)         = KwdArg n <$> embeds f e <*> embeds f k
    embeds f (KwdStar e)            = KwdStar <$> embeds f e
    embeds f KwdNil                 = pure KwdNil

instance Embeds OpArg where
    embeds f (OpArg op e)           = OpArg op <$> embeds f e

instance Embeds Sliz where
    embeds f (Sliz l a b c)         = Sliz l <$> embeds f a <*> embeds f b <*> embeds f c

instance Embeds Comp where
    embeds f (CompFor l p e c)      = CompFor l <$> embeds f p <*> embeds f e <*> embeds f c
    embeds f (CompIf l e c)         = CompIf l <$> embeds f e <*> embeds f c
    embeds f NoComp                 = pure NoComp

instance Embeds Elem where
    embeds f (Elem e)               = Elem <$> embeds f e
    embeds f (Star e)               = Star <$> embeds f e

instance Embeds Assoc where
    embeds f (Assoc k v)            = Assoc <$> embeds f k <*> embeds f v
    embeds f (StarStar e)           = StarStar <$> embeds f e

instance Embeds Branch where
    embeds f (Branch e ss)          = Branch <$> embeds f e <*> embeds f ss

instance Embeds Handler where
    embeds f (Handler ex ss)        = Handler ex <$> embeds f ss

instance Embeds WithItem where
    embeds f (WithItem e p)         = WithItem <$> embeds f e <*> embeds f p


-- The file of a path -------------------------------------------------------------------------------

-- | A path below a directory with each component as its directory lists it,
-- or Nothing when a component is not there.  On a file system that takes one
-- spelling of a name for another, the result can differ from the path as it
-- is written.  The names of a directory come from the given function, which
-- a caller can let remember its answers.
listedPath :: (FilePath -> IO (S.Set FilePath)) -> FilePath -> FilePath -> IO (Maybe FilePath)
listedPath names root path = fmap (intercalate "/") <$> listed root (components path)
  where
    listed _ []         = return (Just [])
    listed dir (c : cs) = do
      listing <- try (names dir)
      case listing of
        Left e
          | isDoesNotExistError e || ioeGetErrorType e == InappropriateType -> return Nothing
          | otherwise                                                       -> throwIO e
        Right entries
          | S.member c entries -> below dir c cs
          | otherwise          -> maybe (return Nothing) (\entry -> below dir entry cs) =<< otherSpelling dir c (S.toList entries)
    below dir entry cs = fmap (entry :) <$> listed (dir </> entry) cs

    -- The listed name of the entry that a name reaches although its
    -- directory does not list it so: the same file by device and inode.
    otherSpelling dir c entries = do
      written <- try (getSymbolicLinkStatus (dir </> c))
      case written of
        Left e
          | isDoesNotExistError e -> return Nothing
          | otherwise             -> throwIO e
        Right st -> firstM (\entry -> same st <$> getSymbolicLinkStatus (dir </> entry)) entries
    same a b = deviceID a == deviceID b && fileID a == fileID b

    firstM _ []       = return Nothing
    firstM p (x : xs) = do found <- p x
                           if found then return (Just x) else firstM p xs

-- | The stat of the file that a path names below a directory, or why embed
-- cannot take it.  The path names a regular file of fewer than 2^31 bytes,
-- and each component is spelled as its directory lists it.  A symbolic link
-- is followed.  The path is one that pathProblem accepts.
embeddedFileStatus :: (FilePath -> IO (S.Set FilePath)) -> FilePath -> FilePath -> IO (Either String FileStatus)
embeddedFileStatus names root path = handle (no . cannotBeRead path) $ do
    listed <- listedPath names root path
    case listed of
      Nothing    -> missing
      Just named -> do
        status <- try (getFileStatus (root </> path))
        case status of
          Left e
            | isDoesNotExistError e -> missing
            | otherwise             -> throwIO e
          Right st
            | isDirectory st          -> no (path ++ " is a directory, and embed takes one file")
            | named /= path           -> no ("the file is named " ++ named ++ ", and the path is written " ++ path)
            | not (isRegularFile st)  -> no (path ++ " is not a regular file")
            | size >= 2 ^ (31 :: Int) -> no (path ++ " has " ++ show size ++ " bytes, and embed takes a file of fewer than 2147483648 bytes")
            | otherwise               -> return (Right st)
            where size = toInteger (fileSize st)
  where
    no      = return . Left
    missing = no ("no file " ++ path ++ " in " ++ root)

-- | The message for a path whose file cannot be read, with the reason that
-- the operating system gives.
cannotBeRead :: FilePath -> IOException -> String
cannotBeRead path e = path ++ " cannot be read: " ++ ioe_description e

-- | The byte order marks that clang refuses at the start of a file it loads,
-- also of a file that #embed names.  It takes the mark of UTF-8.  The C
-- compiler of the toolchain is clang, so the list is to be looked at again
-- when the toolchain changes.  https://github.com/actonlang/acton/issues/3464
-- tracks the bug of clang and lists what to remove when clang has the fix.
refusedMarks :: [BS.ByteString]
refusedMarks = map BS.pack
    [ [0xff, 0xfe], [0xfe, 0xff]                            -- UTF-16, little and big endian
    , [0xff, 0xfe, 0x00, 0x00], [0x00, 0x00, 0xfe, 0xff]    -- UTF-32, little and big endian
    , [0x2b, 0x2f, 0x76]                                    -- UTF-7
    , [0xf7, 0x64, 0x4c]                                    -- UTF-1
    , [0xdd, 0x73, 0x66, 0x73]                              -- UTF-EBCDIC
    , [0x0e, 0xfe, 0xff]                                    -- SCSU
    , [0xfb, 0xee, 0x28]                                    -- BOCU-1
    , [0x84, 0x31, 0x95, 0x33]                              -- GB-18030
    ]

-- | Whether the first bytes of a file are one of the refusedMarks.
startsWithRefusedMark :: FilePath -> IO Bool
startsWithRefusedMark file = do
    start <- withBinaryFile file ReadMode (`BS.hGet` maximum (map BS.length refusedMarks))
    return (any (`BS.isPrefixOf` start) refusedMarks)


-- Watching -----------------------------------------------------------------------------------------

-- | The paths at which a file system event tells of a change to the file
-- that a path names below a directory: those of the directories on the way
-- to the file, and those of the file, which are the path and, when the path
-- is a symbolic link, the file that it links to.  Each is named as an event
-- names it, which is with the links of the directories above it resolved.
-- The given function resolves the links of a directory, and a caller can let
-- it remember its answers.  A part of a path that does not exist stays as it
-- is written.
eventPaths :: (FilePath -> IO FilePath) -> FilePath -> FilePath -> IO ([FilePath], [FilePath])
eventPaths resolved root path = do
    places <- zipWithM (\dir part -> (</> part) <$> resolved dir) (scanl (</>) root parts) parts
    link   <- either (const False) id <$> (try (pathIsSymbolicLink file) :: IO (Either IOException Bool))
    target <- if link then (: []) <$> canonicalizePath file else return []
    let (above, place) = splitAt (length parts - 1) places
    return (above, place ++ target)
  where parts = components path
        file  = root </> path

-- | Whether one watch tells of the events in a whole tree of directories.
-- On macOS it does, and a process there gets events from about 500 watches:
-- a watch beyond them starts without an error and tells of nothing.
-- Elsewhere a watch tells of the events in one directory, and the watch of
-- a tree is a watch of each directory in it.
watchesTrees :: Bool
watchesTrees = os == "darwin"

-- | The directories to watch for an event at one of the given paths: the
-- directory of each path, where it exists.  Where a directory on the way to
-- a file is missing, the directory above the first missing one exists, and
-- a watch of it sees that one appear.  Where one watch tells of a whole
-- tree (watchesTrees), these are the directories that have none of the
-- others above them.
watchDirectories :: S.Set FilePath -> IO (S.Set FilePath)
watchDirectories paths = tops . S.fromList <$> filterM doesDirectoryExist (S.toList (S.map takeDirectory paths))
  where
    tops dirs
      | watchesTrees = S.filter (not . any (`S.member` dirs) . above) dirs
      | otherwise    = dirs
    -- The directories above a directory.
    above dir = let up = takeDirectory dir in if up == dir then [] else up : above up

-- | What a watch says of a directory that it cannot watch, with the error
-- that stopped it.
cannotBeWatched :: FilePath -> IOException -> String
cannotBeWatched dir e = dir ++ " cannot be watched: " ++ show e
