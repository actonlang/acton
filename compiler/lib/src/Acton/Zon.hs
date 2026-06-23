{-# LANGUAGE OverloadedStrings #-}

-- | A small reader for Zig's ZON object notation (@build.zig.zon@).
--
-- Acton only needs enough of ZON to discover the dependencies a zig package
-- declares, so it can pre-seed zig's package cache through the proxy-aware HTTP
-- client. With the cache pre-populated, zig never has to fetch a transitive
-- @.url@ dependency over its own network stack (which cannot traverse an HTTP
-- CONNECT proxy). We therefore extract, for each entry of the top-level
-- @.dependencies@ struct, its @url@ / @hash@ / @path@ / @lazy@ fields.
--
-- The parser is deliberately permissive: it understands the full ZON value
-- grammar well enough to skip over fields it does not care about (name,
-- version, fingerprint, paths, ...) and to reach @.dependencies@ regardless of
-- field order.
module Acton.Zon
  ( ZonValue(..)
  , ZonDep(..)
  , parseZon
  , zonDependencies
  , readZonDependencies
  ) where

import Control.Monad (void)
import Data.Char (chr, isAlphaNum, isDigit)
import Data.List (intercalate)
import Data.Void (Void)
import Numeric (readHex)
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | A parsed ZON value. Only the shapes that actually occur in build.zig.zon
-- are distinguished; everything else is consumed but kept opaque.
data ZonValue
  = ZStruct [(String, ZonValue)]   -- ^ @.{ .field = value, ... }@
  | ZArray  [ZonValue]             -- ^ @.{ value, value, ... }@
  | ZString String
  | ZEnum   String                 -- ^ @.identifier@
  | ZBool   Bool
  | ZNull
  | ZNum    String                 -- ^ kept verbatim; we never interpret it
  deriving (Eq, Show)

-- | One entry of a @build.zig.zon@ @.dependencies@ struct.
data ZonDep = ZonDep
  { zdUrl  :: Maybe String
  , zdHash :: Maybe String
  , zdPath :: Maybe String
  , zdLazy :: Bool
  } deriving (Eq, Show)

-- Lexing ---------------------------------------------------------------------

-- | Whitespace and @//@ line comments (ZON has no block comments).
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Values ---------------------------------------------------------------------

pValue :: Parser ZonValue
pValue = choice
  [ pStructOrArray
  , pString
  , pMultiString
  , pBoolNull
  , pEnum
  , pChar
  , pNumber
  ]

-- | Both structs and arrays start with @.{@. They are told apart by looking for
-- a field head (@.name =@): present means a struct, absent means an array.
pStructOrArray :: Parser ZonValue
pStructOrArray = do
  _ <- try (symbol ".{")
  (ZStruct [] <$ symbol "}")
    <|> do
      isStruct <- option False (True <$ lookAhead (try pFieldHead))
      if isStruct
        then ZStruct <$> (pField `sepEndBy1` symbol ",") <* symbol "}"
        else ZArray  <$> (pValue `sepEndBy1` symbol ",") <* symbol "}"

-- | A field head, used only as look-ahead to classify a @.{ ... }@.
pFieldHead :: Parser ()
pFieldHead = char '.' *> lexeme pFieldName *> void (char '=')

pField :: Parser (String, ZonValue)
pField = do
  _ <- char '.'
  name <- lexeme pFieldName
  _ <- symbol "="
  v <- pValue
  pure (name, v)

-- | A field / enum name: a bare identifier or a @\@"quoted"@ identifier.
pFieldName :: Parser String
pFieldName = pAtString <|> pBareIdent
  where
    pBareIdent = (:) <$> (letterChar <|> char '_')
                     <*> many (alphaNumChar <|> char '_')
    pAtString  = char '@' *> pStringRaw

pEnum :: Parser ZonValue
pEnum = lexeme (ZEnum <$> (char '.' *> pFieldName))

pBoolNull :: Parser ZonValue
pBoolNull = lexeme $ choice
  [ ZBool True  <$ keyword "true"
  , ZBool False <$ keyword "false"
  , ZNull       <$ keyword "null"
  ]
  where keyword w = try (string w <* notFollowedBy (alphaNumChar <|> char '_'))

pString :: Parser ZonValue
pString = lexeme (ZString <$> pStringRaw)

pStringRaw :: Parser String
pStringRaw = char '"' *> manyTill pStringChar (char '"')
  where
    pStringChar = (char '\\' *> pEscape)
              <|> satisfy (\c -> c /= '"' && c /= '\n')

pEscape :: Parser Char
pEscape = choice
  [ '\n' <$ char 'n'
  , '\r' <$ char 'r'
  , '\t' <$ char 't'
  , '\\' <$ char '\\'
  , '"'  <$ char '"'
  , '\'' <$ char '\''
  , char 'x' *> pHex 2
  , char 'u' *> (char '{' *> pHexN <* char '}')
  , anySingle   -- unknown escape: keep the char verbatim
  ]
  where
    pHex n  = chrHex <$> count n hexDigitChar
    pHexN   = chrHex <$> some hexDigitChar
    chrHex ds = case readHex ds of
                  [(v, _)] | v >= (0 :: Int) && v <= 0x10FFFF -> chr v
                  _                                           -> '\xFFFD'

-- | Zig multiline strings: one or more consecutive lines beginning with @\\\\@.
pMultiString :: Parser ZonValue
pMultiString = lexeme (ZString . intercalate "\n" <$> some (try pLine))
  where
    pLine = do
      skipMany (oneOf (" \t" :: String))
      _ <- string "\\\\"
      l <- many (satisfy (/= '\n'))
      _ <- optional eol
      pure l

pChar :: Parser ZonValue
pChar = lexeme $ do
  _ <- char '\''
  c <- (char '\\' *> pEscape) <|> anySingleBut '\''
  _ <- char '\''
  pure (ZNum [c])

-- | Consume a numeric literal in any zig form (decimal, @0x@/@0o@/@0b@, float,
-- with @_@ separators). We never interpret the value, only skip it.
pNumber :: Parser ZonValue
pNumber = lexeme $ do
  c0 <- satisfy (\c -> isDigit c || c == '-' || c == '+')
  cs <- many (satisfy (\c -> isAlphaNum c || c `elem` ("._+-" :: String)))
  pure (ZNum (c0 : cs))

-- Entry points ---------------------------------------------------------------

parseZon :: String -> Either String ZonValue
parseZon input0 =
  case parse (sc *> pValue <* eof) "build.zig.zon" input of
    Left err -> Left (errorBundlePretty err)
    Right v  -> Right v
  where
    -- Zig's tokenizer skips a leading UTF-8 BOM; do the same so a BOM-prefixed
    -- build.zig.zon still parses (otherwise pre-seeding silently turns off).
    input = dropWhile (== '\xFEFF') input0

-- | Extract the entries of the top-level @.dependencies@ struct, if present.
zonDependencies :: ZonValue -> [(String, ZonDep)]
zonDependencies (ZStruct fields) =
  case lookup "dependencies" fields of
    Just (ZStruct deps) -> [ (name, toDep v) | (name, v) <- deps ]
    _                   -> []
zonDependencies _ = []

toDep :: ZonValue -> ZonDep
toDep (ZStruct fs) = ZonDep
  { zdUrl  = strField "url"
  , zdHash = strField "hash"
  , zdPath = strField "path"
  , zdLazy = case lookup "lazy" fs of
               Just (ZBool b) -> b
               _              -> False
  }
  where
    strField k = case lookup k fs of
                   Just (ZString s) -> Just s
                   _                -> Nothing
toDep _ = ZonDep Nothing Nothing Nothing False

-- | Read and parse a @build.zig.zon@, returning its dependency entries. A
-- missing file yields no dependencies; an unparseable file yields a 'Left'.
readZonDependencies :: FilePath -> IO (Either String [(String, ZonDep)])
readZonDependencies p = do
  exists <- doesFileExist p
  if not exists
    then pure (Right [])
    else do
      content <- T.unpack . TE.decodeUtf8With TEE.lenientDecode <$> BS.readFile p
      pure (zonDependencies <$> parseZon content)
