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

{-# LANGUAGE FlexibleInstances #-}
module Acton.Parser where

import qualified Control.Monad.Trans.State.Strict as St
import qualified Control.Exception
import Control.Monad (void)
import Data.Void
import Data.Char
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
--import Control.Monad.Combinators.Expr
import Text_Megaparsec_Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Debug as D
import qualified Data.List.NonEmpty
import qualified Acton.Syntax as S
import qualified Acton.Builtin as Builtin
import qualified Acton.Names as Names
import Utils
import Debug.Trace
import System.IO.Unsafe

-- Context errors -------------------------------------------------------------------------------

tr :: Show a => String -> Parser a -> Parser a
tr msg p = do
  r <-  observing p
  case r of
     Left err -> trace ("failure "++msg ++": "++show err) (parseError err)
     Right ok -> trace ("success "++msg++": "++show ok) (return ok)
     
makeReport (loc, msg) src = errReport (sp, msg) src
  where sp = extractSrcSpan loc src

instance ShowErrorComponent [Char] where
  showErrorComponent s = s
  
--- Main parsing and error message functions ------------------------------------------------------

parseModule :: S.ModName -> String -> String -> IO S.Module
parseModule qn fileName fileContent = 
    case runParser (St.evalStateT file_input initState) fileName (fileContent) of
        Left err -> Control.Exception.throw err
        Right (i,s) -> return $ S.Module qn i s

-- parseTest file = snd (unsafePerformIO (do cont <- readFile file; parseModule (S.modName ["test"]) file cont))

parseTestStr b p str = case runParser (St.evalStateT p (b,[])) "" str of
                         Left err -> putStrLn (errorBundlePretty err)
                         Right t  -> print t

parserError :: ParseErrorBundle String String -> (SrcLoc,String)
parserError err = (NoLoc,errorBundlePretty err)


extractSrcSpan :: SrcLoc -> String -> SrcSpan
extractSrcSpan NoLoc src = SpanEmpty
extractSrcSpan (Loc l r) src = sp
  where Right sp = runParser (St.evalStateT (extractP l r) initState) "" (src ++ "\n")
        extractP :: Int -> Int -> Parser SrcSpan
        extractP l r = do
            setOffset l
            SourcePos f srow scol <- getSourcePos
            setOffset r
            SourcePos _ erow ecol <- getSourcePos
            if srow == erow 
                then if scol == ecol 
                    then return $ SpanPoint f (unPos srow) (unPos scol)
                    else return $ SpanCoLinear f (unPos srow) (unPos scol) (unPos ecol - 1)
                else return $ SpanMultiLine f (unPos srow) (unPos scol) (unPos erow) (unPos ecol - 1)


-- Parser state -----------------------------------------------------------

type ParserState = (Bool, [CTX])  -- (Is numpy imported?, Parser contexts)

type Parser = St.StateT ParserState (Parsec String String)

pushCtx ctx (b,ctxs) = (b, ctx:ctxs)
popCtx (b,ctxs)      = (b,tail ctxs)
getCtxs (_,ctxs)      = ctxs

setNumpy b1 (b,ctxs) = (b1,ctxs)
getNumpy (b,_)       = b

initState            = (False,[])

-- Parser contexts ---------------------------------------------------------

data CTX = TOP | PAR | IF | SEQ | LOOP | DATA | DEF | CLASS | PROTO | EXT | ACTOR deriving (Show,Eq)

withCtx ctx = between (St.modify (pushCtx ctx)) (St.modify popCtx)

ifCtx accept ignore yes no = do
    cs <- St.gets getCtxs
    case filter (`notElem` ignore) cs of
        c:_ | c `elem` accept -> yes
        _                     -> no

ifNotCtx avoid ignore yes no = do
    cs <- St.gets getCtxs
    case filter (`notElem` ignore) cs of
        c:_ | c `elem` avoid -> no
        _  -> yes

-- onlyIn s            = fail ("statement only allowed inside " ++ s)
-- notIn s             = fail ("statement not allowed inside " ++ s)
success             = return ()

contextError                    :: ContextError -> (SrcLoc, String)
contextError err                = (loc err, ctxMsg err)

data ContextError   = OnlyTopLevel SrcLoc String
                    | OnlyInActor SrcLoc String
                    | OnlyInLoop SrcLoc String
                    | OnlyInClassProtoExt SrcLoc String
                    | OnlyInClass SrcLoc String
                    | OnlyInFunction SrcLoc String
                    | OnlyInFunctionOrActor SrcLoc String
                    | NotInClassProtoExt SrcLoc String
                    | NotInData SrcLoc String
                    deriving (Show, Eq)

instance Control.Exception.Exception ContextError

instance HasLoc ContextError where
  loc (OnlyTopLevel l _) = l
  loc (OnlyInActor l _) = l
  loc (OnlyInLoop l _) = l
  loc (OnlyInClassProtoExt l _) = l
  loc (OnlyInClass l _) = l
  loc (OnlyInFunction l _) = l
  loc (OnlyInFunctionOrActor l _) = l
  loc (NotInClassProtoExt l _) = l
  loc (NotInData l _) = l


ctxMsg (OnlyTopLevel loc str)          = str ++ " declaration only allowed on the module top level"
ctxMsg (OnlyInActor loc str)           = str ++ " statement only allowed inside an actor body"
ctxMsg (OnlyInLoop loc str)            = str ++ " statement only allowed inside a loop"
ctxMsg (OnlyInClassProtoExt loc str)   = str ++ " decoration only allowed inside a class, protocol or extension"
ctxMsg (OnlyInClass loc str)           = str ++ " decoration only allowed inside a class"
ctxMsg (OnlyInFunction loc str)        = str ++ " statement only allowed inside a function"
ctxMsg (OnlyInFunctionOrActor loc str) = str ++ " statement only allowed inside a function or actor"
ctxMsg (NotInClassProtoExt loc str)    = str ++ " statement not allowed inside a class, protocol or extension"
ctxMsg (NotInData loc str)             = str ++ " statement not allowed inside a data tree"

assertTop loc str       = ifCtx [TOP]                   []                  success (Control.Exception.throw $ OnlyTopLevel loc str)
assertActBody loc str   = ifCtx [ACTOR]                 []                  success (Control.Exception.throw $ OnlyInActor loc str)
assertLoop loc str      = ifCtx [LOOP]                  [IF,SEQ]            success (Control.Exception.throw $ OnlyInLoop loc str)
assertDecl loc str      = ifCtx [CLASS,PROTO,EXT]       []                  success (Control.Exception.throw $ OnlyInClassProtoExt loc str)
assertClass loc str     = ifCtx [CLASS]                 []                  success (Control.Exception.throw $ OnlyInClass loc str)
assertDef loc str       = ifCtx [DEF]                   [IF,SEQ,LOOP]       success (Control.Exception.throw $ OnlyInFunction loc str)
assertDefAct loc str    = ifCtx [DEF,ACTOR]             [IF,SEQ,LOOP]       success (Control.Exception.throw $ OnlyInFunctionOrActor loc str)
assertNotDecl loc str   = ifNotCtx [CLASS,PROTO,EXT]    [IF]                success (Control.Exception.throw $ NotInClassProtoExt loc str)
assertNotData loc str   = ifNotCtx [DATA]               [IF,SEQ,LOOP]       success (Control.Exception.throw $ NotInData loc str)

ifPar                   = ifCtx [PAR]                   []

--- Indentation error -------------------------------------------------------

data IndentationError  = IndentationError SrcLoc deriving (Show, Eq)

instance Control.Exception.Exception IndentationError

instance HasLoc IndentationError where
   loc (IndentationError l) = l

indentationError     :: IndentationError -> (SrcLoc, String)
indentationError err = (loc err, "Too much indentation")

-- Fail fast error ----------------------------------------------------------

data FailFastError = FailFastError SrcLoc String deriving (Show, Eq)

instance Control.Exception.Exception FailFastError


failFastError :: FailFastError -> (SrcLoc, String)
failFastError (FailFastError loc str) = (loc, str)

failImmediately loc msg =  Control.Exception.throw $ FailFastError loc msg

--- Whitespace consumers ----------------------------------------------------

-- Whitespace consumer, which *does not* consume newlines
-- but accepts line joining.
-- This is used outside parentheses/brackets/braces
sc1 :: Parser ()
sc1 = void $ do
  sc0
  optional (char '\\' *> eol *> sc0) <?> ""
  where sc0 = L.space (void $ takeWhile1P Nothing f) lineCmnt empty
        f x = x == ' ' || x == '\t'
        lineCmnt  = L.skipLineComment "#"

-- Whitespace consumer, which *does* consume also newlines.
-- Used inside parentheses/brackets/braces
sc2 :: Parser ()
sc2 = L.space space1 (L.skipLineComment "#") empty

currSC =  ifPar sc2 sc1


--- Adding position info to a parser ------------------------------------------

-- fetching the start column of a construct
withPos :: Parser a -> Parser (Pos,a)
withPos p = do
          c <- L.indentLevel
--          SourcePos f r c <- getSourcePos
          a <- p
--          getSourcePos
          return (c,a)

-- forcing a construct to start in prescribed column
atPos :: Pos -> Parser a -> Parser a
atPos p a = do
         p2 <- L.indentLevel
         if p2==p then a else L.incorrectIndent EQ p p2

withLoc :: Parser a -> Parser (SrcLoc,a)
withLoc p = do
          off1 <- getOffset
          a  <- p
          off2 <- getOffset
          return (Loc off1 off2, a)          

class AddLoc a where
  addLoc :: Parser a -> Parser a
  
instance AddLoc S.Import where
  addLoc p = do
         (l,stmt) <- withLoc p
         return stmt{S.iloc = l}

instance AddLoc S.Stmt where
  addLoc p = do
         (l,stmt) <- withLoc p
         return stmt{S.sloc = l}

instance AddLoc S.Decl where
  addLoc p = do
         (l,stmt) <- withLoc p
         return stmt{S.dloc = l}

instance AddLoc S.Expr where
  addLoc p = do
         (l,expr) <- withLoc p
         return expr{S.eloc = l}

instance AddLoc S.Name where
  addLoc p = do
         (l,name) <- withLoc p
         case name of
             S.Name _ n -> return (S.Name l n)
             _          -> return name

instance AddLoc S.Except where
  addLoc p = do
         (l,exc) <- withLoc p
         case exc of
           S.ExceptAll _ -> return (S.ExceptAll l)
           S.Except _ e -> return (S.Except l e)
           S.ExceptAs _ e nm -> return (S.ExceptAs l e nm)
     
instance AddLoc S.Sliz where
  addLoc p = do
          (l,i) <- withLoc p
          case i of
            S.Sliz _ e1 e2 e3 -> return (S.Sliz l e1 e2 e3)

instance AddLoc S.Comp where
  addLoc p = do
          (l,cmp) <- withLoc p
          case cmp of
             S.CompFor _ t e c -> return (S.CompFor l t e c)
             S.CompIf _ e c -> return (S.CompIf l e c)
             S.NoComp -> return S.NoComp

instance AddLoc S.TSchema where
  addLoc p = do
          (l, S.TSchema _ q t) <- withLoc p
          return $ S.TSchema l q t

instance AddLoc S.Type where
  addLoc p = do
          (l,ct) <- withLoc p
          return ct{S.tloc = l}

instance AddLoc S.Pattern where
  addLoc p = do
         (l,pat) <- withLoc p
         return pat{S.ploc = l}


rwordLoc :: String -> Parser SrcLoc
rwordLoc word = do
          off <- getOffset
          rword word
          return $ Loc off (off+length word)

locate (Loc l _) = setOffset l

--- Functions recognizing lexical items ----------------------------------------

lexeme:: Parser a -> Parser a
lexeme p = p <* currSC

symbol :: String -> Parser String
symbol str = lexeme (string str)

newline1 :: Parser [S.Stmt]
newline1 = const [] <$> (eol *> sc2)

--- Strings in their many variants -----------------------------------------------

{-

The following principles for bytes and string literals are implemented by the following parse functions and
the transformations in Acton.Normalize

Literal syntax is as in subsections 2.4.1 and 2.4.2 in the Python Language Reference, version 3.10.4, 
except that only the following prefixes are allowed:
- no prefix: plain string literal
- prefix 'b': plain bytes literal
- prefix 'r': raw string literal
- prefix 'rb': raw bytes literals. 

Thus we disallow
- upper case versions of the above prefixes (for no particular reason other than the opinion that it is not an 
  unreasonable burden on the programmer to have to stick to lower case prefixes). For similar reasons we disallow 
  prefix 'br' and thus just form a raw literal by prefixing a plain literal with 'r'.)
- the 'u' prefix which only exists in Python for legacy reasons.
- the 'f' prefix for formatted string literals; a version of these is instead supported in Acton
  using the % operator in the print statement.

Prefix sequences are also as in subsection 2.4.1 with the following exceptions
- unrecognized escape sequences (like \p or \z or any other sequence not listed in the table of 2.4.1)
  are disallowed (unlike Python where they are allowed but the language reference declares that they will
  become illegal in a future Python version.).
- universal character names \uxxxx and \Uxxxxxxxx are unrecognized in bytes literals.
- \x must be followed by exactly two hex digits (unlike in C). To avoid a compilation error in the
  generated C code, e.g. the string "\x12a" is changed to "\x12" "a" so that the 'a' is not interpreted by 
  the C compiler as a third hexadecimal digit in the \x... sequence. Note that C allows sequences of string 
  literals which are concatenated during C compilation.

-}

strings :: Parser S.Expr
strings = addLoc $
       S.BStrings NoLoc <$> some bytesLiteral
       <|>
       S.Strings NoLoc <$> some stringLiteral

bytesLiteral, stringLiteral :: Parser String
bytesLiteral =  plainbytesLiteral <|> rawbytesLiteral <?> "bytes literal"
stringLiteral = plainstrLiteral <|> rawstrLiteral <?> "string literal"

manyTillEsc, someTillEsc :: Parser String -> Parser String -> Parser String -> Parser [String]
manyTillEsc p esc end =  (const [] <$> end) <|> (someTillEsc p esc end)

someTillEsc p esc end = do
    a <- (char '\\' *> esc) <|> p
    b <- manyTillEsc p esc end
    return $ a : b

stringTempl :: String -> Parser String -> Parser String -> String -> Parser String
stringTempl q single esc prefix = (surround q . concat) <$> lexeme (string (prefix++q) >> manyTillEsc single esc (string q))
   where surround s str     = s ++ str ++ s

newlineEscape =  "" <$ newline
singleCharEscape =  (\c -> '\\':c:[]) <$> (oneOf ("\'\"abfnrtv"))
hexEscape = do
      char 'x'
      (loc,cs) <- withLoc (many hexDigitChar)
      if length cs == 2
       then return ("\\x" ++ cs)
       else failImmediately loc "\"\\x\" must be followed by exactly two hexadecimal digits"
octEscape = do
       (loc,cs) <- withLoc (count' 1 3 octDigitChar)
       if length cs == 3 && head cs > '3'
          then  failImmediately loc "octal escape sequence out of range"
          else return ("\\" ++ cs) 
univ1Escape = do
      char 'u'
      (loc,cs) <- withLoc (count' 0 4 hexDigitChar)
      if length cs < 4
        then failImmediately loc "Incomplete universal character name (4 hex digits needed)"
        else return ("\\u" ++ cs)
univ2Escape = do
      char 'U'
      (loc,cs) <- withLoc (count' 0 8 hexDigitChar)
      if length cs < 8
        then failImmediately loc "Incomplete universal character name (8 hex digits needed)"
        else return ("\\U" ++ cs)

asciiC   = do
      (loc,c) <- withLoc anySingle
      if c == '\n'
         then failImmediately loc "unescaped newline in single-quoted bytes literal"
         else if isAscii c
              then return [c]
              else failImmediately loc "Only ASCII chars allowed in bytes literal"

anyC  = do
      (loc,c) <- withLoc anySingle
      if c == '\n'
         then failImmediately loc "unescaped newline in single-quoted string literal"
         else return [c]

unknownEscape charParser = do
         (loc,c) <- withLoc charParser
         failImmediately loc "unknown escape sequence in bytes literal"
             
plainLiteral charParser prefix tailEscapes = stringTempl "\"\"\"" longItem esc prefix
                                          <|> stringTempl "'''" longItem esc prefix
                                          <|> stringTempl "\"" charParser esc prefix
                                          <|> stringTempl "'" charParser esc prefix
    where longItem = ("\\n" <$ newline) <|> charParser  -- newlines allowed in triple-quoted literals
          esc =  newlineEscape <|> singleCharEscape <|> hexEscape <|> octEscape <|> tailEscapes

plainbytesLiteral = plainLiteral asciiC "b" (unknownEscape asciiC)

plainstrLiteral = plainLiteral anyC "" ( univ1Escape <|> univ2Escape <|> unknownEscape anyC)


{-
plainbytesLiteral = stringTempl "\"\"\"" longItem esc "b"
                <|> stringTempl "'''" longItem esc "b"
                <|> stringTempl "\"" asciiC esc "b"
                <|> stringTempl "'" asciiC esc "b"
    where longItem = ("\\n" <$ newline) <|> asciiC  -- newlines allowed in triple-quoted literals
          esc =  newlineEscape <|> singleCharEscape <|> hexEscape <|> octEscape <|> unknownEscape
          unknownEscape = do
             (loc,c) <- withLoc asciiC
             failImmediately loc "unknown escape sequence in bytes literal"

plainstrLiteral = stringTempl "\"\"\"" longItem esc ""
                <|> stringTempl "'''" longItem esc ""
                <|> stringTempl "\"" charLit2 esc ""
                <|> stringTempl "'" charLit2 esc ""
    where longItem = ("\\n" <$ newline) <|> charLit2
          charLit2 = ( :[]) <$> anySingle
          esc =  newlineEscape <|> singleCharEscape <|> hexEscape <|> octEscape <|> univ1Escape <|> univ2Escape <|> unknownEscape
          unknownEscape = do
            (loc,c) <- withLoc charLit2
            failImmediately loc "unknown escape sequence in string literal"
-}

rawLiteral charParser prefix = stringTempl "\"\"\"" longItem esc prefix
              <|> stringTempl "'''" longItem esc prefix
              <|> stringTempl "\"" charParser esc prefix
              <|> stringTempl "'"  charParser  esc prefix 
   where longItem =  ("\\n" <$ newline) <|> charParser
         esc = newlineEscapeRaw <|> singleCharEscapeRaw <|> generalEscapeRaw
         newlineEscapeRaw = "\\\\\\n" <$ newline
         singleCharEscapeRaw = (\c -> "\\\\\\" ++ [c]) <$> (oneOf ("\'\""))
         generalEscapeRaw = return "\\\\"

rawbytesLiteral = rawLiteral asciiC "rb"

rawstrLiteral = rawLiteral ((:[]) <$> anySingle) "r"

 
-- Reserved words, other symbols and names ----------------------------------------------------------

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy (alphaNumChar <|> char '_'))

comma     = symbol "," <?> "comma"
colon     = symbol ":"
semicolon = symbol ";"
equals    = symbol "="
star      = symbol "*"
starstar  = symbol "**"
dot       = symbol "."
arrow     = symbol "->"
fatarrow  = symbol "=>"
qmark     = symbol "?"
vbar      = symbol "|"

-- Parser for operator that is a prefix of another operator
-- Slightly hackish; depends on the (presently true) fact that chars in argument to oneOf are
-- the only chars that can follow directly after the prefix operator in a longer operator name.
opPref :: String -> Parser String
opPref op = (lexeme . try) (string op <* notFollowedBy (oneOf "<>=/*"))

singleStar = (lexeme . try) (char '*' <* notFollowedBy (char '*'))

identifier :: Parser String
identifier = (lexeme . try) $ do
    off <- getOffset
    c <-  satisfy (\c -> isAlpha c || c=='_') <?> "identifier"
    cs <- hidden (takeWhileP Nothing (\c -> isAlphaNum c || c=='_'))
    let x = c:cs
    if S.isKeyword x
      then parseError (TrivialError off (Just (Tokens (N.fromList x))) (S.fromList [Label (N.fromList "identifier")]))
      else return x
      
name, escname, tvarname :: Parser S.Name
name = do off <- getOffset
          x <- identifier
          if isUpper (head x) && all isDigit (tail x)
            then parseError  (TrivialError off (Just (Tokens (N.fromList x))) (S.fromList [Label (N.fromList "name (not type variable)")]))
            else return $ S.Name (Loc off (off+length x)) x

escname = name <|> addLoc ((\str -> S.Name NoLoc  (init (tail str))) <$> plainstrLiteral)

tvarname = do off <- getOffset
              x <- identifier
              if isUpper (head x) && all isDigit (tail x)
               then return $ S.Name (Loc off (off+length x)) x
               else parseError (TrivialError off (Just (Tokens (N.fromList x))) (S.fromList [Label (N.fromList ("type variable (upper case letter optionally followed by digits)"))]))

module_name :: Parser S.ModName
module_name = do
  n <- name
  ns <- many (dot *> escname)
  return $ S.ModName (n:ns)

qual_name :: Parser S.QName
qual_name = do
  n <- name
  ns <- many (dot *> escname)
  case n:ns of
    [n] -> return $ S.NoQ n
    ns' -> return $ S.QName (S.ModName (init ns')) (last ns')
  

-- recognizers for numbers are used directly in function atom below.

--- Helper functions for parenthesised forms -----------------------------------

parens, brackets, braces :: Parser a -> Parser a
parens p = withCtx PAR (L.symbol sc2 "(" *> p <* (char ')' <?> "a closing parenthesis")) <* currSC

brackets p = withCtx PAR (L.symbol sc2 "[" *> p <* char ']') <* currSC

braces p = withCtx PAR (L.symbol sc2 "{" *> p <* char '}') <* currSC

--- Top-level parsers ------------------------------------------------------------
 
file_input :: Parser ([S.Import], S.Suite)
file_input = sc2 *>  do
    is <- imports
    s <-  withCtx TOP top_suite
    eof
    return (is,s)

-- (((,) <$> imports <*> withCtx TOP top_suite) <* eof)

imports :: Parser [S.Import]
imports = many (L.nonIndented sc2 import_stmt <* eol <* sc2)

top_suite :: Parser S.Suite
top_suite = concat <$> (many (L.nonIndented sc2 stmt <|> newline1))

-- Row parsers ----------------------------------------------------------------------

-- non-empty positional row without trailing comma
posItems:: (a -> b -> b) -> (c -> b) -> b -> Parser a -> Parser c -> Parser b
posItems fCons fStar fNil item staritem =
           fStar <$> (star *> staritem)
        <|> 
           do e <- item
              es <- many (try (comma *> item)) 
              mbe <- optional (try (comma *> star *> staritem))
              let tail = maybe fNil fStar mbe
              return (foldr fCons tail (e:es))

--non-empty kwd row with optional trailing comma
kwdItems:: (a -> b -> b) -> (c -> b) -> b -> Parser a -> Parser c -> Parser b
kwdItems fCons fStar fNil item staritem =
          fStar <$> (starstar *> staritem <* optional comma)
        <|>
          do b <- item
             bs <- many (try (comma *> item))
             mbe <- optional (try (comma *> optional ((starstar *> staritem) <* optional comma )))
             let tail = maybe fNil (maybe fNil fStar) mbe
             return (foldr fCons tail (b:bs))

-- parameter list consisting of posItems followed by kwdItems
-- Unfortunately, the parser below does not make use of posItems
funItems :: (a1 -> t1 -> t1) -> (a -> t1) -> t1 -> Parser a1 -> Parser a -> Parser t -> t -> Parser (Either (t1,t) a1)
funItems posCons posStar posNil positem posstaritem kwdItems kwdNil =
            do i <- singleStar *> posstaritem
               mbc <- optional comma
               case mbc of
                  Just _ -> do k <- kwdItems
                               return  (Left (posStar i, k))
                           <|>
                               return  (Left (posStar i, kwdNil))
                  Nothing ->  return  (Left (posStar i, kwdNil))
           <|>
            try (do k <- kwdItems; return (Left (posNil, k)))
           <|>
             do i <- positem
                mbc <- optional comma
                case mbc of
                   Just _ -> do r <- funItems posCons posStar posNil positem posstaritem kwdItems kwdNil
                                case r of
                                   Left (p,k) -> return (Left (posCons i p, k))
                                   Right p -> return (Left (posCons i (posCons p posNil),kwdNil))
                             <|> return (Left (posCons i posNil, kwdNil))
                   Nothing -> return (Right i)
            <|>
               do optional comma; return (Left (posNil, kwdNil))
 
tuple_or_single posItems headItems singleHead tup =
      do pa <- posItems
         mbc <- optional comma
         return (f pa mbc)
    where f pa mbc
            | singleHead pa = maybe (headItems pa) (const (tup pa)) mbc
            | otherwise  = tup pa
        
 
-- Patterns ------------------------------------------------------------------------------------

pospat :: Parser S.PosPat
pospat = posItems S.PosPat S.PosPatStar S.PosPatNil apat apat

kwdpat :: Parser S.KwdPat 
kwdpat = kwdItems (uncurry S.KwdPat) S.KwdPatStar S.KwdPatNil kpat apat
     where kpat = do v <- name
                     equals
                     p <- apat
                     return (v,p)
     
gen_pattern = do r <- funItems  S.PosPat S.PosPatStar S.PosPatNil apat apat (fail "No kwdpat") S.KwdPatNil
                 case r of
                   Left (p,k) -> return $ S.PTuple NoLoc p k
                   Right p -> return p

gen_pattern1 = do r <- funItems  S.PosPat S.PosPatStar S.PosPatNil apat apat kwdpat S.KwdPatNil
                  case r of
                    Left (p,k) -> return $ S.PTuple NoLoc p k
                    Right p -> return p

pelems ::Parser ([S.Pattern], Maybe S.Pattern)
pelems = do
    p <- apat
    ps <- many (try (comma *> apat))
    mbp <- optional (comma *> star *> apat)
    return (p:ps, mbp)

apat :: Parser S.Pattern
apat = addLoc (
            (try $ S.PVar NoLoc <$> name <*> optannot)
        <|>
            ((try . parens) $ return $ S.PParen NoLoc (S.PTuple NoLoc S.PosPatNil S.KwdPatNil))
        <|>
            ((try . parens) $ S.PParen NoLoc <$> gen_pattern1)
        <|>
            (brackets $ (maybe (S.PList NoLoc [] Nothing) (\(ps,mbp)-> S.PList NoLoc ps mbp)) <$> optional pelems)
        )
  where  -- datapat = S.PData NoLoc <$> escname <*> many (brackets exprlist)
        optannot = try (Just <$> (colon *> ttype)) <|> return Nothing

-- Targets -------------------------------------------------------------------------------------

target :: Parser S.Target
target = try $ do 
            tmp <- atom_expr
            case tmp of
                S.Var _ (S.NoQ n) -> return tmp
                S.Dot _ e n       -> return tmp
                S.Index _ e ix    -> return tmp
                S.Slice _ e sl    -> return tmp
                _                 -> locate (loc tmp) >> fail ("illegal target: " ++ show tmp)

------------------------------------------------------------------------------------------------
-- Statements ----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

stmt, simple_stmt :: Parser [S.Stmt]
stmt = ((:[]) <$> compound_stmt)  <|> try ((:[]) <$> (signature <* newline1)) <|> decl_group <|> simple_stmt <?> "statement"

simple_stmt = (small_stmt `sepEndBy1` semicolon) <* newline1 <?> "simple statement"

--- Small statements ---------------------------------------------------------------------------------

small_stmt :: Parser S.Stmt
small_stmt = del_stmt <|> pass_stmt <|> flow_stmt <|> assert_stmt <|> var_stmt <|> after_stmt  <|> expr_stmt

expr_stmt :: Parser S.Stmt
expr_stmt = addLoc $ do
            o <- getOffset
            try ((S.AugAssign NoLoc <$> target <*> augassign <*> rhs) <* assertNotData (Loc o o) "augmented assignment")
              <|> try (S.Assign NoLoc <$> trysome assign <*> rhs)                                 -- Single variable lhs matches here
              <|> try (S.MutAssign NoLoc <$> target <* equals <*> rhs)                            -- and not here
              <|> (((S.Expr NoLoc <$> rhs) <* assertNotData (Loc o o) "call") <?> "expression statement")
   where augassign :: Parser S.Aug
         augassign = augops
          where augops = S.PlusA   <$ symbol "+="
                     <|> S.MinusA  <$ symbol "-="
                     <|> S.MultA   <$ symbol "*="
                     <|> S.MMultA  <$ symbol "@="
                     <|> S.DivA    <$ symbol "/="
                     <|> S.ModA    <$ symbol "%="
                     <|> S.BAndA   <$ symbol "&="
                     <|> S.BOrA    <$ symbol "|="
                     <|> S.BXorA   <$ symbol "^="
                     <|> S.ShiftLA <$ symbol "<<="
                     <|> S.ShiftRA <$ symbol ">>="
                     <|> S.PowA    <$ symbol "**="
                     <|> S.EuDivA  <$ symbol "//="

assign :: Parser S.Pattern
assign = gen_pattern <* equals

rhs :: Parser S.Expr
rhs = yield_expr <|> exprlist

trysome p = do x <- p; rest [x]
  where rest xs = (try p >>= \x -> rest (x:xs)) <|> return (reverse xs)

after_stmt :: Parser S.Stmt
after_stmt = addLoc $ do
                l <- rwordLoc "after"
                assertDefAct l "after"
                e <- expr
                colon
                e' <- addLoc $ do
                    n <- name
                    (ps,ks) <- parens funargs
                    return $ S.Call NoLoc (S.Var (S.nloc n) (S.NoQ n)) ps ks
                return $ S.After NoLoc e e'

var_stmt :: Parser S.Stmt
var_stmt = addLoc $ do
             l <- rwordLoc "var"
             assertActBody l "var"
             S.VarAssign NoLoc <$> trysome assign <*> rhs

del_stmt = addLoc $ do
            l <- rwordLoc "del"
            assertNotData l "del"
            S.Delete NoLoc <$> target

pass_stmt =  S.Pass <$> rwordLoc "pass"

flow_stmt = break_stmt <|>  continue_stmt <|>  return_stmt <|>  raise_stmt <|>  yield_stmt

break_stmt =  do l <- rwordLoc "break"
                 assertLoop l "break"
                 return (S.Break l)

continue_stmt =  do l <- rwordLoc "continue"
                    assertLoop l "continue"
                    return (S.Continue l)

return_stmt = addLoc $ do    -- the notFollowedBy colon here is to avoid confusion with data_stmt return case
                l <- rwordLoc "return" <* notFollowedBy colon
                assertDef l "return"
                S.Return NoLoc <$> optional exprlist
 
yield_stmt = yield_expr >>= \e -> return $ S.Expr (S.eloc e) e

raise_stmt = addLoc $ do
               rword "raise"
               S.Raise NoLoc <$> (optional $ S.Exception <$> expr <*> optional (rword "from" *> expr))
                                     
import_stmt = import_name <|> import_from <?> "import statement"
   where import_name = addLoc $ do
                rword "import"
                S.Import NoLoc <$> module_item `sepBy1` comma
                
         module_item = do
                dn@(S.ModName ns) <- module_name
                iff (length ns==1 && S.nstr(head ns) == "numpy") $  St.modify (setNumpy True)
                S.ModuleItem dn <$> optional (rword "as" *> name)
                          
         import_from = addLoc $ do
                rword "from"
                mr <- import_module
                rword "import"
                is <- import_items
                case is of
                   [] -> return $ S.FromImportAll NoLoc mr
                   _  -> return $ S.FromImport NoLoc mr is
                 
         import_module = do
                ds <- many dot
                mbn <- optional module_name
                let isNumpy Nothing = False
                    isNumpy (Just (S.ModName ns)) = length ns==1 && S.nstr(head ns) == "numpy"
                iff (null ds && isNumpy mbn) $ St.modify (setNumpy True)
                return $ S.ModRef (length ds, mbn)
         import_items = ([] <$ star)   -- Note: [] means all...
                     <|> parens import_as_names
                     <|> import_as_names
         import_as_name = S.ImportItem <$> name <*> optional (rword "as" *> name)
         import_as_names = (:) <$> import_as_name <*> commaList import_as_name 

-- global_stmt: 'global' NAME (',' NAME)*
-- nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
-- assert_stmt: 'assert' expr [',' expr]

assert_stmt = addLoc (rword "assert" >> S.Assert NoLoc <$> expr <*> optional (comma *> expr))

signature :: Parser S.Stmt
signature = addLoc (do dec <- decorator True; (ns,t) <- tsig; return $ S.Signature NoLoc ns t dec)
   where tsig = do v <- name
                   vs <- commaList name
                   colon
                   t <- tschema
                   return (v:vs,t)

-- Declaration groups ------------------------------------------------------------------

decl_group :: Parser [S.Stmt]
decl_group = do p <- L.indentLevel
                g <- some (atPos p decl)
                return [ S.Decl (loc ds) ds | ds <- Names.splitDeclGroup g ]

decl :: Parser S.Decl
decl = funcdef <|> classdef <|> protodef <|> extdef <|> actordef

decorator :: Bool -> Parser S.Deco
decorator sig = do
       p <- L.indentLevel
       d <- decoration
       p1 <- L.indentLevel
       if (p /= p1)
         then fail "Decorated statement must have same indentation as decoration"
         else return d
   where property = do l <- rwordLoc "@property"
                       assertClass l "@property"
                       newline1
                       return S.Property
         static   = do l <-rwordLoc "@staticmethod" <|> rwordLoc "@static"
                       assertDecl l "@static"
                       newline1
                       return S.Static
         decoration = (if sig then property <|> static else static) <|> return S.NoDec

funcdef :: Parser S.Decl
funcdef =  addLoc $ do
              (p,(deco,fx,l)) <- withPos (((,,) <$> decorator False <*> optional effect <*> rwordLoc "def"))
              assertNotData l "def"
              n <- name
              q <- optbinds
              (ppar,kpar) <- parens (funpars True)
              S.Def NoLoc n q ppar kpar <$> optional (arrow *> ttype) <*> suite DEF p <*> return deco <*> return (maybe S.tWild id fx)


binds :: Parser S.QBinds
binds = brackets (do b <- qbind; bs <- many (comma *> qbind); return (b:bs))

optbinds :: Parser S.QBinds
optbinds = binds <|> return []

actordef = addLoc $ do
                (s,l) <- withPos (rwordLoc "actor")
                assertTop l "actor"
                nm <- name <?> "actor name"
                q <- optbinds
                (ppar,kpar) <- parens (funpars True)
                ss <- suite ACTOR s
                return $ S.Actor NoLoc nm q ppar kpar ss

-- classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
-- protodef: 'class' NAME ['(' [arglist] ')'] ':' suite
-- extdef: 'class' NAME ['(' [arglist] ')'] ':' suite

classdef    = classdefGen "class" name CLASS S.Class
protodef    = classdefGen "protocol" name PROTO S.Protocol
--extdef      = classdefGen "extension" qual_name EXT S.Extension

classdefGen k pname ctx con = addLoc $ do
                (s,l) <- withPos (rwordLoc k)
                assertTop l k
                nm <- pname
                q <- optbinds
                cs <- optbounds
                con NoLoc nm q cs <$> suite ctx s

extdef = addLoc $ do
                (s,l) <- withPos (rwordLoc "extension")
                assertTop l "extension"
                (q,c) <- try head1 <|> try head2 <|> head3
                cs <- optbounds
                S.Extension NoLoc q c cs <$> suite EXT s
  where head1 = do q <- binds
                   fatarrow
                   c <- tcon
                   return (q, c)
        head2 = do c <- tcon
                   return ([], c)
        head3 = do n <- qual_name
                   q <- binds
                   return (q, S.TC n [ S.tVar v | S.Quant v _ <- q ])

-- Compound statements -------------------------------------------------------------------------

compound_stmt :: Parser S.Stmt
compound_stmt =  if_stmt <|> while_stmt <|> for_stmt <|> try_stmt <|> with_stmt -- <|> data_stmt


else_part p = atPos p (rword "else" *> suite SEQ p)

if_stmt = addLoc $ do
             (p,_) <- withPos (rword "if")
             b <- branch p
             bs <- many (atPos p (rword "elif" *> branch p))
             S.If NoLoc (b:bs) . maybe [] id <$>  optional (else_part p)

branch p = S.Branch <$> expr <*> suite IF p

while_stmt = addLoc $ do
                 (p,l) <- withPos (rwordLoc "while")
                 assertNotDecl l "while"
                 e <- expr
                 ss1 <- suite LOOP p
                 S.While NoLoc e ss1 . maybe [] id <$>  optional (else_part p)
                 
for_stmt = addLoc $ do
                 (p,l) <- withPos (rwordLoc "for")
                 assertNotDecl l "for"
                 pat <- gen_pattern
                 rword "in"
                 e <- exprlist
                 ss <- suite LOOP p
                 S.For NoLoc pat e ss . maybe [] id <$> optional (else_part p)
      
except :: Parser S.Except
except = addLoc $ do
             rword "except"
             mbx <- optional ((,) <$> qual_name <*> optional (rword "as" *> name))
             return (maybe (S.ExceptAll NoLoc) (\(x,mbn) -> maybe (S.Except NoLoc x) (S.ExceptAs NoLoc x) mbn) mbx)
            
try_stmt = addLoc $ do
                (p,l) <- withPos (rwordLoc "try")
                assertNotData l "try"
                assertNotDecl l "try"
                ss <- suite SEQ p
                do
                    hs <- some (handler p)
                    mbe <- optional (else_part p)
                    S.Try NoLoc ss hs (maybe [] id mbe) . maybe [] id <$>  optional (finally_part p)
                   <|>
                    S.Try NoLoc ss [] [] <$> finally_part p
  where handler :: Pos -> Parser S.Handler
        handler p = atPos p $ do
                        exc <- except
                        S.Handler exc <$> suite SEQ p
        finally_part p = atPos p $ do
                        rword "finally"
                        suite SEQ p
                 
with_stmt = addLoc $ do
                (s,l) <- withPos (rwordLoc "with")
                assertNotData l "with"
                assertNotDecl l "with"
                S.With NoLoc <$> (with_item `sepBy1` comma) <*> suite SEQ s
  where with_item = S.WithItem <$> expr <*> optional (rword "as" *> gen_pattern)
                 
 
data_stmt = addLoc $
           do (s,pat) <- withPos gen_pattern
              S.Data NoLoc (Just pat) <$> suite DATA s
        <|>
           do (s,l) <- withPos (rwordLoc "return")
              assertDef l "data"
              S.Data NoLoc Nothing <$> suite DATA s

suite :: CTX -> Pos -> Parser S.Suite
suite c p = do
    o <- getOffset
    withCtx c colon
    withCtx c (indentSuite p <|> simple_stmt)
  where indentSuite p = do
          newline1
          p1 <- L.indentGuard sc1 GT p
          concat <$> some (do
             p2 <- L.indentLevel
             case compare p1 p2 of
                LT -> do o <- getOffset
                         Control.Exception.throw $ IndentationError (Loc o o) --L.incorrectIndent LT p1 p2
                EQ -> stmt
                GT -> L.incorrectIndent GT p2 p1)

------------------------------------------------------------------------------------------------
--- Expressions ----------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- The most general form of expression
expr :: Parser S.Expr
expr =  lambdef
       <|>
        do
          e1 <- or_expr
          mbp <- optional if_part
          case mbp of
            Nothing -> return e1
            Just (c,e2) -> return $ S.Cond (S.eloc e1) e1 c e2
   where if_part = (,) <$> (rword "if" *> or_expr) <*> (rword "else" *> expr) <?> "if clause"

-- Non-empty list of comma-separated expressions.
-- If more than one expr, build a tuple.
-- if only one, leave as it is unless there is a trailing comma when we build a one-element tuple.
exprlist :: Parser S.Expr
exprlist = addLoc $ tuple_or_single posarg S.posArgHead S.singlePosArg (\p -> S.Tuple NoLoc p S.KwdNil)
 

expr_nocond = or_expr <|> lambdef_nocond

lambdefGen t = addLoc $ do
            fx <- optional effect
            rword "lambda"
            (ppar,kpar) <- funpars False
            colon
            S.Lambda NoLoc ppar kpar <$> t <*> return (maybe S.tWild id fx)

lambdef = try $ lambdefGen expr
lambdef_nocond = try $ lambdefGen expr_nocond

-- Logical expressions ------------------------------------------------------------------

-- The intermediate levels between or_expr and comparison, i.e. expressions involving and, or and not,
-- are handled by makeExprParser from Text.Megaparsec.Expr 6.4.0,  here Text_Megaparsec_Expr

-- Three auxiliary functions used in building tables for makeExprParser
binary name op = InfixL $ do
                name
                return $ \e1 e2 ->  S.BinOp (S.eloc e1 `upto` S.eloc e2) e1 op e2

unop name op = do
                name
                return $ \e -> S.UnOp (S.eloc e) op e

prefix name op = Prefix (unop name op)

or_expr = makeExprParser comparison btable 

btable :: [[Operator Parser S.Expr]]
btable = [ [ prefix (rwordLoc "not") S.Not]
         , [ binary (rwordLoc "and") S.And]
         , [ binary (rwordLoc "or") S.Or] ]

comparison = addLoc (do
  e <- arithexpr
  ps <- many (do
                 op <- comp_op
                 S.OpArg op <$> arithexpr)
  case ps of
        [] -> return e
        _  -> return $ S.CompOp NoLoc e ps) <?> "relational expression"
   where comp_op = S.Lt <$ opPref "<"
                <|> S.Gt <$ opPref ">"
                <|> S.Eq <$ symbol "=="
                <|> S.GE <$ symbol ">="
                <|> S.LE <$ symbol "<="
                <|> S.LtGt <$ symbol "<>"
                <|> S.NEq <$ symbol "!="
                <|> S.In <$ symbol "in"
                <|> S.NotIn <$ (symbol "not" *> symbol "in")
                <|> S.IsNot <$ try (symbol "is" *> symbol "not")
                <|> S.Is <$ (symbol "is")
                <?> "operator"

star_expr :: Parser S.Elem
star_expr = S.Star <$> (star *> arithexpr)                                        

-- Arithmetic expressions ----------------------------------------------------------
-- Again, everything between arithexpr and factor is handled by makeExprParser

arithexpr :: Parser S.Expr
arithexpr = makeExprParser factor table 

table :: [[Operator Parser S.Expr]]
table = [ [ binary (opPref "*") S.Mult, binary (opPref "/") S.Div, binary (opPref "@") S.MMult,
            binary (opPref "//") S.EuDiv, binary (opPref "%") S.Mod]
        , [ binary (opPref "+") S.Plus, binary (opPref "-") S.Minus]
        , [ binary (opPref "<<") S.ShiftL, binary (opPref ">>") S.ShiftR]
        , [ binary (opPref "&") S.BAnd]
        , [ binary (opPref "^") S.BXor]
        , [ binary (opPref "|") S.BOr]
        ]

factor :: Parser S.Expr
factor = (((unop (opPref "+") S.UPlus <|> unop (opPref "-") S.UMinus <|> unop (opPref "~") S.BNot) <?> "unary operator") <*> factor)
        <|> power
        
power = addLoc $ do
           ae <- atom_expr
           mbe <- optional expo
           return (maybe ae (S.BinOp NoLoc ae S.Pow) mbe)
  where expo = do opPref "**"
                  factor
                <?> "operator"
                
isinstance = addLoc $ do
                rword "isinstance"
                (e,c) <- parens ((,) <$> expr <* comma <*> qual_name)
                return $ S.IsInstance NoLoc e c

-- recurring pattern below
commaList p = many (try (comma *> p)) <* optional comma

atom_expr = do
              await <- optional $ withLoc $ rword "await" *> return (S.Await NoLoc)
              async <- optional $ withLoc $ rword "async" *> return (S.Async NoLoc)
              a <- atom
              ts <- many trailer
              let e = foldl app a ts
                  e' = maybe e (app e) async
              return $ maybe e' (app e') await 
              <?> "atomic expression"
  where app a (l,f) = (f a){S.eloc = S.eloc a `upto` l}
             
        atom :: Parser S.Expr
        atom =  addLoc (try strings
               <|>
                 ((try . parens) $ return $ S.Paren NoLoc (S.Tuple NoLoc S.PosNil S.KwdNil))
               <|>
                 ((try . parens) $ S.Paren NoLoc <$> yield_expr)
               <|>
                 (parens $ S.Paren NoLoc <$> expr_or_tuplemaker)
               <|>
                 (brackets $ do
                             mbe <- optional listmaker
                             return $ maybe (S.List NoLoc []) id mbe)
               <|>
                 (braces $ do
                             mbe <- optional dictorsetmaker
                             return $ maybe (S.Dict NoLoc []) id mbe)
               <|> var
               <|> isinstance
               <|> (try ((\f -> S.Imaginary NoLoc f (show f ++ "j")) <$> lexeme (L.float <* string "j")))
               <|> (try ((\f -> S.Float NoLoc f (show f)) <$> lexeme L.float))
               <|> (\i -> S.Int NoLoc i ("0o"++showOct i "")) <$> (string "0o" *> lexeme L.octal)
               <|> (\i -> S.Int NoLoc i ("0x"++showHex i "")) <$> (string "0x" *> lexeme L.hexadecimal)
               <|> (\i -> S.Int NoLoc i (show i)) <$> (lexeme L.decimal)
               <|> (S.Ellipsis <$> rwordLoc "...")
               <|> (S.None <$>  rwordLoc "None")
               <|> (S.NotImplemented  <$>  rwordLoc "NotImplemented")
               <|> (\l -> S.Bool l True) <$> rwordLoc "True"
               <|> (\l -> S.Bool l False) <$> rwordLoc "False")

        expr_or_tuplemaker              = do r <- funItems S.PosArg S.PosStar S.PosNil expr expr kwdarg S.KwdNil
                                             case r of
                                                Left (p,k) -> return (S.Tuple NoLoc p k)
                                                Right e -> return e
             

        -- common pattern in functions building lists, sets and dictionaries
        maker constr constrComp p = do
                   (l,a) <- withLoc p
                   (constrComp l a <$> comp_for) <|> ((\as -> (constr l (a:as))) <$> commaList p)

        -- exprlist_comp version used in  brackets, building a list
        listmaker = maker S.List S.ListComp elem 
           where elem = (S.Elem <$> expr) <|> star_expr

        dictorsetmaker :: Parser S.Expr
        dictorsetmaker = (try $ maker S.Dict S.DictComp assoc)
                            <|> maker S.Set S.SetComp elem
             where elem = (S.Elem <$> expr) <|> star_expr
                   assoc = (S.Assoc <$> expr) <*> (colon *> expr)
                        <|>
                           (S.StarStar <$> (starstar *> arithexpr))

        var = do nm <- name
                 return (S.Var (S.nloc nm) (S.NoQ nm))

        trailer :: Parser (SrcLoc,S.Expr -> S.Expr)
        trailer = withLoc (
                      (do
                        ss <- brackets bslicelist
                        numpyImp <- St.gets getNumpy
                        return (\a -> splitlist a numpyImp ss))
                    <|>
                      (do
                         (ps,ks) <- parens funargs
                         return (\a -> S.Call NoLoc a ps ks))
                     <|>
                      (do
                         dot
                         try intdot <|> iddot <|> strdot))
                     <?> "call arguments, slice/index expression"
                 
           where iddot  = do
                        mb <- optional (opPref "~")
                        nm <- name
                        return (\a -> maybe (S.Dot NoLoc a nm) (const $ S.Rest NoLoc a nm) mb)
                 intdot  = do 
                        mb <- optional (opPref "~")
                        i <- lexeme L.decimal
                        return (\a -> maybe (S.DotI NoLoc a i) (const $ S.RestI NoLoc a i) mb)
                 strdot = do
                        (p,str) <- withPos plainstrLiteral 
                        return (\a -> S.Dot NoLoc a (S.Name NoLoc (init(tail str))))

                 bslicelist = (:) <$> bslice <*> commaList bslice
                 tailslice = (,) <$> (colon *> optional expr) <*> (maybe Nothing id <$> optional (colon *> optional expr))
                 bslice :: Parser S.NDSliz
                 bslice =  S.NDSliz . uncurry (S.Sliz NoLoc Nothing) <$> tailslice
                       <|> do e <- expr
                              mbt <- optional tailslice
                              return (maybe (S.NDExpr e) (S.NDSliz . uncurry (S.Sliz NoLoc (Just e))) mbt)
                 splitlist a _ [S.NDExpr e] = S.Index NoLoc a e
                 splitlist a _ [S.NDSliz s] = S.Slice NoLoc a s
                 splitlist a numpyImp ss
                     | not numpyImp && all isNDExpr ss = S.Index NoLoc a (S.eTuple [e | S.NDExpr e <- ss])
                     | otherwise                       = S.NDSlice NoLoc a ss
                 isNDExpr (S.NDExpr _) =True; isNDExpr _ = False
                     
 
comp_iter, comp_for, comp_if :: Parser S.Comp
comp_iter = comp_for <|> comp_if

comp_for = addLoc (do
            rword "for"
            pat <- gen_pattern
            rword "in"
            e <- or_expr
            S.CompFor NoLoc pat e . maybe S.NoComp id <$> optional comp_iter)

comp_if = addLoc $ do
            rword "if"
            e <- expr_nocond
            S.CompIf NoLoc e . maybe S.NoComp id <$> optional comp_iter
           
yield_expr = addLoc $ do 
             l <- rwordLoc "yield"
             assertDef l "yield"
             (S.YieldFrom NoLoc <$> (rword "from" *> expr)
              <|> S.Yield NoLoc <$> optional exprlist)


--- Params ---------------------------------------------------------------------

parm :: Bool -> Parser (S.Name, Maybe S.Type, Maybe S.Expr)
parm ann = do n <- name
              mbt <- if ann then optional (colon *> ttype) else return Nothing
              mbe <- optional (equals *> expr)
              return (n, mbt, mbe)

pstar :: Bool -> Parser S.Type -> Parser (S.Name, Maybe S.Type)
pstar ann startype = do n <- name
                        mbt <- if ann then optional (colon *> startype) else return Nothing
                        return (n, mbt)

pstartype :: Parser S.Type
pstartype = parens (S.TTuple NoLoc <$> (posrow <|> return S.posNil) <*> return S.kwdNil <* optional comma)

kstartype :: Parser S.Type
kstartype = parens (S.TTuple NoLoc S.posNil <$> (kwdrow <|> return S.kwdNil) <* optional comma)

pospar :: Bool -> Parser S.PosPar
pospar ann = posItems (\(n,t,e) par -> S.PosPar n t e par) (uncurry S.PosSTAR) S.PosNIL (parm ann) (pstar ann pstartype)

kwdpar :: Bool -> Parser S.KwdPar
kwdpar ann = kwdItems (\(n,t,e) par -> S.KwdPar n t e par) (uncurry S.KwdSTAR) S.KwdNIL (parm ann) (pstar ann kstartype)

funpars :: Bool -> Parser (S.PosPar, S.KwdPar)
funpars ann =   (\(n,mbt) -> (S.PosNIL,S.KwdSTAR n mbt)) <$> (starstar *> pstar ann kstartype <* optional comma)
            <|> do ps <- pospar ann
                   mbmbks <- optional (comma *> optional (kwdpar ann))
                   return (maybe (ps,S.KwdNIL) (maybe (ps,S.KwdNIL) (\ks -> (ps,ks))) mbmbks)
            <|> return (S.PosNIL,S.KwdNIL)
 
--- Args -----------------------------------------------------------------------

-- Position/Keyword lists of expr's.
-- posarg is used in exprlist to build the general form of comma-separated expressions 

kwdbind :: Parser (S.Name, S.Expr) 
kwdbind = do v <- escname
             equals
             e <- expr
             return (v,e)

posarg :: Parser S.PosArg
posarg = posItems S.PosArg S.PosStar S.PosNil expr expr

kwdarg :: Parser S.KwdArg
kwdarg = kwdItems (uncurry S.KwdArg) S.KwdStar S.KwdNil kwdbind expr

funargs :: Parser (S.PosArg, S.KwdArg)
funargs = do r <- funItems S.PosArg S.PosStar S.PosNil expr expr kwdarg S.KwdNil
             case r of
               Left p -> return p
               Right t -> return (S.PosArg t S.PosNil, S.KwdNil)

--- Types ----------------------------------------------------------------------

effect  :: Parser S.Type
effect  = addLoc $  
            S.TVar NoLoc <$> tvar
        <|> rword "_" *> return (S.TWild NoLoc)
        <|> rword "action" *> return S.fxAction
        <|> rword "mut" *> return S.fxMut
        <|> rword "pure" *> return S.fxPure
  where optvar f = brackets (f <$> (addLoc $ S.TVar NoLoc <$> tvar)) <|> return S.tWild

posrow :: Parser S.PosRow 
posrow = posItems S.posRow S.posVar S.posNil ttype (optional tvar)

kwdrow :: Parser S.KwdRow                   
kwdrow = kwdItems (uncurry S.kwdRow) S.kwdVar S.kwdNil tsig1 (optional tvar)
   where tsig1 = do v <- name
                    colon
                    t <- ttype
                    return (v,t)
 
funrows :: Parser (S.PosRow, S.KwdRow)
funrows = do r <- funItems S.posRow S.posVar S.posNil ttype (optional tvar) kwdrow S.kwdNil
             case r of
               Left p -> return p
               Right t -> return (S.posRow t S.posNil, S.kwdNil)
               
tcon :: Parser S.TCon
tcon =  do n <- qual_name
           args <- optional (brackets (do t <- ttype
                                          ts <- commaList ttype
                                          return (t:ts)))
           return $ S.TC n (maybe [] id args)

tvar :: Parser S.TVar
tvar = S.TV S.KWild <$> try tvarname

qbind :: Parser S.QBind
qbind = S.Quant <$> tvar <*> optbounds

optbounds :: Parser [S.TCon]
optbounds = do bounds <- optional (parens (optional ((:) <$> tcon <*> commaList tcon)))
               return $ maybe [] (maybe [] id) bounds

tschema :: Parser S.TSchema
tschema = addLoc $
            do 
                bs <- brackets (do n <- qbind
                                   ns <- commaList qbind
                                   return (n:ns))
                fatarrow
                t <- ttype
                return (S.tSchema bs t)
            <|>
            (S.monotype <$> ttype)

ttype :: Parser S.Type
ttype    =  addLoc (
            rword "None" *> return (S.TNone NoLoc)
        <|> (S.TVar NoLoc . S.TV S.KType) <$> (S.Name <$> rwordLoc "Self" <*> return "Self")
        <|> S.TOpt NoLoc <$> (qmark *> ttype)
        <|> braces (do t <- ttype
                       mbt <- optional (colon *> ttype)
                       return (maybe (Builtin.tSetExist t) (Builtin.tMapping t) mbt))
        <|> try (do mbfx <- optional effect
                    (p,k) <- parens funrows
                    arrow
                    t <- ttype
                    return (S.TFun NoLoc (maybe S.fxPure id mbfx) p k t))
        <|> try (do r <- parens (funItems S.posRow S.posVar S.posNil ttype (optional  tvar) kwdrow S.kwdNil)
                    case r of
                      Left (p,k) -> return (S.TTuple NoLoc p k)
                      Right t -> return t)
        <|> parens (return (S.TTuple NoLoc S.posNil S.kwdNil))
        <|> try (brackets (Builtin.tSequence <$> ttype))
        <|> try (S.TVar NoLoc <$> tvar)
        <|> rword "_" *> return (S.TWild NoLoc)
        <|> S.TCon NoLoc <$> tcon)
