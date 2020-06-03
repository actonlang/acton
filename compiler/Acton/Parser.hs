module Acton.Parser where

import qualified Control.Monad.Trans.State.Strict as St
import qualified Control.Exception
import Control.Monad (void)
import Data.Void
import Data.Char
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.List.NonEmpty
import qualified Acton.Syntax as S
import qualified Acton.Builtin as Builtin
import qualified Acton.Names as Names
import Utils
import Debug.Trace
import System.IO.Unsafe

--- Main parsing and error message functions ------------------------------------------------------

parseModule :: S.ModName -> String -> IO (String,S.Module)
parseModule qn file = do
    contents <- readFile file
    case runParser (St.evalStateT file_input []) file (contents ++ "\n") of
        Left err -> Control.Exception.throw err
        Right (i,s) -> return (contents, S.Module qn i s)

parseTest file = snd (unsafePerformIO (parseModule (S.modName ["test"]) file))

parseTestStr p str = case runParser (St.evalStateT p []) "" str of
                       Left err -> putStrLn (errorBundlePretty err)
                       Right t  -> print t

parserError :: ParseErrorBundle String Void -> (SrcLoc,String)
parserError err = (NoLoc,errorBundlePretty err)

extractSrcSpan :: SrcLoc -> String -> String -> SrcSpan
extractSrcSpan NoLoc file src = SpanEmpty
extractSrcSpan (Loc l r) file src = sp
  where Right sp = runParser (St.evalStateT (extractP l r) []) file (src ++ "\n")
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

-- Parser contexts ---------------------------------------------------------

type Parser = St.StateT [CTX] (Parsec Void String)

data CTX = TOP | PAR | IF | SEQ | LOOP | DATA | DEF | CLASS | PROTO | EXT | ACTOR deriving (Show,Eq)

withCtx ctx = between (St.modify (ctx:)) (St.modify tail)

ifCtx accept ignore yes no = do
    cs <- St.get
    case filter (`notElem` ignore) cs of
        c:_ | c `elem` accept -> yes
        _                     -> no

onlyIn s            = fail ("statement only allowed inside " ++ s)
notIn s             = fail ("statement not allowed inside " ++ s)
success             = return ()

assertTop           = ifCtx [TOP]                   [IF]                success (fail "declaration only allowed on the module top level")
assertActBody       = ifCtx [ACTOR]                 []                  success (fail "statement only allowed inside an actor body")
assertActScope      = ifCtx [ACTOR]                 [IF,SEQ,LOOP,DEF]   success (fail "statement only allowed inside an actor scope")
assertLoop          = ifCtx [LOOP]                  [IF,SEQ]            success (fail "statement only allowed inside a loop")
assertDecl          = ifCtx [CLASS,PROTO,EXT]       [IF]                success (fail "decoration only allowed inside a class or protocol")
assertClass         = ifCtx [CLASS]                 [IF]                success (fail "decoration only allowed inside a class")
assertDef           = ifCtx [DEF]                   [IF,SEQ,LOOP]       success (fail "statement only allowed inside a function")
assertNotDecl       = ifCtx [CLASS,PROTO,EXT]       [IF]                (fail "statement not allowed inside a class, protocol or extension") success
assertNotData       = ifCtx [DATA]                  [IF,SEQ,LOOP]       (fail "statement not allowed inside a data tree") success

--ifData              = ifCtx [DATA]                  [IF,SEQ,LOOP]

ifPar               = ifCtx [PAR]                   []


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

currSC = ifPar sc2 sc1


--- Adding position info to a parser ------------------------------------------

-- fetching the start column of a construct
withPos :: Parser a -> Parser (Pos,a)
withPos p = do
          SourcePos f r c <- getSourcePos
          a <- p
          getSourcePos
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

instance AddLoc (S.Op a) where
  addLoc p = do
         (l,S.Op _ a) <- withLoc p
         return $ S.Op l a

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

opPrefLoc :: String -> Parser SrcLoc
opPrefLoc op = do
          off <- getOffset
          opPref op
          return $ Loc off (off+length op)

locate (Loc l _) = setOffset l

--- Functions recognizing lexical items ----------------------------------------

lexeme:: Parser a -> Parser a
lexeme p = p <* currSC

symbol :: String -> Parser String
symbol str = lexeme (string str)

newline1 :: Parser [S.Stmt]
newline1 = const [] <$> (eol *> sc2)

--- Strings in their many variants -----------------------------------------------

stringPP prefix = longString prefix <|> shortString prefix

stringP = stringPP []

surround s str = s ++ str ++ s

stringTempl q prefix = (prefix++) . surround q <$> (lexeme (string (prefix++q) >>  manyTill anySingle (string q)))

longString prefix = stringTempl  "\"\"\"" prefix <|> stringTempl "'''" prefix

shortString prefix =
      (prefix++) . surround "\"" . concat <$> lexeme (string (prefix++"\"") >> manyTill charLit1 (char '"'))
    <|>
      (prefix++) . surround "'" . concat <$> lexeme (string (prefix++"'") >> manyTill charLit2 (char '\''))
   where charLit1 =  string "\\\\" <|> string "\\\"" <|>  ((:[]) <$> anySingle)
         charLit2 =  string "\\\\" <|> string "\\'" <|>  ((:[]) <$> anySingle)

--raw strings
shortStringR prefix = stringTempl "\"" prefix  <|> stringTempl "'" prefix

stringPPR prefix = longString prefix <|> shortStringR prefix

strings :: Parser S.Expr
strings = addLoc $ do
     ss <- some (stringP <|> stringPP "b" <|> stringPPR  "r" <|> stringPPR  "br")
     if all (\s -> head s == 'b') ss 
        then return $ S.BStrings NoLoc ss
        else return $ S.Strings NoLoc ss
      
-- Reserved words, other symbols and names ----------------------------------------------------------

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy (alphaNumChar <|> char '_'))

comma     = symbol ","
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
-- Slightly hackish; depends on that chars in argument to oneOf are the only
-- chars that can follow directly after the prefix operator in a longer operator name.
opPref :: String -> Parser String
opPref op = (lexeme . try) (string op <* notFollowedBy (oneOf "<>=/*"))

singleStar = (lexeme . try) (char '*' <* notFollowedBy (char '*'))

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    initChar = satisfy (\c -> isAlpha c || c=='_') <?> "letter or underscore"
    p       = (:) <$> initChar <*> restChars
    check x = if S.isKeyword x
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
    restChars = takeWhileP Nothing (\c -> isAlphaNum c || c=='_') <?> "alphanumeric char or underscore"

name, escname, tvarname :: Parser S.Name
name = do off <- getOffset
          x <- identifier
          if isUpper (head x) && all isDigit (tail x)
            then fail ("Type variable "++x++" cannot be a name")
            else return $ S.Name (Loc off (off+length x)) x

escname = name <|> addLoc ((\str -> S.Name NoLoc  (init (tail str))) <$> stringP)

tvarname = do off <- getOffset
              x <- identifier
              if isUpper (head x) && all isDigit (tail x)
               then return $ S.Name (Loc off (off+length x)) x
               else fail ("Name "++x++" cannot be a type variable")
  
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
parens p = withCtx PAR (L.symbol sc2 "(" *> p <* char ')') <* currSC

brackets p = withCtx PAR (L.symbol sc2 "[" *> p <* char ']') <* currSC

braces p = withCtx PAR (L.symbol sc2 "{" *> p <* char '}') <* currSC

--- Top-level parsers ------------------------------------------------------------
 
file_input :: Parser ([S.Import], S.Suite)
file_input = sc2 *> ((,) <$> imports <*> withCtx TOP top_suite <* eof)

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
              try (do i <- singleStar *> posstaritem; comma; k <- kwdItems; return (Left (posStar i, k)))
           <|>
              try (do i <- singleStar *> posstaritem; optional comma; return (Left (posStar i, kwdNil)))
           <|>
              try (do k <- kwdItems; return (Left (posNil, k)))
           <|>
              try (do i <- positem
                      comma
                      r <- funItems posCons posStar posNil positem posstaritem kwdItems kwdNil
                      case r of
                         Left (p,k) -> return (Left (posCons i p, k))
                         Right p -> return (Left (posCons i (posCons p posNil),kwdNil)))
           <|>
              try (do i <- positem
                      mb <- optional comma
                      case mb of
                         Just _ -> return (Left (posCons i posNil, kwdNil))
                         Nothing -> return (Right i))
           <|>
               (do optional comma; return (Left (posNil, kwdNil)))
 
tuple_or_single posItems headItems len tup =
      do pa <- posItems
         mbc <- optional comma
         return (f pa mbc)
    where f pa mbc
            | len pa == 1 = maybe (headItems pa) (const (tup pa)) mbc
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
     
gen_pattern = do r <- funItems  S.PosPat S.PosPatStar S.PosPatNil apat apat kwdpat S.KwdPatNil
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
            ((try . parens) $ S.PParen NoLoc <$> gen_pattern)
        <|>
            (brackets $ (maybe (S.PList NoLoc [] Nothing) (\(ps,mbp)-> S.PList NoLoc ps mbp)) <$> optional pelems)
        )
  where  -- datapat = S.PData NoLoc <$> escname <*> many (brackets exprlist)
        optannot = try (Just <$> (colon *> ttype)) <|> return Nothing

-- Targets -------------------------------------------------------------------------------------

target :: Parser S.Target
target = try $ 
         do tmp <- atom_expr
            case tmp of
                S.Var _ (S.NoQ n) -> return $ S.TgVar n
                S.Dot _ e n       -> return $ S.TgDot e n
                S.Index _ e ix    -> return $ S.TgIndex e ix
                S.Slice _ e sl    -> return $ S.TgSlice e sl
                _                 -> locate (loc tmp) >> fail ("illegal target: " ++ show tmp)

------------------------------------------------------------------------------------------------
-- Statements ----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

stmt, simple_stmt :: Parser [S.Stmt]
stmt = (try simple_stmt <|> decl_group <|> ((:[]) <$> compound_stmt)) <?> "statement"

simple_stmt = ((small_stmt `sepEndBy1` semicolon)) <* newline1

--- Small statements ---------------------------------------------------------------------------------

small_stmt :: Parser S.Stmt
small_stmt = try signature <|> expr_stmt <|> del_stmt <|> pass_stmt <|> flow_stmt <|> assert_stmt <|> var_stmt <|> after_stmt

expr_stmt :: Parser S.Stmt
expr_stmt = addLoc $
            try (assertNotData *> (S.AugAssign NoLoc <$> target <*> augassign <*> rhs))
        <|> try (S.Assign NoLoc <$> trysome assign <*> rhs)                                 -- Single variable lhs matches here
        <|> try (S.MutAssign NoLoc <$> target <* equals <*> rhs)                            -- and not here
        <|> assertNotData *> (S.Expr NoLoc <$> rhs)
   where augassign :: Parser (S.Op S.Aug)
         augassign = addLoc (S.Op NoLoc <$> (assertNotData *> augops))
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
                assertActScope
                rword "after"
                e <- expr
                colon
                e' <- addLoc $ do
                    n <- name
                    (ps,ks) <- parens funargs
                    return $ S.Call NoLoc (S.Var (S.nloc n) (S.NoQ n)) ps ks
                return $ S.After NoLoc e e'

var_stmt :: Parser S.Stmt
var_stmt = addLoc $ 
            try (assertActBody *> rword "var" *> (S.VarAssign NoLoc <$> trysome assign <*> rhs))

del_stmt = addLoc $ do
            assertNotData
            rword "del"
            t <- target
            ts <- many (comma *> target)
            return $ S.Delete NoLoc (t:ts)

pass_stmt =  S.Pass <$> rwordLoc "pass"

flow_stmt = break_stmt <|>  continue_stmt <|>  return_stmt <|>  raise_stmt <|>  yield_stmt

break_stmt =  S.Break <$> (assertLoop *> rwordLoc "break")

continue_stmt =  S.Continue <$> (assertLoop *> rwordLoc "continue")

return_stmt = addLoc $ do    -- the notFollowedBy colon here is to avoid confusion with data_stmt return case
                assertDef
                rword "return" <* notFollowedBy colon
                S.Return NoLoc <$> optional exprlist
 
yield_stmt = yield_expr >>= \e -> return $ S.Expr (S.eloc e) e

raise_stmt = addLoc $ do
               rword "raise"
               S.Raise NoLoc <$> (optional $ S.Exception <$> expr <*> optional (rword "from" *> expr))
                                     
import_stmt = import_name <|> import_from
   where import_name = addLoc $ do
                rword "import"
                S.Import NoLoc <$> module_item `sepBy1` comma
         module_item = do
                dn <- module_name
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
decl = try funcdef <|> classdef <|> protodef <|> extdef <|> actordef

decorator :: Bool -> Parser S.Deco
decorator sig = do
       p <- L.indentLevel
       d <- decoration
       p1 <- L.indentLevel
       if (p /= p1)
         then fail "Decorated statement must have same indentation as decoration"
         else return d
   where property = rword "@property" *> assertClass *> newline1 *> return S.Property
         static   = (rword "@staticmethod" <|> rword "@static") *> assertDecl *> newline1 *> return S.Static
         decoration = (if sig then property <|> static else static) <|> return S.NoDec

funcdef :: Parser S.Decl
funcdef =  addLoc $ do
              assertNotData
              (p,(deco,fx)) <- withPos (((,) <$> decorator False <*> optional effect) <* rword "def")
              n <- name
              q <- optbinds
              (ppar,kpar) <- parens (funpars True)
              S.Def NoLoc n q ppar kpar <$> optional (arrow *> ttype) <*> suite DEF p <*> pure deco <*> pure (maybe S.tWild id fx)


optbinds :: Parser [S.TBind]
optbinds = brackets (do b <- tbind; bs <- many (comma *> tbind); return (b:bs))
            <|>
           return []

actordef = addLoc $ do 
                assertNotData
                (s,_) <- withPos (rword "actor")
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
extdef      = classdefGen "extension" qual_name EXT S.Extension

classdefGen k pname ctx con = addLoc $ do
                assertTop
                (s,_) <- withPos (rword k)
                nm <- pname
                q <- optbinds
                cs <- optbounds
                con NoLoc nm q cs <$> suite ctx s

-- Compound statements -------------------------------------------------------------------------

compound_stmt :: Parser S.Stmt
compound_stmt =  if_stmt <|> while_stmt <|> for_stmt <|> try_stmt <|> with_stmt <|> data_stmt


else_part p = atPos p (rword "else" *> suite SEQ p)

if_stmt = addLoc $ do
             (p,_) <- withPos (rword "if")
             b <- branch p
             bs <- many (atPos p (rword "elif" *> branch p))
             S.If NoLoc (b:bs) . maybe [] id <$>  optional (else_part p)

branch p = S.Branch <$> expr <*> suite IF p

while_stmt = addLoc $ do
                 assertNotDecl
                 (p,_) <- withPos (rword "while")
                 e <- expr
                 ss1 <- suite LOOP p
                 S.While NoLoc e ss1 . maybe [] id <$>  optional (else_part p)
                 
for_stmt = addLoc $ do
                 assertNotDecl
                 (p,_) <- withPos (rword "for")
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
                assertNotData
                assertNotDecl
                (p,_) <- withPos (rword "try")
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
                assertNotData
                assertNotDecl
                (s,_) <- withPos (rword "with")
                S.With NoLoc <$> (with_item `sepBy1` comma) <*> suite SEQ s
  where with_item = S.WithItem <$> expr <*> optional (rword "as" *> gen_pattern)
                 
 
data_stmt = addLoc $
           do (s,pat) <- withPos gen_pattern
              S.Data NoLoc (Just pat) <$> suite DATA s
        <|>
           do (s,_) <- withPos (assertDef *> rword "return")
              S.Data NoLoc Nothing <$> suite DATA s

suite :: CTX -> Pos -> Parser S.Suite
suite c p = do
    colon
    withCtx c (try simple_stmt <|> indentSuite p)
  where indentSuite p = do
          newline1
          p1 <- L.indentGuard sc1 GT p
          concat <$> some (do
             p2 <- L.indentLevel
             case compare p1 p2 of
                LT -> L.incorrectIndent LT p1 p2
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
   where if_part = (,) <$> (rword "if" *> or_expr) <*> (rword "else" *> expr)

-- Non-empty list of comma-separated expressions.
-- If more than one expr, build a tuple.
-- if only one, leave as it is unless there is a trailing comma when we build a one-element tuple.
exprlist :: Parser S.Expr
exprlist = addLoc $ tuple_or_single posarg S.posArgHead S.posArgLen (\p -> S.Tuple NoLoc p S.KwdNil)
 

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
-- are handled by makeExprParser from Text.Megaparsec.Expr

-- Three auxiliary functions used in building tables for makeExprParser
binary name op = InfixL $ do
                l <- name
                return $ \e1 e2 ->  S.BinOp (S.eloc e1 `upto` S.eloc e2) e1 (S.Op l op) e2

unop name op = do
                l <- name
                return $ \e -> S.UnOp (l `upto` S.eloc e) (S.Op l op) e

prefix name op = Prefix (unop name op)

or_expr = makeExprParser comparison btable

btable :: [[Operator Parser S.Expr]]
btable = [ [ prefix (rwordLoc "not") S.Not]
         , [ binary (rwordLoc "and") S.And]
         , [ binary (rwordLoc "or") S.Or] ]

comparison = addLoc (do
  e <- arithexpr
  ps <- many (do
                 op <- addLoc (S.Op NoLoc <$> comp_op)
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
                <|> S.Is <$ (symbol "is") <?> "comparison operator"

star_expr :: Parser S.Elem
star_expr = S.Star <$> (star *> arithexpr)                                        

-- Arithmetic expressions ----------------------------------------------------------
-- Again, everything between arithexpr and factor is handled by makeExprParser

arithexpr :: Parser S.Expr
arithexpr = makeExprParser factor table <?> "arithmetic expression"

table :: [[Operator Parser S.Expr]]
table = [ [ binary (opPrefLoc "*") S.Mult, binary (opPrefLoc "/") S.Div, binary (opPrefLoc "@") S.MMult,
            binary (opPrefLoc "//") S.EuDiv, binary (opPrefLoc "%") S.Mod]
        , [ binary (opPrefLoc "+") S.Plus, binary (opPrefLoc "-") S.Minus]
        , [ binary (opPrefLoc "<<") S.ShiftL, binary (opPrefLoc ">>") S.ShiftR]
        , [ binary (opPrefLoc "&") S.BAnd]
        , [ binary (opPrefLoc "^") S.BXor]
        , [ binary (opPrefLoc "|") S.BOr]
        ]

factor :: Parser S.Expr
factor = ((unop (opPrefLoc "+") S.UPlus <|> unop (opPrefLoc "-") S.UMinus <|> unop (opPrefLoc "~") S.BNot) <*> factor)
        <|> power

power = addLoc $ do
           ae <- atom_expr
           mbe <- optional expo
           return (maybe ae (\(l,f) -> S.BinOp NoLoc ae (S.Op l S.Pow) f) mbe)
  where expo = do l <- opPrefLoc "**"
                  f <- factor
                  return (l,f)

-- recurring pattern below
commaList p = many (try (comma *> p)) <* optional comma

atom_expr = do
              await <- optional $ withLoc $ rword "await" *> return (S.Await NoLoc)
              a <- atom
              ts <- many trailer
              let e = foldl app a ts
              return $ maybe e (app e) await 
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
               <|> (try ((\f -> S.Float NoLoc f (show f)) <$> lexeme L.float))
               <|> (\i -> S.Int NoLoc i ("0o"++showOct i "")) <$> (string "0o" *> lexeme L.octal)
               <|> (\i -> S.Int NoLoc i ("0x"++showHex i "")) <$> (string "0x" *> lexeme L.hexadecimal)
               <|> (\i -> S.Int NoLoc i (show i)) <$> (lexeme L.decimal)
               <|> (S.Ellipsis <$> rwordLoc "...")
               <|> (S.None <$>  rwordLoc "None")
               <|> (S.NotImplemented  <$>  rwordLoc "NotImplemented")
               <|> (\l -> S.Bool l True) <$> rwordLoc "True"
               <|> (\l -> S.Bool l False) <$> rwordLoc "False")
               <?> "atomic expression"

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
                      try (do
                        is <- brackets indexlist
                        return (\a -> S.Index NoLoc a is))
                        <|>
                      (do
                        ss <- brackets slicelist
                        return (\a -> S.Slice NoLoc a ss))
                        <|>
                      (do
                        (ps,ks) <- parens funargs
                        return (\a -> S.Call NoLoc a ps ks))
                        <|>
                      (do
                         dot
                         intdot <|> iddot <|> strdot))
                 
           where iddot  = do 
                     nm <- name
                     return (\a -> S.Dot NoLoc a nm)
                 intdot  = do 
                        mbt <- optional star
                        i <- lexeme L.decimal
                        return (\a -> S.DotI NoLoc a i (maybe False (const True) mbt))
                 strdot = do
                        (p,str) <- withPos stringP 
                        return (\a -> S.Dot NoLoc a (S.Name NoLoc (init(tail str))))   -- init/tail?
                 indexlist = (:) <$> expr <*> commaList expr
                 slicelist = (:) <$> slice <*> commaList slice
                 slice = addLoc (do 
                        mbt <- optional expr
                        S.Sliz NoLoc mbt <$> (colon *> optional expr) <*> (maybe Nothing id <$> optional (colon *> optional expr)))
                     
 
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
             assertDef
             rword "yield"
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
funpars ann =   try ((\(n,mbt) -> (S.PosNIL,S.KwdSTAR n mbt)) <$> (starstar *> pstar ann kstartype <* optional comma))
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
        <|> rword "async" *> return S.fxAsync
        <|> rword "act" *> optvar S.fxAct
        <|> rword "mut" *> optvar S.fxMut
        <|> rword "pure" *> return S.fxPure
        <|> return S.fxPure
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
tvar = S.TV S.KWild <$> tvarname

tbind :: Parser S.TBind
tbind = S.TBind <$> tvar <*> optbounds

optbounds :: Parser [S.TCon]
optbounds = do bounds <- optional (parens (optional ((:) <$> tcon <*> commaList tcon)))
               return $ maybe [] (maybe [] id) bounds

tschema :: Parser S.TSchema
tschema = addLoc $
            try (do 
                bs <- brackets (do n <- tbind
                                   ns <- commaList tbind
                                   return (n:ns))
                fatarrow
                t <- ttype
                return (S.tSchema bs t))
            <|>
            (S.monotype <$> ttype)

ttype :: Parser S.Type
ttype    =  addLoc (
            rword "None" *> return (S.TNone NoLoc)
        <|> (S.TVar NoLoc . S.TV S.KType) <$> (S.Name <$> rwordLoc "Self" <*> return "Self")
        <|> S.TOpt NoLoc <$> (qmark *> ttype)
        <|> braces (do t <- ttype
                       mbt <- optional (colon *> ttype)
                       return (maybe (Builtin.tSet t) (Builtin.tMapping t) mbt))
        <|> try (parens (do alts <- some (try (utype <* vbar))
                            alt <- utype
                            return $ S.TUnion NoLoc (alts++[alt])))
        <|> try (do mbfx <- optional effect
                    (p,k) <- parens funrows
                    arrow
                    t <- ttype
                    return (S.TFun NoLoc (maybe S.fxPure id mbfx) p k t))
        <|> try (do (p,k) <- parens funrows
                    return (S.TTuple NoLoc p k))
        <|> parens (return (S.TTuple NoLoc S.posNil S.kwdNil))
        <|> try (brackets (Builtin.tSequence <$> ttype))
        <|> try (S.TVar NoLoc <$> tvar)
        <|> rword "_" *> return (S.TWild NoLoc)
        <|> S.TCon NoLoc <$> tcon)
                

utype :: Parser S.UType
utype    =  S.UCon <$> qual_name
        <|> (\str -> S.ULit (init (tail str))) <$> shortString []

