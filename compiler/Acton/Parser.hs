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
import qualified Acton.Names as Names
import Utils
import Debug.Trace
import System.IO.Unsafe

--- Main parsing and error message functions ------------------------------------------------------

parseModule :: S.QName -> String -> IO (String,S.Module)
parseModule qn file = do
    contents <- readFile file
    case runParser (St.evalStateT file_input []) file (contents ++ "\n") of
        Left err -> Control.Exception.throw err
        Right (i,s) -> return (contents, S.Module qn i s)

parseTest file = snd (unsafePerformIO (parseModule (S.mkqname "test") file))

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

data CTX = PAR | SEQ | LOOP | DATA | DEF | CLASS | ACTOR deriving (Show,Eq)

withCtx ctx = between (St.modify (ctx:)) (St.modify tail)

ifCtx accept ignore yes no = do
    cs <- St.get
    case filter (`notElem` ignore) cs of
        c:_ | c `elem` accept -> yes
        _                     -> no

onlyIn s        = fail ("statement only allowed inside " ++ s)
notIn s         = fail ("statement not allowed inside " ++ s)
success         = return ()

assertActBody   = ifCtx [ACTOR]     []              success (onlyIn "an actor body")
assertActScope  = ifCtx [ACTOR]     [SEQ,LOOP,DEF]  success (onlyIn "an actor")
assertLoop      = ifCtx [LOOP]      [SEQ]           success (onlyIn "a loop")
assertDef       = ifCtx [DEF]       [SEQ,LOOP]      success (onlyIn "a function")
assertDefOrAct  = ifCtx [DEF,ACTOR] [SEQ,LOOP]      success (onlyIn "a function or an actor")
assertNotData   = ifCtx [DATA]      [SEQ,LOOP]      (notIn "a data tree") success

ifActScope      = ifCtx [ACTOR]     [SEQ,LOOP,DEF]
    
ifData          = ifCtx [DATA]      [SEQ,LOOP]

ifPar           = ifCtx [PAR]       []
    

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

instance AddLoc S.StarPar where
  addLoc p = do
         (l,par) <- withLoc p
         case par of
            S.StarPar _ nm mba -> return (S.StarPar l nm mba)
            S.NoStar -> return S.NoStar

instance AddLoc S.Except where
  addLoc p = do
         (l,exc) <- withLoc p
         case exc of
           S.ExceptAll _ -> return (S.ExceptAll l)
           S.Except _ e -> return (S.Except l e)
           S.ExceptAs _ e nm -> return (S.ExceptAs l e nm)
     
instance AddLoc S.Index where
  addLoc p = do
          (l,i) <- withLoc p
          case i of
            S.Index _ e -> return (S.Index l e)
            S.Slice _ e1 e2 e3 -> return (S.Slice l e1 e2 e3)

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

--- strings in their many variants ---

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
     ss <- some (stringP <|> stringPP "u" <|> stringPP "b"
                      <|> stringPPR  "r" <|> stringPPR  "br")
     if all (\s -> head s == 'b') ss then return $ S.BStrings NoLoc ss
      else if all (\s -> head s == 'u') ss then return $ S.UStrings NoLoc ss
           else return $ S.Strings NoLoc ss
      
-- reserved words and other symbols

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

name, escname :: Parser S.Name
name = do off <- getOffset
          x <- identifier
          if isUpper (head x) && all isDigit (tail x)
            then fail ("Type variable "++x++" cannot be a name")
            else return $ S.Name (Loc off (off+length x)) x

tvarname :: Parser S.Name
tvarname = do off <- getOffset
              x <- identifier
              if isUpper (head x) && all isDigit (tail x)
               then return $ S.Name (Loc off (off+length x)) x
               else fail ("Name "++x++" cannot be a type variable")
  
              
                
escname = name <|> addLoc ((\str -> S.Name NoLoc  (init (tail str))) <$> stringP)

-- recognizers for numbers are used directly in function atom below.

--- Helper functions for parenthesised forms -----------------------------------

parens, brackets, braces :: Parser a -> Parser a
parens p = withCtx PAR (L.symbol sc2 "(" *> p <* char ')') <* currSC

brackets p = withCtx PAR (L.symbol sc2 "[" *> p <* char ']') <* currSC

braces p = withCtx PAR (L.symbol sc2 "{" *> p <* char '}') <* currSC

--- Parsers, ordered as in Python 3.6 reference grammar --------------------------

-- single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
-- file_input: (NEWLINE | stmt)* ENDMARKER
-- eval_input: testlist NEWLINE* ENDMARKER

file_input :: Parser ([S.Import], S.Suite)
file_input = sc2 *> ((,) <$> imports <*> top_suite <* eof)

imports :: Parser [S.Import]
imports = many (L.nonIndented sc2 import_stmt <* eol <* sc2)

top_suite :: Parser S.Suite
top_suite = concat <$> (many (L.nonIndented sc2 stmt <|> newline1))


-- decorator: '@' dotted_name [ '(' [arglist] ')' ] NEWLINE
-- decorators: decorator+
-- decorated: decorators (classdef | funcdef)

decorator, decorators :: Parser (S.Decl -> S.Decl)
decorator = (do
       (l,f) <- withLoc $ do 
            assertNotData
            symbol "@"
            nm    <- dotted_name
            mas   <- optional (parens (optional arglist))
            newline1
            return $ S.Decorator NoLoc nm (maybe [] (maybe [] id) mas)
       return (\d -> (f d){S.dloc = l}))
   <?> "decorator"
 
decorators = do
       ds <- some decorator
       return (\st -> foldr ($) st ds)

decorated :: Parser S.Decl
decorated = decorators <*> (classdef <|> funcdef)
         <?> "classdef or funcdef (only function and class definitions can be decorated)"

-- async_funcdef: ASYNC funcdef
-- funcdef: modifier 'def' NAME parameters ['->' test] ':' suite

funcdef :: Parser S.Decl
funcdef =  addLoc $ do
              assertNotData
              (p,md) <- withPos (modifier <* rword "def")
              S.Def NoLoc <$> name <*> optbinds <*> parameters <*> optional (arrow *> ctype) <*> suite DEF p <*> pure md


-- modifier: ['sync' | 'async']

modifier :: Parser S.Modif
modifier = assertActScope *> rword "sync" *> return (S.Sync True) <|> 
           assertActScope *> rword "async" *> return S.Async <|>
           ifActScope (return (S.Sync False)) (return S.NoMod)

optbinds :: Parser [S.TBind]
optbinds = brackets (do b <- cbind; bs <- many (comma *> cbind); return (b:bs))
            <|>
           return []


-- parameters: '(' [typedargslist] ')'
-- typedargslist:
--    (tfpdef ['=' test] (',' tfpdef ['=' test])*
--       [',' [ '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]] | '**' tfpdef [',']]]
--  | '*' [tfpdef] (',' tfpdef ['=' test])* [',' ['**' tfpdef [',']]]
--  | '**' tfpdef [','])
-- tfpdef: NAME [':' test]
--
-- The above grammar for typedargslist is incomprehensible.
-- Also, varargslist has the same syntax with tfpdef replaced by NAME
-- So we do a general, parameterized, version argslist first 
-- To make it a little more readable, we define
--
-- param: tfpdef ['=' test]
-- parlist_starargs:  '*' [tfpdef] (',' param)* [',' ['**' tfpdef [',']]] | '**' tfpdef [',']
-- annot: test
--
-- and get
--
-- tfpdef: NAME [':' annot]
-- parameters: '(' [typedargslist] ')'
-- typedargslist: 
--      (param (',' param)* [',' [ parlist_starargs]]
--    | parlist_starargs
--    | '**' tfpdef [','])

--param :: Parser S.StarPar -> Parser S.Param
--param par = do S.StarPar _ nm mba <- par
--               S.Param nm mba <$> optional (equals *> test)

-- Parameter lists ---------------------------------------------

-- Parameter lists in lambdas may not have type annotations, while those in def and actor declarations do
param1, param2 :: Parser S.Param
param1 = S.Param <$> name <*> optional (colon *> tscheme) <*> optional (equals *> test)

-- parameter lists in lambdas may not have type annotations
param2 = S.Param <$>  name <*> return Nothing <*> optional (equals *> test)

-- parameter list for def and actor
parameters = maybe (S.Params [] S.NoStar [] S.NoStar) id <$> (parens (optional (argslist param1 starpar1)))

starpar1, starpar2 :: Parser S.StarPar
starpar1 = addLoc (S.StarPar NoLoc <$> name <*> optional (colon *> ctype)) 
starpar2 = addLoc (S.StarPar NoLoc <$> name <*> return Nothing) 

--typedargslist and varargslist have the same structure and we define a function argslist for the common pattern
--typedargslist = argslist params1
--varargslist = argslist params2

argslist :: Parser S.Param -> Parser S.StarPar -> Parser S.Params
argslist param starpar = 
               (do p1 <- param
                   ps <- many (try (comma *> param))
                   mb  <- optional (comma *> optional parlist_starargs)
                   return $ maybe (S.Params (p1:ps) S.NoStar [] S.NoStar)
                                  (maybe (S.Params (p1:ps) S.NoStar [] S.NoStar) (\(s1,qs,s2) -> S.Params (p1:ps) s1 qs s2)) mb)
             <|> (do
                   (s1,ps,s2) <- parlist_starargs
                   return $ S.Params [] s1 ps s2)
   where parlist_starargs = (do
                   singleStar
                   mbt <- optional starpar
                   let p1 = maybe S.NoStar id mbt
                   ps <- many (try (comma *> param))
                   mbmb <- optional (comma *> optional (starstar *> (starpar <* optional comma)))
                   let p2 = maybe S.NoStar (maybe S.NoStar id) mbmb 
                   return (p1,ps,p2))
            <|> (do
                   starstar
                   sp <- starpar <* optional comma
                   return (S.NoStar,[],sp))

-- stmt: simple_stmt | compound_stmt
-- simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE

stmt, simple_stmt :: Parser [S.Stmt]
stmt = (try simple_stmt <|> decl_group <|> ((:[]) <$> compound_stmt)) <?> "statement"

simple_stmt = ((small_stmt `sepBy1` semicolon) <* optional semicolon) <* newline1

--- Small statements ---------------------------------------------------------------------------------

-- small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
--             import_stmt | assert_stmt)

small_stmt :: Parser S.Stmt
small_stmt = expr_stmt  <|> del_stmt <|> pass_stmt <|> flow_stmt  <|>
             assert_stmt <|> var_stmt

-- expr_stmt: testlist_star_expr (
--                                   annassign
--                                 | augassign (yield_expr | testlist)
--                                 | ('=' (yield_expr | testlist_star_expr))*
--                               )

-- | 'var' testlist_star_expr many_assign                               { VarAssign ($1><$3) (Target $2:fst $3) (snd $3) }

-- annassign: ':' test ['=' test]
-- testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
-- exprlist: (expr|star_expr) (',' (expr|star_expr))* [',']               **** exprlist and testlist occur
-- testlist: test (',' test)* [',']                                       **** later in grammar but are shown here

-- augassign: ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
--            '<<=' | '>>=' | '**=' | '//=')

-- assign:  ('=' (yield_expr | testlist_star_expr))

assign :: Parser S.Pattern
assign = lhs <* equals

-- common pattern for expression lists
testlist_gen :: Parser (S.Elem S.Expr) -> Parser S.Expr
testlist_gen p = addLoc $ do
     e <- p
     es <- many (try (comma *> p))
     mbc <- optional comma
     return (f NoLoc (e:es) mbc)
  where f _ [S.Elem e] Nothing = e
        f _ [S.Star e] Nothing = e
        f l es _ = S.Tuple l es

exprlist, testlist, testlist_star_expr :: Parser S.Expr
exprlist = testlist_gen (S.Elem <$> expr <|> star_expr)
testlist = testlist_gen (S.Elem <$> test)
testlist_star_expr = testlist_gen  (S.Elem <$> test <|> star_expr)

augassign :: Parser (S.Op S.Aug)
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


rhs :: Parser S.Expr
rhs = yield_expr <|> testlist_star_expr

trysome p = do x <- p; rest [x]
  where rest xs = (try p >>= \x -> rest (x:xs)) <|> return (reverse xs)

expr_stmt :: Parser S.Stmt
expr_stmt = addLoc $
            try (assertNotData *> (S.AugAssign NoLoc <$> lhs <*> augassign <*> rhs))
        <|> try (S.Assign NoLoc <$> trysome assign <*> rhs)
        <|> try (uncurry (S.TypeSig NoLoc) <$> tsig)
        <|> assertNotData *> (S.Expr NoLoc <$> rhs)

var_stmt :: Parser S.Stmt
var_stmt = addLoc $ 
            try (assertActBody *> rword "var" *> (S.VarAssign NoLoc <$> trysome assign <*> rhs))

tsig = do v <- name
          vs <- commaList name
          colon
          t <- tscheme
          return (v:vs,t)
 
tsig1 = do v <- name
           colon
           t <- tscheme
           return (v,t)
 
target = pattern False

lhs = pattern True

pattern :: Bool -> Parser S.Pattern
pattern lh = addLoc $ do
    ps <- pelems lh
    mbc <- optional comma
    return (f ps mbc)
  where 
    f [S.Elem p] Nothing = p
    f [S.Star p] Nothing = p
    f ps _               = S.PTuple NoLoc ps

pelems :: Bool -> Parser [S.Elem S.Pattern]
pelems lh = do
    p <- pelem lh
    ps <- many (try (comma *> pelem lh))
    return (p:ps)

pelem :: Bool -> Parser (S.Elem S.Pattern)
pelem lh = S.Elem <$> apat lh <|> S.Star <$> (star *> apat lh) 

apat :: Bool -> Parser S.Pattern
apat lh = addLoc (
            try (if lh then ifData datapat lvalue else lvalue)
        <|>
            (try $ S.PVar NoLoc <$> name <*> optannot)
        <|>
            ((try . parens) $ return (S.PTuple NoLoc []))
        <|>
            ((try . parens) $ S.PParen NoLoc <$> pattern lh)
        <|>
            (brackets $ (S.PList NoLoc . maybe [] id) <$> optional (pelems lh))
        )
  where lvalue = do
            tmp <- atom_expr
            case tmp of
                S.Dot _ e n  -> return $ S.PDot NoLoc e n
                S.Ix _ e ix  -> return $ S.PIx NoLoc e ix
                _            -> locate (loc tmp) >> fail ("illegal assignment target: " ++ show tmp)
        datapat = S.PData NoLoc <$> escname <*> many (brackets testlist)
        optannot = try (Just <$> (colon *> ctype)) <|> return Nothing

-- del_stmt: 'del' exprlist
-- pass_stmt: 'pass'

del_stmt = addLoc $ do
            assertNotData
            rword "del"
            S.Delete NoLoc <$> target

pass_stmt =  S.Pass <$> rwordLoc "pass"

-- flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt

flow_stmt = break_stmt <|>  continue_stmt <|>  return_stmt <|>  raise_stmt <|>  yield_stmt

-- break_stmt: 'break'
-- continue_stmt: 'continue'
-- return_stmt: 'return' [testlist]

break_stmt =  S.Break <$> (assertLoop *> rwordLoc "break")

continue_stmt =  S.Continue <$> (assertLoop *> rwordLoc "continue")

return_stmt = addLoc $ do    -- the notFollowedBy colon here is to avoid confusion with data_stmt return case
                assertDefOrAct
                rword "return" <* notFollowedBy colon
                S.Return NoLoc <$> optional testlist
                
-- yield_stmt: yield_expr
-- raise_stmt: 'raise' [test ['from' test]]

yield_stmt = yield_expr >>= \e -> return $ S.Expr (S.eloc e) e

raise_stmt = addLoc $ do
               rword "raise"
               S.Raise NoLoc <$> (optional $ S.Exception <$> test <*> optional (rword "from" *> test))
                                     
-- import_stmt: import_name | import_from
-- import_name: 'import' dotted_as_names
-- import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
--               'import' ('*' | '(' import_as_names ')' | import_as_names))
-- import_as_name: NAME ['as' NAME]
-- dotted_as_name: dotted_name ['as' NAME]
-- import_as_names: import_as_name (',' import_as_name)* [',']
-- dotted_as_names: dotted_as_name (',' dotted_as_name)*
-- dotted_name: NAME ('.' NAME)*

import_stmt = import_name <|> import_from

import_name = addLoc $ do
                 rword "import"
                 S.Import NoLoc <$> module_item `sepBy1` comma
  where module_item = do
                          dn <- dotted_name
                          S.ModuleItem dn <$> optional (rword "as" *> name)
                          
import_from = addLoc $ do
                 rword "from"
                 mr <- import_module
                 rword "import"
                 is <- import_items
                 case is of
                   [] -> return $ S.FromImportAll NoLoc mr
                   _  -> return $ S.FromImport NoLoc mr is
  where                   
    import_module = do
                  ds <- many dot
                  mbn <- optional dotted_name
                  return $ S.ModRef (length ds, mbn)
    import_items = ([] <$ star)   -- Note: [] means all...
                <|> parens import_as_names
                <|> import_as_names
    import_as_name = S.ImportItem <$> name <*> optional (rword "as" *> name)
    import_as_names = (:) <$> import_as_name <*> commaList import_as_name 

dotted_name = do
  n <- name
  S.QName n <$> many (dot *> escname)
  
-- global_stmt: 'global' NAME (',' NAME)*
-- nonlocal_stmt: 'nonlocal' NAME (',' NAME)*
-- assert_stmt: 'assert' test [',' test]

assert_stmt = addLoc (rword "assert" >> S.Assert NoLoc <$> test `sepBy1` comma)

--- Compound statements ------------------------------------------------------------------

-- compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt |
--                with_stmt | funcdef | classdef | decorated
-- if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
-- while_stmt: 'while' test ':' suite ['else' ':' suite]
-- for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
-- try_stmt: ('try' ':' suite
--            ((except_clause ':' suite)+
--             ['else' ':' suite]
--             ['finally' ':' suite] |
--            'finally' ':' suite))
-- with_stmt: 'with' with_item (',' with_item)*  ':' suite
-- with_item: test ['as' expr]
-- except_clause: 'except' [test ['as' NAME]]

compound_stmt :: Parser S.Stmt
compound_stmt =  if_stmt <|> while_stmt <|> for_stmt <|> try_stmt <|> with_stmt <|> data_stmt

decl_group :: Parser [S.Stmt]
decl_group = do p <- L.indentLevel
                g <- some (atPos p decl)
                return [ S.Decl (loc ds) ds | ds <- Names.splitDeclGroup g ]

decl :: Parser S.Decl
decl = funcdef <|> classdef <|> structdef <|> protodef <|> extdef <|> actordef <|> decorated

else_part p = atPos p (rword "else" *> suite SEQ p)

if_stmt = addLoc $ do
             (p,_) <- withPos (rword "if")
             b <- branch p
             bs <- many (atPos p (rword "elif" *> branch p))
             S.If NoLoc (b:bs) . maybe [] id <$>  optional (else_part p)

branch p = S.Branch <$> test <*> suite SEQ p

while_stmt = addLoc $ do
                 (p,_) <- withPos (rword "while")
                 e <- test
                 ss1 <- suite LOOP p
                 S.While NoLoc e ss1 . maybe [] id <$>  optional (else_part p)
                 
for_stmt = addLoc $ do
                 (p,_) <- withPos (rword "for")
                 pat <- target
                 rword "in"
                 e <- testlist
                 ss <- suite LOOP p
                 S.For NoLoc pat e ss . maybe [] id <$> optional (else_part p)


except :: Parser S.Except
except = addLoc $ do
             rword "except"
             mbt <- optional ((,) <$> test <*> optional (rword "as" *> name))
             return (maybe (S.ExceptAll NoLoc) (\(t,mbn) -> maybe (S.Except NoLoc t) (S.ExceptAs NoLoc t) mbn) mbt)
            
try_stmt = addLoc $ do
                assertNotData
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
                (s,_) <- withPos (rword "with")
                S.With NoLoc <$> (with_item `sepBy1` comma) <*> suite SEQ s
  where with_item = S.WithItem <$> test <*> optional (rword "as" *> target)
                 
-- data_stmt
--   : testlist_star_expr ':' 'indent' many_stmts 'dedent'  
--   | 'return' ':' 'indent' many_stmts 'dedent'            

data_stmt = addLoc $
           do (s,pat) <- withPos lhs
              S.Data NoLoc (Just pat) <$> suite DATA s
        <|>
           do (s,_) <- withPos (assertDefOrAct *> rword "return")
              S.Data NoLoc Nothing <$> suite DATA s

-- suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT

suite :: CTX -> Pos  -> Parser S.Suite
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

--- Expressions ----------------------------------------------------------------

-- test: or_test ['if' or_test 'else' test] | lambdef
-- test_nocond: or_test | lambdef_nocond
-- lambdef: 'lambda' [varargslist] ':' test
-- lambdef_nocond: 'lambda' [varargslist] ':' test_nocond

-- or_test: and_test ('or' and_test)*
-- and_test: not_test ('and' not_test)*
-- not_test: 'not' not_test | comparison
-- comparison: expr (comp_op expr)*

test =  lambdef
       <|>
        do
          e1 <- or_test
          mbp <- optional if_part
          case mbp of
            Nothing -> return e1
            Just (c,e2) -> return $ S.Cond (S.eloc e1) e1 c e2
   where if_part = (,) <$> (rword "if" *> or_test) <*> (rword "else" *> test)

test_nocond = or_test <|> lambdef_nocond

lambdefGen t = addLoc $ do
            rword "lambda"
            mbvs <- optional (argslist param2 starpar2)
            colon
            S.Lambda NoLoc (maybe (S.Params [] S.NoStar [] S.NoStar) id mbvs) <$> t

lambdef = lambdefGen test
lambdef_nocond = lambdefGen test_nocond

-- the intermediate levels between or_test and comparison, i.e. and_test and not_test,
-- are handled by makeExprParser from Text.Megparsec.Expr

-- Three auxiliary functions used in building tables for mkExprParser
binary name op = InfixL $ do
                l <- name
                return $ \e1 e2 ->  S.BinOp (S.eloc e1 `upto` S.eloc e2) e1 (S.Op l op) e2

unop name op = do
                l <- name
                return $ \e -> S.UnOp (l `upto` S.eloc e) (S.Op l op) e

prefix name op = Prefix (unop name op)

or_test = makeExprParser comparison btable

btable :: [[Operator Parser S.Expr]]
btable = [ [ prefix (rwordLoc "not") S.Not]
         , [ binary (rwordLoc "and") S.And]
         , [ binary (rwordLoc "or") S.Or] ]

comparison = addLoc (do
  e <- expr
  ps <- many (do
                 op <- addLoc (S.Op NoLoc <$> comp_op)
                 S.OpArg op <$> expr)
  case ps of
        [] -> return e
        _  -> return $ S.CompOp NoLoc e ps) <?> "relational expression"

-- comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'

comp_op =   S.Lt <$ opPref "<"
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

star_expr :: Parser (S.Elem S.Expr)
star_expr = S.Star <$> (star *> expr)                                        

-- expr: xor_expr ('|' xor_expr)*
-- xor_expr: and_expr ('^' and_expr)*
-- and_expr: shift_expr ('&' shift_expr)*
-- shift_expr: arith_expr (('<<'|'>>') arith_expr)*
-- arith_expr: term (('+'|'-') term)*
-- term: factor (('*'|'@'|'/'|'%'|'//') factor)*
-- factor: ('+'|'-'|'~') factor | power
-- power: atom_expr ['**' factor]

-- Again, everything between expr and factor is handled by makeExprParser

expr :: Parser S.Expr
expr = makeExprParser factor table <?> "arithmetic expression"

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
                  
-- atom_expr: [AWAIT] atom trailer*
-- atom: ('(' [yield_expr|testlist_comp] ')' |
--        '[' [testlist_comp] ']' |
--        '{' [dictorsetmaker] '}' |
--        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
-- testlist_comp: (test|star_expr) ( comp_for | (',' (test|star_expr))* [','] )
-- dictorsetmaker: ( ((test ':' test | '**' expr)
--                    (comp_for | (',' (test ':' test | '**' expr))* [','])) |
--                   ((test | star_expr)
--                    (comp_for | (',' (test | star_expr))* [','])) )

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
         ((try . parens) $ return (S.Tuple NoLoc []))
       <|>
         ((try . parens) $ S.Paren NoLoc <$> yield_expr)
       <|>
         (try . parens) testlist_comp
       <|>
         (try . parens) recordmaker
       <|>
        (brackets $ do
                     mbe <- optional testlist_comp2
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

-- recurring pattern below
commaList p = many (try (comma *> p)) <* optional comma

-- testlist_comp version used in parentheses, building a tuple
testlist_comp :: Parser S.Expr
testlist_comp = addLoc $ do
      e <- elem
      (S.Generator NoLoc e <$> comp_for) <|> ((S.Paren NoLoc . f) <$> elems e)
   where elem = (S.Elem <$> test) <|> star_expr
         elems e = do
            es <- many (try (comma *> elem))
            mbc <- optional comma
            return (e:es,mbc)
         f ([S.Elem e],Nothing) = e
         f (es,_) = S.Tuple NoLoc es

-- common pattern in functions building lists, sets and dictionaries
maker constr constrComp p = do
           (l,a) <- withLoc p
           (constrComp l a <$> comp_for) <|> ((\as -> (constr l (a:as))) <$> commaList p)

-- testlist_comp version used in  brackets, building a list
testlist_comp2 = maker S.List S.ListComp elem 
   where elem = (S.Elem <$> test) <|> star_expr

recordmaker = addLoc $ do
         f@(S.Field n e) <- eqn         
         (S.RecordComp NoLoc n e <$> comp_for) <|> ((\fs -> (S.Record NoLoc (f:fs))) <$> commaList field)
   where eqn = S.Field <$> escname <*> (equals *> test)
         field = eqn <|> (S.StarStarField <$> (starstar *> expr))                   

dictorsetmaker :: Parser S.Expr
dictorsetmaker = (try $ maker S.Dict S.DictComp assoc)
                    <|> maker S.Set S.SetComp elem
     where elem = (S.Elem <$> test) <|> star_expr
           assoc = (S.Assoc <$> test) <*> (colon *> test)
                <|>
                   (S.StarStarAssoc <$> (starstar *> expr))

var = do
         nm <- name
         return (S.Var (S.nloc nm) nm)

-- trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
-- subscriptlist: subscript (',' subscript)* [',']
-- subscript: test | [test] ':' [test] [sliceop]
-- sliceop: ':' [test]

trailer :: Parser (SrcLoc,S.Expr -> S.Expr)
trailer = withLoc (
              (do
                mbas <- parens (optional arglist)
                return (\a -> S.Call NoLoc a (maybe [] id mbas)))
                <|>
              (do
                mbss <- brackets (optional subscriptlist)
                return (\a -> S.Ix NoLoc a (maybe [] id mbss)))
                <|>
              (do
                 dot
                 intdot <|> iddot <|> strdot))
                 
   where iddot  = do 
             nm <- name
             return (\a -> S.Dot NoLoc a nm)
         intdot  = do 
                i <- lexeme L.decimal
                return (\a -> S.DotI NoLoc a i)
         strdot = do
                (p,str) <- withPos stringP 
                return (\a -> S.Dot NoLoc a (S.Name NoLoc (init(tail str))))   -- init/tail?
         subscriptlist = (:) <$> subscript <*> commaList subscript
         subscript = addLoc (try (do 
                mbt <- optional test
                S.Slice NoLoc mbt <$> (colon *> optional test) <*> optional (colon *> optional test))
              <|>
                S.Index NoLoc <$> test)
                     
--- Actor and class definitions ------------------------------------------------

-- actordef: 'actor' name parameters optarrowannot ':' suite

actordef = addLoc $ do 
                assertNotData
                (s,_) <- withPos (rword "actor")
                nm <- name <?> "actor name"
                q <- optbinds
                ps <- parameters
                mba <- optional (arrow *> ctype)
                ss <- suite ACTOR s
                return $ S.Actor NoLoc nm q ps mba ss

-- classdef: 'class' NAME ['(' [arglist] ')'] ':' suite
-- structdef: 'class' NAME ['(' [arglist] ')'] ':' suite
-- protodef: 'class' NAME ['(' [arglist] ')'] ':' suite
-- extdef: 'class' NAME ['(' [arglist] ')'] ':' suite

classdef    = classdefGen "class" S.Class
structdef   = classdefGen "struct" S.Struct
protodef    = classdefGen "protocol" S.Protocol
extdef      = classdefGen "extension" S.Extension

classdefGen k con = addLoc $ do
                assertNotData
                (s,_) <- withPos (rword k)
                nm <- name
                q <- optbinds
                cs <- optbounds
                con NoLoc nm q cs <$> suite CLASS s

-- arglist: argument (',' argument)*  [',']
-- argument: ( test [comp_for] |
--             test '=' test |
--            '**' test |
--            '*' test )

-- comp_iter: comp_for | comp_if
-- comp_for: [ASYNC] 'for' exprlist 'in' or_test [comp_iter]
-- comp_if: 'if' test_nocond [comp_iter]

arglist = (:) <$> argument <*> commaList argument

argument = ((try $ do
            nm <- escname
            equals
            e1 <- test
            return $ S.KwArg nm e1)
         <|>
                   --fail "Only an identifier allowed to the left of equality sign in argument list" )
            (do e <- test
                maybe (S.Arg e) (S.Arg . S.Generator (S.eloc e) (S.Elem e)) <$> optional comp_for)
         <|> (starstar *> (S.StarStarArg <$> test))
         <|> (star *> (S.StarArg <$> test))) <?> "argument"
           
comp_iter, comp_for, comp_if :: Parser S.Comp
comp_iter = comp_for <|> comp_if

comp_for = addLoc (do
            rword "for"
            pat <- target
            rword "in"
            e <- or_test
            S.CompFor NoLoc pat e . maybe S.NoComp id <$> optional comp_iter)

comp_if = addLoc $ do
            rword "if"
            e <- test_nocond
            S.CompIf NoLoc e . maybe S.NoComp id <$> optional comp_iter
           
-- yield_expr: 'yield' [yield_arg]
-- yield_arg: 'from' test | testlist

yield_expr = addLoc $ do 
             assertDef
             rword "yield"
             (S.YieldFrom NoLoc <$> (rword "from" *> test)
              <|> S.Yield NoLoc <$> optional testlist)

--- Types ----------------------------------------------------------------------

posrow :: Parser S.PosRow                   --non-empty posrow without trailing comma
posrow  = do mbv <- star *> optional cvar  
             return (S.PosVar mbv)
         <|> 
          do t <- tscheme
             ts <- many (try (comma *> tscheme))
             mbv <- optional (comma *> optional (star *> optional cvar))
             let tail = maybe S.PosNil (maybe S.PosNil S.PosVar) mbv
             return (foldr S.PosRow tail (t:ts))

kwdrow :: Parser S.KwdRow                    --non-empty kwrow with optional trailing comma
kwdrow  = do mbv <- starstar *> optional cvar
             return (S.KwdVar mbv)
         <|>
          do p <- tsig1
             ps <- many (try (comma *> tsig1))
             mbv <- optional (comma *> optional ((starstar *> optional cvar) <* optional comma ))
             let tail = maybe S.KwdNil (maybe S.KwdNil S.KwdVar) mbv
             return (foldr (uncurry S.KwdRow) tail (p:ps))

funrows :: Parser (S.PosRow, S.KwdRow)
funrows  = try (do mbv <- (star *> optional cvar); comma; k <- kwdrow; return (S.PosVar mbv, k))
        <|>
           try (do mbv <- (star *> optional cvar); optional comma; return (S.PosVar mbv, S.KwdNil))
        <|>
           try (do k <- kwdrow; return (S.PosNil, k))
        <|>
           try (do t <- tscheme; comma; (p,k) <- funrows; return (S.PosRow t p, k))
        <|>
           try (do t <- tscheme; optional comma; return (S.PosRow t S.PosNil, S.KwdNil))
        <|>
           try (do optional comma; return (S.PosNil, S.KwdNil))

ccon :: Parser S.TCon
ccon =  do n <- name
           args <- optional (brackets (do t <- ctype
                                          ts <- commaList ctype
                                          return (t:ts)))
           return $ S.TC n (maybe [] id args)

cvar :: Parser S.TVar
cvar = S.TV <$> tvarname

cbind :: Parser S.TBind
cbind = S.TBind <$> cvar <*> optbounds

optbounds :: Parser [S.TCon]
optbounds = do bounds <- optional (parens (optional ((:) <$> ccon <*> commaList ccon)))
               return $ maybe [] (maybe [] id) bounds

tscheme :: Parser S.TSchema
tscheme = addLoc $
            try (do 
                bs <- brackets (do n <- cbind
                                   ns <- commaList cbind
                                   return (n:ns))
                fatarrow
                t <- ctype
                return (S.TSchema NoLoc bs t))
            <|>
            (S.TSchema NoLoc [] <$> ctype)

ctype :: Parser S.Type
ctype    =  addLoc (
            rword "int" *> return (S.TInt NoLoc)
        <|> rword "float" *> return (S.TFloat NoLoc)
        <|> rword "bool" *> return (S.TBool NoLoc)
        <|> rword "str" *> return (S.TStr NoLoc)
        <|> rword "None" *> return (S.TNone NoLoc)
        <|> rword "Self" *> return (S.TSelf NoLoc)
        <|> S.TOpt NoLoc <$> (qmark *> ctype)
        <|> braces (do t <- ctype
                       mbt <- optional (colon *> ctype)
                       return (maybe (S.PSet NoLoc t) (S.PMap NoLoc t) mbt))
        <|> try (parens (do alts <- some (try (utype <* vbar))
                            alt <- utype
                            return $ S.TUnion NoLoc (alts++[alt])))
        <|> try (do es <- many name
                    (p,k) <- parens funrows
                    arrow
                    t <- ctype
                    return (S.TFun NoLoc es p k t))
        <|> try (parens (S.TRecord NoLoc <$> kwdrow))
        <|> try (parens (S.TTuple NoLoc <$> posrow <* optional comma))
        <|> parens (return (S.TTuple NoLoc S.PosNil))
        <|> try (brackets (S.PSeq NoLoc <$> ctype))
        <|> try (S.TVar NoLoc <$> cvar)
        <|> S.TAt NoLoc <$> (symbol "@" *> ccon)
        <|> S.TCon NoLoc <$> ccon)
                

utype :: Parser S.UType
utype    =  rword "int" *> return S.UInt
        <|> rword "float" *> return S.UFloat
        <|> rword "bool" *> return S.UBool
        <|> rword "str" *> return S.UStr
        <|> (\str -> S.UStrCon (init (tail str))) <$> shortString []

