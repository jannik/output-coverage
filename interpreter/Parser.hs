module Parser where

import Prelude hiding (head, pred)

import Control.Applicative ((<*>), (<*), (*>), (<$>), (<$))

import Data.Char (isSpace)

import Text.Parsec.Char (alphaNum, anyChar, char, lower, satisfy, string, upper)
import Text.Parsec.Combinator (between, eof, many1, manyTill, sepBy, sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>), many, parse, skipMany, try)
import Text.Parsec.String (Parser)

import AST

type P = Parser

-- Interface

parseFile :: FilePath -> IO (Either ParseError (Signature, UProgram, [Query]))
parseFile = (parseString <$>) . readFile

parseString :: String -> Either ParseError (Signature, UProgram, [Query])
parseString = parse full ""

-- Parsers

full :: P (Signature, UProgram, [Query])
full = (,,) <$> (spaces *> signature) <*> program <*> queries <* eof

signature :: P Signature
signature = Sig <$> types <*> preds

types :: P [Type]
types = many typ

typ :: P Type
typ = Typ <$> (try (symbol "%data") *> atom <* symbol "=") <*> con `sepBy` (symbol "|") <* symbol "."

con :: P Constructor
con = Con <$> atom <*> many atom

preds :: P [(PredName, [TypeName], Mode)]
preds = many pred

pred :: P (PredName, [TypeName], Mode)
pred = do
  try (symbol "%pred")
  pnam <- atom
  (mo, tpSig) <- unzip <$> many ((,) <$> polarity <*> atom)
  symbol "."
  return (pnam, tpSig, mo)

polarity :: P Polarity
polarity = (In <$ symbol "+") <|> (Out <$ symbol "-") <|> (None <$ symbol "*")

program :: P UProgram
program = spaces *> clauses

clauses :: P [UClause]
clauses = many clause

clause :: P UClause
clause = UClause <$> head <*> body

head :: P UStructure
head = struct

body :: P [UStructure]
body = many (symbol "<-" *> struct) <* symbol "."

struct :: P UStructure
struct = UStruct <$> atom <*> terms

atom :: P String
atom = lexeme $ lower <:> many (alphaNum <|> char '/' <|> char '\'')

terms :: P [UTerm]
terms = many term

term :: P UTerm
term = UVar <$> var
    <|> flip UComp [] <$> atom
    <|> (parens $ UComp <$> atom <*> terms)

var :: P VarName
var = lexeme $ upper <:> many (alphaNum <|> char '\'')

queries :: P [Query]
queries = map (map initVars) <$> many query

query :: P [UStructure]
query = symbol "?" *> struct `sepBy1` (symbol ",") <* symbol "."

-- General parser combinators

space :: P Char
space = satisfy isSpace -- <?> "space"

comment :: P String
comment = try (string "%{") *> manyTill anyChar (try (string "}%"))

spaces :: P ()
spaces = skipMany (many1 space <|> comment) -- <?> "white space or comment"

lexeme :: P a -> P a
lexeme = (<* spaces)

symbol :: String -> P String
symbol = lexeme . string

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")

-- Misc

infixr 5 <:>

(<:>) :: P Char -> P String -> P String
(<:>) a b = (:) <$> a <*> b
