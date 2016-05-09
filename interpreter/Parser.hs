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

type Variable = VarName

type Dependency' = Dependency Variable
type Type' = Type Variable
type Clause' = Clause Variable
type Structure' = Structure Variable
type Term' = Term Variable
type Signature' = Signature Variable
type TypeFam' = TypeFam Variable
type Constructor' = Constructor Variable
type Query' = Query Variable

type P = Parser

-- Interface

parseFile :: FilePath -> IO (Either ParseError (Signature', [Clause'], [Query']))
parseFile = (parseString <$>) . readFile

parseString :: String -> Either ParseError (Signature', [Clause'], [Query'])
parseString = parse full ""

-- Parsers

full :: P (Signature', [Clause'], [Query'])
full = (,,) <$> (spaces *> signature) <*> many clause <*> many query <* eof

signature :: P Signature'
signature = Sig <$> many typefam <*> many pred

typefam :: P TypeFam'
typefam = TypeFam <$> (try (symbol "%data") *> atom)
                  <*> (many dep <* symbol "=")
                  <*> con `sepBy` (symbol "|") <* symbol "."

dep :: P Dependency'
dep = braces $ (,) <$> (var <* symbol ":") <*> typ

typ :: P Type'
typ = Typ <$> atom <*> many term

con :: P Constructor'
con = Con <$> (atom <* symbol ":") <*> many dep <*> typ

pred :: P (PredName, [Dependency'], Mode)
pred = do
  try (symbol "%pred")
  pnam <- atom
  (mo, tpSig) <- unzip <$> many ((,) <$> polarity <*> dep)
  symbol "."
  return (pnam, tpSig, mo)

polarity :: P Polarity
polarity = (In <$ symbol "+") <|> (Out <$ symbol "-") <|> (None <$ symbol "*")

clause :: P Clause'
clause = Clause <$> many dep <*> head <*> body

head :: P Structure'
head = struct

body :: P [Structure']
body = many (symbol "<-" *> struct) <* symbol "."

struct :: P Structure'
struct = Struct <$> atom <*> many term

atom :: P String
atom = lexeme $ lower <:> many (alphaNum <|> char '-' <|> char '/' <|> char '\'')

term :: P Term'
term = Var <$> var
    <|> (try $ parens $ Var <$> var)
    <|> flip Comp [] <$> atom
    <|> (parens $ Comp <$> atom <*> many term)

var :: P VarName
var = lexeme $ upper <:> many (alphaNum <|> char '\'')

query :: P [Structure']
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

braces :: P a -> P a
braces = between (symbol "{") (symbol "}")

-- Misc

infixr 5 <:>

(<:>) :: P Char -> P String -> P String
(<:>) a b = (:) <$> a <*> b
