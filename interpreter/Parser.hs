module Parser where

import Prelude hiding (head, pred)

import Control.Applicative ((<*>), (<*), (*>), (<$>))

import Text.Parsec.Char (alphaNum, char, lower, spaces, string, upper)
import Text.Parsec.Combinator (between, eof, sepBy1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>), many, parse)
import Text.Parsec.String (Parser)

import AST

type P = Parser

-- Interface

parseFile :: FilePath -> IO (Either ParseError (Program, [Query]))
parseFile = (parseString <$>) . readFile

parseString :: String -> Either ParseError (Program, [Query])
parseString = parse full ""

-- Parsers

full :: P (Program, [Query])
full = (,) <$> program <*> queries <* eof

program :: P Program
program = spaces *> clauses

clauses :: P [Clause]
clauses = many clause

clause :: P Clause
clause = (:<-:) <$> head <*> body

head :: P Pred
head = pred

body :: P [Pred]
body = many (symbol "<-" *> pred) <* symbol "."

pred :: P Pred
pred = Pred <$> atom <*> terms

atom :: P String
atom = lexeme $ lower <:> many (alphaNum <|> char '/')

terms :: P [Term]
terms = many term

term :: P Term
term = Var <$> ident
    <|> Comp . flip Pred [] <$> atom
    <|> Comp <$> parens pred

ident :: P Ident
ident = lexeme $ flip Ident 0 <$> upper <:> many alphaNum

queries :: P [Query]
queries = many query

query :: P Query
query = symbol "?" *> pred `sepBy1` (symbol ",") <* symbol "."

-- General parser combinators

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
