{-# LANGUAGE OverloadedStrings #-}

module Parser (parseFile) where

import Context
import Control.Monad.Trans.Accum
import Control.Monad.Trans.State
import Data.Functor
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.Void
import Def
import Elaborate
import Error
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseFile :: String -> IO (Either (ParseErrorBundle Text Void) (StateT Ctx (AccumT [Goal] Error) ()))
parseFile file = parse defs file <$> T.readFile file

defs :: Parser (StateT Ctx (AccumT [Goal] Error) ())
defs = do
  d <- many def
  eof
  return $ foldl (>>) (return ()) d

def :: Parser (StateT Ctx (AccumT [Goal] Error) ())
def = do
  s <- identifier
  symbol ":"
  a <- expr
  symbol ":="
  x <- expr
  symbol ";"
  return $ addDef s a x

expr :: Parser Expr
expr = label "expression" $ try ePi <|> try eLam <|> try appChunk

appChunk :: Parser Expr
appChunk = do
  xs <- some appAtom
  return $ foldl1 App xs

appAtom :: Parser Expr
appAtom = try (parens expr) <|> try eVar <|> try eHole

eVar :: Parser Expr
eVar = Var <$> identifier

eHole :: Parser Expr
eHole = Hole <$ symbol "?"

ePi :: Parser Expr
ePi = do
  (s, a) <- try piNamed <|> try piUnnamed
  symbol "->"
  b <- expr
  return $ Pi s a b

piNamed :: Parser (String, Expr)
piNamed = parens $ do
  s <- identifier
  symbol ":"
  a <- expr
  return (s, a)

piUnnamed :: Parser (String, Expr)
piUnnamed = do
  a <- appChunk
  return ("_", a)

eLam :: Parser Expr
eLam = do
  s <- identifier
  symbol "=>"
  x <- expr
  return $ Lam s x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = label "identifier" $ unpack <$> (lexeme $ pack <$> ((:) <$> identifierStartChar <*> many identifierChar))

identifierStartChar :: Parser Char
identifierStartChar = letterChar <|> char '_'

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

symbol :: Text -> Parser ()
symbol s = void $ L.symbol skipSpace s

skipSpace :: Parser ()
skipSpace = L.space space1 (L.skipLineComment "--") empty