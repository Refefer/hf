module SimpleFormatter (
  format
) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Parsec (Parsec, parse, char, many1, digit, alphaNum, noneOf, (<|>), eof)

type SParsec u = Parsec String () u
type FauxMap = [(String, String)]

data Expr = Env String 
          | Pos Int
          | Raw String
          deriving (Show, Eq)

data FormatArgs = FormatArgs FauxMap [String] deriving (Show, Eq)

pos :: SParsec Expr  
pos = do
  _ <- char '{'
  digits <- many1 digit 
  _ <- char '}'
  return $ Pos (read digits)

env :: SParsec Expr
env = do
  _ <- char '$'
  name <- many1 alphaNum
  return $ Env name

raw :: SParsec Expr
raw = do
 other <- many1 $ noneOf ['$', '{']
 return $ Raw other

someEOF :: SParsec [a]
someEOF = do
  eof
  return []

simpleFormat :: SParsec [Expr] 
simpleFormat = do
  exprs <- (many1 $ raw <|> env <|> pos) <|> someEOF
  return exprs

parseF :: String -> Maybe [Expr]
parseF s = case (parse simpleFormat "(source)" s) of
  Right exps -> Just exps
  _          -> Nothing


fauxLookup :: String -> FauxMap -> Maybe String
fauxLookup k = fmap snd . find keyF
  where keyF (k2,_) = k2 == k

rawFormat :: [Expr] -> FormatArgs -> [String]
rawFormat [] _ = []
rawFormat (x:xs) fa@(FormatArgs fenv fargs) = item:(rawFormat xs fa)
  where formatExpr (Raw s) = s
        formatExpr (Pos p) = fargs !! p
        formatExpr (Env n) = fromMaybe "" $ fauxLookup n fenv 
        item = formatExpr x

-- String to format, user Arguments
format :: String -> [String] -> FauxMap -> String
format str as envs = concat $ rawFormat parsed fargs
  where parsed = fromMaybe [] $ parseF str
        fargs  = FormatArgs envs as

