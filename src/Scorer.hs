{-# Language OverloadedStrings #-}
module Scorer (
    ScoreStrat(..)
  , ScoreStrategy(..)
  , liftSS
  , Scorer
  , CQuery
  ) where

import Prelude hiding (break)
import qualified Data.ByteString.Char8 as B 
import Control.Applicative
import Data.Maybe (catMaybes)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import Utils (toLower)

data ScoreStrat = EditDist 
                | InfixLength 
                | CIInfixLength 
                | SlopLength
                deriving (Show, Eq)

data EditDistC    = EditDistC String
data InfixLengthC = InfixLengthC B.ByteString Bool Int
data SlopLengthC  = SlopLengthC B.ByteString

data CQuery = EditStrat EditDistC 
            | InfixStrat InfixLengthC
            | SlopStrat SlopLengthC

type Scorer = B.ByteString -> Maybe Double

class ScoreStrategy a where
  -- Takes a query and a title and scores whether or not it matches
  score :: a -> Scorer
  -- Indicates the bounding area of the query
  range :: a -> B.ByteString -> Maybe (Int, Int)

instance ScoreStrategy CQuery where
  score (EditStrat e)    = score e
  score (InfixStrat ilc) = score ilc
  score (SlopStrat l)  = score l

  range (EditStrat e)    = range e
  range (InfixStrat ilc) = range ilc
  range (SlopStrat l)  = range l

instance ScoreStrategy EditDistC where
  score (EditDistC [c]) t
    | B.elem c t = Just $ tlen - 1
    | otherwise = Nothing
    where tlen  = fromIntegral . B.length $ t

  score (EditDistC qs) t = Just $ fromIntegral . min dist $ (tlen - 1)
    where tlen  = B.length t
          raw_t = B.unpack t
          dist  = levenshteinDistance defaultEditCosts qs raw_t

  range _ _ = Nothing

instance ScoreStrategy InfixLengthC where

  score (InfixLengthC c False 1)  t 
    | B.isInfixOf c t = Just $ fromIntegral $ 1 + (B.length t)
    | otherwise       = Nothing

  score (InfixLengthC qs False _) t
    | B.isInfixOf qs t = Just $ lenScore + prefScore + suffScore
    | otherwise        = Nothing
    where tLen      = fromIntegral . B.length $ t
          lenScore  = tLen ** 0.5 
          prefScore = if B.isPrefixOf qs t then -0.5 else 0
          suffScore = if B.isSuffixOf qs t then -1 else 0

  score (InfixLengthC qs True l) t = score (InfixLengthC qs False l) . toLower $ t

  range (InfixLengthC "" _ _) _ = Nothing
  range (InfixLengthC qs False qsl) t = do
    let (leftS, rightS) = B.breakSubstring qs t 
    let len = B.length leftS
    case rightS of
      "" -> Nothing
      _  -> Just $ (len, len + qsl)

  range (InfixLengthC qs True l) t = range (InfixLengthC qs False l) . toLower $ t

instance ScoreStrategy SlopLengthC where

  score (SlopLengthC qs) t = do
    scores <- findQ qs t
    let total = sum $ if null scores then [] else init scores
    let lenScore  = (fromIntegral . B.length $ t) ** 0.5 
    return $ lenScore + (fromIntegral total)

  range (SlopLengthC "") _ = Nothing
  range (SlopLengthC qs) t = do
    locs <- fmap reverse $ findQ qs t 
    let total = case locs of
               []       -> (0, 0)
               (x:xs) -> (x-1, x + (sum xs))
    return total

findQ :: B.ByteString -> B.ByteString -> Maybe [Int]
findQ qs t = sequence . loop t qs $ []
  where break r c = case B.break (== c) r of
          (_, "")      -> (Nothing, "")
          (h, rest) -> (Just (B.length h + 1), B.drop 1 rest)

        loop :: B.ByteString -> B.ByteString -> [Maybe Int] -> [Maybe Int]
        loop remT qss acc
          | B.null qss = acc
          | otherwise  = case break remT (B.head qss)  of
            (s, rest) -> loop rest (B.tail qss) (s:acc)

  
instance (ScoreStrategy a) => ScoreStrategy [a] where
  score [x] t = score x t
  score xs t = do
    scores <- sequence $ score <$> xs <*> (return t)
    return $ sum scores

  range [x] t = range x t
  range xs t = do
    case catMaybes $ range <$> xs <*> (return t) of
      [] -> Nothing
      as -> return $ last as

liftSS :: ScoreStrat -> String -> CQuery
liftSS EditDist q      = EditStrat $ EditDistC q
liftSS InfixLength q   = InfixStrat $ InfixLengthC (B.pack q) False (length q)
liftSS CIInfixLength q = InfixStrat $ InfixLengthC (toLower . B.pack $ q) True (length q)
liftSS SlopLength q    = SlopStrat . SlopLengthC . B.pack $ q
