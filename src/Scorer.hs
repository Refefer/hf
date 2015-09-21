{-# Language OverloadedStrings #-}
module Scorer (
    ScoreStrat(..)
  , ScoreStrategy(..)
  , liftSS
  , Scorer
  , CQuery
  , variance
  ) where

import Prelude hiding (break)
import qualified Data.ByteString.Char8 as B 
import Control.Applicative
import Data.Maybe (catMaybes)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import Utils (toLower)

data ScoreStrat = EditDist 
                | InfixLength 
                | SlopLength
                | CILength ScoreStrat
                deriving (Show, Eq)

data EditDistC    = EditDistC String
data InfixLengthC = InfixLengthC B.ByteString Int
data SlopLengthC  = SlopLengthC ([Int] -> Double) B.ByteString

data CQuery = EditStrat EditDistC 
            | InfixStrat InfixLengthC
            | SlopStrat SlopLengthC
            | CIStrat CQuery

type Scorer = B.ByteString -> Maybe Double

class ScoreStrategy a where
  -- Takes a query and a title and scores whether or not it matches
  score :: a -> Scorer
  -- Indicates the bounding area of the query
  range :: a -> B.ByteString -> Maybe (Int, Int)

instance ScoreStrategy CQuery where
  score (EditStrat e)    = score e
  score (InfixStrat ilc) = score ilc
  score (SlopStrat l)    = score l
  score (CIStrat cs)     = score cs . toLower

  range (EditStrat e)    = range e
  range (InfixStrat ilc) = range ilc
  range (SlopStrat l)    = range l
  range (CIStrat cs)     = range cs . toLower

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

  score (InfixLengthC c 1)  t 
    | B.isInfixOf c t = Just $ fromIntegral $ 1 + (B.length t)
    | otherwise       = Nothing

  score (InfixLengthC qs _) t
    | B.isInfixOf qs t = Just $ lenScore + prefScore + suffScore
    | otherwise        = Nothing
    where tLen      = fromIntegral . B.length $ t
          lenScore  = tLen ** 0.5 
          prefScore = if B.isPrefixOf qs t then -0.5 else 0
          suffScore = if B.isSuffixOf qs t then -1 else 0

  range (InfixLengthC "" _) _ = Nothing
  range (InfixLengthC qs qsl) t = do
    let (leftS, rightS) = B.breakSubstring qs t 
    let len = B.length leftS
    case rightS of
      "" -> Nothing
      _  -> Just $ (len, len + qsl)

instance ScoreStrategy SlopLengthC where

  score (SlopLengthC f qs) t = do
    scores <- fmap (fmap fromIntegral) $ findQ qs t
    let total = f $ if null scores then [] else init scores
    let lenScore  = (fromIntegral . B.length $ t) ** 0.5 
    return $ lenScore + total

  range (SlopLengthC _ "") _ = Nothing
  range (SlopLengthC _ qs) t = do
    locs <- fmap reverse $ findQ qs t 
    let total = case locs of
               []       -> (0, 0)
               (x:xs) -> (x-1, x + (sum xs))
    return total

variance :: [Int] -> Double
variance [] = 0.0
variance xs = (sum . diff $ total) / (tLen)
  where ixs    = fmap fromIntegral xs
        total  = sum ixs
        diff t = fmap (\x -> (x - t) ** 2) ixs
        tLen   = fromIntegral $ length xs

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
liftSS InfixLength q   = InfixStrat $ InfixLengthC (B.pack q) (length q)
liftSS SlopLength q    = SlopStrat . SlopLengthC variance . B.pack $ q
liftSS (CILength ss) q = CIStrat $ liftSS ss (B.unpack . toLower . B.pack $ q)
