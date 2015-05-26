{-# Language OverloadedStrings #-}
module Scorer (
    ScoreStrat(..)
  , ScoreStrategy(..)
  , liftSS
  , Scorer
  ) where

import qualified Data.ByteString.Char8 as B 
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import Utils (toLower)

data ScoreStrat = EditDist 
                | InfixLength 
                | CIInfixLength 
                deriving (Show, Eq)

data EditDistC    = EditDistC String
data InfixLengthC = InfixLengthC B.ByteString Bool Int

data Query  = EditStrat EditDistC 
            | InfixStrat InfixLengthC

type Scorer = B.ByteString -> Maybe Double

class ScoreStrategy a where
  score :: a -> Scorer
  range :: a -> B.ByteString -> Maybe (Int, Int)

instance ScoreStrategy Query where
  score (EditStrat e)    = score e
  score (InfixStrat ilc) = score ilc

  range (EditStrat e)    = range e
  range (InfixStrat ilc) = range ilc

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

liftSS :: ScoreStrat -> String -> Query
liftSS EditDist q      = EditStrat $ EditDistC q
liftSS InfixLength q   = InfixStrat $ InfixLengthC (B.pack q) False (length q)
liftSS CIInfixLength q = InfixStrat $ InfixLengthC (toLower . B.pack $ q) True (length q)
