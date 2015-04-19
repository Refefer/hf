{-# Language OverloadedStrings #-}
module Main where

import Prelude hiding (lines)
import System.IO (stdin)
import System.Environment (getArgs)
import Safe (headMay)
import Data.Text.IO (hGetContents)
import qualified Data.Text as T
import qualified Data.Heap as H
import qualified Data.Foldable as F
import Control.Monad.State
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

type Scorer = T.Text -> Int
type RankedSet = H.MinHeap (Int, T.Text)

main :: IO ()
main = do
  query  <- getQuery
  lines  <- readLines
  let results = score (buildScorer query) lines 
  let top =  H.take 10 results :: [(Int, T.Text)]
  let serializeItem (x, y) = F.concat [show x, ": ", show y]
  let s = fmap serializeItem top
  mapM_ putStrLn s
  -- mapM_ putStrLn $ fmap snd $ H.take 10 results

-- Read lines from stdin
readLines :: IO [T.Text] 
readLines = do
  inp <- hGetContents stdin 
  return $ T.lines inp

-- Get query as first argument
getQuery :: IO T.Text
getQuery = do
  args <- getArgs
  let targs = fmap T.pack args
  return $ maybe "" id $ headMay targs

-- Builds score function
buildScorer :: T.Text -> Scorer
buildScorer q = mDist
  where qs = T.unpack q
        dist  = levenshteinDistance defaultEditCosts qs
        minF  = F.foldl' min (T.length q)
        filtEmpty = filter (/= "")
        split = T.split (== '/')
        mDist t = minF $ fmap (dist . T.unpack) $ (filtEmpty . split) t

-- Score line accordingly
score :: Scorer -> [T.Text] -> RankedSet
score f ts = execState (mapM_ g ts) H.empty
  where g = modify . H.insert . \x -> (f x, x)

