{-# Language OverloadedStrings #-}
module Main where

import Prelude hiding (lines)
import System.IO (stdin)
import System.Environment (getArgs)
import Safe (headMay)
import Data.Text.Lazy.IO (hGetContents)
import qualified Data.Text.Lazy as T
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
  let topItems =  H.take 10 results :: [(Int, T.Text)]
  let serializeItem (x, y) = F.concat [show x, ": ", show y]
  let s = fmap serializeItem topItems
  let items =  "Items:" ++ (show $ length lines)
  mapM_ putStrLn (items:s)

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

tLength :: T.Text -> Int
tLength t = fromIntegral $ T.length t

-- Builds score function
buildScorer :: T.Text -> Scorer
buildScorer q t = minF $ fmap (dist . T.unpack) $ split t
  where qs = T.unpack q
        dist  = levenshteinDistance defaultEditCosts qs
        minF xs = F.minimum $ (tLength q):xs
        split txt = filter (/= "") $ T.split (== '/') txt

-- Score line accordingly
score :: Scorer -> [T.Text] -> RankedSet
score f ts = execState (mapM_ g ts) H.empty
  where g = modify . H.insert . \x -> (f x, x)

