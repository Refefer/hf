{-# Language OverloadedStrings #-}
module Main where

import Control.Parallel.Strategies
import Data.Text.Lazy.IO (hGetContents)
import Data.List (sort)
import qualified Data.Foldable as F
import qualified Data.Heap as H
import qualified Data.Text.Lazy as T
import Prelude hiding (lines)
import Safe (headMay)
import System.Environment (getArgs)
import System.IO (stdin)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

data Query = Query { q :: String, qLen :: Int } deriving (Show)
data ScoreStrat = EditDist | InfixLength | Length

type Scorer = T.Text -> Int
type RankedSet = H.MinHeap (Int, T.Text)

main :: IO ()
main = do
  query  <- getQuery
  lines  <- readLines
  let results = score (buildScorer Length query) lines 
  let topItems =  take 10 results :: [(Int, T.Text)]
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
getQuery :: IO Query
getQuery = do
  args <- getArgs
  let val = maybe "" id $ headMay args
  return $ Query val (length val)

-- Builds score function
buildScorer :: ScoreStrat -> Query -> Scorer
buildScorer ss query = \t -> minF $ fmap (dist . T.unpack) $ split t
  where dist  = eval ss query
        minF xs = min (qLen query) $ F.minimum xs
        split txt = filter (/= "") $ T.split (== '/') txt

eval :: ScoreStrat -> Query -> String -> Int
eval Length _ t = length t
eval EditDist (Query qs _) t = levenshteinDistance defaultEditCosts qs t
eval InfixLength (Query qs tLen) t = 
  if T.isInfixOf (T.pack qs) (T.pack t) then (length t) - tLen else tLen

-- Score line accordingly
score :: Scorer -> [T.Text] -> [(Int, T.Text)]
--score f = sort . (parMap rdeepseq (\x -> (f x, x)))
score f = sort . fm . fmap (\x -> (f x, x))
  where fm = withStrategy (parBuffer 100 rseq)
