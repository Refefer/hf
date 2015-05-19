{-# Language OverloadedStrings #-}
module Main where

import Control.Parallel.Strategies
import Data.List (sort)
import Data.Maybe (catMaybes)
import GHC.IO.Handle (hDuplicateTo)
import Prelude hiding (lines)
import qualified Data.ByteString.Char8 as B 
import Safe (headDef)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
--import System.Console.Haskeline (runInputT, getInputChar, defaultSettings)
import System.IO (stdin, stdout, hGetChar, hSetBuffering, openFile, hSetEcho, hFlush,
                  IOMode( ReadMode ), BufferMode ( NoBuffering ) )
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

data Query = Query { q :: String, qLen :: Int } deriving (Show)
data ScoreStrat = EditDist | InfixLength | Length
data ResultSet = ResultSet { query   :: Query
                           , strat   :: ScoreStrat
                           , itemSet :: [ScoredList]
                           }

-- Movement keys
data Arrow = Up | Down | Left | Right
data Key = Character Char | BackSpace | Movement Arrow | Enter | Tab | CtrlD

type Scorer = B.ByteString -> Maybe Int
type ResultList = [B.ByteString]
type ScoredList = [(Int, B.ByteString)]

main :: IO ()
main = do
  ss    <- getStrat
  lines <- readLines
  setupIO
  let rs  = zipWith (\x y -> (x, y)) [1..] lines
  let (chunkSize, _) = (length rs) `divMod` 100
  let chunks = chunk (chunkSize + 1) rs
  let qry = Query "" 0
  repl [ResultSet qry ss chunks]

setupIO :: IO ()
setupIO = do
  --hSetBuffering stdout NoBuffering
  hSetEcho stdin False

repl :: [ResultSet] -> IO ()
repl [] = exitSuccess
repl (r:rs) = do
  -- Show current result set
  let is = itemSet r
  let s = formatBest 10 $ merge fst is
  let status = B.pack . show . sum $ fmap length is
  _ <- B.putStrLn ""
  _ <- seq status $ mapM_ B.putStrLn s
  _ <- B.putStr "Items: " 
  _ <- B.putStrLn status
  readInput (r:rs)

-- Updates the state
readInput :: [ResultSet] -> IO ()
readInput [] = exitSuccess
readInput (r:rs) = do
  res <- readChar (query r)
  case res of 
    Just (Character c) -> repl (nr:r:rs)
      where nqry = addChar c (query r)
            nr   = refine r nqry
    Just BackSpace -> case rs of [] -> repl [r]
                                 _  -> repl rs
    Just CtrlD     -> exitSuccess
    Just Enter     -> exitSuccess
    Nothing        -> exitSuccess
    _              -> repl (r:rs)

-- Refine a previous search result with query
refine :: ResultSet -> Query -> ResultSet
refine rs = querySet ss rl
  where rl = (fmap (fmap snd)) . itemSet $ rs
        ss = strat rs

querySet :: ScoreStrat -> [ResultList] -> Query -> ResultSet
querySet ss rl qry = ResultSet qry ss newSet
  where scorer  = buildScorer ss qry
        newSet = score scorer rl

addChar :: Char -> Query -> Query
addChar c (Query qry ql) = Query nq nl
  where nq = qry ++ [c]
        nl = 1 + ql

readChar :: Query -> IO (Maybe Key)
readChar (Query qry _) = do 
  _ <- putStr $ "> " ++ qry
  _ <- hFlush stdout
  c <- hGetChar stdin
  return $ matchChar c

matchChar :: Char -> Maybe Key
matchChar '\DEL' = Just BackSpace
matchChar '\n'   = Just Enter
matchChar '\t'   = Just Tab
matchChar '\ESC' = Nothing
matchChar '\EOT' = Just CtrlD
matchChar a      = Just $ Character a

formatBest :: Int -> ScoredList -> ResultList
formatBest amt sl = fmap serializeItem topItems
  where topItems = take amt sl
        toBS = B.pack . show
        serializeItem (x, y) = B.concat [toBS x, ": ", y]

-- Read lines from stdin
readLines :: IO [B.ByteString] 
readLines = do
  inp <- B.getContents
  _   <- reOpenStdin
  return $ B.lines inp

-- Have to reopen stdin since getContents closes it
reOpenStdin :: IO () 
reOpenStdin = do 
  tty <- openFile "/dev/tty" ReadMode
  hSetBuffering tty NoBuffering
  hDuplicateTo tty stdin

-- Get query as first argument
getStrat :: IO ScoreStrat
getStrat = do
  args <- getArgs
  let edt = headDef "1" args
  return $ case edt of "2" -> EditDist
                       "3" -> Length
                       _   -> InfixLength

-- Builds score function
buildScorer :: ScoreStrat -> Query -> Scorer
buildScorer ss = eval ss

eval :: ScoreStrat -> Query -> B.ByteString -> Maybe Int
eval Length _ t = Just $ B.length t

eval EditDist (Query [c] 1) t
  | B.elem c t = Just $ tlen - 1
  | otherwise = Nothing
  where tlen  = B.length t

eval EditDist (Query qs _) t = Just $ min dist (tlen - 1)
  where tlen = B.length t
        raw_t = B.unpack t
        dist = levenshteinDistance defaultEditCosts qs raw_t

eval InfixLength (Query [c] 1) t 
  | B.elem c t = Just 1 
  | otherwise  = Nothing

eval InfixLength (Query qs _) t
  | B.isInfixOf bqs t = Just $ lenScore + prefScore + suffScore
  | otherwise         = Nothing
  where bqs       = B.pack qs
        tLen      = (fromIntegral $ B.length t) :: Double
        lenScore  = round $ tLen ** 0.5 
        prefScore = if B.isPrefixOf bqs t then -1 else 0
        suffScore = if B.isSuffixOf bqs t then -1 else 0

-- Score line accordingly
score :: Scorer -> [ResultList] -> [ScoredList]
score f rl   = parMap rdeepseq cms rl
  where fo x = fmap (\i -> (i, x)) $ f x
        cms  = sort . catMaybes . (fmap fo)

-- Chunks items into groups
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk amt xs = c1:(chunk amt rest)
  where (c1, rest) = splitAt amt xs

-- Merge facilities for lazy top elements, instead of sorting them all
merge :: Ord b => (a -> b) -> [[a]] -> [a]
merge _ []  = []
merge _ [a] = a
merge f ss = foldr (merge2 f) [] ss

merge2 :: Ord b => (a -> b) -> [a] -> [a] -> [a]
merge2 f (a:as) (b:bs)
  | f(a) < f(b) = a : merge2 f as (b:bs)
  | otherwise   = b : merge2 f (a:as) bs
merge2 _ [] rs = rs
merge2 _ rs [] = rs


