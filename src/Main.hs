{-# Language OverloadedStrings #-}
module Main where

import Control.Parallel.Strategies
import Data.List (sort)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import GHC.IO.Handle (hDuplicateTo)
import Prelude hiding (lines)
import qualified Data.ByteString.Char8 as B 
import Safe (headDef)
import System.Environment (getArgs)
--import System.Console.Haskeline (runInputT, getInputChar, defaultSettings)
import System.IO (stdin, hSetBuffering, openFile, hSetEcho,
                  IOMode( ReadMode ), BufferMode ( NoBuffering ) )
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import UI.NCurses

data Query = Query { q :: String, qLen :: Int } deriving (Show)
data ScoreStrat = EditDist | InfixLength | Length
data ResultSet = ResultSet { query   :: Query
                           , strat   :: ScoreStrat
                           , itemSet :: [ScoredList]
                           }

data SystemState = SystemState { current :: ResultSet
                               , history :: [ResultSet]
                               , cursorPos  :: Int
                               }
data Terminal = Exit | Updated SystemState | Selected SystemState

-- Movement keys
type Scorer = B.ByteString -> Maybe Int
type ResultList = [B.ByteString]
type ScoredList = [(Int, B.ByteString)]

main :: IO ()
main = do
  ss    <- getStrat
  lines <- readLines
  setupIO
  let rs  = zip [1..] lines
  let (chunkSize, _) = (length rs) `divMod` 100
  let chunks = chunk (chunkSize + 1) rs
  let qry = Query "" 0
  _ <- initUI $ SystemState (ResultSet qry ss chunks) [] 0
  return ()

setupIO :: IO ()
setupIO = do
  --hSetBuffering stdout NoBuffering
  hSetEcho stdin False

-- Run the Curses UI
initUI :: SystemState -> IO (Maybe B.ByteString)
initUI rs = runCurses $ do
  setEcho False
  w <- defaultWindow
  ui w rs

ui :: Window -> SystemState -> Curses (Maybe B.ByteString)
ui w ss @ (SystemState r _ _) = do
  coords@(rows, cols) <- screenSize
  updateWindow w $ do
    clearScreen coords
    printTopItems cols (rows - 2) r
    printStatus cols (rows - 1) r
    printQuery cols . query $ r
  render
  event <- readInput w 
  case processEvent ss event of
    Exit -> return Nothing
    Updated newSs -> ui w newSs 
    Selected _ -> return Nothing

-- We don't have a clear screen in this version of the library, so write one
clearScreen :: (Integer, Integer) -> Update ()
clearScreen (rows, cols) = do
  let coords = [(r, c) | r <- [0..(rows - 1)], c <- [0..(cols - 2)]]
  let clearPixel (r,c) = (moveCursor r c) >> (drawString " ")
  mapM_ clearPixel coords

readInput :: Window -> Curses Event
readInput w = do
  ev <- getEvent w $ Just 1000 -- Nothing doesn't work.
  case ev of
    Nothing  -> readInput w
    Just ev' -> return ev'

processEvent :: SystemState -> Event -> Terminal
processEvent ss (EventSpecialKey KeyBackspace) = case ss of
  (SystemState _ (r:rs) _) -> Updated $ ss { current = r, history = rs }
  _ -> Updated ss
processEvent ss (EventCharacter '\n') = Selected ss
processEvent ss (EventCharacter '\EOT') = Exit
processEvent ss@(SystemState r rs _) (EventCharacter c) = Updated newSS
  where newR = refine r . addChar c . query $ r
        newSS = ss { current = newR, history = (r:rs) }

processEvent ss _ = Updated ss

printQuery :: Integer -> Query -> Update ()
printQuery maxC qry = writeAtLine maxC 0 (q qry)

printTopItems :: Integer -> Integer -> ResultSet -> Update ()
printTopItems mc sn rs = do
  let n = fromIntegral sn
  let items = fmap B.unpack . formatBest n . merge fst . itemSet $ rs
  let f = uncurry (writeAtLine mc)
  mapM_ f $ zip [1..] items 

printStatus :: Integer -> Integer -> ResultSet -> Update ()
printStatus c r rs = do
  let status = show . sum . fmap length . itemSet $ rs
  writeAtLine c r status

writeAtLine :: Integer -> Integer -> String -> Update ()
writeAtLine maxC r = writeAt maxC (r, 0) 

writeAt :: Integer -> (Integer, Integer) -> String -> Update ()
writeAt maxC (r, c) content = do
  moveCursor r c
  let maxString = fromIntegral $ maxC - c
  drawString . take maxString $ content

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
