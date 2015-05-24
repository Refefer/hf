{-# Language OverloadedStrings #-}
module Main where

import Control.Parallel.Strategies
import Data.Char (isAsciiUpper, toLower)
import Data.List (sort)
import Data.Maybe (catMaybes)
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import Prelude hiding (lines)
import qualified Data.ByteString.Char8 as B 
import Safe (headDef)
import System.Environment (getArgs)
--import System.Console.Haskeline (runInputT, getInputChar, defaultSettings)
import System.IO (stdin, stdout, stderr, hSetBuffering, openFile, 
                  IOMode( ReadMode ), BufferMode ( NoBuffering ) )
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import UI.NCurses

data Query = Query { q :: String, qLen :: Int } deriving (Show)
data ScoreStrat = EditDist | InfixLength | CIInfixLength | Length
data ResultSet = ResultSet { query   :: Query
                           , strat   :: ScoreStrat
                           , itemSet :: [ScoredList]
                           }

data SystemState = SystemState { current :: ResultSet
                               , history :: [ResultSet]
                               , cursorPos  :: Int
                               }

data Terminal = Exit | Updated SystemState | Selected B.ByteString
data Justify = LJustify | RJustify | Exact Int
data Row = Line Int | Bottom
data Write = Write { justify :: Justify 
                   , row :: Row
                   , contents :: String 
                   , attributes :: [Attribute]
                   }


sWrite :: Justify -> Row -> String -> Write
sWrite j r s = Write j r s []

-- Movement keys
type Scorer = B.ByteString -> Maybe Int
type ResultList = [B.ByteString]
type ScoredList = [(Int, B.ByteString)]

main :: IO ()
main = do
  ss    <- getStrat
  lines <- readLines
  let rs        = zip [1..] lines
  let chunkSize = fst . divMod (length rs) $ 5000
  let chunks    = chunk (chunkSize + 1) rs
  let qry       = Query "" 0
  bs <- initUI $ SystemState (ResultSet qry ss chunks) [] 0
  maybe (return ()) B.putStrLn bs

-- Run the Curses UI
initUI :: SystemState -> IO (Maybe B.ByteString)
initUI rs = do
  redirect $ runCurses $ defaultWindow >>= (ui rs)

redirect :: IO a -> IO a
redirect io = do
  oldStdout <- hDuplicate stdout
  hDuplicateTo stderr stdout
  res <- io
  hDuplicateTo oldStdout stdout
  return res

ui :: SystemState -> Window -> Curses (Maybe B.ByteString)
ui ss@(SystemState r _ cp) w = do
  coords@(rows, _) <- iScreenSize
  updateWindow w $ do
    clearScreen coords
    let top_items = take (rows - 2) . printTopItems $ r
    applyWrites coords $ concat [
        updateAt boldWrite cp top_items,
        [printStatus r],
        [printQuery . query $ r]
      ]
  render
  event <- readInput w 
  case processEvent ss event of
    Exit          -> return Nothing
    Updated newSs -> ui newSs w
    Selected bs   -> return $ Just bs

updateAt :: (a -> a) -> Int -> [a] -> [a]
updateAt f idx = loop idx 
  where loop _ [] = [] 
        loop 0 (x:xs) = (f x):xs
        loop i (x:xs) = x:(loop (i - 1) xs)

iScreenSize :: Curses (Int, Int)
iScreenSize = do
  (r, c) <- screenSize
  return $ (fromIntegral r, fromIntegral c)

-- Evaluates the Writes
applyWrites :: (Int, Int) -> [Write] -> Update ()
applyWrites _ [] = return ()
applyWrites coords@(r,_) (w@(Write _ Bottom _ _):ws) = applyWrites coords (nw:ws)
  where nw = w { row = Line (r - 1) }

applyWrites coords (w@(Write LJustify _ _ _):ws) = applyWrites coords (nw:ws)
  where nw = w { justify = Exact 0 }

applyWrites coords@(_,c) (w@(Write RJustify _ s _):ws) = applyWrites coords (nw:ws)
  where col = fromIntegral . max 0 $ c - (length s) - 1
        nw  = w { justify = Exact col }

applyWrites coords@(_,c) ((Write (Exact col) (Line r) s attrs):ws) = do
  moveCursor (fromIntegral r) (fromIntegral col)
  setAttrs True attrs
  drawString . take (c - col - 1) $ s
  setAttrs False attrs
  applyWrites coords ws
  where setAttrs b = mapM_ ((flip setAttribute) b)

-- We don't have a clear screen in this version of the library, so write one
clearScreen :: (Int, Int) -> Update ()
clearScreen (rows, cols) = do
  let coords = [(fromIntegral r, fromIntegral c) | r <- [0..(rows - 1)], c <- [0..(cols - 2)]]
  let clearPixel (r,c) = (moveCursor r c) >> (drawString " ")
  mapM_ clearPixel coords

readInput :: Window -> Curses Event
readInput w = do
  ev <- getEvent w . Just $ 1000 -- Nothing doesn't work.
  case ev of
    Nothing  -> readInput w
    Just ev' -> return ev'

processEvent :: SystemState -> Event -> Terminal

-- Delete
processEvent ss (EventSpecialKey KeyBackspace) = case ss of
  (SystemState _ (r:rs) _) -> Updated $ ss { current = r, history = rs, cursorPos = 0 }
  _ -> Updated ss

-- Down Arrow
processEvent ss (EventSpecialKey KeyDownArrow) = Updated $ newSS
  where newSS = ss { cursorPos = (cursorPos ss) + 1 } 

-- Up Arrow
processEvent ss (EventSpecialKey KeyUpArrow) = Updated $ newSS
  where newSS = ss { cursorPos = max 0 ((cursorPos ss) - 1) } 

-- Enter
processEvent (SystemState r _ cp) (EventCharacter '\n') = Selected bs
  where bs = snd $ (orderedItems r) !! cp

-- Ctrl + D
processEvent _  (EventCharacter '\EOT') = Exit

-- Add Char
processEvent ss@(SystemState r rs _) (EventCharacter c) = Updated newSS
  where newR = refine r . addChar . query $ r
        newSS = ss { current = newR, history = r:rs, cursorPos = 0 }
        addChar (Query qry ql) = Query (qry ++ [c]) (ql + 1)

processEvent ss _ = Updated ss

printQuery :: Query -> Write
printQuery qry = writeAtLine 0 $ "$ " ++ (q qry)

boldWrite :: Write -> Write
boldWrite w = w { attributes = AttributeBold:(attributes w) }

orderedItems :: ResultSet -> ScoredList
orderedItems = merge fst . itemSet

printTopItems :: ResultSet -> [Write]
printTopItems = zipWith writeAtLine [1..] .  items 
  where items = fmap B.unpack . fmap snd . orderedItems 

printStatus :: ResultSet -> Write
printStatus = sWrite RJustify Bottom . status . count
  where count = show . sum . fmap length . itemSet 
        status c = "[" ++ c ++ "]"

writeAtLine :: Int -> String -> Write
writeAtLine r = sWrite LJustify (Line r)

-- Refine a previous search result with query
refine :: ResultSet -> Query -> ResultSet
refine rs = querySet ss rl
  where rl = (fmap (fmap snd)) . itemSet $ rs
        ss = strat rs

querySet :: ScoreStrat -> [ResultList] -> Query -> ResultSet
querySet ss rl qry = ResultSet qry ss newSet
  where scorer  = buildScorer ss qry
        newSet = score scorer rl

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
  return $ case edt of "1" -> InfixLength
                       "2" -> EditDist
                       "3" -> Length
                       _   -> CIInfixLength

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
        tLen      = (fromIntegral . B.length $ t) :: Double
        lenScore  = round $ tLen ** 0.5 
        prefScore = if B.isPrefixOf bqs t then -1 else 0
        suffScore = if B.isSuffixOf bqs t then -1 else 0

eval CIInfixLength qry t = eval InfixLength qry lt
  where lt = B.map lower t
        lower c
          | isAsciiUpper c = toLower c
          | otherwise      = c

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
