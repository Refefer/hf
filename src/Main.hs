{-# Language OverloadedStrings #-}
module Main where

import Control.Parallel.Strategies
import Data.List (sort)
import Data.Maybe (catMaybes)
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import Prelude hiding (lines)
import qualified Data.ByteString.Char8 as B 
import System.Environment (getArgs)
import System.IO (stdin, stdout, stderr, hSetBuffering, openFile, 
                  IOMode( ReadMode ), BufferMode ( NoBuffering ) )
import UI.NCurses

import Scorer
import HfArgs (compilerOpts, Flag(..))
import Write
import Utils

data Query = Query { q :: String
                   , qLen :: Int 
                   } 
                   deriving (Show, Eq)

data ResultSet = ResultSet { query   :: Query
                           , strat   :: ScoreStrat
                           , itemSet :: [ScoredList]
                           }
                           deriving (Show, Eq)

data SystemState = SystemState { current   :: ResultSet
                               , history   :: [ResultSet]
                               , cursorPos :: Int
                               , rCount    :: Int
                               } deriving (Show, Eq)

data Terminal = Exit 
              | Updated SystemState 
              | Selected B.ByteString
              deriving (Show, Eq)

data AttrWrite = AttrWrite { write       :: Write 
                           , attrs       :: [Attribute]
                           , highlighted :: Bool
                          } deriving (Show, Eq)

type ResultList = [B.ByteString]
type ScoredList = [(Double, B.ByteString)]

iSimple :: Justify -> Row -> String -> AttrWrite
iSimple j r s =  AttrWrite (simple j r s) [] False

main :: IO ()
main = do
  ss    <- getStrat
  lines <- readLines
  let rs        = zip [1..] lines
  let len       = length rs
  let chunkSize = fst . divMod len $ 5000
  let chunks    = chunk (chunkSize + 1) rs
  let qry       = Query "" 0
  bs <- initUI $ SystemState (ResultSet qry ss chunks) [] 0 len
  maybe (return ()) B.putStrLn bs

-- Run the Curses UI
initUI :: SystemState -> IO (Maybe B.ByteString)
initUI rs = do
  redirect . runCurses $ do
    w   <- defaultWindow 
    cid <- newColorID ColorGreen ColorDefault 1
    ui rs w cid

-- Redirects the stdout to stderr
redirect :: IO a -> IO a
redirect io = do
  oldStdout <- hDuplicate stdout
  hDuplicateTo stderr stdout
  res       <- io
  hDuplicateTo oldStdout stdout
  return res

ui :: SystemState -> Window -> ColorID -> Curses (Maybe B.ByteString)
ui ss@(SystemState r _ cp rc) w cid = do
  coords <- iScreenSize
  let top_items = take ((fst coords) - 2) . printTopItems $ r
  renderWith w $ do
    clearScreen coords
    let item_set  = updateAt boldWrite cp top_items
    applyWrites cid coords $ concat [
        item_set >>= (highlight r),
        [printStatus rc r],
        [printQuery . query $ r]
      ]
  event <- readInput w 
  -- We grab it again in case they resized their screen
  c2 <- iScreenSize
  renderWith w $ applyWrites cid c2 [iSimple LJustify Bottom "Updating..."]
  updateState w ss (length top_items) event cid
  
-- Handles updating the system state
updateState :: Window -> SystemState -> Int -> Event -> ColorID -> Curses (Maybe B.ByteString)
updateState w ss itemCount event c = case processEvent ss event of
    Exit          -> return Nothing
    Selected bs   -> return $ Just bs
    Updated newSs -> do
      let newCP = min (itemCount - 1) (cursorPos newSs)
      let safeSs = newSs {cursorPos = newCP}
      ui safeSs w c

renderWith :: Window -> Update () -> Curses ()
renderWith w up = updateWindow w up >> render

-- Update an element in the list at the given index
updateAt :: (a -> a) -> Int -> [a] -> [a]
updateAt f idx = loop idx 
  where loop _ [] = [] 
        loop 0 (x:xs) = (f x):xs
        loop i (x:xs) = x:(loop (i - 1) xs)

-- Because Integers are inconvenient
iScreenSize :: Curses (Int, Int)
iScreenSize = do
  (r, c) <- screenSize
  return $ (fromIntegral r, fromIntegral c)

-- Evaluates the Writes
applyWrites :: ColorID -> (Int, Int) -> [AttrWrite] -> Update ()
applyWrites cid c ws = do
  let realWrites = catMaybes . fmap (constrainAW c) $ ws
  mapM_ (displayWrite cid) realWrites

-- Constrains based on AttrWrite
constrainAW :: (Int, Int) -> AttrWrite -> Maybe ([Attribute], Bool, ExactWrite)
constrainAW coords (AttrWrite e atts hled) = do
  ew <- constrain coords e
  return (atts, hled, ew)

-- Write it out
displayWrite :: ColorID -> ([Attribute], Bool, ExactWrite) -> Update ()
displayWrite cid (atts, hl, (ExactWrite (r, col) s)) = do
  moveCursor (fromIntegral r) (fromIntegral col)
  applyColor cid hl $ applyAttributes atts $ drawString s

-- Apply an attribute for a given amount
applyAttributes :: [Attribute] -> Update () -> Update ()
applyAttributes atts up = do
  setAttrs True atts
  up
  setAttrs False atts
  where setAttrs b = mapM_ ((flip setAttribute) b)

applyColor :: ColorID -> Bool -> Update () -> Update ()
applyColor _ False up = up
applyColor cid _ up = do
  setColor cid
  up
  setColor defaultColorID

-- We don't have a clear screen in this version of the library, so write one
clearScreen :: (Int, Int) -> Update ()
clearScreen (rows, cols) = do
  let coords = [(fromIntegral r, fromIntegral c) | r <- [0..(rows - 1)], c <- [0..(cols - 2)]]
  let clearPixel (r,c) = (moveCursor r c) >> (drawString " ")
  mapM_ clearPixel coords

-- Reads from input
readInput :: Window -> Curses Event
readInput w = do
  ev <- getEvent w . Just $ 1000 -- Nothing doesn't work.
  case ev of
    Nothing  -> readInput w
    -- Alt keys
    Just (EventCharacter '\ESC') -> do
      ev2 <- readInput w 
      case ev2 of
        EventCharacter 'n' -> return $ EventSpecialKey KeyDownArrow
        EventCharacter 'p' -> return $ EventSpecialKey KeyUpArrow
        _ -> readInput w

    Just ev' -> return ev'

processEvent :: SystemState -> Event -> Terminal

-- Delete
processEvent ss (EventSpecialKey KeyBackspace) = case ss of
  (SystemState _ (r:rs) _ _) -> Updated $ ss { current = r, history = rs, cursorPos = 0 }
  _ -> Updated ss

-- Down Arrow
processEvent ss (EventSpecialKey KeyDownArrow) = Updated $ newSS
  where newSS = ss { cursorPos = (cursorPos ss) + 1 } 

-- Up Arrow
processEvent ss (EventSpecialKey KeyUpArrow) = Updated $ newSS
  where newSS = ss { cursorPos = max 0 ((cursorPos ss) - 1) } 

-- Enter
processEvent (SystemState r _ cp _) (EventCharacter '\n') = res
  where res = case (orderedItems r) of
          []    -> Exit
          items -> Selected . snd $ items !! cp

-- Ctrl D
processEvent _  (EventCharacter '\EOT') = Exit

-- Add Char
processEvent ss@(SystemState r rs _ _) (EventCharacter c) = Updated newSS
  where newR = refine r . addChar . query $ r
        newSS = ss { current = newR, history = r:rs, cursorPos = 0 }
        addChar (Query qry ql) = Query (qry ++ [c]) (ql + 1)

processEvent ss _ = Updated ss

printQuery :: Query -> AttrWrite
printQuery qry = writeAtLine 0 $ "$ " ++ (q qry)

boldWrite :: AttrWrite -> AttrWrite
boldWrite = addAttr AttributeBold

addAttr :: Attribute -> AttrWrite -> AttrWrite
addAttr attr aw@(AttrWrite _ attrset _)
  | attr `elem` attrset = aw
  | otherwise = aw { attrs = (attr:attrset) }

orderedItems :: ResultSet -> ScoredList
orderedItems = merge fst . itemSet

printTopItems :: ResultSet -> [AttrWrite]
printTopItems = zipWith writeAtLine [1..] . items 
  where items = fmap B.unpack . fmap snd . orderedItems

printStatus :: Int -> ResultSet -> AttrWrite
printStatus total = iSimple RJustify Bottom . status . count
  where count = show . sum . fmap length . itemSet 
        status c = "[" ++ c ++ "/" ++ (show total) ++ "]"

writeAtLine :: Int -> String -> AttrWrite
writeAtLine r = iSimple LJustify (Line r)

-- Refine a previous search result with query
refine :: ResultSet -> Query -> ResultSet
refine rs = querySet ss rl
  where rl = (fmap (fmap snd)) . itemSet $ rs
        ss = strat rs

querySet :: ScoreStrat -> [ResultList] -> Query -> ResultSet
querySet ss rl qry = ResultSet qry ss newSet
  where scorer  = buildScorer ss qry
        newSet = scoreRL scorer rl

-- Read lines from stdin
readLines :: IO [B.ByteString] 
readLines = do
  inp <- B.getContents
  reOpenStdin
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
  args  <- getArgs
  flags <- fmap fst . compilerOpts $ args
  return $ if CaseSensitive `elem` flags then InfixLength else CIInfixLength

-- Builds score function
buildScorer :: ScoreStrat -> Query -> Scorer
buildScorer ss qs = score $ liftSS ss (q qs)

highlight :: ResultSet -> AttrWrite -> [AttrWrite]
highlight (ResultSet qry ss _) at = do
  let scorer = liftSS ss (q qry)
  let res    = range scorer (B.pack . content $ write at)
  maybe [at] (splitWrite at) res

splitWrite :: AttrWrite -> (Int, Int) -> [AttrWrite]
splitWrite at (lIdx, rIdx) = [lift left, newCenter, lift right]
  where w = write at
        (remaining, right) = split rIdx w
        (left, center) = split lIdx remaining 
        lift w2        = at { write = w2 } 
        newCenter      = at { write = center, highlighted = True }


-- Score line accordingly
scoreRL :: Scorer -> [ResultList] -> [ScoredList]
scoreRL f rl   = parMap rdeepseq cms rl
  where fo x = fmap (\i -> (i, x)) $ f x
        cms  = sort . catMaybes . fmap fo

