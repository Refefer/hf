{-# Language OverloadedStrings #-}
module Main where

--import Debug.Trace (trace)
import Data.Bits ((.&.))
import Data.Maybe (catMaybes, fromMaybe)
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import qualified Data.ByteString.Char8 as B 
import Prelude hiding (sequence)
import System.Environment (getArgs, getEnvironment)
import System.IO (stdin, stdout, stderr, hSetBuffering, openFile,
                  IOMode( ReadMode ), BufferMode ( NoBuffering ) )
import System.Posix (executeFile, getFdStatus, fileMode)
import System.Process (waitForProcess, createProcess, CreateProcess(std_out), shell, StdStream( CreatePipe ))
import UI.NCurses

import Scorer
import HfArgs (compilerOpts, Flag(..))
import SimpleFormatter
import Write
import ResultSet 

data Query = Query { q :: String
                   } 
                   deriving (Show, Eq)

data QueriedSet = QueriedSet { query   :: Query
                             , strat   :: ScoreStrat
                             , results :: Results
                             }
                             deriving (Show, Eq)

data SystemState = SystemState { current   :: QueriedSet
                               , history   :: [QueriedSet]
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

type UIFunc = SystemState -> Curses (Maybe B.ByteString)

iSimple :: Justify -> Row -> String -> AttrWrite
iSimple j r s =  AttrWrite (simple j r s) [] False

main :: IO ()
main = do
  flags <- getArgs >>= fmap fst . compilerOpts
  ss  <- getStrat flags
  res <- readLines
  bs  <- initUI $ SystemState (QueriedSet (Query "") ss res) [] 0 (size res)
  env <- getEnvironment
  let retval = fmap (formatOutput env flags . B.unpack) bs
  case retval of
    Nothing -> return ()
    Just s -> if ExecVP `elem` flags
              then simpleExec env s
              else putStrLn s

-- Execvp into the editor
simpleExec :: [(String, String)] -> String -> IO ()
simpleExec fm s = case words s of
  [x]    -> executeFile (fromMaybe "vim" $ fauxLookup "EDITOR" fm) True [x] (Just fm)
  (x:xs) -> executeFile x True xs (Just fm)
  -- Hmm, should never get here.  Need to encode that
  []     -> return ()

-- Format according to the format string
formatOutput :: [(String, String)] -> [Flag] -> String -> String
formatOutput _ [] o = o
formatOutput env ((SFormat sf):_) o = format sf (o:pieces) env
  where pieces = words o
formatOutput env (_:xs) o = formatOutput env xs o

readLines :: IO Results
readLines = do
  piped <- isPiped
  bLines <- if piped
            then readLinesStdin 
            else runFind "."
  return $ build bLines

runFind :: FilePath -> IO [B.ByteString]
runFind dir = do
  let cmd = concat ["find ", dir, " -type f"]
  -- I can't seem to get proc to correctly build the args, so using shell
  (_, Just hout, _, ph) <- createProcess (shell cmd) { std_out = CreatePipe}
  input <- fmap B.lines . B.hGetContents $ hout
  _ <- waitForProcess ph
  return input

-- Check if our stdin is piped.  conversion song and dance
isPiped :: IO Bool
isPiped = do
  -- Normally, I'd like to call something like handleToFd on stdin, but
  -- that has the side effect of closing the handle
  fs <- getFdStatus 0
  -- Have to mask the filemode
  let fm = (fileMode fs) .&. 61440
  -- 4096, on linux at least, indicates the input is fifo
  let piped = fm .&. 4096
  return $ 0 /= piped

-- Read lines from stdin
readLinesStdin :: IO [B.ByteString]
readLinesStdin = do
  inp <- B.getContents
  reOpenStdin
  return . B.lines $ inp

-- Have to reopen stdin since getContents closes it
reOpenStdin :: IO () 
reOpenStdin = do 
  tty <- openFile "/dev/tty" ReadMode
  hSetBuffering tty NoBuffering
  hDuplicateTo tty stdin

-- Run the Curses UI
initUI :: SystemState -> IO (Maybe B.ByteString)
initUI rs = do
  redirect . runCurses $ do
    w   <- defaultWindow 
    cid <- newColorID ColorGreen ColorDefault 1
    ui w cid rs

-- Redirects the stdout to stderr
redirect :: IO a -> IO a
redirect io = do
  oldStdout <- hDuplicate stdout
  hDuplicateTo stderr stdout
  res       <- io
  hDuplicateTo oldStdout stdout
  return res

ui :: Window -> ColorID -> UIFunc
ui w cid ss@(SystemState r _ cp rc) = do
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
    renderWith w $ applyWrites cid c2 [iSimple LJustify Bottom "Searching..."]
    updateState ss (length top_items) event (ui w cid)
  where renderWith win up = updateWindow win up >> render
  
-- Handles updating the system state
updateState :: SystemState -> Int -> Event -> UIFunc -> Curses (Maybe B.ByteString)
updateState ss itemCount event f = case processEvent ss event of
    Exit          -> return Nothing
    Selected bs   -> return $ Just bs
    Updated newSs -> do
      let newCP = min (itemCount - 1) (cursorPos newSs)
      let safeSs = newSs {cursorPos = newCP}
      f safeSs

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
  return (fromIntegral r, fromIntegral c)

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
processEvent (SystemState qs _ cp _) (EventCharacter '\n') = res
  where res = case (items . results) qs of 
          []    -> Exit
          itemSet -> Selected $ itemSet !! cp

-- Ctrl D
processEvent _  (EventCharacter '\EOT') = Exit

-- Add Char
processEvent ss@(SystemState r rs _ _) (EventCharacter c) = Updated newSS
  where newQ   = addChar . query $ r
        sStrat = compileSS (strat r)
        newR   = refine (results r) . sStrat $ newQ
        newQS  = r { query = newQ, results = newR }
        newSS  = ss { current = newQS, history = r:rs, cursorPos = 0 }
        addChar (Query qry) = Query (qry ++ [c])

processEvent ss _ = Updated ss

printQuery :: Query -> AttrWrite
printQuery qry = writeAtLine 0 $ "$ " ++ (fmap f . q $ qry)
  where f '\t' = '~'
        f c    = c

boldWrite :: AttrWrite -> AttrWrite
boldWrite = addAttr AttributeBold

addAttr :: Attribute -> AttrWrite -> AttrWrite
addAttr attr aw@(AttrWrite _ attrset _)
  | attr `elem` attrset = aw
  | otherwise = aw { attrs = (attr:attrset) }

printTopItems :: QueriedSet -> [AttrWrite]
printTopItems = zipWith writeAtLine [1..] . topItems
  where topItems = fmap B.unpack . items . results

printStatus :: Int -> QueriedSet -> AttrWrite
printStatus total = iSimple RJustify Bottom . status . count
  where count = show . size . results
        status c = "[" ++ c ++ "/" ++ (show total) ++ "]"

writeAtLine :: Int -> String -> AttrWrite
writeAtLine r = iSimple LJustify (Line r)

-- Get query as first argument
getStrat :: [Flag] -> IO ScoreStrat
getStrat flags = return $ if CaseSensitive `elem` flags
                          then ss
                          else CILength ss
  where ss = getSearchStrat flags

getSearchStrat :: [Flag] -> ScoreStrat
getSearchStrat flags
  | SlopSearch `elem` flags = SlopLength
  | otherwise = InfixLength

compileSS :: ScoreStrat -> Query -> [CQuery]
compileSS ss = fmap (liftSS ss) . splitQ
  where splitQ = fmap B.unpack . pieces
        pieces = B.split '\t' . B.pack . q 

highlight :: QueriedSet -> AttrWrite -> [AttrWrite]
highlight (QueriedSet qry ss _) at = do
  let scorer = compileSS ss qry
  let res    = range scorer . B.pack . content $ write at
  maybe [at] (splitWrites at) res

splitWrites :: AttrWrite -> [(Int, Int)] -> [AttrWrite]
splitWrites atw [] = [atw]
splitWrites atw locs = foldr loop [atw] locs
  where loop loc (at:rest) = (splitWrite at loc) ++ rest
        loop _ ats = ats

splitWrite :: AttrWrite -> (Int, Int) -> [AttrWrite]
splitWrite at (lIdx, rIdx) = [lift left, newCenter, lift right]
  where w = write at
        (remaining, right) = split rIdx w
        (left, center) = split lIdx remaining 
        lift w2        = at { write = w2 } 
        newCenter      = at { write = center, highlighted = True }
