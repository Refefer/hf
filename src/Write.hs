module Write (
  Justify(..)
  , Row(..)
  , ExactWrite(..)
  , Write
  , simple
  , split
  , content
  , constrain
) where

data Justify = LJustify 
             | RJustify 
             | Column Int 
             | LeftRelative Write 
             | RightRelative Write
             deriving (Show, Eq)

data Row = Line Int 
         | Bottom 
         | Top 
         deriving (Show, Eq)

data Write = RelWrite { justify  :: Justify 
                      , row      :: Row
                      , contents :: String 
                      }
           | EWrite ExactWrite 
           deriving (Show, Eq)

data ExactWrite = ExactWrite (Int, Int) String deriving (Show, Eq)

-- Simple Write
simple :: Justify -> Row -> String -> Write
simple j r s = RelWrite j r s

-- get string contents
content :: Write -> String
content (RelWrite _ _ s) = s
content (EWrite(ExactWrite _ s)) = s

-- Given dimensions, constrains writes to within that range
constrain :: (Int, Int) -> Write -> Maybe ExactWrite

constrain coords@(r,_) w@(RelWrite _ Bottom _) = constrain coords nw 
  where nw = w { row = Line (r - 1) }

constrain coords w@(RelWrite (LeftRelative ow) _ _) = do
  (ExactWrite (_,col) s) <- constrain coords ow
  constrain coords w { justify = Column (col + (length s)) }

constrain coords w@(RelWrite (RightRelative ow) _ s) = do
  (ExactWrite (_,col) _) <- constrain coords ow
  constrain coords w { justify = Column (col - (length s)) }

constrain coords w@(RelWrite _ Top _) = constrain coords nw 
  where nw = w { row = Line 0 }

constrain coords w@(RelWrite LJustify _ _) = constrain coords nw
  where nw = w { justify = Column 0 }

constrain coords@(_,c) w@(RelWrite RJustify _ s) = constrain coords nw
  where col = fromIntegral . max 0 $ c - (length s) - 1
        nw  = w { justify = Column col }

constrain coords (RelWrite (Column off) (Line line) s) = constrain coords ew
  where ew = EWrite $ ExactWrite (line, off) s

constrain (r,c) (EWrite (ExactWrite (line, off) s))
  | s == ""        = Nothing
  | line >= r      = Nothing
  | off >= (c + 1) = Nothing
  | otherwise      = Just $ ExactWrite (line, off) (take (c - off - 1) s)

-- Splits a Write at the given index
split :: Int -> Write -> (Write, Write)
split idx rw@(RelWrite RJustify _ s) = splitRight rw $splitAt idx s
split idx rw@(RelWrite (RightRelative _) _ s) = splitRight rw $ splitAt idx s
split idx rw@(RelWrite _ _ s) = splitLeft rw $ splitAt idx s
split idx (EWrite (ExactWrite c s)) = (leftEW, rightEW)
  where (leftS, rightS) = splitAt idx s
        leftEW  = EWrite (ExactWrite c leftS)
        rightEW = RelWrite (LeftRelative leftEW) (Line (fst c)) rightS

splitLeft :: Write -> (String, String) -> (Write, Write)
splitLeft w (leftS, rightS) = (leftEW, rightEW)
  where leftEW  = w { contents = leftS }
        rightEW = w { justify = LeftRelative leftEW, contents = rightS }

splitRight :: Write -> (String, String) -> (Write, Write)
splitRight w (leftS, rightS) = (leftEW, rightEW)
  where rightEW = w { contents = rightS }
        leftEW  = w { justify = RightRelative rightEW, contents = leftS }

