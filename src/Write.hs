module Write (
  Justify(..)
  , Row(..)
  , Write(..)
  , ExactWrite(..)
  , simple
  , constrain
) where

import UI.NCurses (Attribute)

data Justify = LJustify | RJustify | Column Int
data Row = Line Int | Bottom
data Write = Write { justify :: Justify 
                   , row :: Row
                   , contents :: String 
                   , attributes :: [Attribute]
                   }

data ExactWrite = ExactWrite (Int, Int) String [Attribute]

-- Simple Write
simple :: Justify -> Row -> String -> Write
simple j r s = Write j r s []

-- Given dimensions, constrains writes to within that range
constrain :: (Int, Int) -> Write -> Maybe ExactWrite

constrain coords@(r,_) w@(Write _ Bottom _ _) = constrain coords nw 
  where nw = w { row = Line (r - 1) }

constrain coords w@(Write LJustify _ _ _) = constrain coords nw
  where nw = w { justify = Column 0 }

constrain coords@(_,c) w@(Write RJustify _ s _) = constrain coords nw
  where col = fromIntegral . max 0 $ c - (length s) - 1
        nw  = w { justify = Column col }

constrain (r,c) (Write (Column off) (Line line) s atts)
  | s == ""        = Nothing
  | line >= r      = Nothing
  | off >= (c + 1) = Nothing
  | otherwise      = Just $ ExactWrite (line, off) (take (c - off - 1) s) atts

