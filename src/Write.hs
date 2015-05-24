module Write (
  Justify(..)
  , Row(..)
  , Write(..)
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

-- Simple Write
simple :: Justify -> Row -> String -> Write
simple j r s = Write j r s []

-- Given a dimension, constrains writes to within that range
constrain :: (Int, Int) -> Write -> Maybe Write

constrain coords@(r,_) w@(Write _ Bottom _ _) = constrain coords nw 
  where nw = w { row = Line (r - 1) }

constrain coords w@(Write LJustify _ _ _) = constrain coords nw
  where nw = w { justify = Column 0 }

constrain coords@(_,c) w@(Write RJustify _ s _) = constrain coords nw
  where col = fromIntegral . max 0 $ c - (length s) - 1
        nw  = w { justify = Column col }

constrain (r,c) w@(Write (Column off) (Line line) s _)
  | s == ""        = Nothing
  | line >= r      = Nothing
  | off >= (c + 1) = Nothing
  | otherwise      = Just $ w { contents = take (c - off - 1) s }

