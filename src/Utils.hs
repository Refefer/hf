module Utils (
  toLower
  , chunk
  , merge

) where

import qualified Data.Char as C
import Prelude hiding (map)
import Data.ByteString.Char8 (map, ByteString)

-- Faster toLower
toLower :: ByteString -> ByteString
toLower = map lower
  where lower c
          | C.isAsciiUpper c = C.toLower c
          | otherwise        = c

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
