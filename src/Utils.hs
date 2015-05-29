module Utils (
  toLower
  , chunkV
  , merge
) where

import qualified Data.Char as C
import qualified Data.Vector as V
import Prelude hiding (map)
import Data.ByteString.Char8 (map, ByteString)

-- Faster toLower
toLower :: ByteString -> ByteString
toLower = map lower
  where lower c
          | C.isAsciiUpper c = C.toLower c
          | otherwise        = c

-- Chunks items into groups
chunkV :: Int -> V.Vector a -> [V.Vector a]
chunkV amt v 
  | V.null v  = []
  | otherwise = c1:(chunkV amt rest)
  where (c1, rest) = V.splitAt amt v

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
