module ResultSet (
    build 
  , ResultSet(..)
  , Results
) where

import Control.Parallel.Strategies
import Control.Monad
import Control.Monad.ST
import Data.Maybe (isJust)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector.Algorithms.Intro (sort)
import Scorer
import Utils (merge, chunkV)

type ScoredList = V.Vector (Double, B.ByteString)

data Results = ParVec [ScoredList] 
               deriving (Show, Eq)

class ResultSet a where
  size   :: a -> Int
  items  :: a -> [B.ByteString]
  refine :: ScoreStrategy s => a -> s -> a

instance ResultSet Results where
  size  (ParVec sl) = sum . fmap V.length $ sl
  items (ParVec sl) = fmap snd . merge fst . fmap V.toList $ sl

  refine (ParVec sl) sc = ParVec newSet
    where rl     = fmap (fmap snd) sl
          newSet = scoreRL (score sc) rl

-- Create
build :: [B.ByteString] -> Results
build lst = ParVec chunks
  where initVec   = V.fromList $ zip [1..] lst
        len       = length lst
        chunkSize = fst . divMod len $ 5000
        chunks    = chunkV (chunkSize + 1) $ initVec
  
-- Score line accordingly
scoreRL :: Scorer -> [V.Vector B.ByteString] -> [ScoredList]
scoreRL f rl = parMap rdeepseq cms rl
  where fo x = fmap (\i -> (i, x)) $ f x
        cms x = runST $ do
              let remaining = V.filter isJust . fmap fo $ x
              let vsize = V.length remaining
              -- Copy the array to a mutable one
              mv <- MV.new vsize
              forM_ [0..(vsize - 1)] $ \idx -> do
                  case (V.!) remaining idx of
                    Just el -> MV.write mv idx el
                    _       -> return ()
                  
              -- Sort
              sort mv
              V.unsafeFreeze mv

