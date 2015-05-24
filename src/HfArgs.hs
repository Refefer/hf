module HfArgs (
  compilerOpts,
  Flag(..)
  ) where

import System.Console.GetOpt

data Flag = CaseSensitive deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['c'] ["case-sensitive"] (NoArg CaseSensitive) "Case-sensitive searching"
 ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hf [OPTION...]"

