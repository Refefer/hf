module HfArgs (
  compilerOpts,
  Flag(..),
  ) where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag = CaseSensitive 
          | SFormat String 
          deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['c'] ["case-sensitive"] (NoArg CaseSensitive) "Case-sensitive searching",
  Option ['f'] ["Format the output"] (OptArg strFormat "Output Format") "Format the output"
 ]

strFormat :: Maybe String -> Flag
strFormat f = SFormat $ fromMaybe "{0}" f

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: hf [OPTION...]"

