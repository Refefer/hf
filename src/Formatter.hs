module Formatter (
  splitWs
) where 

import qualified Data.ByteString.Char8 as BS (splitWith, null, pack, unpack)

splitWs :: String -> [String]
splitWs = map BS.unpack . filter (not . BS.null) . BS.splitWith isWS . BS.pack
  where isWS c = c `elem` " \t\n\r"

