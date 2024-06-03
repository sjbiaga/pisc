module Out_gUgVwYdD8r where

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import Data.Aeson.Encode.Pretty


data JsonO_gUgVwYdD8r where
    JsonO_gUgVwYdD8r :: {
                          dummy_gUgVwYdD8r :: String
                          --
                        } -> JsonO_gUgVwYdD8r
    deriving (Generic)

write_gUgVwYdD8r :: JsonO_gUgVwYdD8r -> FilePath -> IO ()
write_gUgVwYdD8r j fp = do
    let out = C.unpack $ encodePretty j
    writeFile fp out

instance ToJSON JsonO_gUgVwYdD8r
