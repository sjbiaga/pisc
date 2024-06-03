module Inp_gUgVwYdD8r where

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson


data JsonI_gUgVwYdD8r where
    JsonI_gUgVwYdD8r :: {
                          dummy_gUgVwYdD8r :: Maybe String
                        } -> JsonI_gUgVwYdD8r
    deriving (Generic)

read_gUgVwYdD8r :: FilePath -> IO JsonI_gUgVwYdD8r
read_gUgVwYdD8r fp = do
    str <- readFile fp
    return $ case decode $ C.pack str of Just j -> j

instance FromJSON JsonI_gUgVwYdD8r
