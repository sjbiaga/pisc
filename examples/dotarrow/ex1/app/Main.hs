module Main where

import System.Exit (exitFailure)     --  dotarrow
import System.IO (hPutStrLn, stderr) --  dotarrow
import Inp_gUgVwYdD8r                --  dotarrow
import Out_gUgVwYdD8r                --  dotarrow

main :: IO ()
main = do
    x :: Int <- pure ( 2 + 5 )
    y :: Int <- pure ( x * 4 )
    z :: Double <- pure ( 1.0 * fromIntegral x * fromIntegral y )
    print z
    return ()
