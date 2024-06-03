{-# LANGUAGE TemplateHaskell #-}
module Expr where

import Data.ByteString.Lazy.Char8 (unpack)
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import System.Process.Typed

amm :: Quasi q => String -> q String
amm exp =
       do
          (out, _) <- readProcess_ (proc "amm" ["-c", i ++ i2 ++ pr])
          return (unpack out)
    where i = "import $ivy.`org.scalameta:scalameta_2.13:4.9.5`;"
          i2 = "import scala.meta._; import dialects.Scala3;"
          pr = "print(\"\"\"" ++ exp ++ "\"\"\".parse[Stat].get.structure)"

s2h :: Quasi q => String -> q String
s2h exp =
       do
          (out, _) <- readProcess_ (proc "bin/Scalameta2Haskell.pl" [exp])
          return (unpack out)

exp :: Quasi q => q Exp
exp = runQ [| \(it::String) ->
                  do
                    sc <- amm it
                    hs <- s2h sc
                    let Right it' = parseExp hs
                    return it'
           |]
