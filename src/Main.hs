module Main where

import Text.ParserCombinators.Parsec
import Parse
import Test
import Args

main :: IO ()
main = do
  arubala <- readFile "test/Arubala.test"
  let json = jsonTest vcf arubala
  writeFile "Arubala.json" json

m = main
