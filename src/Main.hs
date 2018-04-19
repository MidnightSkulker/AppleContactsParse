module Main where

import Text.ParserCombinators.Parsec
import Parse

main :: IO ()
main = do
  arubala <- readFile "test/Arubala.test"
  parseTest card arubala

m = main
