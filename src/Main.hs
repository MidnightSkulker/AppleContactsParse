module Main where

import Options.Applicative
import Text.ParserCombinators.Parsec
import Parse
import Test
import Args

main :: IO ()
main = do
  parsedOptions <- execParser commandLineOptions
  putStrLn $ "Parsed Options: " ++ show parsedOptions
--  arubala <- readFile "test/Arubala.test"
--  let json = jsonTest vcf arubala
--  writeFile "Arubala.json" json

m = main
