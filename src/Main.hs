module Main where

import Options.Applicative
import Text.ParserCombinators.Parsec
import Parse
import Test
import Args
import Files

main :: IO ()
main = do
  parsedOptions <- execParser commandLineOptions
  putStrLn $ "Parsed Options: " ++ show parsedOptions
  analyzedOptions <- argAnalysis parsedOptions
  putStrLn $ "Analyzed Options: " ++ show analyzedOptions
--  arubala <- readFile "test/Arubala.test"
--  let json = jsonTest vcf arubala
--  writeFile "Arubala.json" json

m = main
