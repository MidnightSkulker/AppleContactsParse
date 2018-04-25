module Main where

import Options.Applicative
import Text.ParserCombinators.Parsec
import System.IO
import System.Exit
import Parse
import Test
import Args
import Files

main :: IO ()
main = do { parsedOptions <- execParser commandLineOptions
          ; putStrLn $ "Parsed Options: " ++ show parsedOptions
          ; analyzedOptions <- argAnalysis parsedOptions
          ; putStrLn $ "Analyzed Options: " ++ show analyzedOptions
          ; case analyzedOptions of
              Left argError -> putStrLn (show argError)
              Right files -> do { vcfInput <- hGetContents (input files)
                                ; putStrLn (show (input files) ++ " is open")
                                ; let json = jsonTest vcf vcfInput
                                ; hPutStrLn (output files) json
                                ; putStrLn (show (output files) ++ " is written")
                                ; hClose (input files)
                                ; hClose (output files)
                                }
          }

m = main
