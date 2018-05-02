module Main where

import Options.Applicative
import Text.ParserCombinators.Parsec
import System.IO
import Data.Either
import System.Exit
import Parse
import Test
import Args
import Files

main :: IO ()
main = do { parsedOptions <- execParser commandLineOptions
          ; analyzedOptions <- argAnalysis parsedOptions
          ; case analyzedOptions of
              Left argError -> putStrLn (show argError)
              Right files -> do { vcfInput <- hGetContents (input files)
                                ; putStrLn (show (input files) ++ " is open")
                                ; let json = jsonTest vcf vcfInput
                                ; let vcf2 = test vcf vcfInput
                                ; case vcf2 of
                                    Left e -> putStrLn ("Error: " ++ show e)
                                    Right v -> putStrLn ("Number of cards " ++ show (length (cards v)))
                                ; hPutStrLn (output files) json
                                ; hClose (input files)
                                ; hClose (output files)
                                }
          }

m = main
