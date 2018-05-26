module Main where

import Options.Applicative
import System.IO
import Parse
import Test
import Args

main :: IO ()
main = do { parsedOptions <- execParser commandLineOptions
          ; analyzedOptions <- argAnalysis parsedOptions
          ; case analyzedOptions of
              Left argError -> putStrLn (show argError)
              Right files -> do { vcfInput <- hGetContents (input files)
                                ; let json = jsonTest vcf vcfInput
                                ; let vcf2 = test vcf vcfInput
                                ; hPutStrLn (output files) json
                                ; case vcf2 of
                                    Left e -> putStrLn ("Error: " ++ show e)
                                    Right v -> putStrLn ("Number of cards " ++ show (length (cards v)))
                                ; hClose (input files)
                                ; hClose (output files)
                                }
          }

m :: IO ()
m = main
