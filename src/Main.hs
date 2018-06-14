module Main where

import Options.Applicative
import System.IO (hClose, hPutStrLn, hGetContents)
import Parse (cards, vcf)
import Test (test, jsonTest)
import Args (Files(..), commandArgAnalysis, commandLineOptions, Command(..))

main :: IO ()
main = do { putStrLn ("Ready to parse command line options")
          ; parsedOptions <- execParser commandLineOptions
          ; analyzedOptions <- commandArgAnalysis parsedOptions
          ; case analyzedOptions of
              Left argError -> putStrLn (show argError)
              Right cmd ->
                do { let fs = files cmd
                   ; vcfInput <- hGetContents (input fs)
                   ; let json = jsonTest vcf vcfInput
                   ; let vcf2 = test vcf vcfInput
                   ; hPutStrLn (output fs) json
                   ; case vcf2 of
                       Left e -> putStrLn ("Error: " ++ show e)
                       Right v -> putStrLn ("# of cards " ++ show (length (cards v)))
                   ; hClose (input fs)
                   ; hClose (output fs)
                   }
          }

m :: IO ()
m = main
