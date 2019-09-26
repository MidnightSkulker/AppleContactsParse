module Main where

import Options.Applicative
import Data.Aeson as Aeson (encode)
import Data.ByteString.Lazy.Char8 as DBLC8 (unpack)
import System.IO (hClose, hPutStrLn, hGetContents)
import Parse (cards, vcf)
import Test (test)
import Args (Files(..), commandArgAnalysis, commandLineOptions, Command(..))

main :: IO ()
main = do { parsedOptions <- execParser commandLineOptions
          -- ; putStrLn $ "Parsed Options: " ++ show parsedOptions
          ; analyzedOptions <- commandArgAnalysis parsedOptions
          -- ; putStrLn $ "Analyzed Options: " ++ show analyzedOptions
          ; case analyzedOptions of
              Left argError -> putStrLn (show argError)
              Right cmd ->
                do { let fs = files cmd
                   ; vcfInput <- hGetContents (input fs)
                   -- ; putStrLn $ "Field Names: " ++ show (fieldNames cmd)
                   ; let ejson = test (vcf (fieldNames cmd)) vcfInput
                   ; case ejson of
                       Left e -> putStrLn ("Error: " ++ show e)
                       Right json ->
                         do { hPutStrLn (output fs) (DBLC8.unpack (encode json))
                            ; putStrLn ("# of cards " ++ show (length (cards json)))
                            }
                   ; hClose (input fs)
                   ; hClose (output fs)
                   }
          }

m :: IO ()
m = main
