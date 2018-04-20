{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

-- import System.Console.ParseArgs
import Options.Applicative
import Data.Semigroup ((<>))

commandLineOptions = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Convert Mac Address Book to JSON"
     <> header "Gronk" )

parseArgs :: Parser ()
parseArgs = () <$
  switch (long "vcf") <*
  switch (long "json")
