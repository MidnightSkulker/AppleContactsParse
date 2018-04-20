{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

-- import System.Console.ParseArgs
import Options.Applicative
import Data.Semigroup ((<>))

-- Parsed arguments: The input (vcf) file and the output (json) file.
data ParsedArgs = ParsedArgs { input :: String, output :: String } deriving (Show)

commandLineOptions = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Convert Mac Address Book to JSON"
     <> header "Gronk" )

-- parseArgs :: Parser ()
-- parseArgs = () <$
--   switch (long "vcf") <*
--   switch (long "json")

parseArgs :: Parser ParsedArgs
parseArgs = ParsedArgs
      <$> strOption
          ( long "vcf"
         <> short 'v'
         <> metavar "STRING"
         <> help "input (vcf) file" )
      <*> strOption
          ( long "json"
         <> short 'j'
         <> metavar "STRING"
         <> help "output (json) file" )
