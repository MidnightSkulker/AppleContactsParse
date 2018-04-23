{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

import Options.Applicative
import Data.Semigroup ((<>))

-- Parsed Arguments as a list.
data ListArgs = ListArgs { args :: [String] } deriving (Show)

commandLineOptions = info (parseArgList <**> helper)
      ( fullDesc <> progDesc "Convert Mac Address Book to JSON" <> header "Gronk" )

-- The VCF file as an explicit parameter
vcfExplicit :: Parser String
vcfExplicit = strOption ( long "vcf" <> short 'v' <> metavar "STRING" <> help "input .vcf" )
-- The VCF file as an positional parameter
vcfPositional :: Parser String
vcfPositional = argument str (metavar "FILE")
-- The JSON file as an explicit parameter
jsonExplicit :: Parser String
jsonExplicit = strOption ( long "json" <> short 'j' <> metavar "STRING" <> help "output .json" )
-- The JSON file as an positional parameter
jsonPositional :: Parser String
jsonPositional = argument str (metavar "FILE")

-- Any of the paremeter options
anyArg :: Parser String
anyArg = vcfExplicit <|> jsonExplicit <|> vcfPositional <|> jsonPositional

-- Return a list of parsed parameters
parseArgList :: Parser ListArgs
parseArgList = ListArgs <$> many anyArg
