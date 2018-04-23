{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

import Options.Applicative
import Data.Semigroup ((<>))

-- Parsed Arguments as a list.
data ListArgs = ListArgs { args :: [String] } deriving (Show)
-- Parsed arguments: The input (vcf) file and the output (json) file.
data ParsedArgs = ParsedArgs { input :: String, output :: String }
                | FilterArgs deriving (Show)

commandLineOptions = info (parseArgList <**> helper)
      ( fullDesc <> progDesc "Convert Mac Address Book to JSON" <> header "Gronk" )

-- For Arguments that are explicitly specified
explicitArgs :: Parser ParsedArgs
explicitArgs =
  -- You can use "vcf" and "json" parameters to be more explicit,
  ParsedArgs <$> vcfExplicit <*> jsonExplicit
  -- Or you can use positional notation, with the .vcf first and .json second.
  <|> (ParsedArgs <$> vcfPositional <*> jsonPositional)
  <|> (ParsedArgs <$> vcfExplicit <*> jsonPositional)
  <|> (ParsedArgs <$> vcfPositional <*> jsonExplicit)

-- For Arguments that are left out: The command line is just the name of the program
-- redirects for stdin and stdout
filterArgs :: Parser ParsedArgs
filterArgs = flag' FilterArgs ( long "filter" <> help "Read from stdin" )

-- The complete command line parser
parseArgs :: Parser ParsedArgs
parseArgs = explicitArgs <|> filterArgs

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
