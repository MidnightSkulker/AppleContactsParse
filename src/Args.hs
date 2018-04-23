{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

import Options.Applicative
import Data.Semigroup ((<>))

-- Parsed arguments: The input (vcf) file and the output (json) file.
data ParsedArgs = ParsedArgs { input :: String, output :: String }
                | FilterArgs deriving (Show)

commandLineOptions = info (parseArgs <**> helper)
      ( fullDesc <> progDesc "Convert Mac Address Book to JSON" <> header "Gronk" )

-- For Arguments that are explicitly specified
explicitArgs :: Parser ParsedArgs
explicitArgs =
  -- You can use "vcf" and "json" parameters to be more explicit,
  ParsedArgs <$>
        strOption ( long "vcf" <> short 'v' <> metavar "STRING" <> help "input .vcf" )
    <*> strOption ( long "json" <> short 'j' <> metavar "STRING" <> help "output .json" )
  -- Or you can use positional notation, with the .vcf first and .json second.
  <|> (ParsedArgs <$> argument str (metavar "FILE") <*> argument str (metavar "FILE"))

-- For Arguments that are left out: The command line is just the name of the program
-- redirects for stdin and stdout
filterArgs :: Parser ParsedArgs
filterArgs = flag' FilterArgs ( long "filter" <> help "Read from stdin" )

-- The complete command line parser
parseArgs :: Parser ParsedArgs
parseArgs = explicitArgs <|> filterArgs
