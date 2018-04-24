{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

import System.IO
import Options.Applicative
import Data.Semigroup ((<>))

-- Parsed Arguments as a list.
data Args = Args { args :: [Arg] } deriving (Show)
-- Number of parsed args
nArgs :: Args -> Int
nArgs Args { args = as } = length as

data Arg = VCF String | JSON String | Positional String deriving (Show)
argString :: Arg -> String
argString (VCF s) = s
argString (JSON s) = s
argString (Positional s) = s

-- Parse the command line options
commandLineOptions = info (parseArgList <**> helper)
      ( fullDesc <> progDesc "Convert Mac Address Book to JSON" <> header "Gronk" )

-- The VCF file as an explicit parameter
vcfExplicit :: Parser Arg
vcfExplicit =
  VCF <$> strOption ( long "vcf" <> short 'v' <> metavar "STRING" <> help "input .vcf" )
-- A positional parameter
positional :: Parser Arg
positional = Positional <$> argument str (metavar "FILE")
-- The JSON file as an explicit parameter
jsonExplicit :: Parser Arg
jsonExplicit =
  JSON <$> strOption( long "json" <> short 'j' <> metavar "STRING" <> help "output .json" )

-- Any of the paremeter options
anyArg :: Parser Arg
anyArg = vcfExplicit <|> jsonExplicit <|> positional

-- Return a list of parsed parameters
parseArgList :: Parser Args
parseArgList = Args <$> many anyArg

data Files = Files { input :: Handle, output :: Handle } deriving (Show)
-- A filter command has stdin as input and stdio as output
filterCmd :: Files
filterCmd = Files { input = stdin, output = stdout }

data ArgErrors = ArgErrors { errors :: [String] } deriving (Show)

-- Analyze the parsed arguments
argAnalysis :: Args -> IO (Either ArgErrors Files)
-- No arguments: It is a filter command
argAnalysis largs | nArgs largs == 0 = return (Right filterCmd)
argAnalysis largs | nArgs largs == 1 = undefined
