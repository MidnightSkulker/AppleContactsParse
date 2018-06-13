{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

import System.IO (Handle, IOMode(..), stdin, stdout)
import Options.Applicative
import Data.Semigroup ((<>))
import Files (safeOpenFile)

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
isVCF :: Arg -> Bool
isVCF (VCF _s) = True
isVCF _ = False
isPositional :: Arg -> Bool
isPositional (Positional _s) = True
isPositional _ = False
isJSON :: Arg -> Bool
isJSON (JSON _s) = True
isJSON _ = False

-- Parse the command line options
commandLineOptions :: ParserInfo Args
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

data ArgError = IOArgError IOError | ArgError String deriving (Show)
data ArgErrors = ArgErrors { errors :: [String] } deriving (Show)

-- Analyze the parsed arguments
argAnalysis :: Args -> IO (Either ArgError Files)
-- No arguments: It is a filter command
argAnalysis (Args []) = return (Right filterCmd)
-- One argument, it is an explicit VCF parameter, or a positional paramter.
-- Output is assumed to be stdout.
argAnalysis (Args [a]) | isVCF a || isPositional a =
  do { eh <- safeOpenFile (argString a) ReadMode
     ; return (either (Left . IOArgError . id) (\h -> Right (Files {input = h, output = stdout})) eh)
     }
-- One argument, it is an explicit JSON parameter. Input is assumed to be stdout.
argAnalysis (Args [a]) | isJSON a =
  do eh <- safeOpenFile (argString a) WriteMode
     return (either (Left . IOArgError . id) (\h -> Right (Files {input = stdin, output = h})) eh)
-- Two arguments, the first is a VCF or positional (so assumed to be VCF),
-- the second is JSON or positional (so assumed to be JSON)
argAnalysis (Args [a,b]) | (isVCF a || isPositional a) && (isJSON b || isPositional b) =
  do { eah <- safeOpenFile (argString a) ReadMode
     ; case eah of
         Left e -> return (Left (IOArgError e))
         Right ah -> do { ebh <- safeOpenFile (argString b) WriteMode
                        ; case ebh of
                            Left e -> return (Left (IOArgError e))
                            Right bh -> return (Right (Files {input = ah, output = bh}))
                        }
     }
-- Two arguments, the first is a VCF or positional (so assumed to be VCF),
-- and the second is VCF. This is an error
argAnalysis (Args [a,b]) | (isVCF a || isPositional a) && isJSON b =
  return (Left (ArgError "Two VCF Files Specified"))
-- Two arguments, the first is JSON, and the second is JSON. This is an error
argAnalysis (Args [a,b]) | isJSON a && isJSON b =
  return (Left (ArgError "Two JSON Files Specified"))
-- Two arguments, the first is JSON and the second is VCF or Positional (so assumed to be VCF in this case)
argAnalysis (Args [a,b]) | isJSON a && (isVCF b || isPositional b) =
  do { eah <- safeOpenFile (argString a) WriteMode
     ; case eah of
         Left e -> return (Left (IOArgError e))
         Right ah -> do { ebh <- safeOpenFile (argString b) ReadMode
                        ; case ebh of
                            Left e -> return (Left (IOArgError e))
                            Right bh -> return (Right (Files {input = bh, output = ah}))
                        }
     }
-- Three or more arguments is an error
argAnalysis (Args _as) =
  return (Left (ArgError "More that two parameters specified"))
