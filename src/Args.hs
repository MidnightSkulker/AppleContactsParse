{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Args where

import System.IO (Handle, IOMode(..), stdin, stdout)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.List (partition)
import Files (safeOpenFile)

-- Flags that can be set
-- The fieldNames is for the list of field names affected by the flags
data Flags = Flags { noPhotos :: Bool } deriving (Show)
type FieldNames = [String]
-- Default is all the flags false
defaultFlags :: Flags
defaultFlags = Flags { noPhotos = False }
-- Set the appropriate flag according to the argument
flagSetter :: Flags -> Arg -> Flags
flagSetter f (NoPhoto _) = f { noPhotos = True }
flagSetter f _ = f
-- Analyze the flag arguments, setting a bit in the Flags structure for
-- each flag Argument
flagArgAnalysis :: [Arg] -> Flags -> Flags
flagArgAnalysis args f = foldl flagSetter f args
-- Compute field names from the flag arguments
flagField :: Arg -> String
flagField (NoPhoto _) = "PHOTO"
flagField (NoProdID _) = "PRODID"
flagField (NoABUID _) = "X-ABUID"
flagField (NoAdr _) = "ADR"
flagField (NoN _) = "N"
flagField _ = "None***"
flagFields :: [Arg] -> FieldNames
flagFields = map flagField
  
-- Parsed Arguments as a list.
data Args = Args { fileArgs :: [Arg], switchArgs :: [Arg] } deriving (Show)
-- Number of parsed args
nArgs :: Args -> Int
nArgs Args { fileArgs = fs, switchArgs = ss } = length fs + length ss

data Arg = VCF String | JSON String
         | Positional String | NoPhoto Bool | NoProdID Bool | NoABUID Bool
         | NoN Bool | NoAdr Bool
           deriving (Show)

-- Get the underlying string out of an argument
argString :: Arg -> String
argString (VCF s) = s
argString (JSON s) = s
argString (Positional s) = s
argString (NoPhoto _) = "No Photo"
argString (NoProdID _) = "No ProdID"
argString (NoABUID _) = "No ABUID"
argString (NoAdr _) = "No Adr"
argString (NoN _) = "No N"
-- Determine if the argument is for a VCF file
isVCF :: Arg -> Bool
isVCF (VCF _s) = True
isVCF _ = False
-- Determine if the argument is for a Positional file (could be VCF or JSON)
isPositional :: Arg -> Bool
isPositional (Positional _s) = True
isPositional _ = False
-- Determine if the argument is for a JSON file
isJSON :: Arg -> Bool
isJSON (JSON _s) = True
isJSON _ = False
-- There are three types of arguments that deal with the input and output files
-- (Positional, JSON, and VCF). The rest of the arguments are various switches.
isFileArgument :: Arg -> Bool
isFileArgument (JSON _s) = True
isFileArgument (Positional _s) = True
isFileArgument (VCF _s) = True
isFileArgument _ = False

-- Parse the command line options
commandLineOptions :: ParserInfo [Arg]
commandLineOptions = info (parseArgList <**> helper)
      ( fullDesc <> progDesc "Convert Mac Address Book to JSON" <> header "Gronk" )

-- The VCF file as an explicit parameter
vcfExplicit :: Parser Arg
vcfExplicit =
  VCF <$> strOption ( long "vcf" <> short 'v' <> metavar "STRING" <> help "input .vcf file" )
-- A positional parameter
positional :: Parser Arg
positional = Positional <$> argument str (metavar "FILE")
-- The JSON file as an explicit parameter
jsonExplicit :: Parser Arg
jsonExplicit =
  JSON <$> strOption( long "json" <> short 'j' <> metavar "STRING" <> help "output .json file" )
-- Parameter to eliminate the Photos from the output contacts
noPhoto :: Parser Arg
noPhoto = NoPhoto <$> flag' False (long "NoPhoto")

-- Parameter to eliminate the PRODID field from the output contacts
noProdID :: Parser Arg
noProdID = NoProdID <$> flag' False (long "NoProdID")

-- Parameter to eliminate the X-ABUID field from the output contacts
noABUID :: Parser Arg
noABUID = NoABUID <$> flag' False (long "NoABUID")

-- Parameter to eliminate the X-ABUID field from the output contacts
noAdr :: Parser Arg
noAdr = NoAdr <$> flag' False (long "NoAdr")

-- Parameter to eliminate the N field from the output contacts
noN :: Parser Arg
noN = NoN <$> flag' False (long "NoN")

-- Any of the paremeter options
anyArg :: Parser Arg
anyArg = vcfExplicit <|> jsonExplicit <|> positional <|>
         noPhoto <|> noProdID <|> noABUID <|> noN <|> noAdr

-- Return a list of parsed parameters
parseArgList :: Parser [Arg]
parseArgList = many anyArg

data Files = Files { input :: Handle, output :: Handle } deriving (Show)
-- A filter command has stdin as input and stdio as output
filterCmd :: Files
filterCmd = Files { input = stdin, output = stdout }

-- The command has a File portion and a Switch Portion
data Command = Command { files :: Files
                       , flags :: Flags
                       , fieldNames :: [String] } deriving (Show)

-- The data for an individual argument error.
data ArgError = IOArgError IOError | ArgError String deriving (Show)
-- The data for several argument erros.
data ArgErrors = ArgErrors { errors :: [String] } deriving (Show)

-- Analyze the parsed arguments that are file arguments
fileArgAnalysis :: [Arg] -> IO (Either ArgError Files)
-- No arguments: It is a filter command
fileArgAnalysis [] = return (Right filterCmd)
-- One argument, it is an explicit VCF parameter, or a positional paramter.
-- Output is assumed to be stdout.
fileArgAnalysis [a] | isVCF a || isPositional a =
  do { eh <- safeOpenFile (argString a) ReadMode
     ; return (either (Left . IOArgError . id) (\h -> Right (Files {input = h, output = stdout})) eh)
     }
-- One argument, it is an explicit JSON parameter. Input is assumed to be stdout.
fileArgAnalysis [a] | isJSON a =
  do eh <- safeOpenFile (argString a) WriteMode
     return (either (Left . IOArgError . id) (\h -> Right (Files {input = stdin, output = h})) eh)
-- Two arguments, the first is a VCF or positional (so assumed to be VCF),
-- the second is JSON or positional (so assumed to be JSON)
fileArgAnalysis [a,b] | (isVCF a || isPositional a) && (isJSON b || isPositional b) =
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
fileArgAnalysis [a,b] | (isVCF a || isPositional a) && isJSON b =
  return (Left (ArgError "Two VCF Files Specified"))
-- Two arguments, the first is JSON, and the second is JSON. This is an error
fileArgAnalysis [a,b] | isJSON a && isJSON b =
  return (Left (ArgError "Two JSON Files Specified"))
-- Two arguments, the first is JSON and the second is VCF or Positional (so assumed to be VCF in this case)
fileArgAnalysis [a,b] | isJSON a && (isVCF b || isPositional b) =
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
fileArgAnalysis _as = return (Left (ArgError "More than two arguments given"))

-- Now do the command analysis, including both file and switch arguments
commandArgAnalysis :: [Arg] -> IO (Either ArgError Command)
commandArgAnalysis args =
  do { let (fileArgz, switchArgz) = partition isFileArgument args
     ; let flagz = flagArgAnalysis switchArgz defaultFlags
     ; filez <- fileArgAnalysis fileArgz
     ; return (case filez of
                 Right fs -> Right (Command { files = fs
                                            , flags = flagz
                                            , fieldNames = flagFields switchArgz
                                            })
                 Left e -> Left e)
     }
