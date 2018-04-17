{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (readFile)
import Text.ParserCombinators.Parsec
import GHC.Generics
import Data.Monoid ((<>))
import Data.Either
import Data.Aeson as Aeson
import Data.ByteString.Lazy (unpack)

{- A VCF file contains a list of entries, each entry has the following:
 Opener: BEGIN:VCARD
 List of fields
 Closer: END:VCARD
 Grammatically, this is

 VCF = [Entry]
 Entry = 'BEGIN:VCARD'[Field]'END:VCARD'

 A field contains name, followed by a colon, and then a list of components,
 separated by semicolons. grammatically this is:

 Field = Name ':' Components
 Components = SeparatedBy Components ';'

 A component can be empty, represented by successive semicolons: ';;'

 Some entries look like this:
item1.EMAIL;type=INTERNET;type=pref:vicky.008@gmail.com
item1.X-ABLabel:Dad

 This happens when you give a custom name to a field in the address book.
 This is really two entries as defined above. We will treat this syntactically as
 two entries, and regard the relation between them as a syntactic issue, not
 a parsing issue.

 To make matters more annoying, some entries seem to have the style of a name
 with attributes. Here is an example:

ADR;type=HOME;type=pref:;;4193 NW Scottsdale Dr;Beaverton;OR;97006;USA

 This IS a syntactic issue, we need to recognize this type of entry as well.
 Here the entry name is 'ADR', i.e. an address field. The components are now
 of two types, some have a name: 'type = HOME', while some are just a
 component without a name, such as '4193 NW Scottsdale Dr'.

 And here is another monkey wrench:
X-ABUID:67347CA9-0ECA-4C99-8D69-7E9EBFE3C209:ABPerson

 It has a name "X-ABUID", with two valuish things separated by colons.

Don't know why the geniuses at Apple did this.
If some of the code here gets ugly, my excuse is that the input is ugly.

-}

whiteSpace :: GenParser Char st String
whiteSpace = many (oneOf " \t\n")
blank :: GenParser Char st Char
blank = char ' '
eol :: GenParser Char st Char -- The end of line character is \n
eol = char '\n'

-- Convert character to appropriate item separator.
data Separator = Colon | Semicolon | End deriving (Show, Generic, ToJSON)
fromSeparator :: Separator -> Char
fromSeparator Colon = ':'
fromSeparator Semicolon = ';'
fromSeparator End = '\n'
toSeparator :: Char -> Separator
toSeparator ':' = Colon
toSeparator ';' = Semicolon
toSeparator '\n' = End

-- Parse an item separator.
itemSeparator :: GenParser Char st Separator
itemSeparator = oneOf(":;\n") >>= return . toSeparator

-- Attributes of an item.
data Attribute = ComplexAttribute { name :: String, value :: String }
               | SimpleAttribute { name :: String }
               | NoAttribute deriving (Show, Generic)
-- Constructor for a Simple Attribute
mkSimpleAttribute :: String -> Attribute
mkSimpleAttribute s = SimpleAttribute { name = s }
instance ToJSON Attribute where
  toJSON (ComplexAttribute { name = n, value = v}) = object ["name" .= n, "value" .= v]
  toJSON (SimpleAttribute { name = n }) = object ["name" .= n]
  toEncoding (ComplexAttribute { name = n, value = v}) = pairs ("name" .= n <> "value" .= v) 
  toEncoding (SimpleAttribute { name = n }) = pairs ("name" .= n)

-- Parser for a complex attribute; it has the form 'name = value'
complexAttribute :: GenParser Char st Attribute
complexAttribute = do { n <- attributeName
               ; _ <- char '='
               ; v <- attributeValue
               ; return ComplexAttribute { name = n, value = v } }

-- Parser for a simple attribute; it has only a name, no value
simpleAttribute :: GenParser Char st Attribute
simpleAttribute = do { n <- attributeValue
                     ; return SimpleAttribute { name = n } }
-- An attribute name must be non-empty
attributeName :: GenParser Char st String
attributeName = many1 (noneOf "=:;\n")

-- An attribute value can be empty, as in this example: "N:;;;;;", thus
-- we parse with 'many', not 'many1'.
attributeValue :: GenParser Char st String
attributeValue = many (noneOf "=:;\n")

-- Parse an attribute.
attribute :: GenParser Char st Attribute
attribute = try complexAttribute <|> simpleAttribute

-- A field has a name, and a list of attributes.
data Field = Field { pangalan :: String
                   , attributes :: [Attribute] } deriving (Show, Generic)

instance ToJSON Field where
  toJSON (Field { pangalan = p, attributes = as}) = object [ "name" .= p, "value" .= toJSON as]
  toEncoding (Field { pangalan = p, attributes = as}) = pairs ("name" .= p <> "value" .= toJSON as) 

-- Safely get the last attribute of the field (return Nothing when there are no attributes)
lastAttribute :: Field -> Attribute
lastAttribute f = if null (attributes f) then NoAttribute else last (attributes f)

-- Get all but the last attribute of a field.
firstAttributes :: Field -> [Attribute]
firstAttributes f | noAttributes f = []
firstAttributes f = init (attributes f)

-- Update the last attribute of a field using an update function.
updateLastAttribute :: Field -> (Attribute -> Attribute) -> Field
updateLastAttribute field update =
  let l = lastAttribute field
      rest = firstAttributes field
  in field { attributes = rest ++ [update l] }

-- To support continuation fields, extend the last attribute of a field
-- with the continuation data.
extendLastAttribute :: [String] -> Attribute -> Attribute
extendLastAttribute ss attr =
  case attr of
    NoAttribute -> ComplexAttribute { name = "Continuation", value = concat ss }
    SimpleAttribute { name = n } ->
      ComplexAttribute { name = "Continuation", value = n ++ concat ss }
    ComplexAttribute { name = n, value = v } ->
      ComplexAttribute { name = n, value = v ++ concat ss }

-- Check if a field has no attributes.
noAttributes :: Field -> Bool
noAttributes = null . attributes

-- Safely add a simple attribute to a field.
addSimpleAttribute :: Field -> String -> Field
addSimpleAttribute f s = f { attributes = attributes f ++ [mkSimpleAttribute s] }

-- Parse a simple Field. A field is <usually> on one line.
simpleField :: GenParser Char st Field
simpleField = do { a <- attributeName
                 ; _ <- oneOf ":;"
                 ; as <- sepBy1 attribute (oneOf ":;")
                 ; _ <- eol
                 ; return Field { pangalan = a, attributes = as } }

-- Parse a field that may have continuation lines.
field :: GenParser Char st Field
field = do { s <- simpleField
           ; cs <- continuations
           ; return (addContinuation cs s)
           }
  where -- Add the continuation data to the field being parsed.
        addContinuation :: [String] -> Field -> Field
        addContinuation [] f = f
        addContinuation ss f | noAttributes f = addSimpleAttribute f (concat ss)
        addContinuation ss f = updateLastAttribute f (extendLastAttribute ss)

-- Some fields (e.g. PHOTO), have continuation lines for lots of data.
-- Apparently this are indicated by a leading blank
-- These continuations always seem to alphanumeric strings
continuation :: GenParser Char st String
continuation = do { blank
                  ; a <- many alphaNum
                  ; _ <- eol
                  ; return a }

continuations :: GenParser Char st [String]
continuations = many continuation

-- Open and close of an entry.
openEntry, closeEntry :: GenParser Char st ()
openEntry = string "BEGIN:VCARD\n" >> return ()
closeEntry = string "END:VCARD" >> return ()

-- An entry consists of one or more fields.
data Entry = Entry { fields :: [Field] } deriving (Show, Generic, ToJSON)

-- Parse an entry.
entry :: GenParser Char st Entry
entry = do { openEntry
           ; fs <- manyTill field (try closeEntry)
           ; return Entry { fields = fs }
           }

-- A VCF file is a list of entries.
data VCF = VCF { entries :: [Entry] } deriving (Show, Generic, ToJSON)

-- Parse a VCF File.
vcfFile :: GenParser Char st VCF
vcfFile = do { es <- sepBy entry (char '\n')
             ; return VCF { entries = es } }

-- Run the address book parse on a test input
test,t :: GenParser Char () a -> String -> Either ParseError a
test p testCase = parse p "(unknown)" testCase
t = test

-- Convert a parser test into a parser and json test.
jsonTest :: (ToJSON a) => GenParser Char () a -> String -> IO ()
jsonTest p s = do { let ea = test p s
                  ; case ea of
                      Left e -> putStrLn (show e)
                      Right a -> putStrLn (show (toJSON a))
                  }

-- Test attribute parser
t1,t2,t3 :: Either ParseError Attribute
t1 = test attribute "a=b"
t2 = test attribute "ab"
t3 = test attribute "=b"
-- Test field parser
t4,t5,t6,t7,t8,t9,t10,t11,t12 :: Either ParseError Field
t4 = test field "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n"
t5 = test field "ab:\n"
t6 = test field "ab:c\n"
t7 = test field "N:;;;;\n"
t8 = test field "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n"
t9 = test field ":kdkdk\n" -- Should fail (left "(unknown)")
t10 = test field "ORG:Macys;\n"
t11 = test field "\n" -- Should fail, empty line not allowed
t12 = test field "TEL;type=CELL;type=VOICE;type=pref:15036451141\n"
t21,t23 :: Either ParseError Field
t21 = test field "ORG:Macys;\n -kdkdkdkd\n" -- Should fail because of '-' in the continuation
t22 = test field "ORG:Macys--\n mcmcmcmc\n"
t23 = test field "\n mcmcmcmc\n"
t30 :: Either ParseError Entry
t30 = test entry "BEGIN:VCARD\nORG:Macys;\nEND:VCARD\n"
t40,t41 :: Either ParseError VCF
t40 = test vcfFile "BEGIN:VCARD\nORG:Macys;\nBDAY:2014-06-09\n continue\nNOTE:Has Immunization Record\nEND:VCARD"
t41 = test vcfFile "BEGIN:VCARD\nORG:Macys;\nEND:VCARD"

main :: IO ()
main = do
  arubala <- readFile "test/Arubala.test"
  parseTest entry arubala

m = main
