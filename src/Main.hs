module Main where

import System.IO (readFile)
import Text.ParserCombinators.Parsec

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

data Separator = Colon | Semicolon | End deriving (Show)
toSeparator :: Char -> Separator
toSeparator ':' = Colon
toSeparator ';' = Semicolon
toSeparator '\n' = End

itemSeparator :: GenParser Char st Separator
itemSeparator = oneOf(":;\n") >>= return . toSeparator

data Attribute = ComplexAttribute { name :: String, value :: String }
               | SimpleAttribute { name :: String } deriving (Show)
-- Constructor for a Simple Attribute
mkSimpleAttribute :: String -> Attribute
mkSimpleAttribute s = SimpleAttribute { name = s }

-- A complex attribute is of the form 'name = value'
complexAttribute :: GenParser Char st Attribute
complexAttribute = do { n <- attributeName
               ; _ <- char '='
               ; v <- attributeValue
               ; return ComplexAttribute { name = n, value = v } }
-- A simple attribute has only a name, no value
simpleAttribute :: GenParser Char st Attribute
simpleAttribute = do { n <- attributeValue
                     ; return SimpleAttribute { name = n } }
-- An attribute name must be non-empty
attributeName :: GenParser Char st String
attributeName = many1 (noneOf "=:;\n")
-- An attribute value can be empty, as in this example: "N:;;;;;"
attributeValue :: GenParser Char st String
attributeValue = many (noneOf "=:;\n")

-- Parse an attribute
attribute :: GenParser Char st Attribute
attribute = try complexAttribute <|> simpleAttribute

data Field = Field { pangalan :: String, attributes :: [Attribute] } deriving (Show)
-- Safely get the last attribute of the field (return Nothing when there are no attributes)
lastAttribute :: Field -> Maybe Attribute
lastAttribute f = if null (attributes f) then Nothing else Just (last (attributes f))
-- Check if a field has no attributs
noAttributes :: Field -> Bool
noAttributes = null . attributes

-- Safely add a simple attribute to a field
addSimpleAttribute :: Field -> String -> Field
addSimpleAttribute f s = f { attributes = attributes f ++ [mkSimpleAttribute s] }

-- Parse a simple Field
simpleField :: GenParser Char st Field
simpleField = do { a <- attributeName
           ; _ <- char ':'
           ; as <- sepBy1 attribute (oneOf ":;")
           ; _ <- eol
           ; return Field { pangalan = a, attributes = as } }

-- Parse a field
field :: GenParser Char st Field
field = do { s <- simpleField
           ; cs <- continuations
           ; return (combineContinuationWithField cs s)
           }
  where combineContinuationWithField :: [String] -> Field -> Field
        combineContinuationWithField [] f = f
        combineContinuationWithField ss f = addSimpleAttribute f (concat ss)

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

-- Open and close of an entry
openEntry, closeEntry :: GenParser Char st ()
openEntry = string "BEGIN:VCARD\n" >> return ()
closeEntry = string "END:VCARD" >> return ()

data Entry = Entry deriving (Show)

entry :: GenParser Char st Entry
entry = do { openEntry
           ; cs <- many field
           ; closeEntry
           ; return Entry
           }

type VCF = [Entry]

vcfFile :: GenParser Char st VCF
vcfFile = sepBy entry (char '\n')

-- Run the address book parse on a test input
test,t :: GenParser Char () a -> String -> Either ParseError a
test p testCase = parse p "(unknown)" testCase
t = test

-- Test attribute parser
t1,t2,t3 :: Either ParseError Attribute
t1 = test attribute "a=b"
t2 = test attribute "ab"
t3 = test attribute "=b"
-- Test field parser
t4,t5,t6,t7,t8,t9,t10 :: Either ParseError Field
t4 = test field "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n"
t5 = test field "ab:\n"
t6 = test field "ab:c\n"
t7 = test field "N:;;;;\n"
t8 = test field "PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN\n"
t9 = test field ":kdkdk\n" -- Shoule fail (left "(unknown)")
t10 = test field "ORG:Macys;\n"

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

main :: IO ()
main = do
  arubala <- readFile "Arubala.test"
  parseTest entry arubala

m = main
