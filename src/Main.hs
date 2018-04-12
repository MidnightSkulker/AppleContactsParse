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

data Separator = Colon | Semicolon | End deriving (Show)
toSeparator :: Char -> Separator
toSeparator ':' = Colon
toSeparator ';' = Semicolon
toSeparator '\n' = End

itemSeparator :: GenParser Char st Separator
itemSeparator = oneOf(":;\n") >>= return . toSeparator

data Attribute = ComplexAttribute { name :: String, value :: String }
               | SimpleAttribute { name :: String } deriving (Show)
complexAttribute :: GenParser Char st Attribute
complexAttribute = do { n <- attributeName
               ; _ <- char '='
               ; v <- attributeValue
               ; return ComplexAttribute { name = n, value = v } }
simpleAttribute :: GenParser Char st Attribute
simpleAttribute = do { n <- attributeValue
                     ; return SimpleAttribute { name = n } }
attributeName :: GenParser Char st String
attributeName = many1 (noneOf "=:;\n")
attributeValue :: GenParser Char st String
attributeValue = many1 (noneOf "=:;\n")

attribute :: GenParser Char st Attribute
attribute = try complexAttribute <|> simpleAttribute

data Field = Field { pangalan :: String, attributes :: [Attribute] } deriving (Show)
field :: GenParser Char st Field
field = do { a <- attribute
           ; _ <- char ':'
           ; as <- sepBy1 attribute (oneOf ":;")
           ; _ <- eol
           ; return Field { pangalan = name a, attributes = as } }

data Item = Item { text :: String
                 , separator :: Separator } deriving (Show)
type Component = [Item]
componentSeparator :: GenParser Char st Char
componentSeparator = oneOf ":;\n"

whiteSpace :: GenParser Char st String
whiteSpace = many (oneOf " \t\n")
blank :: GenParser Char st Char
blank = char ' '
-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

-- A single item (text followed by separator) of a component
-- Notice at this point no distinction is made between a 'name' and a 'value'
-- The input syntax of VCF is too broken to make that conclusion at this time.
item :: GenParser Char st Item
item = do { v <- many (noneOf ":;\n")
          ; s <- oneOf ":;\n"
          ; return Item { text = v, separator = toSeparator s } }

-- A component is a string of characters other than ':' or ';'
component :: GenParser Char st Component
component = many item

-- A list of components, separated by ':' or ';'
components :: GenParser Char st [Component]
components = sepBy component (oneOf ";:")

-- A field is a list of components followed by an end of line
-- field :: GenParser Char st [Component] -- Boo hoo, not a one liner :(
-- field = (continuation >> return []) <|> initial
--   where initial = do { cs <- components
--                      ; char '\n'
--                      ; return cs }

-- Some fields (e.g. PHOTO), have continuation lines for lots of data.
-- Apparently this are indicated by a leading blank
-- These continuations always seem to alphanumeric strings
-- TEMP: For now, ignoring the continuation data
continuation :: GenParser Char st ()
continuation = blank >> many alphaNum >> eol >> return ()

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

t1,t2,t3 :: Either ParseError Attribute
t1 = test attribute "a=b"
t2 = test attribute "ab"
t3 = test attribute "=b"
t4 :: Either ParseError Field
t4 = test field "X-ABUID:4709EC50-7594-4F67-85E1-6870DA65FCBA:ABPerson\n"
t5 = test field "ab:\n"
t6 = test field "ab:c\n"

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

main :: IO ()
main = do
  arubala <- readFile "Arubala.test"
  parseTest entry arubala

m = main
