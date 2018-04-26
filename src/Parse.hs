{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import System.IO (readFile)
import Text.ParserCombinators.Parsec
import GHC.Generics
import Data.Monoid ((<>))
import Data.Either
import Data.Aeson as Aeson (ToJSON(..), object, pairs, (.=), encode, Value(String))
import Data.ByteString.Lazy.Char8 as DBLC8 (putStrLn, pack)
import Data.Text as T (pack)

{- A VCF file contains a list of cards, each card has the following:
 Opener: BEGIN:VCARD
 List of fields
 Closer: END:VCARD
 Grammatically, this is

 VCF = [Card]
 Card = 'BEGIN:VCARD'[Field]'END:VCARD'

 A field contains name, followed by a colon, and then a list of components,
 separated by semicolons. grammatically this is:

 Field = Name ':' Components
 Components = SeparatedBy Components ';'

 A component can be empty, represented by successive semicolons: ';;'

 Some cards look like this:
item1.EMAIL;type=INTERNET;type=pref:vicky.008@gmail.com
item1.X-ABLabel:Dad

 This happens when you give a custom name to a field in the address book.
 This is really two cards as defined above. We will treat this syntactically as
 two cards, and regard the relation between them as a syntactic issue, not
 a parsing issue.

 To make matters more annoying, some cards seem to have the style of a name
 with attributes. Here is an example:

ADR;type=HOME;type=pref:;;4193 NW Scottsdale Dr;Beaverton;OR;97006;USA

 This IS a syntactic issue, we need to recognize this type of card as well.
 Here the card name is 'ADR', i.e. an address field. The components are now
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

-- Separated by <sep>, optionally ended by <end>
sepByEndBy :: GenParser Char st a -> GenParser Char st Char -> GenParser Char st Char -> GenParser Char st  [a]
sepByEndBy p sep end =
  do{ x <- p
    ; do { _ <- end
         ; xs <- sepEndBy p sep
         ; return (x:xs)
         }
         <|> return [x]
    }

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
instance ToJSON Attribute where
  toJSON (ComplexAttribute { name = n, value = v}) = object ["name" .= n, "value" .= v]
  toJSON (SimpleAttribute { name = n }) = object ["name" .= n]
  toEncoding (ComplexAttribute { name = n, value = v}) = pairs ("name" .= n <> "value" .= v) 
  toEncoding (SimpleAttribute { name = n }) = pairs ("name" .= n)

-- Constructor for a Simple Attribute
mkSimpleAttribute :: String -> Attribute
mkSimpleAttribute s = SimpleAttribute { name = s }

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
  toJSON (Field { pangalan = p, attributes = as}) = object [ T.pack p .= toJSON as]
  toEncoding (Field { pangalan = p, attributes = as}) = pairs (T.pack p .= toJSON as)

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

-- Open and close of an card.
openCard, closeCard :: GenParser Char st ()
openCard = string "BEGIN:VCARD\n" >> return ()
closeCard = string "END:VCARD" >> return ()

-- An card consists of one or more fields.
data Card = Card { fields :: [Field] } deriving (Show, Generic)

instance ToJSON Card where
  toJSON (Card { fields = fs}) = object [ "fields" .= toJSON fs ]
  toEncoding (Card { fields = fs}) = pairs ("fields" .= toJSON fs) 

-- Parse an card.
card :: GenParser Char st Card
card = do { openCard
          ; fs <- manyTill field (try closeCard)
          ; return Card { fields = fs }
          }

-- A VCF file is a list of cards.
data VCF = VCF { cards :: [Card] } deriving (Show, Generic)

instance ToJSON VCF where
  toJSON (VCF { cards = es}) = object [ "cards" .= toJSON es ]
  toEncoding (VCF { cards = es}) = pairs ("cards" .= toJSON es) 

-- Parse a VCF File.
vcf :: GenParser Char st VCF
vcf = do { es <- sepByEndBy card (char '\n') (eof >> return '\n')
         ; return VCF { cards = es }
         }
