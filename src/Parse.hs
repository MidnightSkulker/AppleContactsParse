{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Parse where

import System.IO (readFile)
import Text.ParserCombinators.Parsec
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import Data.Aeson as Aeson (ToJSON(..), object, pairs, (.=), encode, Value(..), KeyValue(..), toJSONList)
import Data.ByteString.Lazy.Char8 as DBLC8 (putStrLn, pack)
import Data.Text as T (pack, Text)
import Data.Char (isAlphaNum, isNumber)
import GHC.Exts (fromList)

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

BUMMER: After getting most everything working, I discover URL entries:
Here are three different formats
item1.URL;type=pref:www.target.com
item1.URL;type=pref:campus.educadium.com/OCCD/mod/certificate/view.php?id=3383&action=get
item1.URL;type=pref:http://www.MomsandDads.com
item5.URL:www.employereservices.com
URL;type=HOME:mychartor.providence.org

Most URL fields are of the first format. The second format add a monkey wrench that
a URL can have and equal sign, which ruined my parsing of attributes such as "type=EMAIL".
The third is another variation of the first, but it shows that a URL can have a colon
after "http" or "https". This wrecks the significance of the colon in my parsing.
The fourth format is another variation of the first, and it shows that not all
of the entires have "type=pref". The last is a different format, without the beginning
"item[nnn].URL".

All of this suggests a special case for URL fields, which I will start working on now.

-}

whiteSpace :: GenParser Char st String
whiteSpace = many (oneOf " \t\n")
blank :: GenParser Char st Char
blank = char ' '
number :: GenParser Char st String
number = many1 (satisfy isNumber)

-- The end of line character is \n, but for some reason Apple has chosen
-- to use "\r\n" for these .vcf files
eol :: GenParser Char st ()
eol = many (oneOf "\r\n") >> return ()

-- The following definitions for attributes are for the vast majority of
-- fields. They do not work for URL fields.
-- Attributes of an item.
data Attribute = ComplexAttribute { name :: String, value :: String }
               | SimpleAttribute { name :: String }
               | NoAttribute deriving (Show, Generic)

-- Convert an attribute to a pair
attributeToPair :: Attribute -> (String, Value)
attributeToPair ComplexAttribute { name = n, value = v } = ( n, String (T.pack v) )
attributeToPair SimpleAttribute { name = n } = ( n, Null )
attributeToPair NoAttribute = ( "NoValue", Null )

-- Part of encoding the Attributes
oneField :: Attribute -> Value
oneField ComplexAttribute { name = n, value = v } =
  if v == ""
  then String (T.pack n)
  else object [(T.pack n, String (T.pack v))]
oneField SimpleAttribute { name = n } = String (T.pack n)
oneField NoAttribute = Null

fromPair :: KeyValue kv => (String, String) -> kv
fromPair (s,t) = T.pack s .= t
-- Make an object from a list of items that can be paired.
mkObjectFromPairable :: (a -> (String, Value)) -> [a] -> Value
mkObjectFromPairable toPair = object . map (fromPair . toPair)
  where fromPair :: KeyValue kv => (String, Value) -> kv
        fromPair (s, t) = (T.pack s .= t)

-- How to encode / decode an Attribute
instance ToJSON Attribute where
  toJSON ComplexAttribute { name = n, value = v } =
    if v == []
    then String (T.pack n)
    else object [T.pack n .= v]
  toJSON SimpleAttribute { name = n } = String (T.pack n)
  toJSON NoAttribute = Null

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
attributeName = many1 (noneOf "=:;\r\n")

-- An attribute value can be empty, as in this example: "N:;;;;;", thus
-- we parse with 'many', not 'many1'.
attributeValue :: GenParser Char st String
attributeValue = many (noneOf "=:;\r\n")

-- Parse an attribute.
attribute :: GenParser Char st Attribute
attribute = try complexAttribute <|> simpleAttribute

-- URI Characters
uriChar :: GenParser Char st Char
uriChar = satisfy isAlphaNum <|> oneOf("-._~:/?#[]@!$&'()%*+,;=")

-- URI Parser
uri :: GenParser Char st String
uri = do { uri <- many1 uriChar
         ; eol
         ; return uri
         }

-- Parser for URL fields
urlField :: GenParser Char st Field
urlField = do { optional itemPrefix
              ; string "URL;"
              ; attrs <- complexAttribute `sepBy` char ';'
              ; char ':'
              ; muri <- optionMaybe uri
              ; case muri of
                  Nothing -> return Field { pangalan = "Invalid URI in Address Book" , attributes = attrs }
                  Just urival -> return Field { pangalan = urival, attributes = attrs }
              }
  where itemPrefix :: GenParser Char st ()
        itemPrefix = string "item" >> number >> char '.' >> return ()

-- A field has a name, and a list of attributes.
data Field = Field { pangalan :: String, attributes :: [Attribute] } deriving (Show, Generic)

-- fieldToPair :: Field -> (String, Field { pangalan = p, attributes = attrs }

-- Encode Fields as JSONx
fields :: [Attribute] -> Value
fields [] = Null
-- Do not embed a single value in Array constructor
fields [a] = toJSON a
fields as  = toJSON (map oneField as)

fieldToJSON :: KeyValue kv => Field -> kv
fieldToJSON Field { pangalan = p, attributes = as } =
  T.pack p .= mkObjectFromPairable attributeToPair as

instance ToJSON Field where
--  toJSON f@Field { pangalan = p, attributes = as} = object [T.pack p .= mkObjectFromPairable  attributeToPair as]
  toJSON f@Field { pangalan = p, attributes = as} = object [T.pack p .= toJSON as]

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
                 ; as <- attribute `sepBy1` (oneOf ":;")
                 ; _ <- eol
                 ; return Field { pangalan = a, attributes = as } }

-- Parse a field that may have continuation lines.
field :: GenParser Char st Field
field = try urlField <|> nonUrlField
  where -- Add the continuation data to the field being parsed.
        addContinuation :: [String] -> Field -> Field
        addContinuation [] f = f
        addContinuation ss f | noAttributes f = addSimpleAttribute f (concat ss)
        addContinuation ss f = updateLastAttribute f (extendLastAttribute ss)
        nonUrlField :: GenParser Char st Field
        nonUrlField =  do { s <- simpleField
                          ; cs <- continuations
                          ; return (addContinuation cs s)
                          }

-- Some fields (e.g. PHOTO), have continuation lines for lots of data.
-- Apparently this are indicated by a leading blank
-- These continuations always seem to alphanumeric strings
continuation :: GenParser Char st String
continuation = do { blank
                  ; a <- many (satisfy okJpgChar)
                  ; _ <- eol
                  ; return a
                  }
  where okJpgChar :: Char -> Bool
        okJpgChar c = isAlphaNum c || c == '/' || c == '+' || c == '='

continuations :: GenParser Char st [String]
continuations = many continuation

-- Open and close of an card.
-- Note that the parse does not consume the line return after "END:VCARD", if there is one.
openCard, closeCard :: GenParser Char st ()
openCard = string "BEGIN:VCARD" >> eol >> return ()
closeCard = string "END:VCARD" >> return ()

-- An card consists of one or more fields.
data Card = Card { fieldz :: [Field] } deriving (Show, Generic)

-- mkObjectFromFields :: (Field -> (String, String)) -> [Field] -> Value
-- mkObjectFromFields toPair fields = object [ map (fromPair . fieldToPair) fields ]
--   where fromPair :: KeyValue kv => (String, String) -> kv
--         fromPair (s, t) = (T.pack s .= t)
--         fieldToPair :: KeyValue kv => Field -> kv
--         fieldToPair f = (T.pack (fieldName f) .= mkObjectFromPairable toPair (attributes f

-- Fullname Order: Last, First, Middle, Prefix, Suffix

instance ToJSON Card where
  toJSON Card { fieldz = fs } = object [ "fields" .= mkObjectFromPairable fieldToPair fs ]
    where fieldToPair :: Field -> (String, Value)
          fieldToPair f@Field { pangalan = p, attributes = as } =
            (p, mkObjectFromPairable attributeToPair as)

-- Parse an card.
card :: GenParser Char st Card
card = do { openCard
          ; fs <- field `manyTill` (try closeCard)
          ; return Card { fieldz = fs }
          }

-- A VCF file is a list of cards.
data VCF = VCF { cards :: [Card] } deriving (Show, Generic)

instance ToJSON VCF where
  toJSON = toJSON . cards

-- Parse a VCF File.
-- Apple Contacts likes to end these files without the last eol
-- We allow the file to end with a \n, or not. So the last line can be either
-- END:VCARD\n
-- or just
-- END:VCARD

vcf :: GenParser Char st VCF
vcf = do { es <- card `sepEndBy` eol
         ; eof
         ; return VCF { cards = es }
         }
