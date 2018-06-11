{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse (
    vcf
    -- All the following exports are for testing
  , field
  , urlField
  , card
  , cards
  , attribute
  , mkObjectFromPairable
  , attributeToPair
  , fromPair
  , Attribute (..)
  , Field (..)
  , Card (..) ) where

import Text.ParserCombinators.Parsec
-- import Text.RE.TDFA.String
import GHC.Generics (Generic)
import Data.Aeson as Aeson (ToJSON(..), object, (.=), Value(..), KeyValue(..))
import Data.Text as T (pack)
import Data.Char (isAlphaNum, isNumber)
import Data.List (partition, groupBy, find)
import Data.Maybe (isJust, fromJust)
import Data.Tuple.Utils (snd3)
import RE (isItem, itemNumber)

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

-- whiteSpace :: GenParser Char st String
-- whiteSpace = many (oneOf " \t\n")
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

-- Determine if an attribute is simple
isSimpleAttribute :: Attribute -> Bool
isSimpleAttribute (SimpleAttribute { name = _ }) = True
isSimpleAttribute _ = False

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
  else object [T.pack n .= String (T.pack v)]
oneField SimpleAttribute { name = n } = String (T.pack n)
oneField NoAttribute = Null

fromPair :: KeyValue kv => (String, String) -> kv
fromPair (s,t) = T.pack s .= t
-- Make an object from a list of items that can be paired.
mkObjectFromPairable :: (a -> (String, Value)) -> [a] -> Value
mkObjectFromPairable toPair = object . map (mulaPair . toPair)
  where mulaPair :: KeyValue kv => (String, Value) -> kv
        mulaPair (s, t) = (T.pack s .= t)

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
uri = do { url <- many1 uriChar
         ; eol
         ; return url
         }

-- Parser for URL fields
urlField :: GenParser Char st Field
urlField = do { optional itemPrefix
              ; _ <- string "URL;"
              ; attrs <- complexAttribute `sepBy` char ';'
              ; _ <- char ':'
              ; muri <- optionMaybe uri
              ; case muri of
                  Nothing -> return Field { pangalan = "Invalid URI in Address Book" , attributes = attrs }
                  Just urival -> return Field { pangalan = urival, attributes = attrs }
              }
  where itemPrefix :: GenParser Char st ()
        itemPrefix = string "item" >> number >> char '.' >> return ()

-- A field has a name, and a list of attributes.
data Field = Field { pangalan :: String, attributes :: [Attribute] } deriving (Show, Generic)

-- The VCF file has an unfortunate encoding for custom names for fields. Here is
-- and example of a card with custom names for telephone and E-mail fields:
-- BEGIN:VCARD
-- VERSION:3.0
-- PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN
-- N:Vignesh;Ruthvik;;;
-- FN:Ruthvik Vignesh
-- ORG:Kasalukuyang Estudyante;
-- item1.EMAIL;type=INTERNET;type=pref:vicky.008@gmail.com
-- item1.X-ABLabel:Dad
-- item2.TEL;type=pref:8472081772
-- item2.X-ABLabel:Dad (Vignesh Jeyaraj)
-- item3.TEL:8474034147
-- item3.X-ABLabel:Mom (Kasthuri Thangamariappan)
-- ADR;type=HOME;type=pref:;;16455 SW Estuary Dr. #208;Beaverton;OR;97006;USA
-- NOTE:Has Immunization Record\n
-- BDAY:2014-06-09
-- CATEGORIES:Address Book
-- UID:5f124cfb15813f50
-- X-ABUID:77230A65-1FBF-4BF8-B828-BB7CAA8BABF0:ABPerson
-- END:VCARD
--
-- Notice the EMAIL field (item1.Email). This is an E-mail field with the
-- custom label "Dad". It is split into two lines, thus it will result
-- in two fields in the encoding of the card. The following function will
-- find both of these fields and combine them into one field.
-- Similar remarks apply to the telephone fields.
combineItems :: [Field] -> [Field]
combineItems fs =
  let (items, nonItems) = partition isFieldItem fs
      itemGroups = groupBy sameItemNumber items
      -- From each item group, form a single item with a combined label,
      -- and the value (e.g. Email address or telephone number)
      itemGroupsRestructured = map itemFieldRestructure itemGroups
      -- If there is more than one telephone number or Email address,
      -- further group these item groups into a single item that lists
      -- all the Email addresses or telephone numbers.
      itemGroupGroups :: [[[Field]]]
      itemGroupGroups = groupBy sameItemGroupType itemGroups
      itemGroupGroupFields = map mkItemGroupGroupField itemGroupGroups
  in itemGroupsRestructured ++ nonItems ++ itemGroupGroupFields

-- For an item group group, create a combined item with all of the values
mkItemGroupGroupField :: [[Field]] -> Field
mkItemGroupGroupField fss =
  let labelsAndValues :: [(Field, Field)]
      labelsAndValues = map getLabelAndValueFields fss
  in undefined

-- For fields that are for items, i.e. the name starts with item[0-9]+
-- Put together a new field with a more sane structure
itemFieldRestructure :: [Field] -> Field
itemFieldRestructure [f1, f2] =
  let (labelField, valueField) = getLabelAndValueFields [f1, f2]
      (_match2, after2, _num2) = fieldItemStructure valueField
  in mkItemField labelField valueField after2
itemFieldRestructure _ = brokenItemField "None" "None"

-- Make an item field from the two fields (Label and Value)
mkItemField :: Field -> Field -> String -> Field
mkItemField labelField valueField labelStr =
  let labelValue = maybe "NoName" name (find isSimpleAttribute (attributes labelField))
      attrValue = maybe "NoValue" name (find isSimpleAttribute (attributes valueField))
  in Field { pangalan = labelStr ++ "-" ++ labelValue
           , attributes = [mkSimpleAttribute attrValue] }

-- Get the AB-Label and Value fields of an item field
-- The fields may come in either order
-- Fails if not called with a list of exactly two fields.
getLabelAndValueFields :: [Field] -> (Field, Field) -- (Label, Value)
getLabelAndValueFields [f1, f2] = 
  let (_match1, after1, _num1) = fieldItemStructure f1
  in if (after1 == "X-ABLabel")
     then (f1, f2)
     else (f2, f1)
getLabelAndValueFields fs = error ("getLabelAndValueFields: " ++ show fs)

-- Get the label and value parts of the item field
getLabelAndValue :: [Field] -> (String, String) -- (Label, Value)
getLabelAndValue [f1, f2] =
  let (_match1, after1, _num1) = fieldItemStructure f1
      (_match2, _after2, _num2) = fieldItemStructure f2
  in if (after1 == "X-ABLabel")
     then let attrValue = maybe "NoValue" name (find isSimpleAttribute (attributes f2))
              labelStr = maybe "NoName" name (find isSimpleAttribute (attributes f1))
          in (labelStr, attrValue)
     else let attrValue = maybe "NoValue" name (find isSimpleAttribute (attributes f1))
              labelStr = maybe "NoName" name (find isSimpleAttribute (attributes f2))
          in (labelStr, attrValue)
getLabelAndValue fs = error ("getLabelAndValue: " ++ show fs)

-- Get the "after" part of each field label, e.g. "TEL" for telephone numbers
-- and "EMAIL" for email addresses.
getItemGroupType :: [Field] -> String
getItemGroupType [f1, _f2] = snd3 (fieldItemStructure f1)
getItemGroupType fs = error ("getAfter: " ++ show fs)

-- Determine if two item groups have the same type (e.g. "TEL" or "EMAIL")
sameItemGroupType :: [Field] -> [Field] -> Bool
sameItemGroupType fs1 fs2 = getItemGroupType fs1 == getItemGroupType fs2

-- Generate a broken field when something goes wrong
brokenItemField :: String -> String -> Field
brokenItemField s1 s2 =
  Field { pangalan = "Broken Item Field: " ++ s1 ++ s2, attributes = [] }

-- Need to be sure it is an item field before you call this, otherwise
-- you will get an exception
fieldItemStructure :: Field -> (String, String, String)
fieldItemStructure = fromJust . isItem . pangalan
-- Determine if a field is an "item", i.e. the name starts with item[0-9]+
isFieldItem :: Field -> Bool
isFieldItem = isJust . isItem . pangalan
-- Determine if two fields have the same item number
sameItemNumber :: Field -> Field -> Bool
sameItemNumber f1 f2 = itemNumber (pangalan f1) == itemNumber (pangalan f2)

-- First step in encoding a Field
fieldToPair :: Field -> (String, Value)
fieldToPair Field { pangalan = p, attributes = as } =
  case as of
    [] -> (p, Null)
    [a] -> (p, oneField a)
    az -> (p, toJSON az)

instance ToJSON Field where
  toJSON f = let (s, v) = fieldToPair f in object [T.pack s .= v]
  --  toJSON f@Field { pangalan = p, attributes = as} = object [T.pack p .= mkObjectFromPairable  attributeToPair as]

-- Safely get the last attribute of the field (return Nothing when there are no attributes)
lastAttribute :: Field -> Attribute
lastAttribute f = if null (attributes f) then NoAttribute else last (attributes f)

-- Get all but the last attribute of a field.
firstAttributes :: Field -> [Attribute]
firstAttributes f | noAttributes f = []
firstAttributes f = init (attributes f)

-- Update the last attribute of a field using an update function.
updateLastAttribute :: Field -> (Attribute -> Attribute) -> Field
updateLastAttribute fld update =
  let l = lastAttribute fld
      rest = firstAttributes fld
  in fld { attributes = rest ++ [update l] }

-- To support continuation fields, extend the last attribute of a field
-- with the continuation data.
extendLastAttribute :: [String] -> Attribute -> Attribute
extendLastAttribute ss attr =
  case attr of
    NoAttribute -> SimpleAttribute { name = concat ss }
    SimpleAttribute { name = n } -> SimpleAttribute { name = n ++ concat ss }
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
        nonUrlField =  do { f <- simpleField
                          ; cs <- continuations
                          ; return (addContinuation cs f)
                          }

-- Some fields (e.g. PHOTO), have continuation lines for lots of data.
-- Apparently this are indicated by a leading blank
-- These continuations always seem to alphanumeric strings
continuation :: GenParser Char st String
continuation = do { _ <- blank
                  ; a <- many (satisfy okJpgChar)
                  ; _ <- eol
                  ; return a
                  }
  where okJpgChar :: Char -> Bool
        okJpgChar c = c /= '\r' && c /= '\n'
--        okJpgChar c = isAlphaNum c || c == '/' || c == '+' || c == '='

continuations :: GenParser Char st [String]
continuations = many continuation

-- Open and close of an card.
-- Note that the parse does not consume the line return after "END:VCARD", if there is one.
openCard, closeCard :: GenParser Char st ()
openCard = string "BEGIN:VCARD" >> eol >> return ()
closeCard = string "END:VCARD" >> return ()

-- An card consists of one or more fields.
data Card = Card { fieldz :: [Field] } deriving (Show, Generic)

-- Fullname Order: Last, First, Middle, Prefix, Suffix

instance ToJSON Card where
  toJSON Card { fieldz = fs } = object [ "fields" .= mkObjectFromPairable fieldToPair fs ]
--            (p, mkObjectFromPairable attributeToPair as)

-- Parse an card.
card :: GenParser Char st Card
card = do { openCard
          ; fs <- field `manyTill` (try closeCard)
          ; return Card { fieldz = combineItems fs }
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
