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
import Data.List (partition, groupBy, find, intercalate, sortOn)
import Data.Maybe (isJust, fromJust)
import RE (isItem, itemNumber)
import Args (FieldNames)

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

{-
The VCF file has an unfortunate encoding for custom names for fields. Here is
and example of a card with custom names for telephone and E-mail fields:
BEGIN:VCARD
VERSION:3.0
PRODID:-//Apple Inc.//Mac OS X 10.13.4//EN
N:Vignesh;Ruthvik;;;
FN:Ruthvik Vignesh
ORG:Kasalukuyang Estudyante;
item1.EMAIL;type=INTERNET;type=pref:vicky.008@gmail.com
item1.X-ABLabel:Dad
item2.TEL;type=pref:8472081772
item2.X-ABLabel:Dad (Vignesh Jeyaraj)
item3.TEL:8474034147
item3.X-ABLabel:Mom (Kasthuri Thangamariappan)
ADR;type=HOME;type=pref:;;16455 SW Estuary Dr. #208;Beaverton;OR;97006;USA
NOTE:Has Immunization Record\n
BDAY:2014-06-09
CATEGORIES:Address Book
UID:5f124cfb15813f50
X-ABUID:77230A65-1FBF-4BF8-B828-BB7CAA8BABF0:ABPerson
END:VCARD

Notice the EMAIL field (item1.Email). This is an E-mail field with the
custom label "Dad". It is split into two lines, thus it will result
in two fields in the encoding of the card.
-}

-- Data structure to represent the information from one part of an item
-- field, i.e. either
-- item2.TEL;type=pref:8472081772
--      <or>
-- item2.X-ABLabel:Dad (Vignesh Jeyaraj)
data FieldItemMember = FieldItemMember
  { matchText :: String -- Matched part of field label, i.e. "item2."
  , afterText :: String -- After the matched portion, i.e. "TEL"
                        -- This is also the "type" of the FieldItem
                        -- when the field is not X-ABLabel
  , itemNum   :: String -- The item number, i.e. "2" from "item2"
  , itemValue :: String -- The value of the item, i.e. "8472081772"
                        -- for a telephone number.
  } deriving (Eq, Show)

-- Build a FieldItemMember from the four fields
mkFieldItemMember :: String -> String -> String -> String -> FieldItemMember
mkFieldItemMember m a inum ival =
  FieldItemMember { matchText = m, afterText = a, itemNum = inum, itemValue = ival }

-- Get the field Item type
getFieldItemType :: FieldItem -> String
getFieldItemType FieldItem { labelMember = l } = afterText l
getFieldItemType BrokenFieldItem { debugData = d } = d
-- Determine if two field items have the same type
sameFieldItemType :: FieldItem -> FieldItem -> Bool
sameFieldItemType f1 f2 = getFieldItemType f1 == getFieldItemType f2
-- Get the value of the attribute
getFieldTypeAttrValue :: FieldItem -> String
getFieldTypeAttrValue FieldItem { valueMember = v } = itemValue v
getFieldTypeAttrValue BrokenFieldItem { debugData = d } = d
-- Build a combined Field from a group of field of the same type (e.g. TEL)
mkGroupFieldItem :: [FieldItem] -> Field
mkGroupFieldItem [] = error "mkGroupFieldItem []"
mkGroupFieldItem fs =
  let fieldItemType = getFieldItemType (head fs)
      attrs = map getFieldTypeAttrValue fs
      smushedAttrs = intercalate "," attrs
  in Field { pangalan = fieldItemType ++ "S"
           , attributes = [mkSimpleAttribute smushedAttrs] }
-- Build a single Field from a group of field of the same type (e.g. TEL)
-- by choosing one of the group (the first one)
mkSingleFieldItem :: [FieldItem] -> Field
mkSingleFieldItem [] = error "mkSingleFieldItem []"
mkSingleFieldItem fs =
  let first = head fs
      fieldItemType = getFieldItemType first
      attr = getFieldTypeAttrValue first
  in Field { pangalan = fieldItemType ++ "1"
           , attributes = [mkSimpleAttribute attr]}

-- All of the data for a Field Item, i.e. a FieldItemMember for both items:
-- item2.TEL;type=pref:8472081772
--       <and>
-- item2.X-ABLabel:Dad (Vignesh Jeyaraj)
data FieldItem = FieldItem { labelMember :: FieldItemMember
                           , valueMember :: FieldItemMember
                           } |
                 BrokenFieldItem { debugData :: String } deriving (Eq, Show)

-- Make a FieldItem from a list of two member fields.
-- The fields come from a list of fields, so a priori we do not know
-- which field is the label and which field is the value.
mkFieldItem :: Field -> Field -> FieldItem
mkFieldItem f1 f2 =
  let (match1, after1, num1) = fieldItemStructure f1
      (match2, after2, num2) = fieldItemStructure f2
  in if (after1 == "X-ABLabel")
     then let attrValue = maybe "NoValue" name (find isSimpleAttribute (attributes f2))
              labelStr = maybe "NoName" name (find isSimpleAttribute (attributes f1))
              labelMem = mkFieldItemMember match1 after1 num1 labelStr
              valueMem = mkFieldItemMember match2 after2 num2 attrValue
          in FieldItem { labelMember = labelMem, valueMember = valueMem }
     else let attrValue = maybe "NoValue" name (find isSimpleAttribute (attributes f1))
              labelStr = maybe "NoName" name (find isSimpleAttribute (attributes f2))
              labelMem = mkFieldItemMember match1 after1 num1 labelStr
              valueMem = mkFieldItemMember match2 after2 num2 attrValue
          in FieldItem { labelMember = labelMem, valueMember = valueMem }

-- Make a field item from a list of two members
mkFieldItemFromList :: [Field] -> FieldItem
mkFieldItemFromList [f1, f2] = mkFieldItem f1 f2
mkFieldItemFromList fs = BrokenFieldItem { debugData = show fs }

-- The following function will find both of these fields and combine
-- them into one field. Similar remarks apply to the telephone fields.
combineItems :: FieldNames -> [Field] -> [Field]
combineItems flg fs =
  let (items, nonItems) = partition isFieldItem fs
      -- Get the item groups, i.e. two fields of the form
      -- item2.TEL;type=pref:8472081772
      -- item2.X-ABLabel:Dad (Vignesh Jeyaraj)
      -- Note that the two members of the item group may come in either order.
      itemGroups :: [[Field]]
      itemGroups = groupBy sameItemNumber (sortOn pangalan items)
      -- Compute the information needed from ieach itemGroup
      fieldItems :: [FieldItem]
      fieldItems = map mkFieldItemFromList itemGroups
      -- Turn the field items into single fields
      combinedFields :: [Field]
      combinedFields = map mkItemField fieldItems
      -- If there is more than one telephone number or Email address,
      -- further group these item groups into a single item that lists
      -- all the Email addresses or telephone numbers.
      fieldItemGroups :: [[FieldItem]]
      fieldItemGroups = groupBy sameFieldItemType (sortOn getFieldItemType fieldItems)
      -- Choose one from each group to make representative Field.
      singleFields :: [Field]
      singleFields = map mkSingleFieldItem fieldItemGroups
      -- Combine each group into an aggregate field item
      combinedItems = map mkGroupFieldItem fieldItemGroups
  in combinedFields ++ nonItems ++ combinedItems ++ singleFields

-- Make an item field from an item FieldItem record
mkItemField :: FieldItem -> Field
mkItemField FieldItem { labelMember = lMem, valueMember = vMem } =
  let labelValue = itemValue lMem
      labelStr = afterText lMem
      attrValue = itemValue vMem
  in Field { pangalan = labelStr ++ "-" ++ labelValue
           , attributes = [mkSimpleAttribute attrValue] }
mkItemField BrokenFieldItem { debugData = d } =
  Field { pangalan = "Broken Field", attributes = [SimpleAttribute { name = d }] }

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
field :: FieldNames -> GenParser Char st Field
field _fns = try urlField <|> nonUrlField
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
card :: FieldNames -> GenParser Char st Card
card fns = do { openCard
              ; fs <- field fns `manyTill` (try closeCard)
              ; return Card { fieldz = combineItems fns fs }
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

vcf :: FieldNames -> GenParser Char st VCF
vcf fns =
  do { es <- card fns `sepEndBy` eol
     ; eof
     ; return VCF { cards = es }
     }
