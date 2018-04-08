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

Don't know why the geniuses at Apple did this.
If some of the code here gets ugly, my excuse is that the input is ugly.

-}

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line = 
    do result <- cells
       eol                       -- end of line
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: GenParser Char st [String]
cells = do first <- cellContent
           next <- remainingCells
           return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or EOL
cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

--------------------------------------------------------------------------------
type Component = String
-- A component is a string of characters other than ':' or ';'
component :: GenParser Char st Component
component = many (noneOf ":;\n")

-- A list of components, separated by ':' or ';'
components :: GenParser Char st [Component]
components = sepBy component (oneOf ";:")

-- A field is a list of components followed by an end of line
field :: GenParser Char st [Component] -- Boo hoo, not a one liner :(
field = do { cs <- components
           ; char '\n'
           ; return cs }

-- Run the address book parse on a test input
test :: String -> Either ParseError [Component]
test testCase = parse field "(unknown)" testCase

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

main :: IO ()
main = do
  putStrLn "hello world"
