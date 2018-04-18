module Test where

import Data.Aeson as Aeson (ToJSON(..), encode)
import Text.ParserCombinators.Parsec
import Data.ByteString.Lazy.Char8 as DBLC8 (ByteString, putStrLn, pack)
import Data.Text as T (Text, pack)
import Data.Either
import Parse

-- Run the address book parse on a test input
test,t :: GenParser Char () a -> String -> Either ParseError a
test p testCase = parse p "(unknown)" testCase
t = test

jsonTest :: (ToJSON a) => GenParser Char () a -> String -> IO DBLC8.ByteString
jsonTest p s = do { let ea = test p s
                        card = fromRight (error "OOOOOOPS") ea
                        jsonout = encode card
                  ; DBLC8.putStrLn jsonout
                  ; return jsonout
                  }

