{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Files where

import System.IO
import Control.Exception

safeOpenFile :: FilePath -> IOMode -> IO (Either IOError Handle)
safeOpenFile f m = try ( openFile f m )
