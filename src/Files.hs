{-# LANGUAGE OverloadedStrings #-}
-- Parsing the arguments to the program
module Files ( safeOpenFile ) where

import System.IO (Handle, IOMode, openFile)
import Control.Exception (try)

safeOpenFile :: FilePath -> IOMode -> IO (Either IOError Handle)
safeOpenFile f m = try ( openFile f m )
