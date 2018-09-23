module Debog ( niceListTrace ) where

import Debug.Trace (trace)
import Out (niceList)

niceListTrace :: (Show a) => String -> [a] -> [a]
niceListTrace remark as = trace (remark ++ ":\n" ++ niceList as ++ "\n----------\n") as
