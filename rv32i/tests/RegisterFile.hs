module RegisterFileTests where

import Clash.Prelude
import Test
import RegisterFile
import qualified Data.List as L

test = mapM_ print $ simulate @System system [
        Two 2 1,
        None,
        Two 4 3,
        None,
        One 5,
        One 6,
        One 7,
        One 8,
        One 9,
        One 10,
        Two 12 11,
        Two 12 11,
        Two 14 13,
        None
    ]

test2 = mapM_ print $ L.zip testlist $ simulate @System system testlist
    where testlist = [
            One 2,
            One 2,
            Two 2 8,
            None, 
            Two 8 10,
            None,
            Two 8 11,
            None,
            None, 
            None,
            None ]