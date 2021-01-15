module RegisterFileTests where

import Clash.Prelude
import Test
import RegisterFile

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
