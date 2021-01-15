module IFTests where

import Clash.Prelude
import IF


test = mapM_ print $ simulate @System system [
        (False, 0xfd010113, Nothing),
        (False, 0x02812623, Nothing),
        (False, 0x03010413, Nothing),
        (True, 0x00000000, Nothing),
        (True, 0x00000000, Nothing),
        (True, 0x00000000, Nothing),
        (False, 0x02812623, Nothing),
        (False, 0x03010413, Just 10),
        (True, 0x00000000, Nothing),
        (True, 0x00000000, Nothing)
        
    ]