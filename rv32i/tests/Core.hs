module CoreTests where

import Clash.Prelude
import Core
import qualified Data.List as L

test = mapM_ print $ L.tail $ sampleN @System (256 + 70) core