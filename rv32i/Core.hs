module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits

import Instructions
import BaseTypes
import Fetch
import Decode
import Execute

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

showBin x = showIntAtBase 2 intToDigit x ""

run x = result
    where
        result = trace (show decoded) execute nullstate decoded
        decoded = trace (show parsed) decode parsed
        parsed = trace (showBin (op x)) parse (op x)
