module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import Instructions
import BaseTypes
import Parse
import Decode
