module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits
import qualified Debug.Trace

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import OpCodes
import BaseTypes
import Decode

trace = Debug.Trace.trace
