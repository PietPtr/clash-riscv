{-# LANGUAGE RecordWildCards #-}
module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits

import Instructions
import BaseTypes
import Fetch
import Decode
import ALUFunctions
import Execute
import Memory

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

showBin x = showIntAtBase 2 intToDigit x ""

run x = out
    where
        out = trace ("executed: " L.++ show executed) memory decoded executed emptymem
        executed = trace ("decoded: " L.++ show decoded) execute nullstate decoded
        decoded = trace ("parsed: " L.++ show parsed) decode parsed
        parsed = trace ("input: " L.++ showBin (op x)) parse (op x)
