{-# LANGUAGE RecordWildCards #-}
module Test where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits
import Numeric
import System.Environment

import BaseTypes
import Core
import Debug

convertList :: [String] -> [Signed 32]
convertList stringlist = L.map (readit) stringlist
    where
        readit x = fromIntegral $ fst (readHex x L.!! 0)

makeMem :: [Signed 32] -> Memory
makeMem memdata = L.foldl (\mem unit -> replace (snd unit) (fst unit) mem) (replicate d256 0) (L.zip memdata [0..])

state mem = SystemState
    { pc = 22*4
    , registers = emptyregs
    , memory = mem
    }


sim n mem = L.foldl (\memoire _ -> core memoire 0) (state mem) [0..n]

main a = do
    out <- Test.sim a <$> testMem
    return out
    where
        testMem = ((makeMem . convertList . lines) <$> readFile ("./binaries/mult.bin"))