{-# LANGUAGE RecordWildCards #-}
module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits

import BaseTypes

import Instructions
import Fetch
import Decode
import ALUFunctions
import Execute
import Memory
import Writeback

import Debug

type Tick = Unsigned 0

data SystemState = SystemState
    { pc        :: PC
    , registers :: RegisterBank
    , memory    :: Memory
} deriving (Show, NFDataX, Generic)


initialState = SystemState
    { pc        = 21
    , registers = testregs
    , memory    = testmem
    }

core :: SystemState -> Tick -> SystemState
<<<<<<< HEAD
core SystemState{..} _ = --trace (showProcess (instruction, fetched, decoded, executed, (memory', memValue), InternalRegs{pc=pc', registers=registers'}))
=======
core SystemState{..} _ = trace (showProcess (instruction, fetched, decoded, executed, (memory', memValue), (pc', registers')))
>>>>>>> 11b9b92ec63b990bd7131a2c3e3e2d56d7056eff
    state'
    where
        state' = SystemState {pc = pc', registers = registers', memory = memory'}

        (registers', pc')   = writeback registers pc decoded (result executed) memValue
        (memory', memValue) = memoryAccess memory decoded executed
        executed            = execute registers pc decoded
        decoded             = decode fetched
        fetched             = fetch instruction
        instruction         = conv $ memory !! pc


output :: SystemState -> PC
output SystemState{..} = pc

mooreCore = moore @System core output initialState

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System Tick
    -> Signal System PC
topEntity = exposeClockResetEnable mooreCore


sim n = mapM_ print $ L.take n $ simulate @System mooreCore (cycle [0])

forever = 99999999999999999999
