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
core SystemState{..} _ = --trace (showProcess (instruction, fetched, decoded, executed, (memory', memValue), InternalRegs{pc=pc', registers=registers'}))
    state'
    where
        state' = SystemState {pc = pc', registers = registers', memory = memory'}

        InternalRegs{pc=pc', registers=registers'} = writeback internalRegs decoded (result executed) memValue
        (memory', memValue) = memoryAccess decoded executed memory
        executed = execute decoded registers pc
        decoded = decode fetched
        fetched = fetch instruction
        instruction :: Unsigned 32 = conv $ memory !! pc

        internalRegs = InternalRegs {pc = pc, registers = registers}

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
