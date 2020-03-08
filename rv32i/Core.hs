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


data SystemState = SystemState
    { pc :: PC
    , registers :: RegisterBank
    , memory :: Memory
} deriving (Show, NFDataX, Generic)

type Tick = Unsigned 0

initialState = SystemState
    { pc = 84
    , registers = testregs
    , memory = testmem
    }

core :: SystemState -> Tick -> SystemState
core SystemState{..} _ = trace (showProcess (instruction, fetched, decoded, executed, (memory', memValue), InternalRegs{pc=pc', registers=registers'}))
    state'
    where
        state' = SystemState {pc = pc', registers = registers', memory = memory'}

        InternalRegs{pc=pc', registers=registers'} = writeback partialState decoded (result executed) memValue
        (memory', memValue) = memoryAccess decoded executed memory
        executed = execute partialState decoded
        decoded = decode fetched
        fetched = fetch instruction
        instruction :: Unsigned 32 = conv $ memory !! (pc `shiftR` 2)

        partialState = InternalRegs {pc = pc, registers = registers}



output :: SystemState -> PC
output SystemState{..} = pc `shiftR` 2

mooreCore = moore @System core output initialState

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System Tick
    -> Signal System PC
topEntity = exposeClockResetEnable mooreCore

sim n = mapM_ print $ L.take n $ simulate @System mooreCore [0,0..]

forever = 99999999999999999999
