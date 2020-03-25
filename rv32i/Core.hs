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

-- data SystemState = SystemState
--     { pc        :: PC
--     , registers :: RegisterBank
--     , memory    :: Memory
-- } deriving (Show, NFDataX, Generic)
--
--
-- initialState = SystemState
--     { pc        = 84
--     , registers = testregs
--     , memory    = testmem
--     }
--
-- core :: SystemState -> Tick -> SystemState
-- core SystemState{..} _ = trace (showProcess (instruction, fetched, decoded, executed, (memory', memValue), InternalRegs{pc=pc', registers=registers'}))
--     state'
--     where
--         state' = SystemState {pc = pc', registers = registers', memory = memory'}
--
--         InternalRegs{pc=pc', registers=registers'} = writeback internalRegs decoded (result executed) memValue
--         (memory', memValue) = memoryAccess decoded executed memory
--         executed = execute decoded registers pc
--         decoded = decode fetched
--         fetched = fetch instruction
--         instruction :: Unsigned 32 = conv $ memory !! (pc `shiftR` 2)
--
--         internalRegs = InternalRegs {pc = pc, registers = registers}



initialState = SystemState
    { pc        = 84
    , registers = testregs
    , phase     = Executing
    }


core :: (SystemState, PC) -> RegisterValue -> ((SystemState, PC), Output)
core (state, oldpc) memValue = output
    where
        SystemState{..} = state

        output = case phase of
            Executing -> execution state oldpc memValue
            _ -> error "not implemented"



execution :: SystemState -> PC -> RegisterValue -> ((SystemState, PC), Output)
execution state oldpc instruction = ((state', pc) , (oldpc, Nothing))
    where
        SystemState{..} = state

        state' = trace (show (decoded, oldpc, pc, pc')) SystemState{pc=pc', registers=registers', phase=phase'}
        output = (readAddress, writedata)

        readAddress = case address of
            Just addr -> addr
            Nothing -> pc' `shiftR` 2

        InternalRegs{pc=pc', registers=registers'} = case phase' of
            Executing -> writeback internalRegs decoded (result executed) 0
            Memory -> internalRegs -- delay writeback by one cycle

        -- TODO: niet de hele state returnen als je alleen maar de phase aanpast
        (SystemState{phase=phase'}, (address, writedata)) = blockramAccess state decoded executed
        executed = execute decoded registers oldpc
        decoded = decode fetched
        fetched = fetch $ conv (trace (show instruction) instruction)

        -- trace (showProcess' (conv instruction, fetched, decoded, executed, SystemState{pc=pc', registers=registers', phase=phase'}, output))

        internalRegs = InternalRegs {pc = oldpc, registers = registers}

loadstore :: SystemState -> SystemState
loadstore = id


output :: SystemState -> PC
output SystemState{..} = pc `shiftR` 2


system tick = bundle (readAddress, dataOut)
    where
        memOut = blockRam testmem readAddress dataOut
        (readAddress, dataOut) = mealyB core (initialState, 0) memOut

-- mooreCore = moore @System core output initialState
--
-- topEntity
--     :: Clock System
--     -> Reset System
--     -> Enable System
--     -> Signal System Tick
--     -> Signal System PC
-- topEntity = exposeClockResetEnable mooreCore

-- sim n = mapM_ print $ L.take n $ simulate @System mooreCore (cycle [0])

forever = 99999999999999999999
