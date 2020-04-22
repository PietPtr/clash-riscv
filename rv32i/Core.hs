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
--     { pc        = 21
--     , registers = testregs
--     , memory    = testmem
--     }
--
-- core :: SystemState -> Tick -> SystemState
-- core SystemState{..} _ = -- trace (showProcess (instruction, fetched, decoded, executed, (memory', memValue), (pc', registers')))
--     state'
--     where
--         state' = SystemState {pc = pc', registers = registers', memory = memory'}
--
--         (registers', pc')   = writeback registers pc decoded (result executed) memValue
--         (memory', memValue) = memoryAccess memory decoded executed
--         executed            = execute registers pc decoded
--         decoded             = decode fetched
--         fetched             = fetch instruction
--         instruction         = conv $ memory !! pc


initialPC = 21

initialState = SystemState
    { pc        = initialPC
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
execution state oldpc instruction = ((state', pc) , (conv oldpc, Nothing))
    where
        SystemState{..} = state

        state' = trace (show (decoded, oldpc, pc, pc')) SystemState{pc=pc', registers=registers', phase=phase'}
        output = (readAddress, writedata)

        readAddress = case address of
            Just addr -> addr
            Nothing -> conv pc'

        (registers', pc') = case phase' of
            Executing -> writeback registers oldpc decoded (result executed) 0
            Memory -> (registers, pc) -- delay writeback by one cycle

        -- TODO: niet de hele state returnen als je alleen maar de phase aanpast
        (SystemState{phase=phase'}, (address, writedata)) = blockramAccess state decoded executed
        executed = execute registers oldpc decoded
        decoded = decode fetched
        fetched = fetch $ conv (trace (show instruction) instruction)

        -- trace (showProcess' (conv instruction, fetched, decoded, executed, SystemState{pc=pc', registers=registers', phase=phase'}, output))


loadstore :: SystemState -> SystemState
loadstore = id


output :: SystemState -> PC
output SystemState{..} = pc


system tick = bundle (readAddress, dataOut)
    where
        memOut = blockRam testmem readAddress dataOut
        (readAddress, dataOut) = mealyB core (initialState, initialPC) memOut


-- mooreCore = moore @System core output initialState
--
-- topEntity
--     :: Clock System
--     -> Reset System
--     -> Enable System
--     -> Signal System Tick
--     -> Signal System PC
-- topEntity = exposeClockResetEnable mooreCore
--
--
-- sim n = mapM_ print $ L.take n $ simulate @System mooreCore (cycle [0])
--
-- forever = 99999999999999999999
