{-# LANGUAGE RecordWildCards #-}
module System where

import Clash.Prelude

type Address = Unsigned 5
type Value = Unsigned 8
type Bank = Vec 32 Value

data MemState = MemState Bank Value
    deriving (Show, Generic, NFDataX)

data Mode = Multing | Idling
    deriving (Show, Generic, NFDataX)

data MultState = MultState
    { address :: Address
    , mode :: Mode
    } deriving (Show, Generic, NFDataX)

emptyRAM :: Bank
emptyRAM = replicate d32 3

ram :: MemState -> (Address, Maybe Value) -> (MemState, Value)
ram (MemState bank prev) (addr, value) = ((MemState bank' prev'), prev)
    where
        bank' = case value of
            (Just v) -> replace addr v bank
            Nothing  -> bank

        prev' = bank !! addr

ramModule = mealy @System ram (MemState emptyRAM 0)


multiplier :: MultState -> Value -> (MultState, (Address, Maybe Value, Mode))
multiplier state value = (state, (addr, val, mode state))
    where
        (state', (addr, val)) = case (mode state) of
            Multing -> multiMulting state value
            Idling -> multiIdling state value

multiMulting :: MultState -> Value -> (MultState, (Address, Maybe Value))
multiMulting state value = (state', (address state, Just value'))
    where
        state' = state { mode = Idling }
        value' = value * 2

multiIdling :: MultState -> Value -> (MultState, (Address, Maybe Value))
multiIdling state value = (state', (address', Nothing))
    where
        state' = MultState { address = address', mode = mode' }
        (mode', address') = if (testBit value 0)
            then (Multing, addr)
            else (Idling, addr + 1)
        addr = address state


multModule = mealy @System multiplier (MultState { address = 0, mode = Idling })

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Address, Maybe Value)
    -> Signal System (Value)
topEntity = exposeClockResetEnable ramModule
