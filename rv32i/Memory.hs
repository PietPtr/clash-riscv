{-# LANGUAGE RecordWildCards #-}
module Memory where

import Clash.Prelude
import BaseTypes
import Instructions
import Execute
import qualified Data.List as L

readMem :: AlignedAddress -> Memory -> RegisterValue
readMem addr memory = memory !! addr

writeMem :: AlignedAddress -> RegisterValue -> Memory -> Memory
writeMem addr value memory = replace addr value memory

{--
TODO: In the current implementation only words aligned to 4 bytes can be retrieved
from memory. This is because memory is a Vector of 32-bit words which are retrieved
or written individually. This should be changed to some array of 8-bit words so
that they can be written to as bytes. The current implementation is wrong.

In section 2.6 of the specification it's specified that misalgned reads or writes
should throw an exception. "Loads and stores where the effective address is not
naturally aligned to the referenced datatype (...) have behavior dependent on
the EEI.", so this is probably fine for now.
--}
memoryAccess :: Memory -> Instruction -> ExecutionResult -> (Memory, RegisterValue)
memoryAccess memory instruction ExecutionResult{result=address, op2=op2} =
    case instruction of
        (IType operation _ _ _) -> case operation of
            LB  -> (memory, signExtend byte)
            LBU -> (memory, zeroExtend byte)
            LH  -> (memory, signExtend halfword)
            LHU -> (memory, zeroExtend halfword)
            LW  -> (memory, word)
            _   -> (memory, 0)
        (SType operation _ _ _) -> case operation of
            SB -> (writeMem address' (signExtend byte) memory, 0)
            SH -> (writeMem address' (signExtend halfword) memory, 0)
            SW -> (writeMem address' op2 memory, 0)
            _  -> (memory, 0)
        _                       -> (memory, 0)
    where
         byte       :: Signed 8       = truncateB $ readMem (conv (address `shiftR` 2)) memory
         halfword   :: Signed 16      = truncateB $ readMem (conv (address `shiftR` 2)) memory
         word       :: RegisterValue  = readMem (conv (address `shiftR` 2)) memory

         address'   :: AlignedAddress = conv (address `shiftR` 2)


{--
> shifting the address to divide it by four for this memory model seems weird.
It's fine since we need to address a vector, this could change when we attach
an 'actual' RAM module
-}


blockramAccess
    :: SystemState
    -> Instruction
    -> ExecutionResult
    -> (SystemState, (Maybe Address, WriteData))
blockramAccess state instruction ExecutionResult{result=address, op2=op2} =
    case instruction of
        (IType operation _ _ _) -> if (operation `elem` [LB, LBU, LH, LHU, LW])
            then (state {phase = Memory}, (Just address', Nothing))
            else nop
        (SType operation _ _ _) -> if (operation `elem` [SB, SH, SW])
            then (state, (Nothing, Just (conv address, op2))) -- TODO change op2 to the halfword/byte stuff
            else nop
        _ -> nop
    where
        address' = conv address
        nop = (state, (Nothing, Nothing))

blockramExecute
    :: Instruction
    -> RegisterValue
    -> RegisterValue
blockramExecute instruction memValue =
    case instruction of
        (IType operation _ _ _) -> case operation of
            LB  -> signExtend byte
            LBU -> zeroExtend byte
            LH  -> signExtend halfword
            LHU -> zeroExtend halfword
            LW  -> memValue
        _ -> 0
    where
        byte       :: Signed 8      = truncateB $ memValue
        halfword   :: Signed 16     = truncateB $ memValue
