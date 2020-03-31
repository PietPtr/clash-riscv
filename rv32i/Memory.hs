{-# LANGUAGE RecordWildCards #-}
module Memory where

import Clash.Prelude
import BaseTypes
import Instructions
import Execute
import qualified Data.List as L

readMem :: AlignedAddress -> Memory -> RegisterValue
readMem addr mem = mem !! addr

writeMem :: AlignedAddress -> RegisterValue -> Memory -> Memory
writeMem addr value mem = replace addr value mem

{--
TODO: In the current implementation only words aligned to 4 bytes can be retrieved
from memory. This is because memory is a Vector of 32-bit words which are retrieved
or written individually. This should be changed to some array of 8-bit words so
that they can be written to as bytes. The current implementation is wrong.
--}
memoryAccess :: Instruction -> ExecutionResult -> Memory -> (Memory, RegisterValue)
memoryAccess instruction ExecutionResult{result=address, op2=op2} mem =
    case instruction of
        (IType operation _ _ _) -> case operation of
            LB  -> (mem, signExtend byte)
            LBU -> (mem, zeroExtend byte)
            LH  -> (mem, signExtend halfword)
            LHU -> (mem, zeroExtend halfword)
            LW  -> (mem, word)
            _ -> (mem, 0)
        (SType operation _ _ _) -> case operation of
            SB -> (writeMem address' (signExtend byte) mem, 0)
            SH -> (writeMem address' (signExtend halfword) mem, 0)
            SW -> (writeMem address' op2 mem, 0)
            _ -> (mem, 0)
        _ -> (mem, 0)
    where
         byte       :: Signed 8       = truncateB $ readMem (conv (address `shiftR` 2)) mem
         halfword   :: Signed 16      = truncateB $ readMem (conv (address `shiftR` 2)) mem
         word       :: RegisterValue  = readMem (conv (address `shiftR` 2)) mem

         address'   :: AlignedAddress = conv (address `shiftR` 2)


{--
> shifting the address to divide it by four for this memory model seems weird.
It's fine since we need to address a vector, this could change when we attach
an 'actual' RAM module
-}
