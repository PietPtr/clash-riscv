module Memory where

import Clash.Prelude
import BaseTypes
import Instructions
import Execute

readMem :: Address -> Memory -> RegisterValue
readMem addr mem = mem !! addr

writeMem :: Address -> RegisterValue -> Memory -> Memory
writeMem addr value mem = replace addr value mem

{--
TODO: In the current implementation only words aligned to 4 bytes can be retrieved
from memory. This is because memory is a Vector of 32-bit words which are retrieved
or written individually. This should be changed to some array of 8-bit words so
that they can be written to as bytes. The current implementation is wrong.
--}
memoryAccess :: Instruction -> ExecutionResult -> Memory -> (Memory, RegisterValue)
memoryAccess (IType LB _ _ _) ExecutionResult{result=address} mem = (mem, signExtend byte)
    where byte :: Signed 8 = truncateB $ readMem (conv (address `shiftR` 2)) mem
memoryAccess (IType LBU _ _ _) ExecutionResult{result=address} mem = (mem, zeroExtend byte)
    where byte :: Signed 8 = truncateB $ readMem (conv (address `shiftR` 2)) mem
memoryAccess (IType LH _ _ _) ExecutionResult{result=address} mem = (mem, signExtend halfword)
    where halfword :: Signed 16 = truncateB $ readMem (conv (address `shiftR` 2)) mem
memoryAccess (IType LHU _ _ _) ExecutionResult{result=address} mem = (mem, zeroExtend halfword)
    where halfword :: Signed 16 = truncateB $ readMem (conv (address `shiftR` 2)) mem
memoryAccess (IType LW _ _ _) ExecutionResult{result=address} mem = (mem, readMem (conv (address `shiftR` 2)) mem)

{--
> shifting the address to divide it by four for this memory model seems weird.
It's fine since we need to address a vector, this could change when we attach
an 'actual' RAM module
-}
memoryAccess (SType SB _ _ _) ExecutionResult{result=address, op2=op2} mem =
    (writeMem (conv (address `shiftR` 2)) (signExtend byte) mem, 0)
    where byte :: Signed 8 = truncateB $ op2
memoryAccess (SType SH _ _ _) ExecutionResult{result=address, op2=op2} mem =
    (writeMem (conv (address `shiftR` 2)) (signExtend halfword) mem, 0)
    where halfword :: Signed 16 = truncateB $ op2
memoryAccess (SType SW _ _ _) ExecutionResult{result=address, op2=op2} mem =
    (writeMem (conv (address `shiftR` 2)) op2 mem, 0)

memoryAccess _ _ mem = (mem, 0)
