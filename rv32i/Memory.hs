module Memory where

import Clash.Prelude
import BaseTypes
import Instructions
import Execute

readMem :: Address -> Memory -> RegisterValue
readMem addr mem = mem !! addr

writeMem :: Address -> RegisterValue -> Memory -> Memory
writeMem addr value mem = replace addr value mem

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

-- TODO: This may not be the exact behaviour risc-v is going for, since I assumed sign extension, perhaps they mean overriding...
-- TODO: shifting the address to divide it by for for this memory model seems weird
memoryAccess (SType SB _ _ _) ExecutionResult{result=address, value=value} mem =
    (writeMem (conv (address `shiftR` 2)) (signExtend byte) mem, 0)
    where byte :: Signed 8 = truncateB $ value
memoryAccess (SType SH _ _ _) ExecutionResult{result=address, value=value} mem =
    (writeMem (conv (address `shiftR` 2)) (signExtend halfword) mem, 0)
    where halfword :: Signed 16 = truncateB $ value
memoryAccess (SType SW _ _ _) ExecutionResult{result=address, value=value} mem =
    (writeMem (conv (address `shiftR` 2)) value mem, 0)

memoryAccess _ _ mem = (mem, 0)
