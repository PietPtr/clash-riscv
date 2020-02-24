module Memory where

import Clash.Prelude
import BaseTypes
import Instructions
import Execute

readMem :: Address -> Memory -> RegisterValue
readMem addr mem = mem !! addr

writeMem :: Address -> RegisterValue -> Memory -> Memory
writeMem addr value mem = replace addr value mem

memory :: Instruction -> ExecutionResult -> Memory -> (Memory, RegisterValue)
memory (IType LB _ _ _) ExecutionResult{result=address} mem = (mem, signExtend byte)
    where byte :: Signed 8 = truncateB $ readMem (conv address) mem
memory (IType LBU _ _ _) ExecutionResult{result=address} mem = (mem, zeroExtend byte)
    where byte :: Signed 8 = truncateB $ readMem (conv address) mem
memory (IType LH _ _ _) ExecutionResult{result=address} mem = (mem, signExtend halfword)
    where halfword :: Signed 16 = truncateB $ readMem (conv address) mem
memory (IType LHU _ _ _) ExecutionResult{result=address} mem = (mem, zeroExtend halfword)
    where halfword :: Signed 16 = truncateB $ readMem (conv address) mem
memory (IType LW _ _ _) ExecutionResult{result=address} mem = (mem, readMem (conv address) mem)

-- TODO: This may not be the exact behaviour risc-v is going for, since I assumed sign extension, perhaps they mean overriding...
memory (SType SB _ _ _) ExecutionResult{result=address, value=value} mem = (writeMem (conv address) (signExtend byte) mem, 0)
    where byte :: Signed 8 = truncateB $ value
memory (SType SH _ _ _) ExecutionResult{result=address, value=value} mem = (writeMem (conv address) (signExtend halfword) mem, 0)
    where halfword :: Signed 16 = truncateB $ value
memory (SType SW _ _ _) ExecutionResult{result=address, value=value} mem = (writeMem (conv address) value mem, 0)

memory _ _ mem = (mem, 0)
