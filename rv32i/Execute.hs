{-# LANGUAGE RecordWildCards #-}

module Execute where

import Clash.Prelude
import BaseTypes
import Instructions
import ALUFunctions

data State = State
    { pc :: PC
    , registers :: RegisterBank
    } deriving (Show, Eq)

nullstate :: State
nullstate = State {pc=0, registers=emptyregs}

data ControlCode = ControlCode
    { operand2 :: RegisterValue
    , operand1 :: RegisterValue
    , fbinary :: (RegisterValue -> RegisterValue -> RegisterValue)
    }

nop :: ControlCode
nop = ControlCode
    { operand2 = 0
    , operand1 = 0
    , fbinary = (\a b -> a)
    }

-- TODO: Names are too vague
-- TODO: Change type to something less general
data ExecutionResult = ExecutionResult
    { result :: RegisterValue
    , value :: RegisterValue
    } deriving (Show)

execute :: State -> Instruction -> ExecutionResult
execute state instruction = ExecutionResult{result=result, value=operand2 }
    where
        ControlCode{..} = buildCode instruction state
        result = fbinary operand2 operand1


readRegister :: RegisterID -> RegisterBank -> RegisterValue
readRegister 0 _ = 0
readRegister reg bank = bank !! reg

buildCode :: Instruction -> State -> ControlCode
buildCode (RType op rs2 rs1 rd) State{registers=registers, pc=pc} =
    nop {operand2 = readRegister rs2 registers,
        operand1 = readRegister rs1 registers,
        fbinary = arithmeticFunction}
    where
        arithmeticFunction = case op of
            ADD -> (+)
            SUB -> (-)
            SLL -> shiftLeftLogical
            SLT -> setLessThan
            SLTU -> setLessThanU
            XOR -> xor
            SRL -> shiftRightLogical
            SRA -> shiftRightArithmetical
            OR -> (.|.)
            AND -> (.&.)

buildCode (IType op imm rs1 rd) State{registers=registers, pc=pc} =
    nop {operand2 = imm,
        operand1 = readRegister rs1 registers,
        fbinary = arithmeticFunction}
    where
        arithmeticFunction = case op of
            JALR -> jumpAndLinkReg
            LB -> (+)
            LH -> (+)
            LW -> (+)
            LBU -> (+)
            LHU -> (+)
            ADDI -> (+)
            SLTI -> setLessThan
            SLTIU -> setLessThanU
            XORI -> xor
            ORI -> (.|.)
            ANDI -> (.&.)
            SLLI -> shiftLeftLogical
            SRLI -> shiftRightLogical
            SRAI -> shiftRightArithmetical

buildCode (SType op imm rs2 rs1) State{registers=registers, pc=pc} =
    nop {operand2 = readRegister rs2 registers,
        operand1 = readRegister rs1 registers,
        fbinary = arithmeticFunction}
    where
        arithmeticFunction = case op of
            BEQ -> \o2 o1 -> if (o2 == o1) then (imm + (conv pc)) else (conv pc)
            BNE -> \o2 o1 -> if (o2 /= o1) then (imm + conv pc) else conv pc
            BLT -> \o2 o1 -> if (o1 < o2) then (imm + conv pc) else conv pc
            BGE -> \o2 o1 -> if (o2 >= o2) then (imm + conv pc) else conv pc
            BLTU -> \o2 o1 -> if (compareUnsigned o2 o1 (<)) then (imm + conv pc) else conv pc
            BGEU -> \o2 o1 -> if (compareUnsigned o2 o1 (>)) then (imm + conv pc) else conv pc
            SB -> (\o2 o1 -> imm+o1)
            SH -> (\o2 o1 -> imm+o1)
            SW -> (\o2 o1 -> imm+o1)

buildCode (UType op imm rd) State{registers=registers, pc=pc} =
    nop {operand2 = imm,
        fbinary = arithmeticFunction}
    where
        arithmeticFunction = case op of
            LUI -> loadUpperImmediate
            AUIPC -> \o2 o1 -> loadUpperImmediate o2 o1 + (conv pc)
            JAL -> \_ _ -> (conv pc) + imm
