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

execute :: State -> Instruction -> RegisterValue
execute state instruction = result
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
    nop {operand2 = operand2,
        operand1 = readRegister rs1 registers,
        fbinary = arithmeticFunction}
    where
        operand2 = case op of
            SB -> 0
            SH -> 0
            SW -> 0
            _ -> readRegister rs2 registers
        arithmeticFunction = case op of
            BEQ -> \o2 o1 -> if (o2 == o1) then (imm + pc) else pc
            BNE -> \o2 o1 -> if (o2 /= o1) then (imm + pc) else pc
            BLT -> \o2 o1 -> if (o1 < o2) then (imm + pc) else pc
            BGE -> \o2 o1 -> if (o2 >= o2) then (imm + pc) else pc
            BLTU -> \o2 o1 -> if (compareUnsigned o2 o1 (<)) then (imm + pc) else pc
            BGEU -> \o2 o1 -> if (compareUnsigned o2 o1 (>)) then (imm + pc) else pc
            SB -> (\o2 o1 -> o2+imm+o1)
            SH -> (\o2 o1 -> o2+imm+o1)
            SW -> (\o2 o1 -> o2+imm+o1)

buildCode (UType op imm rd) State{registers=registers, pc=pc} =
    nop {operand2 = imm,
        fbinary = arithmeticFunction}
    where
        arithmeticFunction = case op of
            LUI -> loadUpperImmediate
            AUIPC -> loadUpperImmediate
            JAL -> \_ _ -> pc + imm
