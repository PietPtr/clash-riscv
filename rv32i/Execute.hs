{-# LANGUAGE RecordWildCards #-}

module Execute where

import Clash.Prelude
import BaseTypes
import Instructions
import ALUFunctions

data InternalRegs = InternalRegs
    { pc        :: PC
    , registers :: RegisterBank
    } deriving (Show, Eq, NFDataX, Generic)

nullstate :: InternalRegs
nullstate = InternalRegs {pc=0, registers=emptyregs}

data ControlCode = ControlCode
    { operand2 :: RegisterValue
    , operand1 :: RegisterValue
    , fbinary  :: RegisterValue -> RegisterValue -> RegisterValue
    }

nop :: ControlCode
nop = ControlCode
    { operand2 = 0
    , operand1 = 0
    , fbinary  = (\a b -> a)
    }

data ExecutionResult = ExecutionResult
    { result :: RegisterValue
    , op2    :: RegisterValue -- TODO: How can I give this a better name? (operand2 is already taken)
    } deriving (Show)

execute :: Instruction -> RegisterBank -> PC -> ExecutionResult
execute instruction registers pc = ExecutionResult{result = result, op2 = operand2 }
    where
        ControlCode{..} = buildCode instruction registers pc
        result = fbinary operand2 operand1


readRegister :: RegisterID -> RegisterBank -> RegisterValue
readRegister 0 _ = 0
readRegister reg bank = bank !! reg


buildCode :: Instruction -> RegisterBank -> PC -> ControlCode
buildCode (RType op rs2 rs1 rd) registers pc =
    ControlCode
        { operand2 = readRegister rs2 registers
        , operand1 = readRegister rs1 registers
        , fbinary  = arithmeticFunction }
    where
        arithmeticFunction = case op of
            ADD  -> (+)
            SUB  -> (-)
            SLL  -> shiftLeftLogical
            SLT  -> setLessThan
            SLTU -> setLessThanU
            XOR  -> xor
            SRL  -> shiftRightLogical
            SRA  -> shiftRightArithmetical
            OR   -> (.|.)
            AND  -> (.&.)

buildCode (IType op imm rs1 rd) registers pc =
    ControlCode
        { operand2 = imm
        , operand1 = readRegister rs1 registers
        , fbinary  = arithmeticFunction }
    where
        arithmeticFunction = case op of
            JALR  -> jumpAndLinkReg
            LB    -> (+)
            LH    -> (+)
            LW    -> (+)
            LBU   -> (+)
            LHU   -> (+)
            ADDI  -> (+)
            SLTI  -> setLessThan
            SLTIU -> setLessThanU
            XORI  -> xor
            ORI   -> (.|.)
            ANDI  -> (.&.)
            SLLI  -> shiftLeftLogical
            SRLI  -> shiftRightLogical
            SRAI  -> shiftRightArithmetical

buildCode (SType op imm rs2 rs1) registers pc =
    ControlCode
        { operand2 = readRegister rs2 registers
        , operand1 = readRegister rs1 registers
        , fbinary  = arithmeticFunction }
    where
        arithmeticFunction = case op of
            BEQ  -> \o2 o1 -> if (o2 == o1) then imm else 0
            BNE  -> \o2 o1 -> if (o2 /= o1) then imm else 0
            BLT  -> \o2 o1 -> if (o1 < o2) then imm else 0
            BGE  -> \o2 o1 -> if (o2 >= o2) then imm else 0
            BLTU -> \o2 o1 -> if (compareUnsigned o2 o1 (<)) then imm else 0
            BGEU -> \o2 o1 -> if (compareUnsigned o2 o1 (>)) then imm else 0
            SB   -> \o2 o1 -> imm+o1
            SH   -> \o2 o1 -> imm+o1
            SW   -> \o2 o1 -> imm+o1

buildCode (UType op imm rd) registers pc =
    nop { operand2 = imm
        , fbinary  = arithmeticFunction}
    where
        arithmeticFunction = case op of
            LUI   -> loadUpperImmediate
            AUIPC -> \o2 o1 -> loadUpperImmediate o2 o1 + (conv pc)
            JAL   -> \_ _ -> (conv pc) + imm

buildCode UnknownType _ _ = nop
