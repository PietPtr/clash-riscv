{-# LANGUAGE RecordWildCards #-}

module Execute where

import Clash.Prelude
import BaseTypes
import Instructions
import ALUFunctions

data State = State
    { pc :: PC
    , registers :: RegisterBank
    , intermediateRegs :: Vec 2 RegisterValue
    } deriving (Show, Eq)

nullstate :: State
nullstate = State {pc=0, registers=emptyregs, intermediateRegs=0:>0:>Nil}

data MachineCode = MachineCode
    { operand2 :: RegisterValue
    , operand1 :: RegisterValue
    , destination :: Unsigned 6 -- Larger than RegisterID to allow intermediate registers for e.g. mem calcs
    , arithmetic :: (RegisterValue -> RegisterValue -> RegisterValue)
    }

-- Constant names for the intermediate registers
cJUMPREG :: Unsigned 6
cJUMPREG = 32

nop :: MachineCode
nop = MachineCode
    { operand2 = 0
    , operand1 = 0
    , destination = 0
    , arithmetic = (+)
    }

execute :: State -> Instruction -> State
execute state instruction = state'
    where
        State{..} = state
        MachineCode{..} = buildCode instruction registers

        state' = (State {pc=pc', registers=registers', intermediateRegs=intermediateRegs'})

        pc' = pc
        useIntermediateBank = testBit (trace (show ("de hallo", destination)) destination) 5
        registers' = if (useIntermediateBank)
            then registers
            else replace (destination .&. 0b11111) (arithmetic operand2 operand1) registers

        intermediateRegs' = if (useIntermediateBank)
            then replace (destination .&. 0b1) (arithmetic operand2 operand1) intermediateRegs
            else intermediateRegs


buildCode :: Instruction -> RegisterBank -> MachineCode
buildCode (RType op rs2 rs1 rd) registers =
    nop {operand2 = registers !! rs2, operand1 = registers !! rs1, destination = resize rd, arithmetic = arithmeticFunction}
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
buildCode (IType op imm rs1 rd) registers =
    nop {operand2 = imm,
        operand1 = registers !! rs1,
        destination = resize dest,
        arithmetic = arithmeticFunction}
    where
        dest= case op of
            JALR -> cJUMPREG
            _ -> resize rd
        arithmeticFunction = case op of
            JALR -> jumpAndLinkReg
            -- LB ->
            -- LH ->
            -- LW ->
            -- LBU ->
            -- LHU ->
            ADDI -> (+)
            SLTI -> setLessThan
            SLTIU -> setLessThanU
            XORI -> xor
            ORI -> (.|.)
            ANDI -> (.&.)
            SLLI -> shiftLeftLogical
            SRLI -> shiftRightLogical
            SRAI -> shiftRightArithmetical

-- updateRegisters :: MachineCode -> State -> State
-- updateRegisters machineCode state = state'
--     where
--         MachineCode{..} = machineCode
--         State{..} = state
--
-- updateRegisters :: MachineCode -> State -> RegisterBank
-- updateRegisters machineCode state = replace rd (arithmetic operand2 operand1) bank
--     where
--         MachineCode{..} = machineCode
--         State{..} = state
--         bank = if (testBit rd 5) then intermediateRegs else registers
