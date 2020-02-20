module Decode where

import Clash.Prelude
import BaseTypes
import OpCodes

type OpCodeField = Unsigned 7
type Funct7 = Unsigned 7
type Funct3 = Unsigned 3

-- Needs improvement or is this a deliberate design choice?
type Imm11'0 = Unsigned 12
type Imm11'5 = Unsigned 7
type Imm4'0 = Unsigned 5
type Imm12 = Unsigned 1
type Imm10'5 = Unsigned 6
type Imm4'1 = Unsigned 4
type Imm11 = Unsigned 1
type Imm31'12 = Unsigned 20
type Imm20 = Unsigned 1
type Imm10'1 = Unsigned 10
type Imm19'12 = Unsigned 8

data InstructionForm =
      RType Funct7 RegisterID RegisterID Funct3 RegisterID OpCodeField
    | IType (Imm11'0) RegisterID Funct3 RegisterID OpCodeField
    | SType (Imm11'5) RegisterID RegisterID Funct3 (Imm4'0) OpCodeField
    | BType (Imm12) (Imm10'5) RegisterID RegisterID Funct3 (Imm4'1) (Imm11) OpCodeField
    | UType (Imm31'12) RegisterID OpCodeField
    | JType (Imm20) (Imm10'1) (Imm11) (Imm19'12) RegisterID OpCodeField
    deriving (Show, Eq)

extractOpCode :: Unsigned 32 -> Unsigned 7
extractOpCode instruction = resize $ instruction .&. 0b1111111

funct3_shift = -12
funct7_shift = -25
rd_shift = -7
rs1_shift = -15
rs2_shift = -20

decodeUType :: Unsigned 32 -> InstructionForm
decodeUType instruction = UType imm31'12 rd opcode
    where
        imm31'12 = truncateB $ instruction `shift` (-12)
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction

decodeJType :: Unsigned 32 -> InstructionForm
decodeJType instruction = JType imm20 imm10'1 imm11 imm19'12 rd opcode
    where
        imm20 = truncateB $ instruction `shift` (-31)
        imm10'1 = truncateB $ instruction `shift` (-21)
        imm11 = truncateB $ instruction `shift` (-20)
        imm19'12 = truncateB $ instruction `shift` (-12)
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction

decodeIType :: Unsigned 32 -> InstructionForm
decodeIType instruction = IType imm11'0 rs1 funct3 rd opcode
    where
        imm11'0 = truncateB $ instruction `shift` (-20)
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction

decodeBType :: Unsigned 32 -> InstructionForm
decodeBType instruction = BType imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode
    where
        imm12 = truncateB $ instruction `shift` (-31)
        imm10'5 = truncateB $ instruction `shift` (-25)
        rs2 = truncateB $ instruction `shift` rs2_shift
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        imm4'1 = truncateB $ instruction `shift` (-8)
        imm11 = truncateB $ instruction `shift` (-7)
        opcode = extractOpCode instruction

decodeSType :: Unsigned 32 -> InstructionForm
decodeSType instruction = SType imm11'5 rs2 rs1 funct3 imm4'0 opcode
    where
        imm11'5 = truncateB $ instruction `shift` (-25)
        rs2 = truncateB $ instruction `shift` rs2_shift
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        imm4'0 = truncateB $ instruction `shift` (-7)
        opcode = extractOpCode instruction

decodeRType :: Unsigned 32 -> InstructionForm
decodeRType instruction = RType funct7 rs2 rs1 funct3 rd opcode
    where
        funct7 = truncateB $ instruction `shift` funct7_shift
        rs2 = truncateB $ instruction `shift` rs2_shift
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction


decode :: Unsigned 32 -> InstructionForm
decode instruction = formed
    where
        opcode = extractOpCode instruction
        formed = case opcode of
            0b0110111 -> decodeUType instruction
            0b0010111 -> decodeUType instruction
            0b1101111 -> decodeJType instruction
            0b1100111 -> decodeIType instruction
            0b1100011 -> decodeBType instruction
            0b0000011 -> decodeIType instruction
            0b0100011 -> decodeSType instruction
            0b0010011 -> decodeIType instruction
            0b0110011 -> decodeRType instruction
            -- 0b0001111 -> -- TODO: FENCE, Zifencei
            0b1110011 -> decodeIType instruction
            -- TODO: What happens when an unknown instruction is found?
