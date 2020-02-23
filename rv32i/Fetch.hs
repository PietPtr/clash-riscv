module Fetch where

import Clash.Prelude
import BaseTypes
import Instructions

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

op 1 = 0xff010113 -- addi	x2,x2,-16
op 2 = 0x00000593 -- li 	x11,0
op 3 = 0x00812423 -- sw 	x8,8(x2)
op 4 = 0xfb9ff0ef -- jal	x1,10150
op 5 = 0x00112623 -- sw 	x1,12(x2)
op 6 = 0xfec42703 -- lw 	x14,-20(x8)
op 7 = 0x40f90933 -- sub	x18,x18,x15
op 8 = 0x05778063 -- beq	x15,x23,10400
op 9 = 0x00010537 -- lui    x10,0x0

extractOpCode :: Unsigned 32 -> Unsigned 7
extractOpCode instruction = resize $ instruction .&. 0b1111111

funct3_shift = -12
funct7_shift = -25
rd_shift = -7
rs1_shift = -15
rs2_shift = -20

parseUType :: Unsigned 32 -> InstructionForm
parseUType instruction = UType imm31'12 rd opcode
    where
        imm31'12 = truncateB $ instruction `shift` (-12)
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction

parseJType :: Unsigned 32 -> InstructionForm
parseJType instruction = JType imm20 imm10'1 imm11 imm19'12 rd opcode
    where
        imm20 = truncateB $ instruction `shift` (-31)
        imm10'1 = truncateB $ instruction `shift` (-21)
        imm11 = truncateB $ instruction `shift` (-20)
        imm19'12 = truncateB $ instruction `shift` (-12)
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction

parseIType :: Unsigned 32 -> InstructionForm
parseIType instruction = IType imm11'0 rs1 funct3 rd opcode
    where
        imm11'0 = truncateB $ instruction `shift` (-20)
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction

parseBType :: Unsigned 32 -> InstructionForm
parseBType instruction = BType imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode
    where
        imm12 = truncateB $ instruction `shift` (-31)
        imm10'5 = truncateB $ instruction `shift` (-25)
        rs2 = truncateB $ instruction `shift` rs2_shift
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        imm4'1 = truncateB $ instruction `shift` (-8)
        imm11 = truncateB $ instruction `shift` (-7)
        opcode = extractOpCode instruction

parseSType :: Unsigned 32 -> InstructionForm
parseSType instruction = SType imm11'5 rs2 rs1 funct3 imm4'0 opcode
    where
        imm11'5 = truncateB $ instruction `shift` (-25)
        rs2 = truncateB $ instruction `shift` rs2_shift
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        imm4'0 = truncateB $ instruction `shift` (-7)
        opcode = extractOpCode instruction

parseRType :: Unsigned 32 -> InstructionForm
parseRType instruction = RType funct7 rs2 rs1 funct3 rd opcode
    where
        funct7 = truncateB $ instruction `shift` funct7_shift
        rs2 = truncateB $ instruction `shift` rs2_shift
        rs1 = truncateB $ instruction `shift` rs1_shift
        funct3 = truncateB $ instruction `shift` funct3_shift
        rd = truncateB $ instruction `shift` rd_shift
        opcode = extractOpCode instruction


parse :: Unsigned 32 -> InstructionForm
parse instruction = formed
    where
        opcode = extractOpCode instruction
        formed = case opcode of
            0b0110111 -> parseUType instruction
            0b0010111 -> parseUType instruction
            0b1101111 -> parseJType instruction
            0b1100111 -> parseIType instruction
            0b1100011 -> parseBType instruction
            0b0000011 -> parseIType instruction
            0b0100011 -> parseSType instruction
            0b0010011 -> parseIType instruction
            0b0110011 -> parseRType instruction
            -- 0b0001111 -> -- TODO: FENCE, Zifencei
            0b1110011 -> parseIType instruction
            -- TODO: What happens when an unknown instruction is parsed?
