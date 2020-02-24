module Decode where

import Clash.Prelude
import BaseTypes
import Instructions
import Fetch

-- TODO: Better Unsigned n -> Signed m conversion...
-- TODO: Change custom sign extension to builtin
constructImmediate :: InstructionForm -> Immediate
constructImmediate (ITypeForm imm11'0 _ _ _ _) = sign .|. rest
    where
        sign :: Signed 32 = if (testBit imm11'0 11) then 0xfffff800 else 0
        rest :: Signed 32 = conv imm11'0 .&. 0x7ff
constructImmediate (STypeForm imm11'5 _ _ _ imm4'0 _) = sign .|. rest
    where
        sign :: Signed 32 = if (testBit imm11'5 6) then 0xfffff800 else 0
        rest :: Signed 32 = conv $ ((conv imm11'5 :: Unsigned 12) `shift` 5) .|. (conv imm4'0 :: Unsigned 12)
constructImmediate (BTypeForm imm12 imm10'5 _ _ _ imm4'1 imm11 _) = sign .|. rest
    where
        sign :: Signed 32 = if (imm12 == 1) then 0xfffff000 else 0
        rest :: Signed 32 = conv (pos11 .|. (pos10'5 .|. pos4'1))
        pos11 :: Unsigned 32 = conv imm11 `shift` 11
        pos10'5 :: Unsigned 32 = conv imm10'5 `shift` 5
        pos4'1 :: Unsigned 32 = conv imm4'1 `shift` 1
constructImmediate (UTypeForm imm31'12 _ _) = conv imm31'12 `shift` 12
constructImmediate (JTypeForm imm20 imm10'1 imm11 imm19'12 _ _) = sign .|. rest
    where
        sign :: Signed 32 = if (imm20 == 1) then 0xfff00000 else 0
        rest :: Signed 32 = conv (pos19'12 .|. (pos11 .|. (pos10'1)))
        pos19'12 :: Unsigned 32 = conv imm19'12 `shift` 12
        pos11 :: Unsigned 32 = conv imm11 `shift` 11
        pos10'1 :: Unsigned 32 = conv imm10'1 `shift` 1


decode :: InstructionForm -> Instruction
decode (RTypeForm funct7 rs2 rs1 funct3 rd opcode) = RType instruction rs2 rs1 rd
    where
        instruction = case opcode of
            0b0110011 -> case (funct3, testBit funct7 5) of
                (0b000, False) -> ADD
                (0b000, True)  -> SUB
                (0b001, False) -> SLL
                (0b010, False) -> SLT
                (0b011, False) -> SLTU
                (0b100, False) -> XOR
                (0b101, False) -> SRL
                (0b101, True)  -> SRA
                (0b110, False) -> OR
                (0b111, False) -> AND
decode (ITypeForm imm11'0 rs1 funct3 rd opcode) = IType instruction immediate rs1 rd
    where
        immediate = constructImmediate (ITypeForm imm11'0 rs1 funct3 rd opcode)
        instruction = case opcode of
            0b0000011 -> case (funct3) of
                0b000 -> LB
                0b001 -> LH
                0b010 -> LW
                0b100 -> LBU
                0b101 -> LHU
            0b0010011 -> case (funct3) of
                0b000 -> ADDI
                0b010 -> SLTI
                0b011 -> SLTIU
                0b100 -> XORI
                0b110 -> ORI
                0b111 -> ANDI
            0b1100111 -> JALR
decode (STypeForm imm11'5 rs2 rs1 funct3 imm4'0 opcode) = SType instruction immediate rs2 rs1
    where
        immediate = constructImmediate (STypeForm imm11'5 rs2 rs1 funct3 imm4'0 opcode)
        instruction = case opcode of
            0b0100011 -> case funct3 of
                0b000 -> SB
                0b001 -> SH
                0b010 -> SW
decode (BTypeForm imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode) = SType instruction immediate rs2 rs1
    where
        immediate = constructImmediate (BTypeForm imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode)
        instruction = case opcode of
            0b1100011 -> case funct3 of
                0b000 -> BEQ
                0b001 -> BNE
                0b100 -> BLT
                0b101 -> BGE
                0b110 -> BLTU
                0b111 -> BGEU
decode (UTypeForm imm31'12 rd opcode) = UType instruction immediate rd
    where
        immediate = constructImmediate (UTypeForm imm31'12 rd opcode)
        instruction = case opcode of
            0b0110111 -> LUI
            0b0010111 -> AUIPC
decode (JTypeForm imm20 imm10'1 imm11 imm19'12 rd opcode) = UType instruction immediate rd
    where
        immediate = constructImmediate (JTypeForm imm20 imm10'1 imm11 imm19'12 rd opcode)
        instruction = case opcode of
            0b1101111 -> JAL
