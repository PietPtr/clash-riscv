module Decode where

import Clash.Prelude
import BaseTypes
import Instructions
import Fetch

constructImmediate :: InstructionForm -> Immediate
constructImmediate (IType imm11'0 _ _ _ _) = sign .|. rest
    where
        sign :: Signed 32 = if (testBit imm11'0 11) then 0xfffff800 else 0
        rest :: Signed 32 = fromInteger $ toInteger imm11'0 .&. 0x7ff
constructImmediate (SType imm11'5 _ _ _ imm4'0 _) = sign .|. rest
    where
        sign :: Signed 32 = if (testBit imm11'5 6) then 0xfffff800 else 0
        rest :: Signed 32 = fromInteger $ ((toInteger imm11'5) `shift` 5) .|. (toInteger imm4'0)
constructImmediate (BType imm12 imm10'5 _ _ _ imm4'1 imm11 _) = sign .|. rest
    where
        sign :: Signed 32 = if (imm12 == 1) then 0xfffff000 else 0
        rest :: Signed 32 = fromInteger $ (pos11 .|. (pos10'5 .|. pos4'1))
        pos11 = toInteger imm11 `shift` 11
        pos10'5 = toInteger imm10'5 `shift` 5
        pos4'1 = toInteger imm4'1 `shift` 1
constructImmediate (UType imm31'12 _ _) = fromInteger $ toInteger imm31'12 `shift` 12
constructImmediate (JType imm20 imm10'1 imm11 imm19'12 _ _) = sign .|. rest
    where
        sign :: Signed 32 = if (imm20 == 1) then 0xfff00000 else 0
        rest :: Signed 32 = fromInteger $ (pos19'12 .|. (pos11 .|. (pos10'1)))
        pos19'12 = toInteger imm19'12 `shift` 12
        pos11 = toInteger imm11 `shift` 11
        pos10'1 = toInteger imm10'1 `shift` 1


decode :: InstructionForm -> Instruction
decode (RType funct7 rs2 rs1 funct3 rd opcode) = instructionConstructor rs2 rs1 rd
    where
        instructionConstructor = case opcode of
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
decode (IType imm11'0 rs1 funct3 rd opcode) = instructionConstructor immediate rs1 rd
    where
        instructionConstructor = case opcode of
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
        immediate = constructImmediate (IType imm11'0 rs1 funct3 rd opcode)
decode (SType imm11'5 rs2 rs1 funct3 imm4'0 opcode) = instructionConstructor immediate rs2 rs1
    where
        immediate = constructImmediate (SType imm11'5 rs2 rs1 funct3 imm4'0 opcode)
        instructionConstructor = case opcode of
            0b0100011 -> case funct3 of
                0b000 -> SB
                0b001 -> SH
                0b010 -> SW
decode (BType imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode) = instructionConstructor immediate rs2 rs1
    where
        immediate = constructImmediate (BType imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode)
        instructionConstructor = case opcode of
            0b1100011 -> case funct3 of
                0b000 -> BEQ
                0b001 -> BNE
                0b100 -> BLT
                0b101 -> BGE
                0b110 -> BLTU
                0b111 -> BGEU
decode (UType imm31'12 rd opcode) = instructionConstructor immediate rd
    where
        immediate = constructImmediate (UType imm31'12 rd opcode)
        instructionConstructor = case opcode of
            0b0110111 -> LUI
            0b0010111 -> AUIPC
decode (JType imm20 imm10'1 imm11 imm19'12 rd opcode) = instructionConstructor immediate rd
    where
        immediate = constructImmediate (JType imm20 imm10'1 imm11 imm19'12 rd opcode)
        instructionConstructor = case opcode of
            0b1101111 -> JAL
