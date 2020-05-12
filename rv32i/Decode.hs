module Decode where

import Clash.Prelude
import BaseTypes
import Instructions
import Fetch


constructImmediate :: InstructionForm -> Immediate
constructImmediate instruction = fromIntegral $ case instruction of
    (ITypeForm imm11'0 _ _ _ _) ->
        signExtend imm11'0
    (STypeForm imm11'5 _ _ _ imm4'0 _) ->
        signExtend (imm11'5 ++# imm4'0)
    (BTypeForm imm12 imm10'5 _ _ _ imm4'1 imm11 _) ->
        signExtend (imm12 ++# imm11 ++# imm10'5 ++# imm4'1 ++# (0 :: BitVector 1))
    (UTypeForm imm31'12 _ _) ->
        imm31'12 ++# (0 :: BitVector 12)
    (JTypeForm imm20 imm10'1 imm11 imm19'12 _ _) ->
        signExtend (imm20 ++# imm19'12 ++# imm11 ++# imm10'1 ++# (0 :: BitVector 1))



decode :: InstructionForm -> Instruction
decode (RTypeForm funct7 rs2 rs1 funct3 rd opcode) = RType instruction rs2 rs1 rd
    where
        instruction = case opcode of
            OPCODE_OP -> case (funct3, testBit funct7 5) of
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
            OPCODE_LOAD -> case (funct3) of
                0b000 -> LB
                0b001 -> LH
                0b010 -> LW
                0b100 -> LBU
                0b101 -> LHU
            OPCODE_OP_IMM -> case (funct3) of
                0b000 -> ADDI
                0b010 -> SLTI
                0b011 -> SLTIU
                0b100 -> XORI
                0b110 -> ORI
                0b111 -> ANDI
            OPCODE_JALR -> JALR
decode (STypeForm imm11'5 rs2 rs1 funct3 imm4'0 opcode) = SType instruction immediate rs2 rs1
    where
        immediate = constructImmediate (STypeForm imm11'5 rs2 rs1 funct3 imm4'0 opcode)
        instruction = case opcode of
            OPCODE_STORE -> case funct3 of
                0b000 -> SB
                0b001 -> SH
                0b010 -> SW
decode (BTypeForm imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode) = SType instruction immediate rs2 rs1
    where
        immediate = constructImmediate (BTypeForm imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode)
        instruction = case opcode of
            OPCODE_BRANCH -> case funct3 of
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
            OPCODE_LUI    -> LUI
            OPCODE_AUIPC  -> AUIPC
decode (JTypeForm imm20 imm10'1 imm11 imm19'12 rd opcode) = UType instruction immediate rd
    where
        immediate = constructImmediate (JTypeForm imm20 imm10'1 imm11 imm19'12 rd opcode)
        instruction = case opcode of
            OPCODE_JAL -> JAL
decode UnknownForm = UnknownType
