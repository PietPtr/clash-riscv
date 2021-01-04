{-# LANGUAGE RecordWildCards #-}

module DE where

import Clash.Prelude
import qualified IF
import qualified RegisterFile as RF
import Instructions



constructImmediate :: IF.Form -> Immediate
constructImmediate instruction = fromIntegral $ case instruction of
    (IF.IType imm11'0 _ _ _ _) ->
        signExtend imm11'0
    (IF.SType imm11'5 _ _ _ imm4'0 _) ->
        signExtend (imm11'5 ++# imm4'0)
    (IF.BType imm12 imm10'5 _ _ _ imm4'1 imm11 _) ->
        signExtend (imm12 ++# imm11 ++# imm10'5 ++# imm4'1 ++# (0 :: BitVector 1))
    (IF.UType imm31'12 _ _) ->
        imm31'12 ++# (0 :: BitVector 12)
    (IF.JType imm20 imm10'1 imm11 imm19'12 _ _) ->
        signExtend (imm20 ++# imm19'12 ++# imm11 ++# imm10'1 ++# (0 :: BitVector 1))



decode :: IF.Form -> (Instruction, RF.ID, RF.ID)
decode instrform = (instruction, rs1, rs2)
    where
        (instruction, rs1, rs2) = case instrform of
            IF.RType _   rs2 rs1 _ rd _   -> (RType rinstruction rd,           rs1, rs2)
            IF.IType _       rs1 _ rd _   -> (IType iinstruction immediate rd, rs1, 0)
            IF.SType _   rs2 rs1 _ _  _   -> (SType sinstruction immediate,    rs1, rs2)
            IF.BType _ _ rs2 rs1 _ _  _ _ -> (SType sinstruction immediate,    rs1, rs2)
            IF.UType _             rd _   -> (UType uinstruction immediate rd, 0,   0)
            IF.JType _ _ _ _       rd _   -> (UType uinstruction immediate rd, 0,   0)
            IF.Unknown                    -> (UnknownType,                     0,   0)

        immediate = constructImmediate instrform

        rinstruction = case instrform of
            (IF.RType funct7 _ _ funct3 _ opcode) -> case opcode of
                IF.OP -> case (funct3, testBit funct7 5) of
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
                -- TODO: wat doet clash in de else case?
        
        iinstruction = case instrform of
            (IF.IType _ _ funct3 _ opcode) -> case opcode of
                IF.LOAD   -> case (funct3) of
                    0b000 -> LB
                    0b001 -> LH
                    0b010 -> LW
                    0b100 -> LBU
                    0b101 -> LHU
                IF.OP_IMM -> case (funct3) of
                    0b000 -> ADDI
                    0b010 -> SLTI
                    0b011 -> SLTIU
                    0b100 -> XORI
                    0b110 -> ORI
                    0b111 -> ANDI
                IF.JALR   -> JALR
        
        sinstruction = case instrform of
            (IF.SType _ _ _ funct3 _ opcode) -> case opcode of
                IF.STORE -> case funct3 of
                    0b000 -> SB
                    0b001 -> SH
                    0b010 -> SW
            (IF.BType _ _ _ _ funct3 _ _ opcode) -> case opcode of
                IF.BRANCH -> case funct3 of
                    0b000 -> BEQ
                    0b001 -> BNE
                    0b100 -> BLT
                    0b101 -> BGE
                    0b110 -> BLTU
                    0b111 -> BGEU
        
        uinstruction = case instrform of
            (IF.UType _ _ opcode) -> case opcode of
                IF.LUI   -> LUI
                IF.AUIPC -> AUIPC
            (IF.JType _ _ _ _ _ opcode) -> case opcode of
                IF.JAL -> JAL

