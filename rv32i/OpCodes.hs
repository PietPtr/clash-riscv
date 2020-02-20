module OpCodes where

import Clash.Prelude
import BaseTypes

type Destination = RegisterID
type Source = RegisterID
type Immediate = Unsigned 32
type ShiftAmount = Unsigned 5

data OpCode =
      LUI   Immediate Destination
    | AUIPC Immediate Destination
    | JAL   Immediate Destination
    | JALR  Immediate Source Destination
    | BEQ   Immediate Source Source
    | BNE   Immediate Source Source
    | BLT   Immediate Source Source
    | BGE   Immediate Source Source
    | BLTU  Immediate Source Source
    | BGEU  Immediate Source Source
    | LB    Immediate Source Destination
    | LH    Immediate Source Destination
    | LW    Immediate Source Destination
    | LBU   Immediate Source Destination
    | LHU   Immediate Source Destination
    | SB    Immediate Source Source
    | SH    Immediate Source Source
    | SW    Immediate Source Source
    | ADDI  Immediate Source Destination
    | SLTI  Immediate Source Destination
    | SLTIU Immediate Source Destination
    | ORI   Immediate Source Destination
    | ANDI  Immediate Source Destination
    | SLLI  ShiftAmount Source Destination
    | SRLI  ShiftAmount Source Destination
    | SRAI  ShiftAmount Source Destination
    | ADD   Source Source Destination
    | SUB   Source Source Destination
    | SLL   Source Source Destination
    | SLT   Source Source Destination
    | SLTU  Source Source Destination
    | XOR   Source Source Destination
    | SRL   Source Source Destination
    | SRA   Source Source Destination
    | OR    Source Source Destination
    | AND   Source Source Destination
    | FENCE -- TODO
    | ECALL -- TODO
    | EBREAK -- TODO
    deriving (Show, Eq)

-- code :: Unsigned 7 -> Unsigned 3 -> OpCode
-- code 0b0110111 _ = LUI
-- code 0b0010111 _ = AUIPC
-- code 0b1101111 _ = JAL
-- code 0b1100111 0b000 = JALR
-- code 0b1100011 0b000 = BEQ
-- code 0b1100011 0b001 = BNE
