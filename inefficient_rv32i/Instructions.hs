module Instructions where

import Clash.Prelude
import qualified Data.List as L
import BaseTypes


type Destination    = RegisterID
type Source         = RegisterID
type Immediate      = Signed 32
type ShiftAmount    = Unsigned 5

data RInstruction
    = ADD
    | SUB
    | SLL
    | SLT
    | SLTU
    | XOR
    | SRL
    | SRA
    | OR
    | AND
    deriving (Show, Eq)

data IInstruction
    = JALR
    | LB
    | LH
    | LW
    | LBU
    | LHU
    | ADDI
    | SLTI
    | SLTIU
    | XORI
    | ORI
    | ANDI
    | SLLI
    | SRLI
    | SRAI
    deriving (Show, Eq)

data SInstruction
    = BEQ
    | BNE
    | BLT
    | BGE
    | BLTU
    | BGEU
    | SB
    | SH
    | SW
    deriving (Show, Eq)

data UInstruction
    = LUI
    | AUIPC
    | JAL
    deriving (Show, Eq)

data Instruction
    = RType RInstruction Source Source Destination
    | IType IInstruction Immediate Source Destination
    | SType SInstruction Immediate Source Source
    | UType UInstruction Immediate Destination
    | UnknownType
    deriving (Show, Eq)
