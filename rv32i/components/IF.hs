module IF where

import Clash.Prelude
import qualified RegisterFile as RF

data Opcode
    = LUI
    | AUIPC
    | JAL
    | JALR
    | BRANCH
    | LOAD
    | STORE
    | OP_IMM
    | OP
    | SYSTEM
    | UNKNOWN
    deriving (Show, Eq)

type Funct7         = Unsigned 7
type Funct3         = Unsigned 3

-- Needs improvement or is this a deliberate design choice?
type Imm11'0        = BitVector 12
type Imm11'5        = BitVector 7
type Imm4'0         = BitVector 5
type Imm12          = BitVector 1
type Imm10'5        = BitVector 6
type Imm4'1         = BitVector 4
type Imm11          = BitVector 1
type Imm31'12       = BitVector 20
type Imm20          = BitVector 1
type Imm10'1        = BitVector 10
type Imm19'12       = BitVector 8

data Form
    = RType Funct7 RF.ID RF.ID Funct3 RF.ID Opcode
    | IType Imm11'0 RF.ID Funct3 RF.ID Opcode
    | SType Imm11'5 RF.ID RF.ID Funct3 Imm4'0 Opcode
    | BType Imm12 Imm10'5 RF.ID RF.ID Funct3 Imm4'1 Imm11 Opcode
    | UType Imm31'12 RF.ID Opcode
    | JType Imm20 Imm10'1 Imm11 Imm19'12 RF.ID Opcode
    | Unknown
    deriving (Show, Eq)