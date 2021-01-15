module Instructions where

import Clash.Prelude
import Globals
import qualified RegisterFile as RF

type Destination    = RF.ID
type Source         = RF.ID

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
    deriving (Show, Eq, Generic, NFDataX)

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
    deriving (Show, Eq, Generic, NFDataX)

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
    deriving (Show, Eq, Generic, NFDataX)

data UInstruction
    = LUI
    | AUIPC
    | JAL
    deriving (Show, Eq, Generic, NFDataX)

data Instruction
    = RType RInstruction Destination
    | IType IInstruction Immediate Destination
    | SType SInstruction Immediate 
    | UType UInstruction Immediate Destination
    | UnknownType
    deriving (Show, Eq, Generic, NFDataX)
