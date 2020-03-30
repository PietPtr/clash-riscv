module Fetch where

import Clash.Prelude
import Control.Exception
import qualified Data.List as L
import BaseTypes
import Instructions

{--
These are the opcodes as defined in table 24.1 of the RISC-V spec. The names are
the same as in the spec, but to resolve name issues the suffix _OPCODE is used.
--}
data Opcode
    = OPCODE_LUI
    | OPCODE_AUIPC
    | OPCODE_JAL
    | OPCODE_JALR
    | OPCODE_BRANCH
    | OPCODE_LOAD
    | OPCODE_STORE
    | OPCODE_OP_IMM
    | OPCODE_OP
    | OPCODE_SYSTEM
    | OPCODE_UNKNOWN
    deriving (Show, Eq)

instance BitMapping Opcode where
    fromBits code = case code of
        0b0110111 -> OPCODE_LUI
        0b0010111 -> OPCODE_AUIPC
        0b1101111 -> OPCODE_JAL
        0b1100111 -> OPCODE_JALR
        0b1100011 -> OPCODE_BRANCH
        0b0000011 -> OPCODE_LOAD
        0b0100011 -> OPCODE_STORE
        0b0010011 -> OPCODE_OP_IMM
        0b0110011 -> OPCODE_OP
        0b1110011 -> OPCODE_SYSTEM
        _ -> OPCODE_UNKNOWN


type Funct7         = Unsigned 7
type Funct3         = Unsigned 3

-- Needs improvement or is this a deliberate design choice?
type Imm11'0        = Unsigned 12
type Imm11'5        = Unsigned 7
type Imm4'0         = Unsigned 5
type Imm12          = Unsigned 1
type Imm10'5        = Unsigned 6
type Imm4'1         = Unsigned 4
type Imm11          = Unsigned 1
type Imm31'12       = Unsigned 20
type Imm20          = Unsigned 1
type Imm10'1        = Unsigned 10
type Imm19'12       = Unsigned 8

data InstructionForm
    = RTypeForm Funct7 RegisterID RegisterID Funct3 RegisterID Opcode
    | ITypeForm Imm11'0 RegisterID Funct3 RegisterID Opcode
    | STypeForm Imm11'5 RegisterID RegisterID Funct3 Imm4'0 Opcode
    | BTypeForm Imm12 Imm10'5 RegisterID RegisterID Funct3 Imm4'1 Imm11 Opcode
    | UTypeForm Imm31'12 RegisterID Opcode
    | JTypeForm Imm20 Imm10'1 Imm11 Imm19'12 RegisterID Opcode
    | UnknownForm
    deriving (Show, Eq)

extractOpCode :: Unsigned 32 -> Opcode
extractOpCode instruction = fromBits $ instruction .&. 0b1111111

funct3_shift = -12
funct7_shift = -25
rd_shift = -7
rs1_shift = -15
rs2_shift = -20

fetchUType :: Unsigned 32 -> InstructionForm
fetchUType instruction = UTypeForm imm31'12 rd opcode
    where
        imm31'12    = truncateB $ instruction `shift` (-12)
        rd          = truncateB $ instruction `shift` rd_shift
        opcode      = extractOpCode instruction

fetchJType :: Unsigned 32 -> InstructionForm
fetchJType instruction = JTypeForm imm20 imm10'1 imm11 imm19'12 rd opcode
    where
        imm20       = truncateB $ instruction `shift` (-31)
        imm10'1     = truncateB $ instruction `shift` (-21)
        imm11       = truncateB $ instruction `shift` (-20)
        imm19'12    = truncateB $ instruction `shift` (-12)
        rd          = truncateB $ instruction `shift` rd_shift
        opcode      = extractOpCode instruction

fetchIType :: Unsigned 32 -> InstructionForm
fetchIType instruction = ITypeForm imm11'0 rs1 funct3 rd opcode
    where
        imm11'0     = truncateB $ instruction `shift` (-20)
        rs1         = truncateB $ instruction `shift` rs1_shift
        funct3      = truncateB $ instruction `shift` funct3_shift
        rd          = truncateB $ instruction `shift` rd_shift
        opcode      = extractOpCode instruction

fetchBType :: Unsigned 32 -> InstructionForm
fetchBType instruction = BTypeForm imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode
    where
        imm12       = truncateB $ instruction `shift` (-31)
        imm10'5     = truncateB $ instruction `shift` (-25)
        rs2         = truncateB $ instruction `shift` rs2_shift
        rs1         = truncateB $ instruction `shift` rs1_shift
        funct3      = truncateB $ instruction `shift` funct3_shift
        imm4'1      = truncateB $ instruction `shift` (-8)
        imm11       = truncateB $ instruction `shift` (-7)
        opcode      = extractOpCode instruction

fetchSType :: Unsigned 32 -> InstructionForm
fetchSType instruction = STypeForm imm11'5 rs2 rs1 funct3 imm4'0 opcode
    where
        imm11'5     = truncateB $ instruction `shift` (-25)
        rs2         = truncateB $ instruction `shift` rs2_shift
        rs1         = truncateB $ instruction `shift` rs1_shift
        funct3      = truncateB $ instruction `shift` funct3_shift
        imm4'0      = truncateB $ instruction `shift` (-7)
        opcode      = extractOpCode instruction

fetchRType :: Unsigned 32 -> InstructionForm
fetchRType instruction = RTypeForm funct7 rs2 rs1 funct3 rd opcode
    where
        funct7      = truncateB $ instruction `shift` funct7_shift
        rs2         = truncateB $ instruction `shift` rs2_shift
        rs1         = truncateB $ instruction `shift` rs1_shift
        funct3      = truncateB $ instruction `shift` funct3_shift
        rd          = truncateB $ instruction `shift` rd_shift
        opcode      = extractOpCode instruction



fetch :: Unsigned 32 -> InstructionForm
fetch instruction = formed
    where
        opcode = extractOpCode instruction
        formed = case opcode of
            OPCODE_LUI    -> fetchUType instruction
            OPCODE_AUIPC  -> fetchUType instruction
            OPCODE_JAL    -> fetchJType instruction
            OPCODE_JALR   -> fetchIType instruction
            OPCODE_BRANCH -> fetchBType instruction
            OPCODE_LOAD   -> fetchIType instruction
            OPCODE_STORE  -> fetchSType instruction
            OPCODE_OP_IMM -> fetchIType instruction
            OPCODE_OP     -> fetchRType instruction
            OPCODE_SYSTEM -> fetchIType instruction
            OPCODE_UNKNOWN-> UnknownForm
            -- 0b0001111 -> -- TODO: FENCE, Zifencei
