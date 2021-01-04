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
        _         -> OPCODE_UNKNOWN
    fromVector code = fromBits (fromIntegral code :: Unsigned 7)


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

data InstructionForm
    = RTypeForm Funct7 RegisterID RegisterID Funct3 RegisterID Opcode
    | ITypeForm Imm11'0 RegisterID Funct3 RegisterID Opcode
    | STypeForm Imm11'5 RegisterID RegisterID Funct3 Imm4'0 Opcode
    | BTypeForm Imm12 Imm10'5 RegisterID RegisterID Funct3 Imm4'1 Imm11 Opcode
    | UTypeForm Imm31'12 RegisterID Opcode
    | JTypeForm Imm20 Imm10'1 Imm11 Imm19'12 RegisterID Opcode
    | UnknownForm
    deriving (Show, Eq)


fetchUType :: BitVector 32 -> InstructionForm
fetchUType instruction = UTypeForm imm31'12 rd opcode
    where
        imm31'12    = slice d31 d12 instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchJType :: BitVector 32 -> InstructionForm
fetchJType instruction = JTypeForm imm20 imm10'1 imm11 imm19'12 rd opcode
    where
        imm20       = slice d31 d31 instruction
        imm10'1     = slice d30 d21 instruction
        imm11       = slice d20 d20 instruction
        imm19'12    = slice d19 d12 instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchIType :: BitVector 32 -> InstructionForm
fetchIType instruction = ITypeForm imm11'0 rs1 funct3 rd opcode
    where
        imm11'0     = slice d31 d20 instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchBType :: BitVector 32 -> InstructionForm
fetchBType instruction = BTypeForm imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode
    where
        imm12       = slice d31 d31 instruction
        imm10'5     = slice d30 d25 instruction
        rs2         = fetchrs2      instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        imm4'1      = slice d11 d8  instruction
        imm11       = slice d7 d7   instruction
        opcode      = fetchOpcode   instruction

fetchSType :: BitVector 32 -> InstructionForm
fetchSType instruction = STypeForm imm11'5 rs2 rs1 funct3 imm4'0 opcode
    where
        imm11'5     = slice d31 d25 instruction
        rs2         = fetchrs2      instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        imm4'0      = slice d11 d7  instruction
        opcode      = fetchOpcode   instruction

fetchRType :: BitVector 32 -> InstructionForm
fetchRType instruction = RTypeForm funct7 rs2 rs1 funct3 rd opcode
    where
        funct7      = fetchFunct7   instruction
        rs2         = fetchrs2      instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchrd :: BitVector 32 -> RegisterID
fetchrd instruction = fromIntegral $ slice d11 d7 instruction

fetchrs2 :: BitVector 32 -> RegisterID
fetchrs2 instruction = fromIntegral $ slice d24 d20 instruction

fetchrs1 :: BitVector 32 -> RegisterID
fetchrs1 instruction = fromIntegral $ slice d19 d15 instruction

fetchFunct3 :: BitVector 32 -> Funct3
fetchFunct3 instruction = fromIntegral $ slice d14 d12 instruction

fetchFunct7 :: BitVector 32 -> Funct7
fetchFunct7 instruction = fromIntegral $ slice d31 d25 instruction

fetchOpcode :: BitVector 32 -> Opcode
fetchOpcode instruction = fromVector $ slice d6 d0 instruction

fetch :: Unsigned 32 -> InstructionForm
fetch instruction = formed
    where
        bitvector = fromIntegral instruction
        opcode = fetchOpcode bitvector
        formed = case opcode of
            OPCODE_LUI    -> fetchUType bitvector
            OPCODE_AUIPC  -> fetchUType bitvector
            OPCODE_JAL    -> fetchJType bitvector
            OPCODE_JALR   -> fetchIType bitvector
            OPCODE_BRANCH -> fetchBType bitvector
            OPCODE_LOAD   -> fetchIType bitvector
            OPCODE_STORE  -> fetchSType bitvector
            OPCODE_OP_IMM -> fetchIType bitvector
            OPCODE_OP     -> fetchRType bitvector
            OPCODE_SYSTEM -> fetchIType bitvector
            OPCODE_UNKNOWN-> UnknownForm
            -- 0b0001111 -> -- TODO: FENCE, Zifencei
