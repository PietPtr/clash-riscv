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




system inp = bundle (memRead, formed)
    where
        (doStall, memData) = unbundle inp
        formed = fetch <$> memData

        memRead = -- het address waarvandaan we van de memory controller willen lezen







fetchUType :: BitVector 32 -> Form
fetchUType instruction = UType imm31'12 rd opcode
    where
        imm31'12    = slice d31 d12 instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchJType :: BitVector 32 -> Form
fetchJType instruction = JType imm20 imm10'1 imm11 imm19'12 rd opcode
    where
        imm20       = slice d31 d31 instruction
        imm10'1     = slice d30 d21 instruction
        imm11       = slice d20 d20 instruction
        imm19'12    = slice d19 d12 instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchIType :: BitVector 32 -> Form
fetchIType instruction = IType imm11'0 rs1 funct3 rd opcode
    where
        imm11'0     = slice d31 d20 instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchBType :: BitVector 32 -> Form
fetchBType instruction = BType imm12 imm10'5 rs2 rs1 funct3 imm4'1 imm11 opcode
    where
        imm12       = slice d31 d31 instruction
        imm10'5     = slice d30 d25 instruction
        rs2         = fetchrs2      instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        imm4'1      = slice d11 d8  instruction
        imm11       = slice d7 d7   instruction
        opcode      = fetchOpcode   instruction

fetchSType :: BitVector 32 -> Form
fetchSType instruction = SType imm11'5 rs2 rs1 funct3 imm4'0 opcode
    where
        imm11'5     = slice d31 d25 instruction
        rs2         = fetchrs2      instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        imm4'0      = slice d11 d7  instruction
        opcode      = fetchOpcode   instruction

fetchRType :: BitVector 32 -> Form
fetchRType instruction = RType funct7 rs2 rs1 funct3 rd opcode
    where
        funct7      = fetchFunct7   instruction
        rs2         = fetchrs2      instruction
        rs1         = fetchrs1      instruction
        funct3      = fetchFunct3   instruction
        rd          = fetchrd       instruction
        opcode      = fetchOpcode   instruction

fetchrd :: BitVector 32 -> RF.ID
fetchrd instruction = fromIntegral $ slice d11 d7 instruction

fetchrs2 :: BitVector 32 -> RF.ID
fetchrs2 instruction = fromIntegral $ slice d24 d20 instruction

fetchrs1 :: BitVector 32 -> RF.ID
fetchrs1 instruction = fromIntegral $ slice d19 d15 instruction

fetchFunct3 :: BitVector 32 -> Funct3
fetchFunct3 instruction = fromIntegral $ slice d14 d12 instruction

fetchFunct7 :: BitVector 32 -> Funct7
fetchFunct7 instruction = fromIntegral $ slice d31 d25 instruction

fetchOpcode :: BitVector 32 -> Opcode
fetchOpcode instruction = case code of
    0b0110111 -> LUI
    0b0010111 -> AUIPC
    0b1101111 -> JAL
    0b1100111 -> JALR
    0b1100011 -> BRANCH
    0b0000011 -> LOAD
    0b0100011 -> STORE
    0b0010011 -> OP_IMM
    0b0110011 -> OP
    0b1110011 -> SYSTEM
    _         -> UNKNOWN
    where
        code = slice d6 d0 instruction

fetch :: Unsigned 32 -> Form
fetch instruction = formed
    where
        bitvector = fromIntegral instruction
        opcode = fetchOpcode bitvector
        formed = case opcode of
            LUI    -> fetchUType bitvector
            AUIPC  -> fetchUType bitvector
            JAL    -> fetchJType bitvector
            JALR   -> fetchIType bitvector
            BRANCH -> fetchBType bitvector
            LOAD   -> fetchIType bitvector
            STORE  -> fetchSType bitvector
            OP_IMM -> fetchIType bitvector
            OP     -> fetchRType bitvector
            SYSTEM -> fetchIType bitvector
            UNKNOWN-> IF.Unknown
            -- 0b0001111 -> -- TODO: FENCE, Zifencei


