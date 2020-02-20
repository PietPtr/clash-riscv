module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits
import qualified Debug.Trace

import OpCodes
import BaseTypes

trace = Debug.Trace.trace


type OpCodeField = Unsigned 7
type Funct7 = Unsigned 7
type Funct3 = Unsigned 3

-- Needs improvement or is this a deliberate design choice?
type Imm11'0 = Unsigned 12
type Imm11'5 = Unsigned 7
type Imm4'0 = Unsigned 5
type Imm12 = Unsigned 1
type Imm10'5 = Unsigned 6
type Imm4'1 = Unsigned 4
type Imm11 = Unsigned 1
type Imm31'12 = Unsigned 20
type Imm20 = Unsigned 1
type Imm10'1 = Unsigned 10
type Imm19'12 = Unsigned 8

data InstructionForm =
      RType Funct7 RegisterID RegisterID Funct3 RegisterID OpCodeField
    | IType (Imm11'0) RegisterID Funct3 RegisterID OpCodeField
    | SType (Imm11'5) RegisterID RegisterID Funct3 (Imm4'0) OpCodeField
    | BType (Imm12) (Imm10'5) RegisterID RegisterID Funct3 (Imm4'1) (Imm11) OpCodeField
    | UType (Imm31'12) RegisterID OpCodeField
    | JType (Imm20) (Imm10'1) (Imm11) (Imm19'12) RegisterID OpCodeField
    deriving (Show, Eq)

testop :: Unsigned 32
testop = 0b10000001111100000000111101100011

extractOpCode :: Unsigned 32 -> Unsigned 7
extractOpCode instruction = resize $ instruction .&. 0b1111111

extractDestination :: Unsigned 32 -> Unsigned 5
extractDestination instruction = resize ((instruction `shift` (-7)) .&. 0b11111)

parseUType :: Unsigned 32 -> InstructionForm
parseUType instruction = UType imm31'12 rd opcode
    where
        imm31'12 = resize (instruction `shift` (-12)) .&. 0b11111111111111111111
        rd = extractDestination instruction
        opcode = extractOpCode instruction

-- parse :: Unsigned 32 -> InstructionForm
-- parse instruction = formed
--     where
--         opcode = instruction .&. 0b1111111
--         formed = case opcode of
--             0b0110111 -> parseUType instruction
