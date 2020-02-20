module Core where

import Clash.Prelude
import qualified Data.List as L
import qualified Data.Bits as Bits
import qualified Debug.Trace
trace = Debug.Trace.trace

type Register = Unsigned 32
type RegisterID = Unsigned 5
type Registers = Vec 32 Register

type OpCode = Unsigned 7
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


-- instance Show (Immediate) where
--     show (Imm width start)
--         | width == 1 = "imm[" L.++ (show (start)) L.++ "]"
        -- | otherwise = "imm[" L.++ (show (start + width - 1)) L.++ "," L.++ (show start) L.++ "]"


data Instruction =
      RType Funct7 RegisterID RegisterID Funct3 RegisterID OpCode
    | IType (Imm11'0) RegisterID Funct3 RegisterID OpCode
    | SType (Imm11'5) RegisterID RegisterID Funct3 (Imm4'0) OpCode
    | BType (Imm12) (Imm10'5) RegisterID RegisterID Funct3 (Imm4'1) (Imm11) OpCode
    | UType (Imm31'12) RegisterID OpCode
    | JType (Imm20) (Imm10'1) (Imm11) (Imm19'12) RegisterID OpCode
    deriving (Show, Eq)
