module Decode where

import Clash.Prelude
import BaseTypes
import Instructions
import Parse

constructImmediate :: InstructionForm -> Immediate
constructImmediate (IType imm11'0 _ _ _ _) = sign .|. rest
    where
        sign :: Signed 32 = if (testBit imm11'0 11) then 0xfffff800 else 0
        rest :: Signed 32 = fromInteger $ toInteger imm11'0 .&. 0x7ff
constructImmediate (SType imm11'5 _ _ _ imm4'0 _) = sign .|. rest
    where
        sign :: Signed 32 = if (testBit imm11'5 6) then 0xfffff800 else 0
        rest :: Signed 32 = fromInteger $ ((toInteger imm11'5) `shift` 5) .|. (toInteger imm4'0)
constructImmediate (BType imm12 imm10'5 _ _ _ imm4'1 imm11 _) = sign .|. rest
    where
        sign :: Signed 32 = if (imm12 == 1) then 0xfffff000 else 0
        rest :: Signed 32 = fromInteger $ (pos11 .|. (pos10'5 .|. pos4'1))
        pos11 = toInteger imm11 `shift` 11
        pos10'5 = toInteger imm10'5 `shift` 5
        pos4'1 = toInteger imm4'1 `shift` 1
constructImmediate (UType imm31'12 _ _) = fromInteger $ toInteger imm31'12 `shift` 12
constructImmediate (JType imm20 imm10'1 imm11 imm19'12 _ _) = sign .|. rest
    where
        sign :: Signed 32 = if (imm20 == 1) then 0xfff00000 else 0
        rest :: Signed 32 = fromInteger $ (pos19'12 .|. (pos11 .|. (pos10'1)))
        pos19'12 = toInteger imm19'12 `shift` 12
        pos11 = toInteger imm11 `shift` 11
        pos10'1 = toInteger imm10'1 `shift` 1

decode :: InstructionForm -> Instruction
decode a = FENCE
