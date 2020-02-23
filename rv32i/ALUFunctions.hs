{-# LANGUAGE RecordWildCards #-}
module ALUFunctions where

import Clash.Prelude
import BaseTypes

setLessThan :: RegisterValue -> RegisterValue -> RegisterValue
setLessThan operand2 operand1 = if (operand1 < operand2) then 1 else 0

setLessThanU :: RegisterValue -> RegisterValue -> RegisterValue
setLessThanU operand2 operand1 = if (unsigned1 < unsigned2) then 1 else 0
    where
        unsigned2 :: Unsigned 32 = conv operand2
        unsigned1 :: Unsigned 32 = conv operand1

shiftRightArithmetical :: RegisterValue -> RegisterValue -> RegisterValue
shiftRightArithmetical operand2 operand1 = operand1 `shiftR` (fromIntegral $ operand2 .&. 0b11111)

shiftRightLogical :: RegisterValue -> RegisterValue -> RegisterValue
shiftRightLogical operand2 operand1 = conv $ value `shiftR` (fromIntegral $ operand1 .&. 0b11111)
    where value :: Unsigned 32 = conv operand2

shiftLeftLogical :: RegisterValue -> RegisterValue -> RegisterValue
shiftLeftLogical operand2 operand1 = operand1 `shiftL` (fromIntegral $ operand2 .&. 0b11111)

jumpAndLinkReg :: RegisterValue -> RegisterValue -> RegisterValue
jumpAndLinkReg operand2 operand1 = (operand2 + operand1) .&. 0xfffffffc

zero :: RegisterValue -> RegisterValue -> RegisterValue
zero _ _ = 0

compareUnsigned :: RegisterValue -> RegisterValue -> (Unsigned 32 -> Unsigned 32 -> Bool) -> Bool
compareUnsigned operand2 operand1 func = unsigned1 `func` unsigned2
    where
        unsigned2 :: Unsigned 32 = conv operand2
        unsigned1 :: Unsigned 32 = conv operand1

loadUpperImmediate :: RegisterValue -> RegisterValue -> RegisterValue
loadUpperImmediate imm _ = imm `shift` 12
