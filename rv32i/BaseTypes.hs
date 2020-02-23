module BaseTypes where

import Clash.Prelude

import qualified Debug.Trace
trace = Debug.Trace.trace

type RegisterValue = Signed 32
type RegisterID = Unsigned 5 -- TODO: Maybe it's better form to make this of type Index?
type RegisterBank = Vec 32 RegisterValue

type PC = RegisterValue



-- Utility conversion function...
conv = (fromInteger . toInteger)

-- test values used in clashi to test
op 1 = 0xff010113 -- addi	x2,x2,-16
op 2 = 0x0c000593 -- li 	x11,192
op 3 = 0x00812423 -- sw 	x8,8(x2)
op 4 = 0xfb9ff0ef -- jal	x1,10150
op 5 = 0x10112623 -- sw 	x1,12(x2)
op 6 = 0xfec42703 -- lw 	x14,-20(x8)
op 7 = 0x40f90933 -- sub	x18,x18,x15
op 8 = 0x05778063 -- beq	x15,x23,10400
op 9 = 0x00010537 -- lui    x10,0x0
op 10 = 0x00318fb3-- add    x31,x3,x3
op 11 = 0xfa0680e7-- jalr   -96(x13)
op 12 = 0x0e042783-- lw     x15,0(x8)

emptyregs =
    0:>0:>0:>5:>0:>0:>0:>0:>0:>0:>0:>0:>0:>197:>0:>0:>
    0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>Nil
