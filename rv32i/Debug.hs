module Debug where

import Clash.Prelude
import qualified Data.List as L

import BaseTypes
import Instructions
import Fetch
import Execute

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)



showBin x = showIntAtBase 2 intToDigit x ""
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

testregs = replicate d32 0
    -- 0:>1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>13:>14:>15:>
    -- 16:>17:>18:>19:>20:>21:>22:>23:>24:>25:>26:>27:>28:>29:>30:>31:>Nil

testmem = mem
    where
        mem = L.foldl (\mem unit -> replace (snd unit) (fst unit) mem) (replicate d256 0) (L.zip memdata [0..])
        -- memdata = [0xfe010113, 0x812e23, 0x2010413, 0x500793, 0xfef42623, 0x300793, 0xfef42223, 0xfe042423, 0x1c0006f, 0xfe842783, 0x378793, 0xfef42423, 0xfec42783, 0xfff78793, 0xfef42623, 0xfec42783, 0xfef042e3, 0x13, 0x78513, 0x1c12403, 0x2010113, 0x8067, 0xff010113, 0x112623, 0x812423, 0x1010413, 0xf99ff0ef, 0x793, 0x78513, 0xc12083, 0x812403, 0x1010113, 0xffffffff]
        -- memdata = [0xfe010113, 0x812e23, 0x2010413, 0x21600793, 0xfef42623, 0xd500793, 0xfef42223, 0xfe042423, 0x200006f, 0xfe842703, 0xfe442783, 0xf707b3, 0xfef42423, 0xfec42783, 0xfff78793, 0xfef42623, 0xfec42783, 0xfef040e3, 0x13, 0x78513, 0x1c12403, 0x2010113, 0x8067, 0xff010113, 0x112623, 0x812423, 0x1010413, 0xf95ff0ef, 0x793, 0x78513, 0xc12083, 0x812403, 0x1010113, 0xffffffff]
        memdata = [0xfd010113, 0x2812623, 0x3010413, 0xfca42e23, 0xfcb42c23, 0xfe042623, 0x200006f, 0xfec42703, 0xfd842783, 0xf707b3, 0xfef42623, 0xfdc42783, 0xfff78793, 0xfcf42e23, 0xfdc42783, 0xfef040e3, 0xfec42783, 0x78513, 0x2c12403, 0x3010113, 0x8067, 0xfe010113, 0x112e23, 0x812c23, 0x2010413, 0x500793, 0xfef42623, 0x300793, 0xfef42423, 0xfe842583, 0xfec42503, 0xf85ff0ef, 0xfea42223, 0x793, 0x78513, 0x1c12083, 0x1812403, 0x2010113, 0xffffffff]

showProcess :: (Unsigned 32, InstructionForm, Instruction, ExecutionResult, (Memory, RegisterValue), State) -> String
showProcess (input, form, instr, executed, (mem, memRes), state) =
    "input: " L.++ (showBin input) L.++ "\n" L.++
    "fetched: " L.++ (show form) L.++ "\n" L.++
    "decoded: " L.++ (show instr) L.++ "\n" L.++
    "executed: " L.++ (show executed) L.++ "\n" L.++
    "new memory: " L.++ (show mem) L.++ "\n" L.++
    "mem result: " L.++ (show memRes) L.++ "\n" L.++
    "new state: " L.++ (show state) L.++ "\n"
