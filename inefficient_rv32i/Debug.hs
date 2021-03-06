module Debug where

import Clash.Prelude
import qualified Data.List as L

import BaseTypes
import Instructions
import Fetch
import Execute

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

import Text.Printf


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

-- testmem = mem
--     where
--         mem = L.foldl (\mem unit -> replace (snd unit) (fst unit) mem) (replicate d256 0) (L.zip memdata [0..])
--         -- memdata = [0xfe010113, 0x812e23, 0x2010413, 0x500793, 0xfef42623, 0x300793, 0xfef42223, 0xfe042423, 0x1c0006f, 0xfe842783, 0x378793, 0xfef42423, 0xfec42783, 0xfff78793, 0xfef42623, 0xfec42783, 0xfef042e3, 0x13, 0x78513, 0x1c12403, 0x2010113, 0x8067, 0xff010113, 0x112623, 0x812423, 0x1010413, 0xf99ff0ef, 0x793, 0x78513, 0xc12083, 0x812403, 0x1010113, 0xffffffff]
--         -- memdata = [0xfe010113, 0x812e23, 0x2010413, 0x21600793, 0xfef42623, 0xd500793, 0xfef42223, 0xfe042423, 0x200006f, 0xfe842703, 0xfe442783, 0xf707b3, 0xfef42423, 0xfec42783, 0xfff78793, 0xfef42623, 0xfec42783, 0xfef040e3, 0x13, 0x78513, 0x1c12403, 0x2010113, 0x8067, 0xff010113, 0x112623, 0x812423, 0x1010413, 0xf95ff0ef, 0x793, 0x78513, 0xc12083, 0x812403, 0x1010113, 0xffffffff]
--         memdata = [0xfd010113, 0x2812623, 0x3010413, 0xfca42e23, 0xfcb42c23, 0xfe042623, 0x200006f, 0xfec42703, 0xfd842783, 0xf707b3, 0xfef42623, 0xfdc42783, 0xfff78793, 0xfcf42e23, 0xfdc42783, 0xfef040e3, 0xfec42783, 0x78513, 0x2c12403, 0x3010113, 0x8067, 0xfe010113, 0x112e23, 0x812c23, 0x2010413, 0x500793, 0xfef42623, 0x300793, 0xfef42423, 0xfe842583, 0xfec42503, 0xf85ff0ef, 0xfea42223, 0x793, 0x78513, 0x1c12083, 0x1812403, 0x2010113, 0xffffffff]

testmem :: Memory
testmem =
    4244701459:>42018339:>50398227:>4238618147:>4239666211:>4261684771:>33554543:>4274267907:>4253296515:>16189363:>4277413411:>4257490819:>4294412179:>4243861027:>4257490819:>4277158115:>4274268035:>492819:>46212099:>50397459:>32871:>4261478675:>1125923:>8465443:>33621011:>5244819:>4277413411:>3147667:>4277412899:>4270073219:>4274267395:>4167037167:>4272169507:>1939:>492819:>29433987:>25240579:>33620243:>0xfffff06f:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0x001080b3:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>0:>Nil

showProcess :: (Unsigned 32, InstructionForm, Instruction, ExecutionResult, (Memory, RegisterValue), (PC, RegisterBank)) -> String
showProcess (input, form, instr, executed, (mem, memRes), (pc, regs)) =
    "input: " L.++ (showBin input) L.++ "\n" L.++
    "fetched: " L.++ (show form) L.++ "\n" L.++
    "decoded: " L.++ (show instr) L.++ "\n" L.++
    "executed: " L.++ (show executed) L.++ "\n" L.++
    "new memory: " L.++ (show mem) L.++ "\n" L.++
    "mem result: " L.++ (show memRes) L.++ "\n" L.++
    "new state: pc=" L.++ (show pc) L.++ ", regs=" L.++ (show regs) L.++ "\n"


pretty :: Instruction -> String
pretty (RType instr rs2 rs1 rd) = (show instr) L.++ ": " L.++ case instr of
    ADD  -> printf "store %s + %s into %s" rs2' rs1' rd'
    SUB  -> printf "store %s - %s into %s" rs2' rs1' rd'
    SLL  -> printf "store %s << (%s & 0b11111) into %s" rs2' rs1' rd'
    SLT  -> printf "if %s < %s then set %s to 1 else 0" rs2' rs1' rd'
    SLTU -> printf "if %s < %s (unsigned) then set %s to 1 else 0" rs2' rs1' rd'
    XOR  -> printf "store %s XOR %s into %s" rs2' rs1' rd'
    SRL  -> printf "store %s >> (%s & 0b11111) into %s" rs2' rs1' rd'
    SRA  -> printf "store %s >> (%s & 0b11111) (arithmetic) into %s" rs2' rs1' rd'
    OR   -> printf "store %s OR %s into %s" rs2' rs1' rd'
    AND  -> printf "store %s AND %s into %s" rs2' rs1' rd'
    where
        rs2' = regnumber $ conv rs2
        rs1' = regnumber $ conv rs1
        rd'  = regnumber $ conv rd
pretty (IType instr imm rs1 rd) = (show instr) L.++ ": " L.++ case instr of
    JALR  -> printf ""
    LB    -> printf "load byte from %s + imm(%i) into %s" rs1' imm' rd'
    LH    -> printf "load halfword from %s + imm(%i) into %s" rs1' imm' rd'
    LW    -> printf "load word from %s + imm(%i) into %s" rs1' imm' rd'
    LBU   -> printf "load byte (unsigned) from %s + imm(%i) into %s" rs1' imm' rd'
    LHU   -> printf "load halfworde (unsigned) from %s + imm(%i) into %s" rs1' imm' rd'
    ADDI  -> printf "store %s + imm(%i) into %s" rs1' imm' rd'
    SLTI  -> printf "if %s < imm(%i) then set %s to 1 else 0" rs1' imm' rd'
    SLTIU -> printf "if %s < imm(%i) (unsigned) then set %s to 1 else 0" rs1' imm' rd'
    XORI  -> printf "store %s XOR imm(%i) into %s" rs1' imm' rd'
    ORI   -> printf "store %s OR imm(%i) into %s" rs1' imm' rd'
    ANDI  -> printf "store %s AND imm(%i) into %s" rs1' imm' rd'
    SLLI  -> printf "store %s << imm(%i) into %s" rs1' imm' rd'
    SRLI  -> printf "store %s >> imm(%i) into %s" rs1' imm' rd'
    SRAI  -> printf "store %s << imm(%i) (arithmetic) into %s" rs1' imm' rd'
    where
        rs1' = regnumber $ conv rs1
        rd'  = regnumber $ conv rd
        imm' :: Int = conv imm
pretty (SType instr imm rs2 rs1) = (show instr) L.++ ": " L.++ case instr of
    BEQ  -> printf "if %s == %s then branch to pc + imm(%i)" rs2' rs1' imm'
    BNE  -> printf "if %s != %s then branch to pc + imm(%i)" rs2' rs1' imm'
    BLT  -> printf "if %s < %s then branch to pc + imm(%i)" rs2' rs1' imm'
    BGE  -> printf "if %s >= %s then branch to pc + imm(%i)" rs2' rs1' imm'
    BLTU -> printf "if %s < %s, unsigned then branch to pc + imm(%i)" rs2' rs1' imm'
    BGEU -> printf "if %s >= %s, unsigned then branch to pc + imm(%i)" rs2' rs1' imm'
    SB   -> printf "store byte at src(%s) in base(%s) + imm(%i)" rs2' rs1' imm'
    SH   -> printf "store halfword at src(%s) in base(%s) + imm(%i)" rs2' rs1' imm'
    SW   -> printf "store word at src(%s) in base(%s) + imm(%i)" rs2' rs1' imm'
    where
        rs2' = regnumber $ conv rs2
        rs1' = regnumber $ conv rs1
        imm' :: Int = conv imm
pretty (UType instr imm dest) = (show instr) L.++ ": " L.++ case instr of
    LUI   -> printf "add %i to dest(x%s)" imm' dest'
    AUIPC -> printf "add %i to pc, store in %s " imm' dest'
    JAL   -> printf "jump to pc + %i, store (pc+4) in %s" imm' dest'
    where
        imm' :: Int = conv imm
        dest' = regnumber $ conv dest
pretty (UnknownType) = "Unknown instruction"

regnumber :: Integer -> String
regnumber reg = "x" L.++ show reg

regnames :: Integer -> String
regnames reg = case reg of
    0  -> "zero"
    1  -> "ra"
    2  -> "sp"
    3  -> "gp"
    4  -> "tp"
    5  -> "t0"
    6  -> "t1"
    7  -> "t2"
    8  -> "fp"
    9  -> "s1"
    10 -> "a0"
    11 -> "a1"
    12 -> "a2"
    13 -> "a3"
    14 -> "a4"
    15 -> "a5"
    16 -> "a6"
    17 -> "a7"
    18 -> "s2"
    19 -> "s3"
    20 -> "s4"
    21 -> "s5"
    22 -> "s6"
    23 -> "s7"
    24 -> "s8"
    25 -> "s9"
    26 -> "s10"
    27 -> "s11"
    28 -> "t3"
    29 -> "t4"
    30 -> "t5"
    31 -> "t6"
