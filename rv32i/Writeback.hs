{-# LANGUAGE RecordWildCards #-}
module Writeback where

import Clash.Prelude
import BaseTypes
import Execute
import Instructions

writeback
    :: RegisterBank -> PC
    -> Instruction
    -> RegisterValue
    -> RegisterValue
    -> (RegisterBank, PC)
writeback registers pc instruction execResult memValue = (registers', pc')
    where
        pc' = updatepc pc instruction execResult
        registers' = updateRegisters registers instruction execResult memValue pc

-- TODO: Consider making the cases explicit instead of a catch-all
updatepc :: PC -> Instruction -> RegisterValue -> PC
updatepc pc (RType _ _ _ _) _ = pc + 1
updatepc pc (IType op _ _ _) execResult = case op of
    JALR  -> conv execResult
    _     -> pc + 1
updatepc pc (SType op _ _ _) execResult = case op of
    BEQ   -> branchOrIncrement pc execResult
    BNE   -> branchOrIncrement pc execResult
    BLT   -> branchOrIncrement pc execResult
    BGE   -> branchOrIncrement pc execResult
    BLTU  -> branchOrIncrement pc execResult
    BGEU  -> branchOrIncrement pc execResult
    SB    -> pc + 1
    SH    -> pc + 1
    SW    -> pc + 1
updatepc pc (UType op _ _) execResult = case op of
    LUI   -> pc + 1
    AUIPC -> pc + 1
    JAL   -> conv (execResult `shiftR` 2)
updatepc pc UnknownType _ = pc + 1

branchOrIncrement :: PC -> RegisterValue -> PC
branchOrIncrement pc value = (if (value == 0) then (pc + 1) else (pc + conv shiftedValue))
    where
        shiftedValue = value `shiftR` 2

writeRegister :: RegisterBank -> RegisterID -> RegisterValue -> RegisterBank
writeRegister bank rd value = case rd of
    0 -> bank
    _ -> replace rd value bank

-- TODO: group these symbols in a record?
updateRegisters :: RegisterBank -> Instruction -> RegisterValue -> RegisterValue -> PC -> RegisterBank
updateRegisters bank instruction execResult memValue pc = case instruction of
    (RType _ _ _ rd)  -> writeRegister bank rd execResult
    (IType op _ _ rd) -> writeRegister bank rd (iValue op)
    (SType _ _ _ _)   -> bank
    (UType op _ rd)   -> writeRegister bank rd (uValue op)
    (UnknownType)     -> bank
    where
        iValue op = case op of
            JALR  -> conv (pc + 1)
            LB    -> memValue
            LH    -> memValue
            LW    -> memValue
            LBU   -> memValue
            LHU   -> memValue
            _     -> execResult
        uValue op = case op of
            LUI   -> execResult
            AUIPC -> execResult
            JAL   -> conv (pc + 1)
