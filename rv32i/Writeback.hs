{-# LANGUAGE RecordWildCards #-}
module Writeback where

import Clash.Prelude
import BaseTypes
import Execute
import Instructions

writeback :: State -> Instruction -> RegisterValue -> RegisterValue -> State
writeback state instruction execResult memValue = state'
    where
        State{..} = state
        state' = State{pc = pc', registers = registers'}

        pc' = updatepc pc instruction execResult
        registers' = updateRegisters registers instruction execResult memValue pc

-- TODO: Consider making the cases explicit instead of a catch-all
updatepc :: PC -> Instruction -> RegisterValue -> PC
updatepc pc (RType _ _ _ _) _ = pc + 4
updatepc pc (IType op _ _ _) execResult = case op of
    JALR -> conv execResult
    _ -> pc + 4
updatepc pc (SType op _ _ _) execResult = case op of
    BEQ -> branchOrIncrement pc execResult
    BNE -> branchOrIncrement pc execResult
    BLT -> branchOrIncrement pc execResult
    BGE -> branchOrIncrement pc execResult
    BLTU -> branchOrIncrement pc execResult
    BGEU -> branchOrIncrement pc execResult
    SB -> pc + 4
    SH -> pc + 4
    SW -> pc + 4
updatepc pc (UType op _ _) execResult = case op of
    LUI -> pc + 4
    AUIPC -> pc + 4
    JAL -> conv execResult

branchOrIncrement :: PC -> RegisterValue -> PC
branchOrIncrement pc value = (if (value == 0) then (pc + 4) else (pc + conv value))

writeRegister :: RegisterBank -> RegisterID -> RegisterValue -> RegisterBank
writeRegister bank 0 _ = bank
writeRegister bank rd value = replace rd value bank

-- TODO: group these symbols in a record?
updateRegisters :: RegisterBank -> Instruction -> RegisterValue -> RegisterValue -> PC -> RegisterBank
updateRegisters bank (RType op _ _ rd) execResult _ _ = writeRegister bank rd execResult
updateRegisters bank (IType op _ _ rd) execResult memValue pc = writeRegister bank rd value
    where
        value :: RegisterValue = case op of
            JALR -> conv (pc + 4)
            LB -> memValue
            LH -> memValue
            LW -> memValue
            LBU -> memValue
            LHU -> memValue
            _ -> execResult
updateRegisters bank (SType _ _ _ _) _ _ _ = bank
updateRegisters bank (UType op _ rd) execResult memValue pc = writeRegister bank rd value
    where
        value :: RegisterValue = case op of
            LUI -> execResult
            AUIPC -> execResult
            JAL -> conv (pc + 4)
