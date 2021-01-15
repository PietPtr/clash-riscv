module RegisterFile where

import Clash.Prelude
import Globals

data ReadInstr
    = None
    | One ID
    | Two ID ID
    deriving (Show, Generic, NFDataX)

data ReadState
    = Ready
    | Read2
    | Read1
    deriving (Show, Generic, NFDataX)

type ID = Index 32

type State  = ReadInstr
type Input  = (ReadInstr, Value)
type Output = (Stall, ID, (Maybe Value))

reader :: State -> Input -> (State, Output)
reader (workingInstr) (inInstr, bramValue) = (state', output)
    where
        -- inputs

        -- state
        
        workingInstr' = case workingInstr of
            Two id1 id2 -> One id1
            One id1 -> inInstr
            None -> inInstr

        state' = workingInstr'

        -- outputs
        stall = case workingInstr of
            Two _ _ -> True
            One _ -> False
            None -> False

        readID = case workingInstr' of
            Two _ id2 -> id2
            One id1 -> id1
            None -> 31

        readResult = case workingInstr of
            Two _ _ -> Just bramValue
            One _ -> Just bramValue
            None -> Nothing

        output = (stall, readID, readResult)

       

readerB :: HiddenClockResetEnable dom =>
    Signal dom Input -> Signal dom Output
readerB = mealy reader None


-- system :: HiddenClockResetEnable dom =>
--     (Signal dom ReadInstr) -> Signal dom (Maybe Value)
system readInstrs = bundle (stall, out)
    where
        (stall, readID, out) = unbundle $ readerB $ bundle (readInstrs, bramOut)

        bramOut = blockRam (iterate d32 (+1) 100) readID (register Nothing $ pure Nothing)


-- hmm het werkt maar dan moet je wel 1 cycle later beginnen...
-- system :: HiddenClockResetEnable dom =>
--     (Signal dom (ID, ID)) -> {-Signal dom (Maybe (ID, Value)) ->-} Signal dom (Maybe (Value, Value))
-- system reads  = out
--     where
--         (rs1, rs2) = unbundle reads
--         (readAddr, out) = unbundle $ readerB $ bundle (rs1, rs2, value)
--         value = blockRam (iterate d32 (+1) 100) readAddr (register Nothing $ pure Nothing)

