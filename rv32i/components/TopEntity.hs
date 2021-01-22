module TopEntity where

import Clash.Prelude
import Globals
import Core


topEntity ::
    Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Signed 8)
topEntity = exposeClockResetEnable slowcore



slowcore :: HiddenClockResetEnable dom => Signal dom (Signed 8)
slowcore = resize <$> output 
    where
        core' = exposeEnable core
        output = core' (toEnable $ isEnabled <$> counter)    

        counter = register 0 ((+1) <$> counter)

isEnabled :: Unsigned 24 -> Bool
isEnabled ctr = ctr == 0


test = mapM_ print $ sampleN @System 50 slowcore