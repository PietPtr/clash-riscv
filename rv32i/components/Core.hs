module Core where

import Clash.Prelude
import qualified IF
import qualified RegisterFile as RF
import Instructions
import Globals




core = 
    where
        (rfStall, regValue) = RF.system regInstr

        doStall = or <$> (bundle (rfStall:>Nil))