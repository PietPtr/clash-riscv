module Core where

import Clash.Prelude
import qualified IF
import qualified DE
import qualified RegisterFile as RF
import Instructions
import Globals




core = (maybe 0 id) <$> regValue
    where
        (rfStall, regValue) = unbundle $ RF.system regInstr

        (decoded, regInstr) = unbundle $ DE.system fetched

        (fetched, readPC)   = unbundle $ IF.system $ bundle 
                (doStall, progmem, register Nothing $ pure Nothing)

        doStall = or <$> (bundle (rfStall:>Nil))

        -- dit moet natuurlijk een los memory blokje worden maar dat komt wel
        progmem = blockRam testmem (cutPC <$> readPC) (register Nothing $ pure Nothing)
        









cutPC :: IF.PC -> Unsigned 8
cutPC pc = resize pc

testmem = 
    (-50265837):>(42018339):>(50398227):>(-56349149):>(-55301085):>(-33282525):>(33554543):>(-20699389):>
    (-41670781):>(16189363):>(-17553885):>(-37476477):>(-555117):>(-51106269):>(-37476477):>(-17809181):>
    (-20699261):>(492819):>(46212099):>(50397459):>(32871):>(-33488621):>(1125923):>(8465443):>(33621011):>
    (5244819):>(-17553885):>(3147667):>(-17554397):>(-24894077):>(-20699901):>(-127930129):>(-22797789):>
    (1939):>(492819):>(29433987):>(25240579):>(33620243):>(32871):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>
    (0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>(0):>Nil