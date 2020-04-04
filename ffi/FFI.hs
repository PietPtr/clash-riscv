{-# LANGUAGE RecordWildCards #-}
module RISCV_FFI where

import Clash.Prelude
import BaseTypes

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import qualified Data.List as L

import Core

data CState = CState
    { c_pc :: CInt
    , c_registers :: [CInt]
    , c_memory :: [CInt]
    } deriving (Show)

-- instance Storable CState where
--     alignment _ = 8 -- lcm 8 8 4 = 8
--     sizeOf _    = 20 -- 8 + 8 + 4 = 20
--     peek ptr    = CState
--         <$> peekByteOff ptr 0
--         <*> peekByteOff ptr 4
--         <*> peekByteOff ptr 12
--     poke ptr (CState pc regs mem) = do
--         pokeByteOff ptr 0 pc
--         pokeByteOff ptr 4 regs
--         pokeByteOff ptr 12 mem

to_c :: SystemState -> CState
to_c state = c_state
    where
        SystemState{..} = state

        c_state = CState
            { c_pc = fromIntegral $ toInteger pc
            , c_registers = L.map (fromIntegral . toInteger) (toList registers)
            , c_memory = L.map (fromIntegral . toInteger) (toList memory)
            }

from_c :: CState -> SystemState
from_c c_state = state
    where
        CState{..} = c_state

        state = SystemState
            { pc = fromIntegral $ toInteger c_pc
            , registers = (regs_to_vec 32 d32) c_registers
            -- , memory = regs_to_vec 256 d256 c_memory
            }

regs_to_vec :: (KnownNat n) => Int -> SNat n -> [CInt] -> Vec n (RegisterValue)
regs_to_vec size d cint_list = bank
    where
        bank = L.foldl (\regs (value, index) -> replace index value regs) (replicate d 0) zipped
        zipped = L.zip regvalue_list [0..]
        regvalue_list :: [RegisterValue] = L.map (fromIntegral . toInteger) cint_list


-- query_core :: CState -> CState
-- query_core c_state = to_c $ core state 0
    -- where state = from_c c_state

state_to_array :: SystemState -> [CInt]
state_to_array state = L.map (conv) list
    where
        SystemState{..} = state
        list = toList $ (pc_reg:>registers) ++ memory
        pc_reg :: RegisterValue = conv pc

new_state :: [CInt] -> [CInt] -> [CInt] -> [CInt]
new_state [c_pc] c_regs c_mem = state_to_array $ core state 0
    where
        state = SystemState
            { pc = fromIntegral $ toInteger c_pc
            , registers = regs_to_vec 32 d32 c_regs
            , memory = regs_to_vec 256 d256 c_mem
            }



query_core :: (Ptr CInt) -> IO (Ptr CInt)
query_core state_data = do
    as_list <- peekArray (1 + 32 + 256) state_data
    as_ptr <- newArray as_list
    return (as_ptr)


foreign export ccall query_core :: Ptr CInt -> IO (Ptr CInt)

-- main = do
--     mapM_ print $ query_core state_data)

state_data :: [CInt]
state_data = [84] L.++ arrayregs L.++ arraymem
arrayregs = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
arraymem = [-50265837,42018339,50398227,-56349149,-55301085,-33282525,33554543,-20699389,-41670781,16189363,-17553885,-37476477,-555117,-51106269,-37476477,-17809181,-20699261,492819,46212099,50397459,32871,-33488621,1125923,8465443,33621011,5244819,-17553885,3147667,-17554397,-24894077,-20699901,-127930129,-22797789,1939,492819,29433987,25240579,33620243,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
