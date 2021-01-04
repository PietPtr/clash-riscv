module BaseTypes where

import Clash.Prelude
import qualified Data.List as L

import qualified Debug.Trace

trace = Debug.Trace.trace

type RegisterValue  = Signed 32
type RegisterID     = Unsigned 5 -- TODO: Maybe it's better form to make this of type Index?
type RegisterBank   = Vec 32 RegisterValue

type Memory         = Vec 256 RegisterValue
type Address        = Unsigned 10
type AlignedAddress = Unsigned 8
type PC             = AlignedAddress

type WriteData =  Maybe (Address, RegisterValue)
type Output = (Address, WriteData)


-- Utility conversion function...
conv = (fromInteger . toInteger)

emptyregs = replicate d32 0
emptymem = replicate d256 0

-- Classes
class BitMapping a where
    fromBits :: KnownNat n => Unsigned n -> a
    fromVector :: KnownNat n => BitVector n -> a
