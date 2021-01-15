module Globals where

import Clash.Prelude


type Value          = Signed 32 -- the basic value on which the entire processor operates
type Immediate      = Value

type Stall = Bool