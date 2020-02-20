module BaseTypes where

import Clash.Prelude

type Register = Unsigned 32
type RegisterID = Unsigned 5
type Registers = Vec 32 Register
