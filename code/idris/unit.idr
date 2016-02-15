module unit

import bool
import eq
import Serialize

data unit = void

unit_id: unit -> unit
unit_id void = void

instance eq unit where
  eql u1 u2 = true

instance Serialize unit where
  toString u = " "
