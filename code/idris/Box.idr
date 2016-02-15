module Box

import bool
import eq
import Serialize

data Box a = mkBox a

unbox: Box a -> a
unbox (mkBox b) = b

instance (eq a) => eq (Box a) where
  eql (mkBox v1) (mkBox v2) = eql v1 v2

instance (Serialize a) => Serialize (Box a) where
  toString (mkBox v) = "(" ++ (toString v) ++ ")"
