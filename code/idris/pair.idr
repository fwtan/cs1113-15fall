module pair

import bool
import eq
import Serialize

data pair a b = mkPair a b

fst: pair a b -> a
fst (mkPair f s) = f

snd: pair a b -> b
snd (mkPair f s) = s

instance (eq a, eq b) => eq (pair a b) where
  eql (mkPair a1 b1) (mkPair a2 b2) = and (eql a1 a2) (eql b1 b2)
  
instance (Serialize a, Serialize b) => Serialize (pair a b) where
  toString (mkPair a1 b1) = "(" ++ (toString a1) ++ "," ++ (toString b1) ++ ")" 
