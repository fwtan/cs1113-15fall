module nat

import bool
import pair
import eq
import Serialize

data nat = O | S nat

isZero: nat -> bool
isZero O = true
isZero _ = false

succ: nat -> nat
succ n = S n

pred: nat -> nat
pred O = O
pred (S n) = n

evenb: nat -> bool
evenb O = true
evenb (S O) = false
evenb (S (S n)) = evenb n


add: nat->nat->nat
add O m = O
add (S n) m = add n (S m)

||| given a pair of natural numbers, return its sum
mult: nat -> nat -> nat
mult O m = O
mult (S n) m = add m (mult n m)

fact: nat -> nat
fact O = S O
fact (S n) = mult (S n) (fact n)

expp: nat->nat -> nat
expp m O = S O
expp m (S n)= mult m (expp m n)

lepp: nat -> nat -> bool
lepp O _ = true
lepp _ O = false
lepp (S m) (S n) = lepp m n

eqpp: nat->nat ->bool
eqpp  a b = and (lepp a b) (lepp b a)

gtpp: nat->nat ->bool
gtpp  a b = not_bool (lepp a b)

gepp: nat-> nat -> bool
gepp  _ O = true
gepp  O _ = false
gepp (S m) (S n) = gepp m n

ltpp:  nat-> nat ->bool
ltpp a b = lepp b a

instance eq nat where
  eql n1 n2 = eqpp n1 n2
  
  
instance Serialize nat where
  toString O = "Z"
  toString (S n) = "s" ++ (toString n)
