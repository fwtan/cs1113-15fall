module scratch

import list
import bool
import nat
import ifThenElse

l: list nat
l = (S O)::(S (S O))::(S (S (S O)))::S (S (S (S (S (S O)))))::nil

evens: list nat -> list nat
evens nil = nil
evens (h::t) = ite (evenb h) (h::(evens t)) (evens t)

l': list nat
l' = evens l

filter: (a -> bool) -> list a -> list a
filter f nil = nil
filter f (h::t) = ite (f h) (h::(filter f t)) (filter f t)
