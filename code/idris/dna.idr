module dna

import nat
import pair
import list

data base = A | T | C | G

complement_base: base -> base
complement_base A = T
complement_base T = A
complement_base C = G
complement_base G = C

complement_strand: list base -> list base
complement_strand l = map complement_base l

strand1: list (pair base base) -> list base
strand1 l = map fst l

strand2: list (pair base base) -> list base
strand2 l = map snd l

make_base_pair: base -> pair base base
make_base_pair a = mkPair a (complement_base a)

complete: list base -> list (pair base base)
complete l = map make_base_pair l

isEqu: base -> base -> nat
isEqu A A = (S O)
isEqu T T = (S O)
isEqu C C = (S O)
isEqu G G = (S O)
isEqu _ _ = O


countBase: base -> list base -> nat
countBase h l = reduce add O (map (isEqu h) l)

