module reduce

import list
import nat
import bool



reduce: list Nat -> Nat
reduce nil = 0
reduce (h::l) = h + (reduce l)


-- binary operator
-- identity element (check)
-- type of list element (check)


fancy:(a -> a -> a) -> a -> (list a) -> a
fancy f id nil = id
fancy f id (h::t) = f h (fancy f id t)
