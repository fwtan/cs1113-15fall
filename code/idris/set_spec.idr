module set_spec

-- imports needed for specification
import bool
import option
import pair
import list
import nat
import eq
import Serialize
import reduce

import public list
import ifThenElse

{-
*********************************************
**************** SPECIFICATION **************
*********************************************
-}

------------
-- data type
------------

-- A polymorphic set type. We interpret values of type (set a) as
-- representing sets of values of type a.
data set: (a: Type) -> Type

------------
-- functions
------------

-- The empty set of a
emptySet: set a

-- If s is empty then true, otherwise false
isEmpty: (s: set a) -> bool


-- Return the set, s union {v}
set_insert: (eq a) => a -> set a -> set a


-- Return the set, s - {v}
set_remove: (eq a) => (v: a) -> (s: set a) -> set a


-- Return the number of elements in s
set_cardinality: (s: set a) -> nat


-- If v is in s return true, otherwise false
set_member: (eq a) => (v: a) -> (s: set a) -> bool


-- Return union of s1 and s2
set_union: (eq a) => (s1: set a) -> (s2: set a) -> set a


-- Return the intersection of s1 and s2
set_intersection: (eq a) => (s1: set a) -> (s2: set a) -> set a


-- Return the set difference, s1 minus s2
set_difference: (eq a) => (s1: set a) -> (s2: set a) -> set a


-- Return true p is true for every v in s, otherwise false
set_forall: (p: a -> bool) -> (s: set a) -> bool


-- Return true if p is true for some v in s, otherwise else false
set_exists: (p: a -> bool) -> (s: set a) -> bool


-- If (set_exists p s), return (some v) such that (p v) is true, else
-- return none. We need to return an option because there might not be
-- an element in s with property p.  If there is such an element, we
-- call it a "witness to the property p."
set_witness: (p: a -> bool) -> (s: set a) -> option a


-- Return the cartesian product of s1 and s2. That is, return the
-- set of all pairs whose first element is taken from s1 and whose
-- second element is taken from s2. For example, the product of the
-- sets {1, 2} and {a, b} is { (1, a), (1, b), (2, a), (2, b) }.
set_product: (s1: set a) -> (s2: set b) -> set (pair a b)


-- Extra credit: Return the set of all sets of elements of s.
-- For example, the powerset of {1, 2, 3} is the following set:
-- { {}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3} }. The
-- cardinality of the powerset of a set of cardinality n is 2^n.
set_powerset: (s: set a) -> set (set a)


-- return true if s1 and s2 are equal, else false
set_eql: (eq a) => (s1: set a) -> (s2: set a) -> bool


-- Return a string representation of s
set_toString: (Serialize a) => set a -> String


------------------
-- class instances
------------------

-- Overloaded eql for sets: it just calls set_eql
-- Idris won't let us separate specification from
-- implementation for class instances, so we write
-- the implementation here, but it's just a call to
-- the specified (but not yet implemented) seq_eql.
-- You do not need to implement this class instance
-- any further in the code below.
instance (eq a) => eq (set a) where
  eql s1 s2 = set_eql s1 s2


-- Overloaded toString for sets, calls set_toString
-- The same comment right above applies here, too
instance (Serialize a) => Serialize (set a) where
  toString s = set_toString s


{-
*********************************************
*************** IMPLEMENTATION **************
*********************************************
-}

------------------------------------
data set a = mkSet (list a)

------------------------------------

emptySet = mkSet nil

-------------------------------------

-- isEmpty function specified above.
-- isEmpty: (s: set a) -> bool
isEmpty (mkSet nil) = true
isEmpty _ = false

-------------------------------------
-- set_insert: (v: a) -> (s: set a) -> set a
-- Return the set, s union {v}
set_insert v (mkSet l) = ite (member v l)
                             (mkSet l)
                             (mkSet (v::l))

-------------------------------------

-- helper function: return list l without value v
list_remove: (eq a) => (v: a) -> (l: list a) -> list a
list_remove v nil = nil
list_remove v (h::t) = ite (eql v h) t (h::list_remove v t)

-- set_remove: (eq a) => (v: a) -> (s: set a) -> set a
-- Return the set, s - {v}
set_remove v (mkSet l) = mkSet (list_remove v l)

-------------------------------------

-- Return the number of elements in s
-- set_cardinality: (s: set a) -> nat
-- relies on rep invariant
set_cardinality (mkSet l) = length l

-------------------------------------

-- If v is in s return true, otherwise false
--set_member: (v: a) -> (s: set a) -> bool
set_member v (mkSet l) = member v l

-------------------------------------

-- Return union of s1 and s2
-- set_union: (s1: set a) -> (s2: set a) -> set a

-- hint, write a function, combine, that combines two
-- lists into one, avoiding duplicate elements

--combine: (eq a) => list a -> list a -> list a
--combine nil l2 = l2
--combine (h::t) l2 = ite (member h l2) (combine t l2) (h::(combine t l2))

--set_union (mkSet l1) (mkSet l2) = mkSet (combine l1 l2)
-- combine 2 lists into one, avoiding duplicate elements: check list 1
  -- member with list 2 members
combine: (eq a) => list a -> list a -> list a
combine nil l1 = l1
combine (h::t) l2 = ite (member h l2) (combine t (list_remove h (append (h::t) l2))) (combine t l2)
--check list 2 members with list 1 members and return final combined list
combine2: (eq a)=> list a -> list a -> list a
combine2 l1 l2 = append (combine l1 l2) (combine l2 l1)

--return union of s1 and s2
--set_union: (eq a) => (s1: set a) -> (s2: set a) -> set a
set_union (mkSet l1) (mkSet l2) = mkSet (combine2 l1 l2)

-------------------------------------

intersect: (eq a) => list a -> list a -> list a
intersect nil _ = nil
intersect (h::t) l2 = ite (member h l2) (h::(intersect t l2)) (intersect t l2)

-- Return the intersection of s1 and s2
-- set_intersection: (eq a) => list a -> list a -> list a
set_intersection (mkSet l1) (mkSet l2) = mkSet (intersect l1 l2)

-------------------------------------

--list_difference: (eq a) => list a -> list a -> list a
--list_difference l1 nil = l1
--list_difference l1 (h::t) = list_difference --(list_remove h l1) t

list_difference: (eq a) => list a -> list a -> list a
list_difference l1 nil = l1
list_difference (h1::t1) (h2::t2) = ite (eql h1 h2)
                                      (list_difference t1 t2)
                                      (h1::list_difference t1 t2)

-- Return the set difference, s1 minus s2
--set_difference: (eq a) => (s1: set a) -> (s2: set a) -> set a
set_difference (mkSet l1) (mkSet l2) = mkSet (list_difference l1 l2)

-------------------------------------

-- Return true p is true for every v in s, otherwise false
-- set_forall: (p: a -> bool) -> (s: set a) -> bool

set_forall p (mkSet l) = fancy and true (map p l)

-------------------------------------

-- Return true if p is true for some v in s, otherwise else false
-- set_exists: (p: a -> bool) -> (s: set a) -> bool
set_exists p (mkSet l) = fancy or false (map p l)

-------------------------------------

list_witness: (a -> bool) -> list a -> option a
list_witness p nil = none
list_witness p (h::t) = ite (p h) (some h) (list_witness p t)

-- If (set_exists p s), return (some v) such that (p v) is true,
-- else return none. We need to return an option because of course
-- in general there might not be an element in s with property p.
-- If there is one, we call it a "witness to the property, p."
-- set_witness: (p: a -> bool) -> (s: set a) -> option a

set_witness p (mkSet l) = list_witness p l

-------------------------------------

listh: a -> list b -> list (pair a b)
listh a nil = nil
listh a (h::t) = (mkPair a h)::(listh a t)
listab: list a -> list b -> list (pair a b)
listab nil lb = nil
listab (h::t) lb = append (listh h lb)  (listab t lb)

-- Return the cartesian product of s1 and s2. That is, return the
-- set of all pairs whose first element is taken from s1 and whose
-- second element is taken from s2. For example, the product of the
-- sets {1, 2} and {a, b} is { (1, a), (1, b), (2, a), (2, b) }.
-- set_product: (s1: set a) -> (s2: set b) -> set (pair a b)

-------------------------------------

set_product (mkSet la) (mkSet lb) = mkSet (listab la lb)

-- Extra credit: Return the set of all sets of elements of s.
-- For example, the powerset of {1, 2, 3} is the following set:
-- { {}, {1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3} }. The
-- cardinality of the powerset of a set of cardinality n is 2^n.
-- set_powerset: (s: set a) -> set (set a)

append_set: a -> set a -> set a
append_set h (mkSet l) = mkSet (h::l)

append_listset: a -> list (set a) -> list (set a)
append_listset v nil = (mkSet (v::nil))::nil
append_listset v (h::l) = (append_set v h)::  (append_listset v l)

list_powerlist: list a -> list (set a)
list_powerlist nil = nil
list_powerlist (h::l) = append (append_listset h (list_powerlist l)) (list_powerlist l)

--set_powerset (mkSet l) = mkSet (list_powerlist l)

list_powerset: list a -> list a
list_powerset nil = nil
list_powerset (h::t) = append (h::t)  (list_powerset t)

set_powerset (mkSet nil) = mkSet (nil)
set_powerset (mkSet (h::t)) = mkSet ((mkSet (h::t))::(mkSet (list_powerset t))::nil)

-------------------------------------

subset_elements: (eq a) => list a -> list a -> bool
subset_elements nil l2 = true
subset_elements (h::t) l2 = and
                             (member h l2)
                             (subset_elements t l2)

same_elements: (eq a) => list a -> list a -> bool
same_elements l1 l2 =
                 and
                  (subset_elements l1 l2)
                  (subset_elements l2 l1)

set_eql (mkSet l1) (mkSet l2) = same_elements l1 l2

--set2string: (eq a, Serialize a) => set a -> String
--set2string (mkSet nil) = ""
--set2string (mkSet(h::nil)) = toString h
--set2string (mkSet(h::l)) = (toString h) ++ (",") ++ (set2string l)


--set_toString: (Serialize a) => set a -> String
--set_toString s = ("[") ++ (set2string s) ++ ("]")


-------------------------------------

