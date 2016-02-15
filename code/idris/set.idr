module set_list_impl

--import public set
import list

import ifThenElse
import eq
import bool

data set a = mkSet (list a)

set_empty: set a
set_empty = mkSet nil

set_insert: (eq a) => a -> set a -> set a
set_insert v (mkSet l) = ite (member v l)
                             (mkSet l)
                             (mkSet (v::l))
                             
set_eql: (eq a) => (s1: set a) -> (s2: set a) -> bool
set_eql (mkSet l1) (mkSet l2) = sameList l1 l2
                             
--instance (eq a) => eq (set a) where
--  eql (mkSet l1) (mkSet l2) = sameList l1 l2
