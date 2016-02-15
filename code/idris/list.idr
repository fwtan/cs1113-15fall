module list

import bool
import nat
import ifThenElse
import Person
import eq
import Serialize

data list a = nil | (::) a (list a)

length: list a -> nat
length nil = O
length (n::l) = S (length l)

append: list a -> list a -> list a
append nil l = l
append (h::t) l = h::(append t l)

map: (a -> b) -> list a -> list b
map f nil = nil
map f (h::t) = (f h)::(map f t)

filter: (a -> bool) -> list a -> list a
filter f nil = nil
filter f (h::t) = ite (f h) (h::(filter f t)) (filter f t)

foldr: (a -> a -> a) -> a -> list a -> a
foldr f id nil = id
foldr f id (h::t) = f h (list.foldr f id t)

--query: (value -> value -> value) -> value -> 
--         (tuple -> value) -> (tuple -> bool) -> 
--            (list tuple) -> value
--query reduce id project select relation =
--       list.foldr reduce id (map project (filter select relation))
       
--query2:  (list tuple) -> (tuple -> bool) -> (tuple -> value) ->
--            (value -> value -> value) -> value -> value
--query2 relation select project reduce id =
--         list.foldr reduce id (map project (filter select relation))


member: (eq a) => a -> list a -> bool 
member v nil = false
member v (h::l) = ite (eql v h)
                  true
                  (member v l)  
                  
instance (eq a) => eq (list a) where
  eql nil nil = true
  eql h nil = false
  eql nil h = false
  eql (h1::l1) (h2::l2) = ite (eql h1 h2)
                          (eql l1 l2)
                          false

containList: (eq a) => list a -> list a -> bool
containList nil t = true
containList (h::l) t = ite (member h t)
                    (containList l t)
                    false
                    
sameList: (eq a) => list a -> list a -> bool
sameList l1 l2 = and (containList l1 l2) (containList l2 l1)  

list2string: (eq a, Serialize a) => list a -> String
list2string nil = ""
list2string (h::l) = (toString h) ++ (ite (eql l nil) ("") (",") ++ (list2string l))                  

instance (eq a, Serialize a) => Serialize (list a) where
  toString l = "[" ++ (list2string l) ++ "]"
  

