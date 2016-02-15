module dict

||| Dict data type; abstract (without implementation details)
||| represent a dictionary as a set of "key-value" pairs with the
||| invariant that at most one pair can ahve a given key
data dict: (k: Type) -> (v: Type) -> Type
data dict k v = mkDict (set (pair k v))


||| an empty dictionary
||| non-trivial dictionaries
emptyDict: dict k v
emptyDict = mkDict emptySet

matchKey: (eq k) => k -> pair k v -> bool
matchKey k (mkPair k1 p1) = ite (eql k k1) true false

notMatchKey: (eq k) => k -> pair k v -> bool
notMatchKey k p = not (matchKey k p)



||| derive a new dictionary by removing any pair matching a key
dict_remove: (eq k) => k -> dict k v -> dict k v
dict_remove k (mkDict S) = mkDict (set_filter (notMatchKey k) s)



||| this function derives a new dictionary by tuple override. It inserts
||| a new pair into a dictionary, first removing any pair in the
||| dictionary with a matching key
dict_override: (eq k, eq (pair k v)) => k -> v -> dict k v -> dict k v


key2value: (eq k) => k -> pair k v -> option v
key2value k (mkPair k1 p1) = ite (eql k k1) (some p1) none




||| this function "computes the partial function" that the dictionary
||| represents. It takes a key and returns and option value: the
||| value in the pair with the matching key, if any, otherwise none
dict_lookup: (eq k) => k -> dict k v -> option v
dict_lookup k (mkDict S) = 


dict_eql: (eq k, eq v) => (dict k v) -> (dict k v) -> bool

instance (eq k, eq v) => eq (dict k v) where
  eql d1 d2 = dict_eql d1 d2



dict_toString: (Serialize k, Serialize v) => (dict k v) -> String

instance (Serialize k, Serialize v) => Serialize dict k v where
  toString d = dict_to String d
