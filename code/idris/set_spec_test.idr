module set_spec_test

import set_spec
import nat
import eq
import Serialize
import bool
import list

l1: list nat
l1 = (S O)::O::nil

s1: set nat
s1 = mkSet l1

l2: list nat
l2 = (S (S O))::O::nil

s2: set nat
s2 = mkSet l2



