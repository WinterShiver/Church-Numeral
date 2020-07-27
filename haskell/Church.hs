-- Church.hs
-- Church Numeral: https://karczmarczuk.users.greyc.fr/Essays/church.html

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

-- Part 1. Defining church numbers

-- Church type and church values
-- Church values are defined by `zero` and `incr`

type Church a = (a -> a) -> a -> a

zero :: Church a
zero s z = z  
-- or: zero = const id

incr :: Church a -> Church a
incr n s = n s . s  
-- or: incr n s = s . n s

-- Frequently used church values

one = incr zero
two = incr one
three = incr two
four = incr three
five = incr four
six = incr five
seven = incr six
eight = incr seven
nine = incr eight
ten = incr nine
eleven = incr ten
twelve = incr eleven

-- Transform between church numbers and integer values

ch2num :: Integral a => Church a -> a
ch2num ch = ch (1+) 0


num2ch :: Integral a1 => a1 -> Church a
num2ch n = case compare n 0 of
  GT -> incr . num2ch $ pred n
  EQ -> zero
  LT -> error "in num2ch: arg n < 0"


-- Part 2. Defining operators on church numbers

add :: Church (Church b) -> Church b -> Church b
add n m = n incr m
-- or: add n m s = n s . m s

-- TODO: auto inference of tape var. 
--       eg. in add, etc. 