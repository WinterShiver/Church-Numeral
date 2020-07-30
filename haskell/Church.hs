-- Church.hs
-- Church Numeral: https://karczmarczuk.users.greyc.fr/Essays/church.html

-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE RankNTypes #-}

{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2)
import Data.Function ((&))

-- Part 1. Defining church numbers

-- Church type and church values
-- Church values are defined by `zero` and `incr`

type Church a = (a -> a) -> a -> a

zero :: Church a
zero s z = z  
-- or: zero = const id
-- or: zero = flip const

incr :: Church a -> Church a
incr = liftA2 (.) id -- from: incr n s = s . n s
-- or: incr n s = n s . s  

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

-- binary oprs: add, mul, pow

add :: Church a -> Church a -> Church a
add = liftA2 (.) -- from: add n m s z = n s (m s z)
-- or: add n m = n incr m, but this causes complicated type 

mul :: Church a -> Church a -> Church a
mul = (.) -- from: mul n m s z = n (m s) z
-- or: mul n m = n (add m) zero, but this causes complicated type 

pow :: Church a -> Church (a -> a) -> Church a
pow = (&)
-- from: pow n m = m n, this has caused complicated type
-- or: pow n m = m (mul n) one, but this causes complicated type 

-- predecessor (decr) and subtraction 

decr :: Church ((a -> a) -> a) -> Church a
decr n s z = ($ id) $ n ((&) . ($ s)) (const z) -- from: decr n s z = n (\g h -> h (g s)) (const z) id


-- not able to construct sub n m = n decr m



-- Part 3. Peano numbers (Nats)
-- Basic oprs, transformation between chs and nats, and so on

data Nat = Zero | Succ Nat deriving (Show)

fold :: (a -> a) -> a -> Nat -> a
fold s z Zero = z
fold s z (Succ n) = s (fold s z n)

-- fold s z n = (nat2ch n) s z

padd = fold Succ -- padd n m = fold Succ n m
pmul = ($ Zero). fold . padd -- pmul n m = fold (padd n) Zero m
ppow = ($ Succ Zero) . fold . pmul -- ppow n m = fold (pmul n) (Succ Zero) m

nat2ch = fold incr zero -- nat2ch n = fold incr zero n
ch2nat = ($ Zero) . ($ Succ) -- ch2nat ch = ch Succ Zero

pincr = Succ
pdecr = \case 
  Zero -> Zero
  Succ n -> n

