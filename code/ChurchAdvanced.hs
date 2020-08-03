-- ChurchAdvanced.hs: solve the type problem
-- Church Numeral: https://karczmarczuk.users.greyc.fr/Essays/church.html

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ChurchAdvanced where

import Control.Applicative (liftA2)
import Data.Function ((&))

import Peano

-- Part 1. Defining church numbers

-- Church type and church values
-- Church values are defined by `zero` and `incr`

newtype Church = Ch {
  runCh :: forall a. (a -> a) -> a -> a
}

zero :: Church
zero = Ch (\s z -> z)  

incr :: Church -> Church
incr (Ch n) = Ch (\s z -> s (n s z)) 

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

ch2num :: Integral a => Church -> a
ch2num (Ch n) = n (1+) 0

num2ch :: Integral a => a -> Church
num2ch n = case compare n 0 of
  GT -> incr . num2ch $ pred n
  EQ -> zero
  LT -> error "in num2ch: arg n < 0"


-- Part 2. Defining operators on church numbers

-- binary oprs: add, mul, pow

add :: Church -> Church -> Church
add (Ch n) (Ch m) = Ch (liftA2 (.) n m)

mul :: Church -> Church -> Church
mul (Ch n) (Ch m) = Ch (n . m)

pow :: Church -> Church -> Church
pow (Ch n) (Ch m) = Ch (m n)

decr :: Church -> Church
decr (Ch n) = Ch (\s z -> n (\g h -> h (g s)) (const z) id)

sub :: Church -> Church -> Church
sub (Ch n) (Ch m) = m decr (Ch n)


-- Part 3. Peano numbers (Nats)
-- Transformation between chs and nats, and so on

nat2ch = fold incr zero -- nat2ch n = fold incr zero n
ch2nat (Ch n) = n Succ Zero

fold' s z n = runCh (nat2ch n) s z  -- Redefining Peano.fold


-- Part 4. Making comparison (by judging the difference)

isZero :: Church -> Bool
isZero (Ch n) = n (const False) True

notZero :: Church -> Bool
notZero = not . isZero

instance Eq Church where
  (==) :: Church -> Church -> Bool
  n == m = isZero (sub n m) && isZero (sub m n)

instance Ord Church where
  compare :: Church -> Church -> Ordering
  compare n m 
    | notZero $ sub n m = GT
    | notZero $ sub m n = LT
    | otherwise = EQ



