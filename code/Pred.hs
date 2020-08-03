{-# LANGUAGE RankNTypes #-}

module Pred where

import ChurchAdvanced

-- Method 1
-- Count from -1: using Maybe Church

type MaybeCh = Maybe Church

incr1 :: MaybeCh -> MaybeCh
incr1 Nothing = Just zero
incr1 (Just ch) = Just (incr ch)

decr1 :: Church -> MaybeCh
decr1 (Ch n) = n incr1 Nothing 

-- decr1 zero = Nothing, decr1 one = Just zero, decr1 two = Just one, ..


-- Method 2
-- Count from -1: using Tuple Church

type TupleCh = (Church, Church)

incr2 :: TupleCh -> TupleCh
incr2 (_, ch) = (ch, incr ch)

decr2' :: Church -> TupleCh
decr2' (Ch n) = n incr2 (undefined, zero) 
-- decr2' zero = (u, zero), decr2' one = (one, two), ..

decr2 :: Church -> Church
decr2 = fst . decr2'
-- decr2 zero = u, decr2 one = zero, decr2 two = one, ..


-- Method 1c
-- Count from -1: pure lambda version
-- This form is deducted by CPS transform. See `Pred.md`.

newtype CPSCh = CPSCh {
  runCPSCh :: forall r. (Church -> r) -> r -> r
}

incr1c :: CPSCh -> CPSCh
incr1c (CPSCh c) = CPSCh (
  \ks kf -> ks (c incr zero) 
  )

decr1c' :: Church -> CPSCh
decr1c' (Ch n) = n incr1c (CPSCh (\ks kf -> kf))

decr1c :: Church -> MaybeCh
decr1c ch = let c = runCPSCh (decr1c' ch) in c Just Nothing

decr'1c :: Church -> Church
decr'1c ch = let c = runCPSCh (decr1c' ch) in c id zero