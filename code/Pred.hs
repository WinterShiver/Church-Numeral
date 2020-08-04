{-# LANGUAGE RankNTypes #-}

module Pred where

import ChurchAdvanced

-- Method 1
-- Count from -1: using Maybe Church

type MaybeCh = Maybe Church

incr1 :: MaybeCh -> MaybeCh
incr1 Nothing = Just zero
incr1 (Just ch) = Just (incr ch)
-- incr1 Nothing = Just zero, incr1 (Just zero) = Just one, ..

decr1 :: Church -> MaybeCh
decr1 (Ch n) = n incr1 Nothing 
-- decr1 zero = Nothing, decr1 one = Just zero, decr1 two = Just one, ..


-- Method 2
-- Count from -1: using Tuple Church

type TupleCh = (Church, Church)

incr2 :: TupleCh -> TupleCh
incr2 (_, ch) = (ch, incr ch)
-- incr2 (u, zero) = (zero, one), incr2 (zero, one) = (one, two), ..

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

cpsNothing :: (a -> r) -> r -> r
cpsNothing ks kf = kf

cpsJust :: a -> (a -> r) -> r -> r
cpsJust a ks kf = ks a
 
incr1c :: CPSCh -> CPSCh
incr1c (CPSCh c) = CPSCh (cpsJust (c incr zero))
-- incr1c cpsNothing = cpsJust zero, incr1c (cpsJust zero) = cpsJust one, ..

decr1c' :: Church -> CPSCh
decr1c' (Ch n) = n incr1c (CPSCh cpsNothing)
-- decr1c' zero = cpsNothing, decr1c' one = cpsJust zero, ..

decr1c :: Church -> MaybeCh
decr1c ch = c Just Nothing
  where (CPSCh c) = decr1c' ch
-- decr1c zero = Nothing, decr1c one = Just zero, ..

decr'1c :: Church -> Church
decr'1c ch = c id zero
  where (CPSCh c) = decr1c' ch
-- decr'1c zero = zero, decr'1c one = zero, decr'1c two = one, ..