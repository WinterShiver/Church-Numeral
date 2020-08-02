{-# LANGUAGE LambdaCase #-}

module Peano where

-- Defining Peano numbers and basic oprs on it

data Nat = Zero | Succ Nat deriving (Show)

fold :: (a -> a) -> a -> Nat -> a
fold s z Zero = z
fold s z (Succ n) = s (fold s z n)

pincr = Succ
padd = fold pincr -- padd n m = fold pincr n m
pmul = ($ Zero). fold . padd -- pmul n m = fold (padd n) Zero m
ppow = ($ Succ Zero) . fold . pmul -- ppow n m = fold (pmul n) (Succ Zero) m

pdecr = \case 
  Zero -> Zero
  Succ n -> n
psub = fold pdecr -- psub n m = fold pdecr n m