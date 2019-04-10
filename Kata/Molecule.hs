{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}

module Kata.Molecule () where

newtype Electrons = Electrons Int
  deriving (Eq, Ord, Show, Enum)
  deriving newtype (Num, Integral,Real)

data Atom = Hydrogen Electrons | Oxygen Electrons | Carbon Electrons deriving (Show, Eq)
data Substance = Ether | Water deriving (Show, Eq)

isH :: Atom -> Bool
isH _ = False
isH (Hydrogen (Electrons x)) = x == 2

isO :: Atom -> Bool
isO _ = False
isO (Oxygen (Electrons x)) = x == 6

class Molecule a where
  (<->) :: [a] -> [a] -> Substance

instance Molecule Atom where
  (<->) (x1:x2:[]) (y:[]) = let isWater = isH x1 && isH x2 && isO y in if isWater then Water else Ether
