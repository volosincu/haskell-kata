{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kata.DataTypes
  (
    Value(..)
  , Height(..)
  , User(..)
  , PendingTx(..)
  ) where


newtype Value a = Value a
  deriving (Eq, Ord, Show, Read)
  deriving newtype (Num)

data Height a = Height a deriving (Eq, Ord, Show, Read)

data User = User {
  uuid :: String
  , username :: String } deriving (Eq, Ord, Show, Read)

data PendingTx = PendingTx {
  contrib :: Value Int
  , sender :: User
  , deadline :: Height Int } deriving (Eq, Ord, Show, Read)
