module NatL where

import Data.Foldable

import DataL
import PreludeL


data Nat = Z | S Nat
  deriving Show

instance DataL Nat where
  unrestrict Z     = Unrestricted Z
  unrestrict (S x) = unrestrict x &. \(Unrestricted x')
                  -> Unrestricted (S x')


natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S x) = succ (natToInt x)

natLength :: Foldable f => f a -> Nat
natLength = go . toList
  where
    go []     = Z
    go (_:xs) = S (go xs)
