{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}

module Tricks where
  
import Data.Foldable
import Data.Function
  
data Nat = Z | S Nat deriving Eq

data Vec n a where
  Nil  :: Vec Z a
  (:-) :: a -> Vec n a -> Vec (S n) a
  
instance Foldable (Vec n) where
  foldr _ i Nil = i
  foldr f i (x :- xs) = f x (foldr f i xs)
  
instance Eq a => Eq (Vec n a) where
  (==) = (==) `on` toList
  
instance Show a => Show (Vec n a) where
  show = show . toList
  
instance Ord a => Ord (Vec n a) where
  compare = compare `on` toList
  
type Mat n a = Vec n (Vec n a)

