{-# LANGUAGE DeriveFunctor #-}

module Tricks where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid

newtype DefaultDict key value = 
  DefaultDict { getMap     :: Map key value
              } deriving (Functor, Show)
              
lookup :: (Ord k, Monoid v) => k -> DefaultDict k v -> v
lookup k (DefaultDict m) = fromMaybe mempty (Map.lookup k m)

insert :: Ord k => k -> v -> DefaultDict k v -> DefaultDict k v
insert k v (DefaultDict m) = DefaultDict (Map.insert k v m)

alter :: (Ord k, Monoid v) => (v -> v) -> k -> DefaultDict k v -> DefaultDict k v
alter f k (DefaultDict m) = 
  DefaultDict (Map.alter (Just . f . fromMaybe mempty) k m) where

instance (Ord k, Eq v, Monoid v) => Eq (DefaultDict k v) where
  DefaultDict m == DefaultDict n = 
    Map.null (Map.mergeWithKey neq ndf ndf m n) where
      neq _ x y | x == y = Nothing
                | otherwise = Just x
      ndf = Map.filter (mempty/=)
      
delete :: Ord k => k -> DefaultDict k v -> DefaultDict k v
delete k (DefaultDict m) = DefaultDict (Map.delete k m)

fromList :: (Foldable f, Ord k, Eq v) => v -> f (k,v) -> DefaultDict k v
fromList d = foldr (uncurry insert) (DefaultDict Map.empty)

count :: (Foldable f, Ord k) => f k -> DefaultDict k (Sum Int)
count = foldr (alter succ') empty where
  succ' (Sum n) = Sum (succ n)

empty :: DefaultDict k v
empty = DefaultDict Map.empty