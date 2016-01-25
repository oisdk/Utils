module Maps 
  ( increment
  , counts
  , fromListNoRep
  , mostFrequent
  ) where
  
import Data.Map.Strict (Map, insertWith, empty, insertLookupWithKey, singleton)
import Data.Foldable (foldr, foldr1, Foldable)
import Prelude hiding (foldr)
import Data.Maybe (isNothing)
import Data.Monoid
import Control.Arrow (first)

import AppFunc
  
increment :: (Integral a, Ord k) => k -> Map k a -> Map k a
increment k = insertWith (+) k 1

incrementRet :: (Integral a, Ord k) => k -> Map k a -> (a, Map k a)
incrementRet k = first (maybe 1 succ) . insertLookupWithKey (const (+)) k 1

counts :: (Integral a, Ord k, Foldable f) => f k -> Map k a
counts = foldr increment empty

fromListNoRep :: (Ord k, Foldable f) => f (k,a) -> Maybe (Map k a)
fromListNoRep xs = foldr f (Just empty) xs where
  f (k,v) m = snd <$< ensure (isNothing.fst) . insertLookupWithKey undefined k v =<< m
  
mostFrequent :: (Ord k, Foldable f) => f k -> Maybe (Int, k)
mostFrequent = (\(_,a,b) -> (a,b)) <$< foldr f Nothing where
  f e Nothing = Just (singleton e 1, 1, e)
  f e (Just (m, c, b)) = case compare d c of
    LT -> Just (n, c, b)
    EQ -> Just (n, c, b)
    GT -> Just (n, d, e)
    where (d, n) = incrementRet e m
