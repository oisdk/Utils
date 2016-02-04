module Folds
  ( count
  , unfoldl
  , shorterThan
  , longerThan
  , filterAccumL
  , pairs 
  ) where
  
import Data.Foldable (Foldable, foldl', foldr)
import Prelude hiding (foldr)
import Control.Monad.State (runState, state)
import Control.Monad (filterM)
import Data.Maybe (isNothing)
import Data.Bits
import AppFunc
  

count :: (Eq a, Foldable f, Integral n) => a -> f a -> n
count x = foldl' (\a e -> if x == e then a+1 else a) 0

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f = r [] where r a x = maybe a (\(x,y) -> r (y:a) x) (f x)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = fst . foldr f ([],[]) where
  f e t = (if p e then fst t else xs,xs) where xs = e : snd t

shorterThan :: (Foldable f, Integral n) => n -> f a -> Bool
shorterThan n xs = foldr (\_ a n -> 1 < n && a (n-1)) (const True) xs n 

longerThan :: (Foldable f, Integral n) => n -> f a -> Bool
longerThan n xs = foldr (\_ a n -> 1 > n || a (n-1)) (const False) xs n

filterAccumL :: (x -> acc -> (Bool, acc)) -> acc -> [x] -> ([x], acc)
filterAccumL f s t = runState (filterM (state . f) t) s

pairs :: Foldable f => f a -> Maybe [(a,a)]
pairs = snd <$< ensure (isNothing.fst) . foldr f (Nothing,[]) where 
  f e (Nothing,a) = (Just e ,       a)
  f e (Just b ,a) = (Nothing, (e,b):a)
  
powerSet :: [a] -> [[a]]
powerSet xs = (\n -> [ x | (i, x) <- zip [0..] xs, testBit n i ] ) <$> [0..]

clines :: [String] -> String
clines xs = foldr f id xs "" where
  f e a = showString e . showString ";\n" . a
 