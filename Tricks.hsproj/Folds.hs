{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.Set (empty, member, insert)
import qualified Control.Arrow
import Data.Bits
import AppFunc
import Control.Applicative hiding (empty)
import Function  
import Control.Arrow
import Data.Bifunctor
import Data.Traversable

count :: (Eq a, Foldable f, Integral n) => a -> f a -> n
count x = foldl' (\a e -> if x == e then a+1 else a) 0

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f = r [] where r a x = maybe a (\(x,y) -> r (y:a) x) (f x)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = fst . foldr f ([],[]) where
  f e t = (if p e then fst t else xs,xs) where xs = e : snd t

shorterThan :: (Foldable f, Integral n) => f a -> n -> Bool
shorterThan = foldr (\_ a !n -> 1 < n && a (n-1)) (const True)

longerThan :: (Foldable f, Integral n) => f a -> n -> Bool
longerThan = foldr (\_ a !n -> 1 > n || a (n-1)) (const False)

filterAccumL :: (x -> acc -> (Bool, acc)) -> acc -> [x] -> ([x], acc)
filterAccumL f s t = runState (filterM (state . f) t) s

uniq :: Ord a => [a] -> [a]
uniq = fst . filterAccumL (not .: member &&&& insert) empty where 
  infixr 8 &&&&
  (&&&&) = liftA2 (Control.Arrow.&&&)

pairs :: Foldable f => f a -> Maybe [(a,a)]
pairs = snd <$< ensure (isNothing.fst) . foldr f (Nothing,[]) where 
  f e (Nothing,a) = (Just e ,       a)
  f e (Just b ,a) = (Nothing, (e,b):a)
  
powerSet :: [a] -> [[a]]
powerSet xs = (\n -> [ x | (i, x) <- zip [0..] xs, testBit n i ] ) <$> [0..]

clines :: [String] -> String
clines xs = foldr f id xs "" where
  f e a = showString e . showString ";\n" . a

unzipWith :: (a -> (b,c)) -> [a] -> ([b], [c])
unzipWith f = foldr (uncurry bimap . bimap (:) (:) . f) ([],[])

newtype RecFR a ans = RecFR { unRecFR :: a -> (RecFR a ans -> ans) -> ans }

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs = foldr f (\_ -> i) xs . RecFR . foldr g (\_ _ -> i) where
  g e2 r2 e1 r1 = c e1 e2 (r1 (RecFR r2))
  f e r x = unRecFR x e r

newtype RecAccu a b = RecAccu { unRecAccu :: a -> (RecAccu a b, b) }

zipInto :: (Traversable t, Foldable f) => (a -> Maybe b -> c) -> t a -> f b -> t c
zipInto f xs = snd . flip (mapAccumL unRecAccu) xs . RecAccu . foldr h i where
  i e = (RecAccu i, f e Nothing)
  h e2 a e1 = (RecAccu a, f e1 (Just e2))



