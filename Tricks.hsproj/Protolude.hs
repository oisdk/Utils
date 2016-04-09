{-# LANGUAGE NoImplicitPrelude #-}


module Protolude where
  
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Prelude as P
import Data.Ord
import Data.Bool
import Data.Maybe
import Data.Function
import Data.List (uncons)
import Data.Tuple
import Data.Foldable
import Data.Traversable
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Safe.Foldable
import Data.Coerce

increment :: (P.Integral a, Ord k) => k -> Map k a -> Map k a
increment k = Map.insertWith (P.+) k 1

counts :: (P.Integral a, Ord k, Foldable f) => f k -> Map k a
counts = foldr increment Map.empty

infixr 9 .: 
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) g f a b = g (f a b)

filterAccumL :: (x -> acc -> (Bool, acc)) -> acc -> [x] -> ([x], acc)
filterAccumL f s t = runState (filterM (state . f) t) s

newtype RecFR a ans = RecFR { unRecFR :: a -> (RecFR a ans -> ans) -> ans }

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs = foldr f (\_ -> i) xs . RecFR .# foldr g (\_ _ -> i) where
  g e2 r2 e1 r1 = c e1 e2 (r1 (coerce r2))
  f e r x = unRecFR x e r

newtype RecAccu a b = RecAccu { unRecAccu :: a -> (RecAccu a b, b) }

zipInto :: (Traversable t, Foldable f) => (a -> Maybe b -> c) -> t a -> f b -> t c
zipInto f xs = snd . flip (mapAccumL coerce) xs . RecAccu .# foldr h i where
  i e = (RecAccu i, f e Nothing)
  h e2 a e1 = (RecAccu a, f e1 (Just e2))

zipWith :: (Foldable f, Foldable g) => (a -> b -> c) -> f a -> g b -> [c]
zipWith f = foldr2 (\a b c -> f a b : c) []

zip :: (Foldable f, Foldable g) => f a -> g b -> [(a,b)]
zip = zipWith (,)

iterEnd :: (a -> Maybe a) -> a -> [a]
iterEnd f = g where g x = x : maybe [] g (f x)

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p x = bool (pure x) empty (p x)

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f = r [] where r a x = maybe a (\(x,y) -> r (y:a) x) (f x)

infixr 9 .#
(.#) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(.#) _ = coerce 

