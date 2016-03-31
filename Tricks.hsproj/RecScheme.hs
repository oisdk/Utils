{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecScheme where
  
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Data.Foldable
import Control.Monad
import Data.Map (Map, alter, empty, assocs, lookup)
import Prelude hiding (lookup)
import Data.Maybe
import Control.Comonad.Cofree


-- | A monadic catamorphism.
cataM
  :: (Functor.Foldable t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Unfoldable t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

newtype RecFR a ans = RecFR (a -> (RecFR a ans -> ans) -> ans)
newtype RecF f b a = RecF { unRecF :: forall b. Base f (b -> a) -> b -> a }
newtype FR1 f b w = FR1 (forall w. (f b -> w) -> w)
unFR1 (FR1 x) = x

zipWithF' :: [a] -> [b] -> [(a,b)]
zipWithF' xs ys = cata f ys (cata alg xs) where
  alg Nil = FR1 (\f -> f Nil)
--  f :: Prim [b] (FR1 (Prim [a]) b0 w0 -> [(a, b)]) -> FR1 (Prim [a]) b0 w0 -> [(a, b)]
  f Nil _ = []
  f (Cons x (FR1 f)) l = f l
--  alg _ Nil = []
--  alg (Cons x xs) (Cons y ys) = (x,y) : ys xs
--  f = flip unFR1


newtype FR a = FR (forall ans. (a -> ans -> ans) -> ans -> ans)
unFR (FR x) = x

type Alg = Mu -- General algebra
--
--mutu :: (Functor.Foldable f, Functor.Foldable g) 
--     => (Base f (RecF g b c) -> RecF g b c) -> f -> g -> c
--mutu alg xs ys = cata (flip unRecF) ys (cata alg xs)
--
--
--zipWithF :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWithF c = mutu alg where
--  alg Nil _ = []
--  alg _ Nil = []
--  alg (Cons x xs) (Cons y ys) = c x y : ys xs
--  
--zipF :: [a] -> [b] -> [(a,b)]
--zipF = zipWithF (,)
--
--data Trie a = Trie { endHere :: Bool
--                   , getTrie :: Map a (Trie a)
--                   } deriving Show
--                   
--data TrieF a f = TrieF { endHereF :: Bool
--                       , getTrieF :: Map a f
--                       } deriving (Show, Functor)
--                   
--type instance Base (Trie a) = TrieF a
--instance Functor.Foldable (Trie a) where project (Trie e m) = TrieF e m
--instance Unfoldable (Trie a) where embed (TrieF e m) = Trie e m
--
----insert :: (Foldable f, Ord a) => f a -> Trie a -> Trie a
----insert = foldr f base where
----  base (Trie _ m) = Trie True m
----  f x a (Trie e m) = Trie e (alter (Just . a . fromMaybe (Trie False empty) ) x m)
--  
--tToList :: Trie a -> [[a]]
--tToList = cata alg where
--  alg (TrieF e m) = (if e then ([]:) else id) [ h:r | (h,t) <- assocs m, r <- t]
--
--
