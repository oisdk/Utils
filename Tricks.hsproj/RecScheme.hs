{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecScheme where
  
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Data.Foldable hiding (fold)
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

mutu :: (Functor.Foldable f, Functor.Foldable g) 
     => (Base f (Mu (Base f)) -> Base g (Mu (Base f) -> c) -> c)
     -> f -> g -> c
mutu alg xs ys = cata (flip (alg . project)) ys (Mu (flip cata xs))

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith c = mutu alg where
  alg Nil _ = []
  alg _ Nil = []
  alg (Cons x xs) (Cons y ys) = c x y : ys xs
  