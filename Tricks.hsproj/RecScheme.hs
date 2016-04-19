{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module RecScheme where

import           Control.Monad
import           Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import           Data.Traversable
import           Prelude               hiding (zipWith)

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

zipo :: (Functor.Foldable f, Functor.Foldable g)
     => (Base f (Mu (Base g) -> c) -> Base g (Mu (Base g)) -> c)
     -> f -> g -> c
zipo alg xs = cata (\x -> alg x . project) xs . refix

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith c = zipo alg where
  alg Nil _ = []
  alg _ Nil = []
  alg (Cons x xs) (Cons y ys) = c x y : xs ys
