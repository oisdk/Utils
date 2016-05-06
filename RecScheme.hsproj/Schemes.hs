{-# LANGUAGE FlexibleContexts #-}

module Schemes where
  
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Control.Monad

zipo :: (Functor.Foldable g, Functor.Foldable h) 
      => (Base g (h -> c) -> Base h h -> c) -- ^ An algebra for two Foldables
      -> g                                  -- ^ first fixed point
      -> h                                  -- ^ second
      -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project

-- | A monadic catamorphism.
cataM
  :: (Functor.Foldable t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< traverse c . project

-- | A monadic anamorphism
anaM
  :: (Unfoldable t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g