module AppFunc
  ( ensure
  , (<$<)
  ) where
  
import Control.Applicative (Alternative, pure, empty)

infixr 9 <$<
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(g <$< f) x = fmap g (f x)

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure f x = if f x then pure x else empty