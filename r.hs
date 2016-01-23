{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , RankNTypes
  , ScopedTypeVariables
  #-}
  
import Data.Functor.Foldable hiding (Foldable, unfold)
import Control.Arrow
import Data.Foldable
import Control.Comonad.Cofree
import Control.Comonad

-- (&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
-- (|||) :: (a -> c) -> (b -> c) -> Either a b -> c
-- (***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
-- (+++) :: (a -> b) -> (c -> d) -> Either a c -> Either b d

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd

-- unfix :: Fix f -> f (Fix f)

-- Notation: Fix TypeF == Type

-- Cofree == Ann (!!!!!!)

-- cata :: (tF a -> a) -> t -> a
-- para :: (tF (t, a)  -> a) -> t -> a
-- zygo :: (tF b -> b) -> (tF (b, a) -> a) -> t -> a
-- ana  :: (a -> tF a) -> a -> t
-- apo  :: (a -> tF (Either t a)) -> a -> t
-- hylo :: (f b -> b) -> (a -> f a) -> a -> b
-- histo :: (tF (Cofree tF a) -> a) -> t -> a

algProd :: Functor f => (f a -> a) -> (f b -> b) -> f (a, b) -> (a, b)
algProd f g = (f *** g) . funzip

algCoprod :: (f a -> a) -> (g a -> a) -> Either (f a) (g a) -> a
algCoprod = (|||)

ann :: (f (Cofree f a), a) -> Cofree f a
ann (f,a) = a :< f

synthesize :: forall f a. Functor f => (f a -> a) -> Fix f -> Cofree f a
synthesize f = cata alg where    
  alg c = (f . fmap extract) c :< c

sizes :: (Functor f, Foldable f) => Fix f -> Cofree f Int
sizes = synthesize $ (1+) . sum

inherit :: Functor f => (Fix f -> a -> a) -> a -> Fix f -> Cofree f a
inherit f root n = para alg n root where
  alg sh p = a :< n' where
    (n, ff) = funzip sh
    a = f (Fix n) p
    n' = fmap ($ a) ff
    
depths :: Functor f => Fix f -> Cofree f Int
depths = inherit (const (1+)) 0
