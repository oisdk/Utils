{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , RankNTypes
  , TypeFamilies
  #-}
  
import Data.Functor.Foldable hiding (Foldable, unfold)
import qualified Data.Functor.Foldable as F
import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad
import Data.Foldable
import Data.Ratio

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

newtype CofreeF f a r = CF { unCofreeF :: (a, f r) } deriving (Functor, Eq, Ord)

type instance Base (Cofree f a) = CofreeF f a

instance Functor f => F.Foldable (Cofree f a) where
  project (a :< c) = CF (a, c)

instance Functor f => Unfoldable (Cofree f a) where
  embed = uncurry (:<) . unCofreeF
  ana alg = unfold (unCofreeF . alg)
  
data NatF a = Zero | Succ a deriving (Functor, Show, Eq, Ord)

newtype Nat = Nat { getNat :: Fix NatF } deriving (Eq, Ord)

nalg :: Num n => NatF n -> n
nalg Zero = 0
nalg (Succ n) = 1 + n

talg :: (Num n, Eq n) => n -> NatF n
talg 0 = Zero
talg n = Succ (n-1) 

instance Num Nat where
  signum = cata alg . getNat where
    alg Zero = Nat (Fix Zero)
    alg (Succ _) = (Nat . Fix . Succ . Fix) Zero
  abs = id
  Nat a + Nat b = Nat (cata alg a b) where
    alg Zero = id
    alg (Succ n) = Fix . Succ . n
  fromInteger = Nat . ana talg
  Nat a * b = cata alg a b where
    alg Zero _ = Nat (Fix Zero)
    alg (Succ n) b = b + (n b)
  Nat a - Nat (Fix b) = Nat (cata alg a b) where
    alg Zero _ = Fix Zero
    alg (Succ n) Zero = Fix (Succ (n Zero))
    alg (Succ n) (Succ (Fix b)) = n b
  
instance Show Nat where show = show . cata nalg . getNat
    
instance Enum Nat where
  toEnum = Nat . ana talg
  fromEnum = cata nalg . getNat
  succ = Nat . Fix . Succ . getNat
  pred (Nat (Fix (Succ n))) = Nat n
  
instance Real Nat where toRational = (%1) . cata nalg . getNat

--mutu :: ((Base t a, Base u b) -> (a, b))
