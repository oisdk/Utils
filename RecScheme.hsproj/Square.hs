{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Square where
  
import Data.Traversable
import Control.Monad
import Control.Arrow (first, (***))
import Data.List (uncons)
import Control.Applicative
import Data.Functor.Identity
import Data.Coerce

newtype Pair v w a = P (v a, w a) 
  deriving (Functor, Foldable, Traversable)

newtype Empty a = Empty () 
  deriving (Functor, Foldable, Traversable)

data Square a = 
  Square { size :: Int
         , getSquare :: Square_ Empty Identity a
         } deriving (Functor, Foldable)

data Square_ v w a =
    Zero (forall b. Int -> v b -> b) (v (v a))
  | Even (Square_ v (Pair w w) a)
  | Odd (Square_ (Pair v w) (Pair w w) a)
  deriving (Functor, Foldable)

subP :: (Int -> v a -> a) 
     -> (Int -> w a -> a) 
     -> Int -> Int 
     -> Pair v w a -> a
subP subv subw vsize i (P (v,w))
  | i < vsize = subv i v
  | otherwise = subw (i-vsize) w
  
mkP :: (a -> v b) -> (a -> w b) -> a -> Pair v w b
mkP mkv mkw x = P (mkv x, mkw x)

create :: Int -> a -> Square a
create n = Square n . create_ undefined subv 0 1 mke mki n where
  subv :: Int -> Identity a -> a
  subv _ = coerce
  mke _ = Empty ()
  mki :: a -> Identity a
  mki = coerce

create_ :: (forall b. Int -> v b -> b)
        -> (forall b. Int -> w b -> b)
        -> Int -> Int
        -> (forall b. b -> v b)
        -> (forall b. b -> w b)
        -> Int -> a
        -> Square_ v w a
create_ subv subw vsize wsize mkv mkw n
  | n == 0 = Zero subv . mkv . mkv
  | even n = Even . create_ 
      subv (subP subw subw wsize) 
      vsize (wsize+wsize) 
      mkv (mkP mkw mkw) 
      (n `div` 2)
  | otherwise = Odd . create_ 
      (subP subv subw vsize) (subP subw subw wsize) 
      (vsize+wsize) (wsize+wsize) 
      (mkP mkv mkw) (mkP mkw mkw) 
      (n `div` 2)
     
sub :: Int -> Int -> Square a -> Maybe a
sub i j (Square n m) 
  | i >= n = Nothing 
  | j >= n = Nothing
  | otherwise = Just (sub_ i j m)

sub_ :: Int -> Int -> Square_ v w a -> a
sub_ i j = sub' where
  sub' (Zero subv vv) = subv i (subv j vv)
  sub' (Even m) = sub_ i j m
  sub' (Odd m) = sub_ i j m

instance Traversable Square where
  traverse f (Square n m) = Square n <$> tra f m where
    tra :: (Traversable t, Traversable s, Applicative f) 
        => (a -> f b) -> Square_ t s a -> f (Square_ t s b)
    tra f (Zero s v) = Zero s <$> (traverse.traverse) f v
    tra f (Even s) = Even <$> tra f s
    tra f (Odd s) = Odd <$> tra f s
    
instance Show a => Show (Square a) where
  showsPrec _ s@(Square n _) = ((\(_,a,_) -> a) . foldr f ([],id,n)) s where
    f e (l,a,1) = ([], showList (e:l) . showChar '\n' . a, n)
    f e (l,a,m) = (e:l, a, m-1)


    
