{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module BST where
  
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Data.List (partition)
import Data.Monoid
import Schemes
import Peano
  
data BST a = Leaf | Node (BST a) a (BST a) deriving (Functor, Traversable, Show)

data BSTF a r = LeafF | NodeF r a r deriving Functor

type instance Base (BST a) = BSTF a

instance Functor.Foldable (BST a) where
  project Leaf = LeafF
  project (Node l x r) = NodeF l x r
  
instance Unfoldable (BST a) where
  embed LeafF = Leaf
  embed (NodeF l x r) = Node l x r
  
treeSort :: Ord a => [a] -> [a]
treeSort = hylo alg coalg where
  alg LeafF = []
  alg (NodeF l x r) = l ++ [x] ++ r

fromList :: Ord a => [a] -> BST a
fromList = ana coalg

coalg (x:xs) = uncurry (flip NodeF x) (partition (x>) xs)
coalg [] = LeafF

instance Foldable BST where
  foldMap f = cata alg where
    alg LeafF = mempty
    alg (NodeF l x r) = l <> f x <> r
  foldr f b t = cata alg t b where
    alg LeafF = id
    alg (NodeF l x r) = l . f x . r

correct :: Ord a => BST a -> Bool
correct = zygo head alg where
  head LeafF = Nothing
  head (NodeF _ x _) = Just x
  alg LeafF = True
  alg (NodeF (le,lc) x (re,rc)) = lc && rc && maybe True (x>=) le && maybe True (x<=) re
  
index :: [Bool] -> BST a -> Maybe a
index = zipo alg where
  alg Nil (NodeF _ x _) = Just x
  alg (Cons True  xs) (NodeF ys _ _) = xs ys
  alg (Cons False xs) (NodeF _ _ ys) = xs ys
  alg _ _ = Nothing
  
indexOf :: Ord a => a -> BST a -> Maybe [Bool]
indexOf x = cata alg where
  alg LeafF = Nothing
  alg (NodeF xs y ys) = case compare x y of
    LT -> (True :) <$> xs
    EQ -> Just []
    GT -> (False:) <$> ys
  