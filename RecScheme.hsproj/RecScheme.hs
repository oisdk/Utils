{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

import Expr
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Schemes
import Peano
import Control.Applicative
import Test.QuickCheck

eval :: Fractional a => Expr a -> a
eval = cata evalAlg
      
fact :: Integer -> Integer
fact = para alg where
  alg = \case
    Zero -> 1
    Succ (0, m) -> m
    Succ (n, m) -> (n+1) * m
    
-- zygo :: Foldable t => (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a
countMax :: Ord a => [a] -> Int
countMax = zygo malg alg where
  malg Nil = Nothing
  malg (Cons x xs) = max (Just x) xs
  alg Nil = 0
  alg (Cons x (m,c)) = 
    case compare (Just x) m of
      GT -> 1
      EQ -> c + 1
      LT -> c
      
countMax' :: Ord a => [a] -> Int
countMax' xs = length . filter (\e -> m == Just e) $ xs where 
  m = foldr (max . Just) Nothing xs
  
countTest :: [Int] -> Bool
countTest = liftA2 (==) countMax countMax'
