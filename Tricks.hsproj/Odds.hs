module Odds where
  
import Data.Ratio
import Data.Monoid
import Data.Foldable
import Control.Applicative
  
-- | A Odds monad. The singleton is a certainty, with more than one, the
-- Odds of the head element is given by a rational number.
data Odds a = Certainly a | (Rational, a) ::: Odds a

instance Functor Odds where
  fmap f (Certainly x) = Certainly (f x)
  fmap f (x ::: xs) = fmap f x ::: fmap f xs
  
oddsOf :: Eq a => a -> Odds a -> Rational
oddsOf x (Certainly y) | x == y = 1
                       | otherwise = 0
oddsOf x ((p,y) ::: ys) | x == y = (r + p) / (p + 1)
                        | otherwise = r / (p + 1)
                        where r = oddsOf x ys

equalOdds :: [a] -> Odds a
equalOdds xs = oddsLength (fromIntegral $ length xs - 1) xs where
  oddsLength 0 (y:_) = Certainly y
  oddsLength n (y:ys) = (1 % n, y) ::: (oddsLength (n-1) ys)
  
instance Show a => Show (Odds a) where
  show = ('[':) . show' where
    show' (Certainly x) = "(_ % _, " ++ show x ++ ")]"
    show' (x ::: xs) = show x ++ ", " ++ show' xs

conc :: Rational -> Odds a -> Odds a -> Odds a
conc p (Certainly x) ys = (p,x) ::: ys
conc p ((i,y) ::: ys) zs = (p * i / (p + i + 1), y) ::: (conc (p / (i + 1)) ys zs)

flatten :: Odds (Odds a) -> Odds a
flatten (Certainly xs) = xs
flatten ((p,x) ::: xs) = conc p x (flatten xs)
    
instance Foldable Odds where
  foldMap f (Certainly x) = f x
  foldMap f ((_,x) ::: xs) = f x <> foldMap f xs
 
instance Applicative Odds where
  pure = Certainly
  Certainly f <*> xs = f <$> xs
  ((p,f) ::: fs) <*> xs = conc p (f <$> xs) (fs <*> xs)

instance Monad Odds where
  return = Certainly
  x >>= f = flatten (f <$> x)
  
distribution :: [(Integer, a)] -> Odds a
distribution ((n,x):xs) = snd (foldr f (n, Certainly x) xs) where
  f (p,e) (c,a) = (c+p, (p % c, e) ::: a)