{-# LANGUAGE LambdaCase #-}

module Odds where
  
import Data.Ratio
import Data.Monoid
import Data.Foldable
import Control.Applicative
import Bool (bool)

-- | A Odds monad. The singleton is a certainty, with more than one, the
-- Odds of the head element is given by a rational number.
data Odds a = Certainly a | (Rational, a) ::: Odds a

instance Functor Odds where
  fmap f (Certainly x) = Certainly (f x)
  fmap f (x ::: xs) = fmap f x ::: fmap f xs
  
oddsOf :: (a -> Bool) -> Odds a -> Rational
oddsOf p (Certainly  x) = bool 1 0 (p x)
oddsOf p ((n,x) ::: xs) = bool (r + n) r (p x) / (n + 1) where r = oddsOf p xs

equalOdds :: [a] -> Odds a
equalOdds xs = oddsLength (fromIntegral $ length xs - 1) xs where
  oddsLength 0 (y:_ ) = Certainly y
  oddsLength n (y:ys) = (1 % n, y) ::: oddsLength (n - 1) ys
  
instance Show a => Show (Odds a) where
  show = ('[':) . show' where
    show' (Certainly x) = show x ++ "]"
    show' ((p,x) ::: xs) = show x ++ " (" ++ n ++ ":" ++ d ++ "), " ++ show' xs where
      n = show (numerator p)
      d = show (denominator p)

conc :: Rational -> Odds a -> Odds a -> Odds a
conc p (Certainly  x) xs = (p ,x) ::: xs
conc p ((i,x) ::: xs) ys = (ip,x) ::: (conc op xs ys) where
  ip = p * i / (p + i + 1)
  op = p / (i + 1)

flatten :: Odds (Odds a) -> Odds a
flatten (Certainly xs) = xs
flatten ((p,x) ::: xs) = conc p x (flatten xs)
    
instance Foldable Odds where
  foldMap f = \case
    Certainly x  -> f x
    (_,x) ::: xs -> f x <> foldMap f xs
  foldr f i = \case 
    Certainly x  -> f x i
    (_,x) ::: xs -> f x (foldr f i xs)
 
instance Applicative Odds where
  pure = Certainly
  Certainly f <*> xs = f <$> xs
  ((p,f) ::: fs) <*> xs = conc p (f <$> xs) (fs <*> xs)

instance Monad Odds where
  return = Certainly
  x >>= f = flatten (f <$> x)
  
distribution :: [(Integer, a)] -> Odds a
distribution ((n,x):xs) = snd (foldr f (n, Certainly x) xs) where
  f (p,e) (c,a) = (c + p, (p % c, e) ::: a)
