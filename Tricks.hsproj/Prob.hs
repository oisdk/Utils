module Prob where
  
import Data.Ratio
import Data.Monoid
import Data.Foldable
import Control.Applicative
  
-- | A Probability monad. The singleton is a certainty, with more than one, the
-- probability of the head element is given by a rational number.
data Prob a = Certainly a | (Rational, a) :/: Prob a

instance Functor Prob where
  fmap f (Certainly x) = Certainly (f x)
  fmap f (x :/: xs) = fmap f x :/: fmap f xs
  
probOf :: Eq a => a -> Prob a -> Rational
probOf x (Certainly y) | x == y = 1
                       | otherwise = 0
probOf x ((p,y) :/: ys) | x == y = (r + p) / (p + 1)
                        | otherwise = r / (p + 1)
                        where r = probOf x ys

equalProbs :: [a] -> Prob a
equalProbs xs = probLength (fromIntegral $ length xs - 1) xs where
  probLength 0 (y:_) = Certainly y
  probLength n (y:ys) = (1 % n, y) :/: (probLength (n-1) ys)
  

instance Show a => Show (Prob a) where
  show = ('[':) . show' where
    show' (Certainly x) = "(_ % _, " ++ show x ++ ")]"
    show' (x :/: xs) = show x ++ ", " ++ show' xs

conc :: Rational -> Prob a -> Prob a -> Prob a
conc p (Certainly x) ys = (p,x) :/: ys
conc io ((ii,y) :/: ys) zs = (oi, y) :/: (conc (io / (ii+1)) ys zs) where 
  oi = io * ii / (io + ii + 1)

flatten :: Prob (Prob a) -> Prob a
flatten (Certainly xs) = xs
flatten ((p,x) :/: xs) = conc p x (flatten xs)
    
instance Foldable Prob where
  foldMap f (Certainly x) = f x
  foldMap f ((_,x) :/: xs) = f x <> foldMap f xs
 
instance Applicative Prob where
  pure = Certainly
  Certainly f <*> xs = f <$> xs
  ((p,f) :/: fs) <*> xs = conc p (f <$> xs) (fs <*> xs)


instance Monad Prob where
  return = Certainly
  x >>= f = flatten (fmap f x)
--  
--data Coin   = H | T deriving (Show, Eq)
--data Result = Win | Lose deriving (Show, Eq)
--
--play H = (1,Win) :/: Certainly Lose
--play T = equalProbs [Win,Lose]
--
