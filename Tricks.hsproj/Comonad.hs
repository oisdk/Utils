module Comonad where
  
import Control.Comonad
import Prelude hiding (iterate, (:), head, tail)
import Data.Function
import Data.Monoid
  
loeb' :: Functor f => f (f a -> a) -> f a
loeb' fs = xs where xs = fmap ($ xs) fs

data Stream a = a :< Stream a

head :: Stream a -> a
head (x :< _) = x

tail :: Stream a -> Stream a
tail (_ :< xs) = xs

iterateS :: (a -> a) -> a -> Stream a
iterateS f = iterate' where 
  iterate' x = x :< iterate' x
  
instance Functor Stream where
  fmap f (x :< xs) = f x :< fmap f xs
  
instance Applicative Stream where
  pure x = x :< pure x
  (f :< fs) <*> (x :< xs) = f x :< (fs <*> xs)
  
instance Comonad Stream where
  extract = head
  duplicate = iterateS tail
  
instance Foldable Stream where
  foldr f _ (x :< xs) = f x (foldr f undefined xs)
  foldMap f (x :< xs) = f x <> foldMap f xs
  
instance ComonadApply Stream where (<@>) = (<*>)

data Tape a = Tape { left   :: Stream a
                   , cursor :: a 
                   , right  :: Stream a
                   }

instance Functor Tape where
  fmap f (Tape xs a ys) = Tape (fmap f xs) (f a) (fmap f ys)
  
moveL, moveR :: Tape a -> Tape a
moveL (Tape (x :< xs) a ys) = Tape xs a (x :< ys)
moveR (Tape xs a (y :< ys)) = Tape (y :< xs) a ys

iterate :: (a -> a) -> (a -> a) -> a -> Tape a
iterate prev next x = Tape (iterateS prev x) x (iterateS next x)

instance Foldable Tape where
  foldr f _ (Tape (x :< xs) a (y :< ys)) = f a .  f x .  foldr   f undefined $ Tape xs y ys
  foldMap f (Tape (x :< xs) a (y :< ys)) = f a <> f x <> foldMap f            (Tape xs y ys)

instance Comonad Tape where
  extract = cursor
  duplicate = iterate moveL moveR
  
instance ComonadApply Tape where
  (Tape fs f gs) <@> (Tape xs x ys) = Tape (fs <@> xs) (f x) (gs <@> ys)
  
loeb :: Functor f => f (f a -> a) -> f a
loeb fs = fix $ \xs -> fmap ($ xs) fs

evaluate :: ComonadApply w => w (w a -> a) -> w a
evaluate fs = fix ((<@>) fs . duplicate)

