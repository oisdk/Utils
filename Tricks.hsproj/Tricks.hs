{-# LANGUAGE DataKinds, TypeFamilies, GADTs, RankNTypes #-}

module Tricks where
  
import Data.Traversable
import Data.List
import Control.Monad.State
import Data.Coerce
  
newtype FR1 a b = FR1 { unFR1 :: forall w. (a -> b -> w) -> w -> w }
newtype FR a = FR { unFR :: forall ans. (a -> ans -> ans) -> ans -> ans }

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 t i xs = foldr f (\_ -> i) xs . tFR where
  f e r x = p x e r
  p ys e r = unFR1 (unFR ys ssk (FR1 (\_ b -> b))) (sk e r) i
  sk e1 r1 e2 r2 = t e1 e2 (r1 r2)
  ssk v fk = FR1 (\a _ -> a v z) where z = FR (\s' f' ->  unFR1 fk (\ v'' x  -> s' v'' (unFR x s' f')) f')
    
unFRF :: Foldable f => f a -> (a -> b -> b) -> b ->  b
unFRF xs f i = foldr f i xs

tFR :: Foldable f => f a -> FR a
tFR xs = FR (flip flip xs . foldr)

groupBy' :: Foldable f => (a -> a -> Bool) -> f a -> [[a]]
groupBy' eq = maybe [] snd . foldr (\e -> Just . maybe (e,[[e]]) (f e)) Nothing where
  f e (x,xs:xxs) | eq e x = (x,(e:xs):xxs)
  f e (_,xs) = (e,[e]:xs)