{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Comonad where
  
import Data.Foldable hiding (toList)

newtype FR1 a b = FR1 { unFR1 :: forall w. (a -> b -> w) -> w -> w }
newtype FR a = FR { unFR :: forall ans. (a -> ans -> ans) -> ans -> ans }

szipWith :: (Foldable f, Foldable g) => (c -> Int -> Double -> c) -> c -> f Int -> g Double -> c
szipWith t i xs ys = lll where
  lll = foldr (\e r x -> p x e r) (const i) xs (toFR ys) where
    p str e r = unFR1 (unFR str ssk (FR1 (const id))) (sk e r) i
    sk e1 r1 e2 r2 = t (r1 r2) e1 e2
    ssk v fk = FR1 (\a _ -> a v (FR (\s' f' -> unFR1 fk (\ v'' x  -> s' v'' (unFR x s' f')) f')))
    

zip' :: (Foldable f, Foldable g) => f a -> g b -> [(a,b)]
zip' xs ys = (foldr f base xs) (foldr g bbase ys) [] where
  base  = FR1 (\i -> i)
  bbase = FR (\_ i -> i)
  f e a r = FR1 (\c
  g e a = (\c _ -> 
  

toFR :: Foldable f => f a -> FR a
toFR xs = FR (\f i -> foldr f i xs)

toList :: FR a -> [a]
toList xs = unFR xs (:) []

zipF :: (Foldable f, Foldable g) => f Int -> g Double -> [(Int,Double)]
zipF = szipWith (\i a b -> (a,b) : i) []