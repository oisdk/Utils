module Numbers
  ( isSelfRef
  , partitions
  , digits
  , debase
  , selfRefs
  , isLookSay
  ) where
  
import Data.Foldable (Foldable, foldl', foldr)
import Data.List (foldl1')
import Prelude hiding (foldr)
import AppFunc
import Folds
import Maps
import Data.Tuple (swap)

isSelfRef :: (Integral a, Foldable f) => f a -> Bool
isSelfRef xs = foldr f (const True) xs 0 where 
  f e a n = count n xs == e && a (n+1)

partitions :: Int -> [[Int]]
partitions n = f n n where
  f 1 x = [[x]]
  f l x = [ h:t | h <- [0..x], t <- f (pred l) (x-h) ]

digits :: Integral a => a -> a -> [a]
digits base = unfoldl (flip divMod base <$< ensure (0/=))

debase :: (Integral a) => a -> [a] -> a
debase base = foldl1' (\a e -> e + base * a)

selfRefs :: [Int]
selfRefs = [ debase 10 d | b <- [1..10]
                         , d <- partitions b
                         , isSelfRef d ]
                         
isLookSay :: Integral a => a -> Bool
isLookSay n = maybe False (counts d ==) (fromListNoRep =<< fmap (map swap) (pairs d)) where 
  d = digits 10 n