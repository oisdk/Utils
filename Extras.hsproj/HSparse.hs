import Data.Foldable (foldr, foldl', Foldable, foldrM)
import Prelude hiding (foldr, length)
import Data.List (foldl1', unfoldr, transpose)
import Data.Map.Strict (insertWith, Map, insertLookupWithKey)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Tuple (swap)
import Control.Monad.State
import Control.Applicative (Alternative, pure, empty, (<$>), liftA2)
import Text.PrettyPrint hiding (empty)

isSelfRef :: (Integral a, Foldable f) => f a -> Bool
isSelfRef xs = foldr f (const True) xs 0 where 
  f e a n = count n xs == e && a (n+1)
  
count :: (Eq a, Foldable f, Integral n) => a -> f a -> n
count x = foldl' (\a e -> if x == e then a+1 else a) 0

partitions :: Int -> [[Int]]
partitions n = f n n where
  f 1 x = [[x]]
  f l x = [ h:t | h <- [0..x], t <- f (pred l) (x-h) ]

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure f x = if f x then pure x else empty

digits :: Integral a => a -> a -> [a]
digits base = unfoldl (flip divMod base <$< ensure (0/=))

infixr 9 <$<
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(g <$< f) x = fmap g (f x)

filterAccumL :: (x -> acc -> (Bool, acc)) -> acc -> [x] -> ([x], acc)
filterAccumL f s t = runState (filterM (state . f) t) s

debase :: (Integral a) => a -> [a] -> a
debase base = foldl1' (\a e -> e + base * a)

selfRefs :: [Int]
selfRefs = [ debase 10 d | b <- [1..10]
                         , d <- partitions b
                         , isSelfRef d ]

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f = r [] where r a x = maybe a (\(x,y) -> r (y:a) x) (f x)
  
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = fst . foldr f ([],[]) where
  f e t = (if p e then fst t else xs,xs) where xs = e : snd t

increment :: (Integral a, Ord k) => k -> Map k a -> Map k a
increment k = insertWith (+) k 1

counts :: (Integral a, Ord k, Foldable f) => f k -> Map k a
counts = foldr increment M.empty

infixr 9 .: 
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) g f a b = g (f a b)

pairs :: Foldable f => f a -> Maybe [(a,a)]
pairs = snd <$< ensure (isNothing.fst) . foldr f (Nothing,[]) where 
  f e (Nothing,a) = (Just e ,       a)
  f e (Just b ,a) = (Nothing, (e,b):a)

fromListNoRep :: (Ord k, Foldable f) => f (k,a) -> Maybe (Map k a)
fromListNoRep xs = foldr f (Just M.empty) xs where
  f (k,v) m = snd <$< ensure (isNothing.fst) . insertLookupWithKey undefined k v =<< m

isLookSay :: Integral a => a -> Bool
isLookSay n = maybe False (counts d ==) (fromListNoRep =<< map swap <$> pairs d) where 
  d = digits 10 n
  
shorterThan :: (Foldable f, Integral n) => n -> f a -> Bool
shorterThan n xs = foldr (\_ a n -> 1 < n && a (n-1)) (const True) xs n 

longerThan :: (Foldable f, Integral n) => n -> f a -> Bool
longerThan n xs = foldr (\_ a n -> 1 > n || a (n-1)) (const False) xs n

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

-- Repeatedly applies a function until it no longer changes its input
converge :: Eq a => (a -> a) -> a -> a
converge f x | x == y = y
             | otherwise = converge f y
             where y = f x

