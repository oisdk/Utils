module Function
  ( converge
  , (.:)
  ) where
  
infixr 9 .: 
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) g f a b = g (f a b)

converge :: Eq a => (a -> a) -> a -> a
converge f x | x == y = y
             | otherwise = converge f y
             where y = f x