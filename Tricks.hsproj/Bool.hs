module Bool 
  ( bool
  ) where
  
bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f
