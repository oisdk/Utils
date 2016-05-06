{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
    
module Peano where

import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
  
data NumF a = Zero | Succ a deriving Functor

type instance Base Integer = NumF

instance Functor.Foldable Integer where
  project 0 = Zero
  project n = Succ (n-1)
  
instance Unfoldable Integer where
  embed Zero = 0
  embed (Succ n) = n+1
  
