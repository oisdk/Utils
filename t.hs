{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , TypeFamilies
  , LambdaCase
  #-}
  
import Data.Functor.Foldable hiding (Foldable, unfold, fold)
import qualified Data.Functor.Foldable as F
import Data.Functor (void)
import Control.Comonad.Cofree
import Control.Comonad
import Data.Set (Set, insert)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype CofreeF f a r = CF { unCofreeF :: (a, f r) } deriving (Functor, Eq, Ord)

type instance Base (Cofree f a) = CofreeF f a

instance Functor f => F.Foldable (Cofree f a) where project (a :< c) = CF (a, c)

instance Functor f => Unfoldable (Cofree f a) where
  embed = uncurry (:<) . unCofreeF
  ana alg = unfold (unCofreeF . alg)

data TrieF a = TrieF { endHere :: Bool 
                     , children :: Map Char a 
                     } deriving Functor
                     
newtype Trie = Trie { getTrie :: Fix TrieF }

member :: Trie -> String -> Bool
member = mutu alg where
  alg (TrieF e c) Nil = e
  alg (TrieF e c) (Cons x xs) = maybe (const False) (M.lookup x c) xs
  
mutu :: (F.Foldable t, F.Foldable u) => (Base t (a -> b) -> (a -> b)) -> (Base u a -> a) -> t -> u -> b
mutu blg rlg x = cata blg x . cata rlg
