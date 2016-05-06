{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module RecScheme where

import           Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Data.Function
import Data.Functor
import Control.Monad
import Test.QuickCheck

zipo :: (Functor.Foldable g, Functor.Foldable h) 
      => (Base g (h -> c) -> Base h h -> c) -- ^ An algebra for two Foldables
      -> g                                  -- ^ first fixed point
      -> h                                  -- ^ second
      -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project
  
data Expr a =
    Abs (Expr a)
  | Cst a
  | Neg (Expr a)
  | Mul (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving Functor
            
data ExprF a r =
    AbsF r
  | CstF a
  | NegF r
  | MulF r r
  | SubF r r
  | AddF r r
  deriving (Functor, Foldable, Traversable, Eq, Ord)
    
type instance Base (Expr a) = ExprF a

instance Functor.Foldable (Expr a) where
  project = \case
    Abs x   -> AbsF x
    Cst k   -> CstF k
    Neg x   -> NegF x
    Mul x y -> MulF x y
    Sub x y -> SubF x y
    Add x y -> AddF x y

instance Unfoldable (Expr a) where
  embed = \case
    AbsF x   -> Abs x
    CstF k   -> Cst k
    NegF x   -> Neg x
    MulF x y -> Mul x y
    SubF x y -> Sub x y
    AddF x y -> Add x y
    
eval :: Num a => Expr a -> a
eval = cata alg where
  alg = \case
    CstF k -> k
    AbsF x -> abs x
    NegF x -> negate x
    MulF x y -> x * y
    SubF x y -> x - y
    AddF x y -> x + y
    
instance Num a => Num (Expr a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate = Neg
  abs = Abs
  fromInteger = Cst . fromInteger
  signum = Cst . signum . eval
  
pprAlg :: Show a => (ExprF a (t, ShowS) -> t -> Bool) -> ExprF a (t, ShowS) -> ShowS
pprAlg cmp e = case e of
  CstF k   -> shows k
  NegF x   -> showChar '-' . par x
  AddF x y -> par x . showString " + " . par y
  SubF x y -> par x . showString " - " . par y
  MulF x y -> par x . showString " * " . par y
  AbsF (_,x) -> showString "abs(" . x . showChar ')'
  where par (c,p) = showParen (cmp e c) p

instance Show a => Show (Expr a) where 
  showsPrec _ = (para . pprAlg) (on (<) (void . project . void) . embed . fmap fst)

-- | A monadic anamorphism
anaM
  :: (Unfoldable t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = sized (anaM alg) where
    alg n | n <= 0 = fmap CstF arbitrary
    alg n = elements
      [ AbsF nest
      , NegF nest
      , MulF nest nest
      , SubF nest nest
      , AddF nest nest
      ] where nest = n `div` 4






