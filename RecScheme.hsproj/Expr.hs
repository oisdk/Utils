{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module Expr
  ( Expr(..)
  , ExprF(..)
  , evalAlg
  ) where
    
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as Functor
import Data.Ratio

data Expr a =
    Abs (Expr a)
  | Sig (Expr a)
  | Cst a
  | Neg (Expr a)
  | Mul (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving (Functor, Eq, Show)
            
data ExprF a r =
    AbsF r
  | SigF r
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
      
evalAlg :: Num a => ExprF a a -> a
evalAlg = \case
  CstF k -> k
  AbsF x -> abs x
  SigF x -> signum x
  NegF x -> negate x
  MulF x y -> x * y
  SubF x y -> x - y
  AddF x y -> x + y
  
instance Integral a => Num (Expr a) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  negate = Neg
  abs = Abs
  fromInteger = Cst . fromInteger
  signum = Sig
      
      
      




