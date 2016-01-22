{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , RankNTypes
  #-}
  
import Data.Functor.Foldable hiding (Foldable)
import Text.PrettyPrint
import Data.Functor (void)

data ExprF r = CstF Integer
             | NegF r
             | PrdF r r
             | SumF r r
             deriving (Functor, Foldable, Eq, Ord, Show)
             
newtype Expr = Expr { getExpr :: Fix ExprF }

eval :: Expr -> Integer
eval = cata alg . getExpr where
  alg (CstF d)   = d
  alg (NegF a)   = negate a
  alg (SumF a b) = a + b
  alg (PrdF a b) = a * b

instance Show Expr where 
  show = show . zygo f alg . getExpr where
    f = void
    alg e@(CstF i  ) = integer i
    alg e@(NegF a  ) = char '-' <> par e a
    alg e@(SumF a b) = par e a <+> char '+' <+> par e b
    alg e@(PrdF a b) = par e a <+> char '*' <+> par e b    
    par e (c,p) = if c > void e then parens p else p
    -- par e (c,p) = (bool <*> parens) p (c > void e)
    
bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

num :: Integer -> Expr
num = Expr . ana CstF

instance Num Expr where
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs         = num . abs . eval
  signum      = num . signum . eval
  fromInteger = num . fromInteger
  negate      = Expr . Fix . NegF . getExpr
  