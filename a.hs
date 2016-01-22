{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , RankNTypes
  #-}
  
import Data.Functor.Foldable hiding (Foldable)
import Text.PrettyPrint
import Data.Functor (void)
import Control.Arrow hiding ((<+>))

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
  show = show . zygo void alg . getExpr where
    alg e@(CstF i  ) = integer i
    alg e@(NegF a  ) = char '-' <> par e a
    alg e@(SumF a b) = par e a <+> char '+' <+> par e b
    alg e@(PrdF a b) = par e a <+> char '*' <+> par e b    
    par e (c,p) = if c > void e then parens p else p

instance Num Expr where
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs         = fromInteger . abs . eval
  signum      = fromInteger . signum . eval
  fromInteger = Expr . ana CstF
  negate      = Expr . Fix . NegF . getExpr
  
newtype AnnF f a r = AnnF { getAnn :: (f r, a) } deriving (Functor)
type Ann f a = Fix (AnnF f a)

newtype NamedExpr = Named { getNamed :: Ann ExprF (Maybe String) }

stripAll :: Functor f => Ann f a -> Fix f
stripAll = cata alg where alg (AnnF (x,_)) = Fix x

named :: String -> NamedExpr -> NamedExpr
named s (Named (Fix (AnnF (e, _)))) = (Named . Fix . AnnF) (e, Just s)

unnamed :: ExprF (Ann ExprF (Maybe String)) -> NamedExpr
unnamed e = (Named . Fix . AnnF) (e, Nothing)

evalName :: NamedExpr -> Integer
evalName = eval . Expr . stripAll . getNamed

instance Num NamedExpr where
  Named a + Named b = unnamed (SumF a b)
  Named a * Named b = unnamed (PrdF a b)
  fromInteger = unnamed . CstF
  abs e | evalName e < 0 = negate e
        | otherwise      = e
  signum = fromInteger . signum . evalName
  negate = unnamed . NegF . getNamed
  
-- instance Show NamedExpr where
--   show = show . zygo void alg . getNamed where
--     alg (AnnF (_, Just s)) =
--     alg e@(CstF i  ) = integer i
--     alg e@(NegF a  ) = char '-' <> par e a
--     alg e@(SumF a b) = par e a <+> char '+' <+> par e b
--     alg e@(PrdF a b) = par e a <+> char '*' <+> par e b
--     par e (c,p) = if c > void e then parens p else p
