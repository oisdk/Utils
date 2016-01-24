{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , TypeFamilies
  #-}
  
import Data.Functor.Foldable hiding (Foldable, unfold)
import qualified Data.Functor.Foldable as F
import Text.PrettyPrint
import Data.Functor (void)
import Control.Comonad.Cofree
import Control.Comonad

newtype CofreeF f a r = CF { unCofreeF :: (a, f r) } deriving (Functor, Eq, Ord)

type instance Base (Cofree f a) = CofreeF f a

instance Functor f => F.Foldable (Cofree f a) where
  project (a :< c) = CF (a, c)

instance Functor f => Unfoldable (Cofree f a) where
  embed = uncurry (:<) . unCofreeF
  ana alg = unfold (unCofreeF . alg)

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
  show = show . zygo void (ppAlg cmp) . getExpr where
    cmp = (<) . void
    
ppAlg :: (ExprF (t, Doc) -> t -> Bool) -> ExprF (t, Doc) -> Doc
ppAlg cmp e = case e of
  CstF i   -> integer i
  NegF a   -> char '-' <> par a
  SumF a b -> par a <+> char '+' <+> par b
  PrdF a b -> par a <+> char '*' <+> par b
  where par (c,p) = if cmp e c then parens p else p 

instance Num Expr where
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs         = fromInteger . abs . eval
  signum      = fromInteger . signum . eval
  fromInteger = Expr . ana CstF
  negate      = Expr . Fix . NegF . getExpr
  
newtype NamedExpr = Named { getNamed :: Cofree ExprF (Maybe String) }

named :: String -> NamedExpr -> NamedExpr
named s (Named e) = Named (Just s :< unwrap e)

unnamed :: ExprF (Cofree ExprF (Maybe String)) -> NamedExpr
unnamed = Named . (Nothing :<)

evalName :: NamedExpr -> Integer
evalName = eval . Expr . ana unwrap . getNamed

instance Num NamedExpr where
  Named a + Named b = unnamed (SumF a b)
  Named a * Named b = unnamed (PrdF a b)
  fromInteger = unnamed . CstF
  abs e | evalName e < 0 = negate e
        | otherwise      = e
  signum = fromInteger . signum . evalName
  negate = unnamed . NegF . getNamed

instance Show NamedExpr where
  show = show . zygo void alg . getNamed where
    alg (CF (s, e)) = maybe (ppAlg cmp e) text s
    cmp e (CF (_, c)) = void e < c

x = named "x" 3
e = 1 + 2 - 3 :: NamedExpr
a = (x + e) * 4