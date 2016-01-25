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

newtype CofreeF f a r = CF { unCofreeF :: (a, f r) } deriving (Functor, Eq, Ord)

type instance Base (Cofree f a) = CofreeF f a

instance Functor f => F.Foldable (Cofree f a) where project (a :< c) = CF (a, c)

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
  alg = \case
    CstF n   -> n
    NegF n   -> negate n
    SumF a b -> a + b
    PrdF a b -> a * b

instance Show Expr where showsPrec _ = zygo void (pprAlg ((<) . void)) . getExpr

pprAlg :: (ExprF (t, ShowS) -> t -> Bool) -> ExprF (t, ShowS) -> ShowS
pprAlg cmp e = case e of
  CstF i   -> shows i
  NegF a   -> showChar '-' . par a
  SumF a b -> par a . showString " + " . par b
  PrdF a b -> par a . showString " * " . par b
  where par (c,p) = showParen (cmp e c) p

instance Num Expr where
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs             = fromInteger . abs . eval
  signum          = fromInteger . signum . eval
  fromInteger     = Expr . ana CstF
  negate          = Expr . Fix . NegF . getExpr
  
newtype NamedExpr = Named { getNamed :: Cofree ExprF (Maybe String) }

named :: String -> NamedExpr -> NamedExpr
named s (Named e) = Named (Just s :< unwrap e)

unnamed :: ExprF (Cofree ExprF (Maybe String)) -> NamedExpr
unnamed = Named . (Nothing :<)

names :: NamedExpr -> Set String
names = cata alg . getNamed where
  alg (CF (Just s, e)) = insert s (fold e)
  alg (CF (_     , e)) = fold e

evalName :: NamedExpr -> Integer
evalName = eval . Expr . ana unwrap . getNamed

instance Num NamedExpr where
  Named a + Named b = unnamed (SumF a b)
  Named a * Named b = unnamed (PrdF a b)
  fromInteger       = unnamed . CstF
  abs e             = if 0 > evalName e then negate e else e
  signum            = fromInteger . signum . evalName
  negate            = unnamed . NegF . getNamed

instance Show NamedExpr where
  showsPrec _ = zygo void alg . getNamed where
    alg   (CF (s, e)) = maybe (pprAlg cmp e) showString s
    cmp e (CF (_, c)) = void e < c
