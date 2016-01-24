{-# LANGUAGE 
    DeriveFunctor
  , DeriveFoldable
  , RankNTypes
  , TypeFamilies
  , FlexibleInstances
  , FlexibleContexts
  #-}
  
import Data.Functor.Foldable hiding (Foldable)
import qualified Data.Functor.Foldable as F
import Text.PrettyPrint
import Data.Functor (void)
import Control.Arrow hiding ((<+>))
import Data.Maybe (fromMaybe)
import Control.Comonad.Cofree hiding (unfold)
import qualified Control.Comonad.Cofree as C
import Control.Comonad
import Data.Maybe (isNothing)

-- zygo :: (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a
-- para :: (Base t (t, a)  -> a) -> t -> a
-- cata :: (Base t a -> a) -> t -> a
-- ana  :: (a -> Base t a) -> a -> t

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
  show = show . zygo void (ppAlg par) . getExpr where
    par e (c,p) = if c > void e then parens p else p
    
ppAlg :: (ExprF t -> t -> Doc) -> ExprF t -> Doc
ppAlg par e@(CstF i  ) = integer i
ppAlg par e@(NegF a  ) = char '-' <> par e a
ppAlg par e@(SumF a b) = par e a <+> char '+' <+> par e b
ppAlg par e@(PrdF a b) = par e a <+> char '*' <+> par e b    

instance Num Expr where
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs         = fromInteger . abs . eval
  signum      = fromInteger . signum . eval
  fromInteger = Expr . ana CstF
  negate      = Expr . Fix . NegF . getExpr
  
newtype NamedExpr = Named { getNamed :: Cofree ExprF (Maybe String) }

stripAll :: Functor f => Cofree f a -> Fix f
stripAll = ana unwrap

named :: String -> NamedExpr -> NamedExpr
named s (Named e) = Named (Just s :< unwrap e)

unnamed :: ExprF (Cofree ExprF (Maybe String)) -> NamedExpr
unnamed = Named . (Nothing :<)

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

newtype CofreeF f a r = CF { unCofreeF :: (a, f r) } deriving (Functor, Eq, Ord)

type instance Base (Cofree f a) = CofreeF f a

instance Functor f => F.Foldable (Cofree f a) where
  project (a :< c) = CF (a, c)

instance Functor f => Unfoldable (Cofree f a) where
  embed (CF (a, f)) = (a :< f)
  ana alg = C.unfold (unCofreeF . alg)

instance Show NamedExpr where
  show = show . zygo void alg . getNamed where
    alg (CF (s, e)) = maybe (ppAlg par e) text s
    par e (c,p) = if (snd . unCofreeF) c > void e then parens p else p

x = named "x" 3
e = 1 + 2 - 3 :: NamedExpr