module Tricks where
  
import qualified Data.Set as S
import Control.Monad (filterM)
import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as M

dedupe :: Ord key => [(key,val)] -> [val]
dedupe xs = catMaybes $ evalState (mapM f xs) S.empty where
  f :: Ord k => (k,v) -> State (S.Set k) (Maybe v)
  f (k,v) = do
    set <- get
    if S.member k set 
      then
        return Nothing
      else do
        put (S.insert k set)
        return (Just v)

canonize = snd . mapAccumL f (0, M.empty) where
  f a@(n,m) e = case M.lookup e m of
    Nothing -> ((n+1, M.insert e n m), n)
    Just n -> (a, n)