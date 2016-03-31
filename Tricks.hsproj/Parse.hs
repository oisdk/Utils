module Parse where

import Data.Text (pack, lines, words, Text, unpack)
import Data.Text.Read (double)
import Prelude hiding (lines, words)
import Data.Map.Strict (fromList, Map)

sureDouble :: Text -> Double
sureDouble = (\(Right (d,_)) -> d) . double

parseVars :: Text -> Map String Double
parseVars = fromList . map ((\(x:n:_) -> (unpack x, sureDouble n)) . words ) . lines