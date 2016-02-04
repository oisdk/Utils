{-# LANGUAGE BangPatterns #-}

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as ByteString

anagrams :: [ByteString] -> [[ByteString]]
anagrams = filter ( (>1) . length )
         . map dedupe
         . Map.elems
         . Map.fromListWith (++)
         . map mappify'
 where
   mappify' orig = (key, [(wkey, orig)])
       where
           !toks = tokenise orig
           !wkey = List.sort toks
           !key  = ByteString.sort $ ByteString.concat toks
