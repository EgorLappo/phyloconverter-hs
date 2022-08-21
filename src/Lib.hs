{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lib (convert) where

import Types (NamedTree, FileFormat(..))
import Newick (parseNewick, writeNewick,)
import Nexus  (parseNexus, writeNexus, )
import Data.Maybe (fromJust)

convert :: FileFormat -> FileFormat -> String -> Either String String
convert inpf outf = fmap (writeF outf) . readF inpf
    where writeF f = fromJust $ lookup f writers
          readF  f = fromJust $ lookup f readers

writers :: [(FileFormat, [NamedTree String] -> String)]
writers = [(Newick, writeNewick), (Nexus, writeNexus)]
readers :: [(FileFormat, String -> Either String [NamedTree String])]
readers = [(Newick, parseNewick), (Nexus, parseNexus)]





