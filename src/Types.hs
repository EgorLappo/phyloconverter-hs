module Types 
    ( FileFormat(..)
    , Tree(..)
    , NamedTree
    , Parser
    ) where 
    
import Data.Void ( Void )
import Text.Megaparsec (Parsec)

data FileFormat = Newick | Nexus deriving (Show, Eq)

data Tree a = Node (String, a) [Tree a] | Leaf (String, a) deriving (Show, Eq)

type NamedTree a = (String, Tree a)

type Parser = Parsec Void String