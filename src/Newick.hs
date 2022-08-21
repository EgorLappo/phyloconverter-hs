{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TupleSections #-}

module Newick (parseNewick, writeNewick, writeNewick', writeNewickLine, subtree) where

import Types (Tree(..), NamedTree, Parser)

import Text.Megaparsec
    ( (<|>),
      noneOf,
      oneOf,
      between,
      choice,
      option,
      endBy1,
      many,
      sepBy1,
      some,
      parse )
import Text.Megaparsec.Char
    ( alphaNumChar, char, digitChar, space )
import Data.List (intercalate)

-- write named trees by forgetting the names
writeNewick :: [NamedTree String] -> String
writeNewick = writeNewick' . map snd

-- write unnamed trees
writeNewick' :: [Tree String] -> String
writeNewick' = intercalate "\n" . map ((++ ";") . writeNewickLine)

writeNewickLine :: Tree String -> String
writeNewickLine (Leaf (n,l)) = n ++  ":" ++ l
writeNewickLine (Node (n,l) ts) = "(" ++ intercalate "," (map writeNewickLine ts) ++ ")"
                                      ++ n ++ ":" ++ l

parseNewick :: String -> Either String [NamedTree String]
parseNewick = fmap (fmap ("", )) . parseNewick'

-- get parsed trees or an error string
parseNewick' :: String -> Either String [Tree String]
parseNewick' s = case parse newick "newick" s of
    Left e -> Left $ "error parsing your Newick file: \n" ++ show e
    Right ts -> Right ts

-- parses the whole file, which can be one or multiple trees
newick :: Parser [Tree String]
newick = endBy1 subtree $ separator ';'

-- a subtree, which is either a leaf or an internal node
subtree :: Parser (Tree String)
subtree = internal <|> leaf

-- a leaf is a name followed by a branch length specification, 
-- with an arbitrary amount of whitespace
leaf :: Parser (Tree String)
leaf = do
    space
    n <- nodeName
    space
    l <- branchLength
    space
    return $ Leaf (n,l)

-- a node name is either empty, a quoted string containing any characters,
-- or an unquoted string which can only have alphanumeric characters or sppecified punctuation
nodeName :: Parser String
nodeName = quotedString <|> unquotedString
    where namePunctuation = ['.','_','-']
          unquotedString = many $ choice [alphaNumChar, oneOf namePunctuation]
          quotedString = between (char '\'') (char '\'') $ some $ noneOf ['\'']

-- a branch length is either empty or a number preceded by ':'
branchLength :: Parser String
branchLength = option "" $ do
    char ':'
    space
    num --choice [flt, int]

-- an internal node is a collection of comma-separated subtrees, followed by 
-- the node's name and branch length
internal :: Parser (Tree String)
internal = do
    children <- between (separator '(') (separator ')') $ subtree `sepBy1` separator ','
    n <- nodeName
    space
    l <- branchLength
    space
    return (Node (n,l) children)

-- a separator is a character possibly surrounded by whitespace
separator :: Char -> Parser Char
separator c = do
    space
    s <- char c
    space
    return s

-- an num is a nonempty collection of digits, possibly separated by a point
num :: Parser String
num = do
    xs <- some digitChar
    frac <- option "" fracPart
    return (xs ++ frac)

fracPart :: Parser String
fracPart = do
    char '.'
    xs <- some digitChar
    return ('.':xs)


