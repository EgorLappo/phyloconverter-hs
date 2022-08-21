{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Nexus (parseNexus, writeNexus, writeNexus') where

import Types (Tree(..), NamedTree, Parser)
import Newick (subtree, writeNewickLine)

import Text.Megaparsec
    ( (<|>),
      anySingle,
      anySingleBut,
      oneOf,
      parse,
      between,
      choice,
      many,
      manyTill,
      skipManyTill,
      some,
      MonadParsec(try) )
import Text.Megaparsec.Char ( alphaNumChar, char, space, string' )
import Data.Char ( toLower )

-- write named trees
writeNexus :: [NamedTree String] -> String
writeNexus ts = "#NEXUS\nBEGIN TREES;\n" ++ concat treeLines  ++ "END;\n"
    where treeLines = zipWith3 (\ t n i -> "    TREE " ++ showName n i ++ " = " ++ t ++ ";\n") newickLines names ([1..] :: [Int])
          names = map fst ts
          newickLines = map (writeNewickLine . snd) ts
          showName n i = if n == "" then "Tree"++show i else n

-- write unnamed trees
writeNexus' :: [Tree String] -> String
writeNexus' ts = "#NEXUS\nBEGIN TREES;\n" ++ concat treeLines  ++ "END;\n"
    where treeLines = zipWith (\ t i -> "    TREE Tree" ++ show i ++ " = " ++ t ++ ";\n") newickLines ([1..] :: [Int])
          newickLines = map writeNewickLine ts

-- get parsed trees or an error string
parseNexus :: String -> Either String [NamedTree String]
parseNexus s = do
    content <- showLeft $ parse findTreeBlock "" s -- content is Maybe String
    case content of Nothing -> Left "error parsing your Nexus file: \nno trees found"
                    Just content' -> showLeft $ parse treeBlock "" $ content' ++ "END"

-- parse the whole newick file and return only Just the contents of the "trees" block
-- if there is no such block, return Nothing
findTreeBlock :: Parser (Maybe String)
findTreeBlock = do
    string' "#nexus"
    tb <- some block
    return $ lookup "trees" tb

-- command block parser giving (name, contents)
block :: Parser (String, String)
block = do
    space
    blockBegin
    space
    lab <- some alphaNumChar
    space
    char ';'
    cont <- manyTill anySingle blockEnd
    space
    char ';'
    return (map toLower lab, cont)

-- just the block delimiter shorthands
blockBegin :: Parser String
blockBegin = string' "begin" -- using case-insensitive matches

blockEnd :: Parser String
blockEnd = string' "end"

-- skip lines till we reach actual trees
-- TODO: maybe add parsing of the "Translate" block
treeBlock :: Parser [NamedTree String]
treeBlock = do
    ts <- some treeLine
    return $ concat ts

-- a line in the file that contains a newick string
treeLine :: Parser [NamedTree String]
treeLine = do
    n <- try $ skipManyTill anySingle treeHeader
    t <- subtree
    char ';'
    return [(n,t)]

treeHeader :: Parser String
treeHeader = do
    string' "tree "
    space
    l <- treeLabel
    space
    char '='
    space
    return l

-- a taxon label is either a quoted string or an unquoted string with limited punctuation
treeLabel :: Parser String
treeLabel = quotedString '\"' <|> quotedString '\''<|> unquotedString
    where unquotedString = many $ choice [alphaNumChar, oneOf ['.','_','-']]
          quotedString :: Char -> Parser String
          quotedString c = between (char c) (char c) $ some $ anySingleBut c


-- apply `show` to the left summand of Either
showLeft :: Show a => Either a b -> Either String b
showLeft (Right a) = Right a
showLeft (Left a)  = Left $ "error parsing your Nexus file: \n" ++ show a

-- test1 :: String
-- test1 = "#NEXUS\nBEGIN TAXA;\n      Dimensions NTax=4;\nTaxLabels fish frog snake mouse;\nEND;\n\nBEGIN CHARACTERS;\n      Dimensions NChar=20;\n      Format DataType=DNA;\n      Matrix\n        fish   ACATA GAGGG TACCT CTAAG\n        frog   ACATA GAGGG TACCT CTAAG\n        snake  ACATA GAGGG TACCT CTAAG\n        mouse  ACATA GAGGG TACCT CTAAG\nEND;\nBEGIN TREES;\n      Tree best=(fish, (frog, (snake, mouse)));\nEND;"

-- test2 :: String
-- test2 = "#NEXUS\nBEGIN TAXA;\n      dimensions ntax=4;\n      taxlabels A B C D;\nEND;\nBEGIN CHARACTERS;\n      dimensions nchar=5;\n      format datatype=protein gap=-;\n      charlabels 1 2 3 4 Five;\n      matrix\nA     MA-LL\nB     MA-LE\nC     MEATY\nD     ME-TE\nEND;\nBEGIN TREES;\n       tree \"basic bush\" = ((A:1,B:1):1,(C:1,D:1):1);\nEND;"

-- test3 :: String
-- test3 = "#NEXUS\nBEGIN TAXA;\n    TaxLabels Scarabaeus Drosophila Aranaeus;\nEND;\nBEGIN TREES;\n    Translate beetle Scarabaeus, fly Drosophila, spider Aranaeus;\n    Tree tree1 = ((1,2),3);\n    Tree tree2 = ((beetle,fly),spider);\n    Tree tree3 = ((Scarabaeus,Drosophila),Aranaeus);\nEND;"
