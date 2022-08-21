module Main (main) where

import System.Environment   
import Data.List.Split
import Lib (convert)
import Types (FileFormat(..))

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
    then putStrLn "please provide two arguments: input file and output file"
    else processInput args
    
    
processInput :: [String] -> IO ()
processInput args = do 
    contents <- readFile iFile
    case convert iFormat oFormat contents of 
        Left e -> putStr e
        Right result -> writeFile oFile result
    where iFile = args !! 0
          oFile = args !! 1
          iFormat = getFormat iFile
          oFormat = getFormat oFile

getFormat :: String -> FileFormat
getFormat s = case ext of 
                "newick" -> Newick
                "tree"   -> Newick
                "nxs"    -> Nexus
                "nex"    -> Nexus
                _        -> error "unsupported file format"
              where ext = last $ splitOn "." s
                