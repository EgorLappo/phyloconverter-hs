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
    else 
        let ifile = args !! 0
            oFile = args !! 1
            iFormat = getFormat ifile
            oFormat = getFormat ofile
        in 
        

getFormat :: String -> FileFormat
getFormat s = case ext of 
                "newick" -> Newick
                "nxs"    -> Nexus
                "nex"    -> Nexus
              where ext = split