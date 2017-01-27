module Main where

import Lib
import System.IO
import System.Environment


main :: IO ()
main = do
        [fname] <- getArgs
        handle <- openFile fname ReadMode  
        contents <- hGetContents handle
        
        let cleanCode = (\word -> filterEmpty . splitByWhiteChars $ word) contents
        let  separated = (\words -> filterEmpty . splitByOperators $ words) cleanCode
        let output = printFormated separated 0 True
        writeFile ("formated" ++ fname) output
       -- putStrS cleanCode
        hClose handle