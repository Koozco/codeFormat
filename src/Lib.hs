module Lib
    ( splitByWhiteChars,
      filterEmpty,
      splitByOperators,
      printFormated
    ) where

import Data.List.Split
import System.IO
import Data.Char



aritDoubleOperators = ["++", "--", "/=", "+=", "-=", "*=", "%="]
logicDoubleOperators = ["==", "||", "&&", "!="]
doubleOperators = aritDoubleOperators ++ logicDoubleOperators ++ ["->", "<-", "<<", ">>" ]
aritOperators = ["+", "-", "=", "*", "/", "%"]
operators = aritOperators ++ [ "&", "!", ";", "(", ")", "{", "}", "<", ">", "#"]

-- | Splits given string by all white characters.
--
-- For example:
--
-- >>>splitByWhiteChars "The \n\t whit\ne chars in\t this sentence looks\n strange\t."
-- ["The","","","","whit","e","chars","in","","this","sentence","looks","","strange","."]
splitByWhiteChars :: String -- ^ String that will be splited.
                    -> [String] -- ^ List of words made by splitting.
splitByWhiteChars word = splitOneOf " \t\n" word


-- | Filters empty strings from a list.
--
-- For example:
--
-- >>>filterEmpty ["The","","","","whit","e","chars","in","","this","sentence","looks","","strange","."]
-- ["The","whit","e","chars","in","this","sentence","looks","strange","."]
filterEmpty :: [String] -- ^ List to be filtered.
            -> [String] -- ^ Filtered list.
filterEmpty words = filter (/="") words

splitWithForb :: String -> [String] -> String -> [String]
splitWithForb op forb word = if elem word forb
    then [word]
    else
    split (onSublist op) word

splitByList :: [String] -> [String] -> [String] -> [String]
splitByList [] forb words = words
splitByList (x:xs) forb words = splitByList xs forb ( concatMap (splitWithForb x forb) words)


-- | Splits strings from a given list by cpp operators [eg. +] and returns splited words lists concatenated. 
--
-- For example:
--
-- >>>splitByOperators ["ala+ma-kota", "cout<<should+it+=work+=like==this?"]
-- ["ala","+","ma","-","kota","cout","<<","should","+","it","+=","work","+=","like","==","this?"]

splitByOperators :: [String] -- ^ List of words that need to splited by operators.
                    -> [String] -- ^ List of words and separated operators.
splitByOperators = (\words -> filterEmpty . splitByList operators doubleOperators . splitByList doubleOperators [] $ words )

successor :: String -> String
successor x | x == ";" || x == "{" || x == "}" = "\n"
            | otherwise = ""
            
predecessor :: String -> String
predecessor x | x == "using" = "\n"
              | elem x aritDoubleOperators || x == "->" || x == "<-" || x == ";" || x == "(" || x == "#" = ""
              | x =="include" = ""
              | otherwise = " "

-- | Adds to each word from list its successors and predecessors and tabs and concatenates all in one string. 
--
-- For example:
--
-- >>>printFormated [ "void", "main", "(", ")", "{", "return", "0", ";", "}"] 0 True
-- "\nvoid main( ) {\n\t return 0;\n }\n"


printFormated :: [String] -- ^ List of words to be printed into new string.
                -> Int -- ^ Number of tabs for actual row.
                -> Bool -- ^ Is first element first in new line?
                -> String -- ^ Concatenated input strings with right successorss and predecessors attached for them.
printFormated [] t nl = []
printFormated (x:xs) t True | x=="}" = tab(t-1) ++ printFormated (x:xs) t False
                            | otherwise = tab(t) ++ printFormated (x:xs) t False
printFormated (x:xs) t False | x == "{" = predecessor(x) ++ x ++ successor(x) ++ printFormated xs (t+1) ((\x -> successor(x) == "\n") x)
                             | x == "}" = predecessor(x) ++ x ++ successor(x) ++ printFormated xs (t-1) ((\x -> successor(x) == "\n") x)
                             | otherwise = predecessor(x) ++ x ++ successor(x) ++ printFormated xs t ((\x -> successor(x) == "\n") x)

    
          
tab :: Int -> String
tab 0 = "";
tab n = "\t" ++  tab (n-1)