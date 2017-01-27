
import Data.List.Split
import System.IO
import Data.Char

aritDoubleOperators = ["++", "--", "/=", "+=", "-=", "*=", "%="]
logicDoubleOperators = ["==", "||", "&&", "!="]
doubleOperators = aritDoubleOperators ++ logicDoubleOperators ++ ["->", "<-", "<<", ">>" ]
aritOperators = ["+", "-", "=", "*", "/", "%"]
operators = aritOperators ++ [ "&", "!", ";", "(", ")", "{", "}", "<", ">", "#"]
funcDecs = ["void", "int", "bool", "double", "string", "char"]

someFunc :: IO ()
someFunc = do
    
    putStrLn "jest git"
   -- putStrLn "l"

putStrS :: [String] -> IO()
putStrS (x:xs) = do
    putStr $ x ++ "!"
    putStrS xs
putStrS [] = do
    return()

splitByWhiteChars :: String -> [String]
splitByWhiteChars word = splitOneOf " \t\n" word

filterEmpty :: [String] -> [String]
filterEmpty words = filter (/="") words

splitWithForb :: String -> [String] -> String -> [String]
splitWithForb op forb word = if elem word forb
    then [word]
    else
    split (onSublist op) word

splitByList :: [String] -> [String] -> [String] -> [String]
splitByList [] forb words = words
splitByList (x:xs) forb words = splitByList xs forb ( concatMap (splitWithForb x forb) words)

splitByOperators :: [String] -> [String]
splitByOperators = (\words -> filterEmpty . splitByList operators doubleOperators . splitByList doubleOperators [] $ words )

successor :: String -> String
successor x | x == ";" || x == "{" || x == "}" = "\n"
            | otherwise = ""
            
predecessor :: String -> String
predecessor x | x == "using" = "\n"
              | elem x aritDoubleOperators || x == "->" || x == "<-" || x == ";" || x == "(" || x == "#" = ""
              | elem  (map toLower x) funcDecs = "\n"
              | otherwise = " "



printFormated :: [String] -> Int -> Bool -> String
printFormated [] t nl = []
printFormated (x:xs) t True | x=="}" = tab(t-1) ++ printFormated (x:xs) t False
                            | otherwise = tab(t) ++ printFormated (x:xs) t False
printFormated (x:xs) t False | x == "{" = predecessor(x) ++ x ++ successor(x) ++ printFormated xs (t+1) ((\x -> successor(x) == "\n") x)
                             | x == "}" = predecessor(x) ++ x ++ successor(x) ++ printFormated xs (t-1) ((\x -> successor(x) == "\n") x)
                             | otherwise = predecessor(x) ++ x ++ successor(x) ++ printFormated xs t ((\x -> successor(x) == "\n") x)
                            


          
tab :: Int -> String
tab 0 = "";
tab n = "\t" ++  tab (n-1)
