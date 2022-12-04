module Main where

import Data.List

main :: IO ()
main = do
  contents <- readFileContents' "./2022-Day4.txt"
  let total = computeTotal contents
  putStrLn ("total contained " ++ show total)

computeTotal contents = do
    let pairs = map lineToPairs contents
    let result = map (\x->fromEnum $ pairsAreIncluded x) pairs
    sum result

data Included = NotIncluded | Included deriving (Enum) 

type Pair = (Int, Int)
type Pairs = (Pair, Pair)

pairsAreIncluded :: Pairs -> Included
pairsAreIncluded ((pa1,pa2),(pb1,pb2)) = 
  if 
   isFirstInSecond ||isSecondInFirst 
  then Included else NotIncluded
 where 
  isFirstInSecond = (pa1 >= pb1 && pa1 <= pb2) || (pa2 >= pb1 && pa2 <= pb2)
  isSecondInFirst= (pb1 >= pa1 && pb1 <= pa2) || (pb2 >= pa1 && pb2 <= pa2) 

readFileContents' :: String -> IO [String]
readFileContents' s = do 
  content <- readFile s 
  return (lines content)

lineToPairs :: String -> Pairs 
lineToPairs l = case (break (==',') l) of
  (t1, ',':t2) -> (splitPair t1, splitPair t2)

splitPair :: String -> Pair 
splitPair s = case (break (=='-') s) of
 (t1,'-':t2) -> (toInt t1, toInt t2)
 where toInt toParse = read toParse :: Int

test = computeTotal ["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"] == 4