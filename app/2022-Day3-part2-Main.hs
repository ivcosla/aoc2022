module Main where

import qualified Data.Map as M

main :: IO ()
main = do
  contents <- readFileContents' "./2022-Day3.txt"
  let result = computeLines buildPriorities contents
  putStrLn ("total priority " ++ show result)

type Scores = M.Map Char Int

readFileContents' :: String -> IO [String]
readFileContents' s = do 
  content <- readFile s 
  return (lines content)

computeLines scores l = sum $ map (\x-> scoreForChar scores $ findElementInCommon x) $ splitInChunksOf3 l

splitInChunksOf3 :: [a] -> [(a,a,a)]
splitInChunksOf3 [] = []
splitInChunksOf3 (x1:x2:x3:rest) = [(x1,x2,x3)] ++ splitInChunksOf3 rest 

findElementInCommon :: (String,String,String) -> Char
findElementInCommon (line1,line2,line3) = 
  case (foldl mapping Nothing line1) of
  Just c -> c
 where mapping acc c = case (acc) of 
        Just c -> Just c
        Nothing -> case ((charAppearsInString line2 c) && (charAppearsInString line3 c)) of
         True -> Just c
         False -> Nothing

stringToChars :: String -> [Char]
stringToChars s = id s

scoreForChar :: Scores -> Char -> Int
scoreForChar cache c = case M.lookup c cache of
  Just s -> s
  Nothing -> 0

charAppearsInString :: String -> Char -> Bool
charAppearsInString s c = isThere . length $ filter (== c) s 
 where isThere n = n > 0

buildPriorities :: Scores 
buildPriorities = 
  let lowerCases = ['a'..'z']
      upperCases = ['A'..'Z']
      lowerScores = zip lowerCases [1..26]
      upperScores = zip upperCases [27..56]
  in
    M.fromList $ lowerScores++upperScores 

test = computeLines buildPriorities ["vJrwpWtwJgWrhcsFMMfFFhFp","jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"] == 70 