module Main where

import qualified Data.Map as M

main :: IO ()
main = do
  contents <- readFileContents' "./2022-Day3.txt"
  let scoreChache = buildPriorities
  let compute = computeScore scoreChache
  let splitAndCompute = compute . splitLineInTwo
  let freqs = map splitAndCompute contents  
  let total = sum freqs
  putStrLn ("frequencies " ++ show total)

type Cache = M.Map Char Int

readFileContents' :: String -> IO [String]
readFileContents' s = do 
  content <- readFile s 
  return (lines content)

computeScore :: Cache -> (String, String) -> Int
computeScore cache (pack1, pack2) = foldl (\f x-> if (f>0) then f else score pack2 x) 0 $ stringToChars pack1
 where score pack c = case (charAppearsInString pack c) of 
                      True -> scoreForChar cache c
                      False -> 0 

stringToChars :: String -> [Char]
stringToChars [] = []
stringToChars (x:xs) = [x] ++ stringToChars xs

scoreForChar :: Cache -> Char -> Int
scoreForChar cache c = case M.lookup c cache of
  Just s -> s
  Nothing -> 0

charAppearsInString :: String -> Char -> Bool
charAppearsInString s c = isThere . length $ filter (== c) s 
 where isThere n = n > 0

splitLineInTwo :: String -> (String, String)
splitLineInTwo l = 
  let halfLength = div (length l) 2
  in
    splitAt halfLength l

buildPriorities :: Cache
buildPriorities = 
  let lowerCases = ['a'..'z']
      upperCases = ['A'..'Z']
      lowerScores = zip lowerCases [1..26]
      upperScores = zip upperCases [27..56]
  in
    M.fromList $ lowerScores++upperScores 