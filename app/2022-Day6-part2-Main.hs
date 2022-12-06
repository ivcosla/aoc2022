
module Main where
import Data.List (nub)

main :: IO ()
main = do
  contents <- readFile "./2022-Day6.txt"
  let marker = findMarker 14 contents
  putStrLn ("result: " ++ show marker)

findMarker :: (Eq a) => Int -> [a] -> Int
findMarker i =  marker . findWindowInEnumerate i . enumerate

marker :: [(Int, a)] -> Int
marker = fst . last

inspectWindow :: (Eq a) =>  [a] -> Bool
inspectWindow w = (==) (length w) (length $ nub w)

findWindowInEnumerate :: (Eq a) => Int -> [(Int, a)] -> [(Int, a)]
findWindowInEnumerate i l
  | inspectWindow $ map snd (take i l) = take i l
  | otherwise = findWindowInEnumerate i (tail l) 

enumerate :: [a] -> [(Int, a)]
enumerate l = zip [1..length l] l

test = 19 == findMarker 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"