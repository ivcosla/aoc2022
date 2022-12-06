
module Main where
import Data.List (nub)

main :: IO ()
main = do
  contents <- readFile "./2022-Day6.txt"
  let marker = findMarker contents
  putStrLn ("result: " ++ show marker)

findMarker :: (Eq a) => [a] -> Int
findMarker =  marker . findWindowInEnumerate . enumerate

marker :: [(Int, a)] -> Int
marker = fst . last

inspectWindow :: (Eq a) =>  [a] -> Bool
inspectWindow w = (==) (length w) (length $ nub w)

findWindowInEnumerate :: (Eq a) => [(Int, a)] -> [(Int, a)]
findWindowInEnumerate l = 
  let window = take 14 l
      rest = drop 14 l
      isPacket = inspectWindow $ map snd window 
  in
    case (isPacket) of
      True -> window
      False -> findWindowInEnumerate $ (tail window)++rest

enumerate :: [a] -> [(Int, a)]
enumerate l = zip [1..length l] l

test = 19 == findMarker "mjqjpqmgbljsphdztnvjfqwrcgsmlb"