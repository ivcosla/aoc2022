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
findWindowInEnumerate (l1:l2:l3:[]) = []
findWindowInEnumerate (l1:l2:l3:l4:rest) = 
  let isPacket = inspectWindow $ map snd [l1,l2,l3,l4]
  in
    case (isPacket) of
      True -> [l1,l2,l3,l4]
      False -> findWindowInEnumerate $ [l2,l3,l4]++rest

enumerate :: [a] -> [(Int, a)]
enumerate l = zip [1..length l] l

test = 5 == findMarker "bvwbjplbgvbhsrlpgdmjqwftvncz"