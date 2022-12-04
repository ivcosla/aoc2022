module Main where
import Data.List

main :: IO ()
main = do
  contents <- readFileContents' "./calories.txt"
  let splitted = reverse $ splitListByEmpty contents [[]]
  let sorted = sortByCalories $ enumerate $ stringListToIntList splitted
  putStrLn ("elf with max calories: " ++ show (last sorted))
  putStrLn ("total calories: " ++ show (sum . snd . last $ sorted))
  
enumerate :: [[a]] -> [(Int, [a])]
enumerate l = let rangeOfLength = [1..(length l)] in 
  zip rangeOfLength l  

readFileContents' :: String -> IO [String]
readFileContents' s = do 
  content <- readFile s 
  return (lines content)

splitListByEmpty :: [String] -> [[String]] -> [[String]]
splitListByEmpty [] acc = acc
splitListByEmpty ("":xr) acc = splitListByEmpty xr $ [[]] ++ acc
splitListByEmpty (xl:xr) (aFirst:aRest) = splitListByEmpty xr $ (aFirst ++ [xl]): aRest 

stringListToIntList :: [[String]] -> [[Int]]
stringListToIntList l = map (\x -> map (\y -> read y :: Int) x) l

sortByCalories :: [(Int, [Int])] -> [(Int,[Int])]
sortByCalories l = sortBy (\(i1, e1) (i2, e2) -> compare (sum e1) (sum e2)) l
