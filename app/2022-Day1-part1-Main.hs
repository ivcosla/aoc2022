{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read
import Data.List

main :: IO ()
main = do
  contents <- TIO.readFile  "./calories.txt"
  let groups = T.splitOn "\n\n" contents
  let listOfGroups = textListToIntList $ map T.lines groups 
  let sorted = sortByCalories listOfGroups
  let maxCalories = sum $ head sorted
  putStrLn $ show maxCalories 

textListToIntList :: [[T.Text]] -> [[Int]]
textListToIntList l = map (\x -> map (\y -> case (decimal y) of {Right (d,_)->d} ) x) l

sortByCalories :: [[Int]] -> [[Int]]
sortByCalories l = reverse $ sortBy (\e1 e2 -> compare (sum e1) (sum e2)) l
