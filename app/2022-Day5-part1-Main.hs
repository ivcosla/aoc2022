{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.List

import qualified Data.Text as T

type Stack = [Char]

main :: IO ()
main = do
  contents <- readFile "./Day5-input.txt"
  let (part1, part2) = fileIntoParts contents
  let stacks = parseStacks part1
  let moves = map parseInstruction part2
  let resultingStack = applyMoves stacks moves
  let onTop = concatMap (take 1) resultingStack
  putStrLn ("result: " ++ show onTop)

applyMoves :: [Stack] -> [(Int,Int,Int)] -> [Stack]
applyMoves st moves = foldl applyMove st moves

applyMove :: [Stack] -> (Int,Int,Int) -> [Stack]
applyMove st (n, f, t) = 
  let moving = take n (st !! (f-1))
      from = drop n (st !! (f-1))
      to = (reverse moving) ++ (st !! (t-1))
      replacedFrom = replace (f-1) from st 
  in
  replace (t-1) to replacedFrom

fileIntoParts :: String -> ([String], [String])
fileIntoParts s =  let l = lines s in
  (take 8 l, drop 10 l)

parseStacks :: [String] -> [Stack]
parseStacks l = let parseForIndexes = parseStack l in
  map parseForIndexes [0..8]

parseStack :: [String] -> Int -> Stack
parseStack stack offset = 
  let getAndParse = parseItem . getItem 
  in 
      foldl (foldParsing getAndParse) [] stack
  where getItem l = take 3 $ drop (offset * 4) l

foldParsing :: (String->Maybe Char) -> Stack -> String -> Stack
foldParsing parser stack s = let res = parser s in
  case res of
    Nothing -> stack
    Just r -> stack ++ [r]

parseItem :: String -> Maybe Char
parseItem "   " = Nothing
parseItem (_:c:_) = Just c

parseInstruction :: String -> (Int, Int, Int)
parseInstruction s = 
  let parsed = map read instructions 
  in
     (parsed !! 0, parsed !! 1, parsed !! 2)
  where 
    instructions = map T.unpack $ T.splitOn " " $ T.replace "move " "" $ T.replace " from" "" $ T.replace " to" "" $ T.pack s

replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]