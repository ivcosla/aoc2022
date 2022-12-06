{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  contents <- readFileContents' "./Day2-input.txt"
  let plays = map lineToMatch contents
  let score = sum $ map computePlayResult plays
  putStrLn ("total score: " ++ show score)

data Play = Rock | Paper | Scissors deriving (Eq, Show)

instance Enum Play where
  fromEnum Rock = 1
  fromEnum Paper = 2
  fromEnum Scissors = 3
  toEnum _ = Rock

instance Ord Play where
  compare Rock Scissors = GT
  compare Paper Rock = GT
  compare Scissors Paper = GT
  compare a b 
   | fromEnum a == fromEnum b = EQ 
   | otherwise = LT 

parsePlay :: Char -> Play
parsePlay p 
  | isIn ['A','X'] = Rock
  | isIn ['B','Y'] = Paper 
  | isIn ['C','Z'] = Scissors 
  where isIn = elem p

lineToMatch :: String -> (Play, Play)
lineToMatch l = 
  (parsePlay opponent, parsePlay me)
      where opponent = head l; me = last l;

computePlayResult :: (Play, Play) -> Int
computePlayResult (opponent, me) = case (compare me opponent) of
  GT -> 6 + fromEnum me
  EQ -> 3 + fromEnum me
  LT -> 0 + fromEnum me