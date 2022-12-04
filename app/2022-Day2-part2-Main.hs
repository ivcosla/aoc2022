{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  contents <- TIO.readFile "./Day2-input.txt"
  let score = computeScore contents
  putStrLn ("total score: " ++ show score)

computeScore :: T.Text -> Int
computeScore contents = do
  let lineToScore = computePlayResult . choosePlay . lineToMatch
  let results = map lineToScore $ T.lines contents
  sum results

data Play = Rock | Paper | Scissors deriving (Eq, Show)

data Strategy = Win | Draw | Loose

instance Enum Play where
  fromEnum Rock = 1
  fromEnum Paper = 2
  fromEnum Scissors = 3
  toEnum 1 = Rock
  toEnum 2 = Paper 
  toEnum 3 = Scissors 

instance Ord Play where
  compare Rock Scissors = GT
  compare Paper Rock = GT
  compare Scissors Paper = GT
  compare a b 
   | fromEnum a == fromEnum b = EQ 
   | otherwise = LT 

parsePlay :: Char -> Play
parsePlay 'A' = Rock
parsePlay 'B' = Paper
parsePlay 'C' = Scissors

parseStrategy :: Char -> Strategy
parseStrategy 'X' = Loose
parseStrategy 'Y' = Draw 
parseStrategy 'Z' = Win 

lineToMatch :: T.Text -> (Play, Strategy)
lineToMatch l = 
  (parsePlay opponent, parseStrategy me)
      where opponent = head $ T.unpack l; me = last $ T.unpack l;

choosePlay :: (Play, Strategy) -> (Play, Play)
choosePlay (Rock, Win) =  (Rock, Paper)
choosePlay (Paper, Win) =  (Paper, Scissors)
choosePlay (Scissors, Win) =  (Scissors, Rock)
choosePlay (p, Draw) =  (p, p)
choosePlay (Rock, Loose) =  (Rock, Scissors)
choosePlay (Paper, Loose) =  (Paper, Rock)
choosePlay (Scissors, Loose) =  (Scissors, Paper)

computePlayResult :: (Play, Play) -> Int
computePlayResult (opponent, me) = case (compare me opponent) of
  GT -> 6 + fromEnum me
  EQ -> 3 + fromEnum me
  LT -> 0 + fromEnum me

test = computeScore "A Y\nB X\nC Z" == 12