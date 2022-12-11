module Main where

import Control.Monad.State (State, evalState, get, modify, runState, sequence)
import Data.List (intercalate)

data Instruction = Noop | Addx Int deriving (Show)

type InstructionWithTime = (Int, Instruction)

data GameState = GameState
  { 
    cycleStep :: Int,
    registerValue :: [Int]
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "2022-Day10.txt"
  let g = computeGameState $ lines input
  let registers = registerValue g
  let compute = computeLine . take 40
  let rows = take 6 $ iterate (\r -> drop 40 r) registers
  let screen = intercalate "\n" $ map compute rows
  putStr screen

computeLine :: [Int] -> [Char]
computeLine regs =
  let enumerate = zip [0..] regs
  in
     map (\(a,b)->if ((a>=(b-1))&&(a<=(b+1))) then '#' else '.') enumerate

computeGameState ::[String] -> GameState
computeGameState input = 
 let init = GameState {cycleStep=0,registerValue=[1]}
     game = gameLoop $ parseInput input
 in
 flip evalState init game

gameLoop :: [InstructionWithTime] -> State GameState GameState
gameLoop ins = do
  g <- get
  let clock = cycleStep g
  let regs = registerValue g
  let (clock', regs') = foldl gameFold (clock, regs) ins
  return g {cycleStep=clock', registerValue=regs'}

gameFold :: (Int, [Int]) -> InstructionWithTime -> (Int, [Int])
gameFold (clock, regs) (i, Noop) = (clock+i, regs ++ [(last regs)])
gameFold (clock, regs) (i, Addx n) = 
  let lastReg = last regs
      fill = take (i-1) $ repeat lastReg 
  in (clock+i, regs ++ fill ++ [lastReg + n])

parseInput :: [String] -> [InstructionWithTime]
parseInput s = map parseInstruction s

parseInstruction :: String -> InstructionWithTime
parseInstruction s = 
  let parts = words s
  in case (parts) of
      ("noop":_) -> (1, Noop) 
      ("addx":n:_) -> (2, Addx (read n))