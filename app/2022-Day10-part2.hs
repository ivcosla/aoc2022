module Main where

import Control.Monad.State (State, evalState, get, modify, runState, sequence)
import qualified Data.Set as S
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
  let screen = intercalate "" $ map compute rows
  putStr screen

computeLine :: [Int] -> [Char]
computeLine regs =
  let enumerate = zip [0..] regs
      line = map (\(a,b)->if ((a>=(b-1))&&(a<=(b+1))) then '#' else '.') enumerate
  in line ++ ['\n']

computeGameState ::[String] -> GameState
computeGameState input = do
 let init = GameState {cycleStep=0,registerValue=[1]}
 let parsedInput = parseInput input
 let game = gameLoop parsedInput
 let state = flip evalState init game
 state

gameLoop :: [InstructionWithTime] -> State GameState GameState
gameLoop [] = do
  g <- get
  return g
gameLoop ((1, i):rest) = do
  g <- get
  let newCycle = (cycleStep g) + 1
  let currentRegister = last $ registerValue g
  let currentRegister' = applyInstruction currentRegister (1,i)
  let registers = (registerValue g) ++ [currentRegister']
  modify (\g -> g {cycleStep=newCycle, registerValue=registers})
  gameLoop rest
gameLoop ((cCount, i):rest) = do
  g <- get
  let newCycle = (cycleStep g) + 1
  let currentRegister = last $ registerValue g
  let register' = (registerValue g) ++ [currentRegister]
  modify (\g' -> g'{cycleStep=newCycle, registerValue=register'})
  gameLoop ([(cCount-1, i)] ++ rest)


applyInstruction :: Int->InstructionWithTime -> Int
applyInstruction reg (1, Noop) = reg 
applyInstruction reg (1, Addx n) = reg + n 

parseInput :: [String] -> [InstructionWithTime]
parseInput s = map parseInstruction s

parseInstruction :: String -> InstructionWithTime
parseInstruction s = 
  let parts = words s
  in
    case (parts) of
      ("noop":_) -> (1, Noop) 
      ("addx":n:_) -> let parsedVal = read n :: Int
                    in (2, Addx parsedVal)