module Main where

import Control.Monad.State (State, evalState, get, modify, runState, sequence)
import qualified Data.Set as S

data Instruction = Noop | Addx Int deriving (Show)

type InstructionWithTime = (Int, Instruction)

data GameState = GameState
  { 
    cycleStep :: Int,
    registerValue :: [Int],
    instructions :: [InstructionWithTime]
  }
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "2022-Day10.txt"
  print ""

computeGameState ::[String] -> GameState
computeGameState input = do
 let init = GameState {cycleStep=0,registerValue=[1], instructions=[]}
 let parsedInput = parseInput input
 let game = gameLoop parsedInput
 let state = flip evalState init game
 state

gameLoop :: [InstructionWithTime] -> State GameState GameState
gameLoop [] = do
  g <- get
  return g
gameLoop (newInstruction:rest) = do
  g <- get
  let newCycle = (cycleStep g) + 1
  let currentRegister = last $ registerValue g
  let instructions' = (instructions g) ++ [newInstruction]

  let (currentRegister', instructions'') = foldl applyInstructions (currentRegister, []) instructions'

  let instructions''' = filter (\(i,_)-> i>0) instructions''

  let registers = (registerValue g) ++ [currentRegister']
  modify (\g -> g {cycleStep=newCycle, registerValue=registers, instructions=instructions'''})

  current <- get
  gameLoop rest

applyInstructions :: (Int, [InstructionWithTime]) -> InstructionWithTime -> (Int, [InstructionWithTime])
applyInstructions (accX, accI) (1, Noop) = (accX, accI ++ [(0, Noop)])
applyInstructions (accX, accI) (1, Addx n) = (accX + n, accI ++ [(0, Addx n)]) 
applyInstructions (accX, accI) (i, ins) = (accX, accI ++ [(i-1, ins)]) 

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

testData = do
  input <- readFile "2022-Day10-example.input"
  return $ lines input

test = do
  dat <- testData
  return $ computeGameState dat