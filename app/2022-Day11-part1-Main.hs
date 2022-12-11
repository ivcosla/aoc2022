module Main where

import Control.Monad.State (State, evalState, get, put, modify, runState, sequence)
import Data.List (intercalate, sortOn)

type Item = Int

data Monkey = Monkey {
  index :: Int,
  items :: [Item],
  inspected :: Int,
  test :: (Item->Bool),
  actionOnTest :: (Bool->Item->State GameState ()),
  operation :: (Item->Item)
}

instance Show Monkey where
  show (Monkey{items=i}) = show i

data GameState = GameState
  { 
    roundCount :: Int,
    monkeys :: [Monkey]
  }
  deriving (Show)

main :: IO ()
main = do
  let res = evalState (gameLoop 20) initGame
  let score = monkeyBusiness $ monkeys res
  print score

gameLoop :: Int -> State GameState GameState
gameLoop 0 = do 
  g<-get
  return g
gameLoop rounds = 
  do
    g <- get
    sequence $ map processMonkey $ monkeys g
    modify (\s -> s{roundCount=succ (roundCount s)})
    gameLoop $ pred rounds

initGame :: GameState
initGame = do
  let m0 = Monkey {
    index= 0,
    inspected= 0,
    items=[72, 64, 51, 57, 93, 97, 68],
    operation = (*19),
    test = divisibleBy 17,
    actionOnTest = monkeyAction (4,7) 
  }
  let m1 = Monkey {
    index= 1,
    inspected= 0,
    items=[62],
    operation = (*11),
    test = divisibleBy 3,
    actionOnTest = monkeyAction (3,2)
  }
  let m2 = Monkey {
    index= 2,
    inspected= 0,
    items=[57, 94, 69, 79, 72],
    operation = (+6),
    test = divisibleBy 19,
    actionOnTest = monkeyAction (0,4)
  }
  let m3 = Monkey {
    index= 3,
    inspected= 0,
    items=[80, 64, 92, 93, 64, 56],
    operation = (+5),
    test = divisibleBy 7,
    actionOnTest = monkeyAction (2,0)
  }
  let m4 = Monkey {
    index= 4,
    inspected= 0,
    items=[70, 88, 95, 99, 78, 72, 65, 94],
    operation = (+7),
    test = divisibleBy 2,
    actionOnTest = monkeyAction (7,5)
  }
  let m5 = Monkey {
    index= 5,
    inspected= 0,
    items=[57, 95, 81, 61],
    operation = (^2),
    test = divisibleBy 5,
    actionOnTest = monkeyAction (1,6)
  }
  let m6 = Monkey {
    index= 6,
    inspected= 0,
    items=[79,99],
    operation = (+2),
    test = divisibleBy 11,
    actionOnTest = monkeyAction (3,1)
  }
  let m7 = Monkey {
    index= 7,
    inspected= 0,
    items=[68, 98, 62],
    operation = (+3),
    test = divisibleBy 13,
    actionOnTest = monkeyAction (5,6)
  }
  GameState{roundCount=0, monkeys=[m0,m1,m2,m3,m4,m5,m6,m7]}

divisibleBy n = (\i -> (mod i n) == 0)

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness mons =
  let sorted = reverse $ sortOn (\Monkey{inspected=ins} -> ins) mons
      twoBigger = take 2 sorted
      twoInspected = map (\Monkey{inspected=ins}->ins) twoBigger
  in
    product twoInspected

processMonkey :: Monkey -> State GameState () 
processMonkey Monkey{index=n} = 
  do
    g <- get
    let monkey = (monkeys g) !! n
    processMonkey' monkey

processMonkey' :: Monkey -> State GameState () 
processMonkey' Monkey{index=n, items=[], operation=op, test=t, actionOnTest=action, inspected=ins} = 
  do
    g <- get
    let prevMonkeys = monkeys g
    let monkeys' = replace n Monkey{index=n,items=[], operation=op, test=t, actionOnTest=action, inspected=ins} prevMonkeys
    modify (\s -> s{monkeys=monkeys'})
    return ()
processMonkey' Monkey{index=n, items=(i:rest), operation=op, test=t, actionOnTest=action, inspected=ins} =
  do
    let worryAfterInspect = op i
    let worryAfterBored = div worryAfterInspect 3
    let testResult = t worryAfterBored
    action testResult worryAfterBored
    processMonkey' Monkey {index=n, items=rest, operation=op, test=t, actionOnTest=action, inspected=succ ins}

monkeyAction :: (Int, Int) -> Bool -> Item -> State GameState ()
monkeyAction (_, monkeyIfFalse) False item =
  modify (\g -> g{monkeys=(addItemToMonkey monkeyIfFalse item (monkeys g))})
monkeyAction (monkeyIfTrue,_) True item =
  modify (\g -> g{monkeys=(addItemToMonkey monkeyIfTrue item (monkeys g))})

addItemToMonkey :: Int -> Item -> [Monkey] -> [Monkey]
addItemToMonkey to item monkeys =
  let target = monkeys !! to 
      targetWithItem = (items target) ++ [item] 
  in replace to target{items=targetWithItem} monkeys

replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]

testState :: GameState
testState = do
  let m0 = Monkey {
    index= 0,
    inspected= 0,
    items=[79,98],
    operation = (*19),
    test = divisibleBy 23,
    actionOnTest = monkeyAction (2,3) 
  }
  let m1 = Monkey {
    index= 1,
    inspected= 0,
    items=[54,65,75,74],
    operation = (+6),
    test = divisibleBy 19,
    actionOnTest = monkeyAction (2,0)
  }
  let m2 = Monkey {
    index= 2,
    inspected= 0,
    items=[79,60,97],
    operation = (^2),
    test = divisibleBy 13,
    actionOnTest = monkeyAction (1,3)
  }
  let m3 = Monkey {
    index= 3,
    inspected= 0,
    items=[74],
    operation = (+3),
    test = divisibleBy 17,
    actionOnTest = monkeyAction (0,1)
  }
  GameState{roundCount=0, monkeys=[m0,m1,m2,m3]}

test1 = do
  let s = evalState (gameLoop 20) initGame
  let mons = monkeys s
  let check1 = (inspected (mons !! 0)) == 101
  let check2 = (inspected (mons !! 3)) == 105
  check1 == check2