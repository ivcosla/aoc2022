{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Control.Monad.State (runState,modify, get, put, evalState, State, state, sequence)
import qualified Data.Set as S

type Position = (Int, Int)

data Movement = L | R | U | D deriving (Show, Eq)

data GameState = GameState {
    ropeHead :: Position,
    ropeTail :: Position,
    alreadyVisited :: S.Set Position
} deriving (Show)

main :: IO ()
main = do
    input <- readFile "2022-Day9.txt"
    let res = computeGameState input
    print $ length $ alreadyVisited res

computeGameState :: String -> GameState
computeGameState input = last $ gameLoop $ map parseMovement $ lines input 

gameLoop :: [(Int, Movement)] -> [GameState]
gameLoop movements = do
    let init = GameState { ropeHead = (4,0), ropeTail = (4,0), alreadyVisited = S.singleton (4,0)}
    let gameTransitions = map applyMovementChange movements
    let games = flip evalState init $ sequence gameTransitions
    games

applyMovementChange :: (Int, Movement) -> State GameState GameState 
applyMovementChange (0, mov) = do
    game <- get
    return game 
applyMovementChange (howMany, mov) = do
    game <- get
    let ropeHead' = applyMove (ropeHead game) mov
    modify (\g->g {ropeHead=ropeHead'})
    areTouching <- touching
    moveTailIfShould areTouching 
    modify (\g->g {alreadyVisited=(S.insert (ropeTail g) (alreadyVisited g))})
    applyMovementChange (howMany-1, mov)

moveTailIfShould :: Bool -> State GameState () 
moveTailIfShould should 
    | should == True = return ()
    | should == False = do
        game <- get
        let (h, t) = (ropeHead game, ropeTail game)
        modify (\g -> g{ ropeTail=(computeTailPosition h t) })
        return ()

computeTailPosition :: Position -> Position -> Position
computeTailPosition (hx, hy) (tx, ty)
    | hx == tx = let dir = hy - ty
                 in if (dir > 0) then (tx, ty+1) else (tx, ty-1)
    | hy == ty = let dir = hx - tx
                 in if (dir > 0) then (tx+1, ty) else (tx-1, ty)
    | hx < tx && hy < ty = (tx-1,ty-1)
    | hx < tx && hy > ty = (tx-1, ty+1)
    | hx > tx && hy < ty = (tx+1, ty-1)
    | hx > tx && hy > ty = (tx+1, ty+1)

parseMovement :: String -> (Int, Movement)
parseMovement s = 
    let parts = words s
    in case (parts) of
        ("L":n:_) -> (read n, L)
        ("R":n:_) -> (read n, R) 
        ("U":n:_) -> (read n, U) 
        ("D":n:_) -> (read n, D) 

applyMove :: Position -> Movement -> Position
applyMove (x, y) L = (x, y-1) 
applyMove (x, y) R = (x, y+1) 
applyMove (x, y) D = (x+1, y) 
applyMove (x, y) U = (x-1, y) 

touching :: State GameState Bool
touching = do
        state <- get
        let (h, t) = (ropeHead state, ropeTail state)
        return $ positionsTouching h t

positionsTouching :: Position -> Position -> Bool
positionsTouching (hx, hy) (tx, ty)
    | hx == tx =
        let onTop = hy == ty
            left = ty == (hy -1)
            right = ty == (hy + 1)
        in any (==True) [onTop, left, right]
    | hy == ty =
        let up = tx == (hx - 1)
            down = tx == (hx + 1)
        in any (==True) [up, down]
    | otherwise =
        let topLeftDiag = ((tx == hx - 1) && (ty == hy-1))
            topRightDiag = ((tx == hx + 1) && (ty == hy-1))
            bottomLeftDiag = ((tx == hx -1) && (ty == hy + 1))
            bottomRightDiag = ((tx == hx + 1) && (ty == hy + 1))
        in any (==True) [topLeftDiag, topRightDiag, bottomLeftDiag, bottomRightDiag]

testInput = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
test = do 
    let state = computeGameState testInput
    let finalHead = ropeHead state
    let finalTail = ropeTail state
    let visited = length $ alreadyVisited state
    finalHead == (2,2) && finalTail == (2,1) && visited == 13