module Main where
import Control.Monad.State (modify, get, evalState, State, sequence)
import qualified Data.Set as S

type Position = (Int, Int)

data Movement = L | R | U | D deriving (Show, Eq)

data GameState = GameState {
    ropes :: [Position],
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
    let init = GameState { ropes = take 10 $ repeat (15,11), alreadyVisited = S.singleton (15,11)}
    let gameTransitions = map applyMovementChange movements
    let games = flip evalState init $ sequence gameTransitions
    games

applyMovementChange :: (Int, Movement) -> State GameState GameState 
applyMovementChange (0, mov) = do
    game <- get
    return game 
applyMovementChange (howMany, mov) = do
    game <- get
    let knots = ropes game 
    let ropeHead' = applyMove (head $ knots) mov
    let knots' = replace 0 ropeHead' knots
    let knots'' = [ropeHead'] ++ moveKnots knots'
    let alreadyVisited' = S.insert (last knots'') (alreadyVisited game)
    modify (\g -> g{ropes=knots'', alreadyVisited=alreadyVisited'})
    applyMovementChange (howMany-1, mov)

moveKnots :: [(Position)] -> [(Position)]
moveKnots (_:[]) = []
moveKnots (x:next:xs) = 
    let position' = moveKnot x next
    in [position'] ++ moveKnots ([position']++xs)

moveKnot :: Position -> Position -> Position
moveKnot (hx, hy) (tx, ty) =
    let xDiff = abs (tx-hx)
        yDiff = abs (ty-hy)
        moveX = if (hx>tx) then tx+1 else tx-1
        moveY = if (hy>ty) then ty+1 else ty-1
    in case (xDiff, yDiff) of
        (2,2) -> (moveX,moveY)
        (2,_) -> (moveX,hy)
        (_,2) -> (hx, moveY)
        _ -> (tx, ty)

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

testInput = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
test = do 
    let state = computeGameState testInput
    let knots = ropes state
    let visited = length $ alreadyVisited state
    visited == 36

replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]
