{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Main where
import Data.List (intersperse)
import Control.Exception (throw, Exception)
import qualified Data.Tree as T

main :: IO ()
main = do 
    input <- readFile "./2022-Day8.txt"
    let grid = parseInput $ lines input
    let total = check grid
    putStrLn $ "total: " ++ show total

type X = Int
type Y = Int

type Position = (X, Y)

type Grid = [[Int]]
type Column = [Int]
type Row = [Int]

parseInput :: [String] -> Grid
parseInput l = map (\x-> parseString $ words $ intersperse ' ' x) l

parseString :: [String] -> Row
parseString s = map (\x -> (read x)::Int) s

check :: Grid -> Int
check g = 
    do
        let tall = length g
        let wide = length (g !! 0)
        let treesInBorder = ((tall + wide) * 2) -4
        let toCheck = [ (x, y) |x<-[1..tall-2], y<-[1..wide-2]]
        treesInBorder + computeVisibilityOfPositions g toCheck

computeVisibilityOfPositions :: Grid -> [Position] -> Int
computeVisibilityOfPositions g [] = 0
computeVisibilityOfPositions g (p:rest) =
    let 
        r = computeVisibilityForPosition g p
        countTree = if (r>0) then 1 else 0
    in
        countTree + computeVisibilityOfPositions g rest

computeVisibilityForPosition :: Grid -> Position -> Int
computeVisibilityForPosition g (x, y) =
    let row = computeRowInGrid g x
        column = computeColumnInGrid g y
        (subR1, subR2) = computeSubRowsFromIndex row y
        (subC1, subC2) = computeSubRowsFromIndex column x
        computeForRow = computeIfVisible (row !! y)
        computeForColumn = computeIfVisible (column !! x)
        horizontalVisibility = [computeForRow subR1, computeForRow subR2]
        visibility = horizontalVisibility ++ [computeForColumn subC1, computeForColumn subC2]
    in
        sum visibility

computeIfVisible :: Int -> Row -> Int
computeIfVisible tall r = 
    let visible = all (<tall) r
    in
        if (visible) then 1 else 0 

computeSubRowsFromIndex :: Row -> Int -> (Row, Row)
computeSubRowsFromIndex r target =
    let onTheLeft = take target r
        onTheRight = drop (target+1) r
    in
    (onTheLeft, reverse onTheRight) 

computeColumnInGrid :: Grid -> Y -> Column
computeColumnInGrid g target =
    foldl (\acc xs -> acc ++ [xs!!target]) [] g

computeRowInGrid :: Grid -> X -> Row
computeRowInGrid g target = g !! target

testGrid = "30373\n25512\n65332\n33549\n35390"