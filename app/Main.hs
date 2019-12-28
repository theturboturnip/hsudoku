module Main where

import qualified Data.Map as Map
import Data.Either
import Data.String

import Solvers.DFS (solveBoard)
import BoardTypes
import Sudoku
import KillerSudoku

makeSudokuTable :: [String] -> [Maybe Int]
makeSudokuTable ls = 
    let ws = concat [words l | l <- ls]
    in map wordAsMaybeInt ws
    where wordAsMaybeInt :: String -> Maybe Int
          wordAsMaybeInt cs
              | all (\x -> x `elem` "123456789") cs = Just $ (read :: String -> Int) cs
              | otherwise = Nothing
              
makeKillerSudokuGroups :: [String] -> [String]
makeKillerSudokuGroups ls = concat [words l | l <- ls]

example2x2Table = makeSudokuTable [
        "1 x x 4",
        "x x x x",
        "x x x x",
        "4 x x 3"
    ]
    
example2x2KillerGroups = makeKillerSudokuGroups [
        "a a a b",
        "c b b b",
        "c c d d",
        "e e d d"
    ]
    
example2x2KillerGroupTargets =  (Map.fromList [("a", 6), ("b", 11), ("c", 8), ("d", 10), ("e", 5)])

exampleBoard :: SudokuBoard
exampleBoard = makeSudokuBoard 4 2 example2x2Table
{-[
    Just 1,  Nothing, Nothing, Just 4,
    Nothing, Nothing, Nothing, Nothing,
    Nothing, Nothing, Nothing, Nothing,
    Just 4,  Nothing, Nothing, Just 3
    ]-}

example3x3Expert :: SudokuBoard
example3x3Expert = makeSudokuBoard 9 3
    $ makeSudokuTable [
        "x x x x x 8 x x 1",
        "x x 6 4 x x 3 x x",
        "x 5 x x 9 x x 2 x",
        "x x x 3 x x x x x",
        "1 x x x 2 x x 7 x",
        "x 9 x x x 6 x x 4",
        "3 x x x 7 x 6 x x",
        "x 2 x 1 x x x x x",
        "x x x x x 5 x x 9"
    ]
    
example3x3Normal :: SudokuBoard
example3x3Normal = makeSudokuBoard 9 3
    $ makeSudokuTable [
        "x x x x x x x x x",
        "x x x 6 4 1 x x x",
        "x x 7 5 x 3 9 x x",
        "x x 1 x x x 7 x x",
        "4 5 x x x x x 6 3",
        "x 8 6 x x x 4 5 x",
        "x x 8 9 x 4 5 x x",
        "x x 2 7 8 5 6 x x",
        "1 x x x x x x x 9"
    ]
    
example2x2Killer :: KillerSudokuBoard
example2x2Killer = makeKillerSudokuBoard 4 2 example2x2Table example2x2KillerGroups example2x2KillerGroupTargets
    

invalidSolvedSoln :: SudokuBoard
invalidSolvedSoln = makeSudokuBoard 4 2 
    $ makeSudokuTable [
        "1 2 3 4",
        "1 2 3 4",
        "1 2 3 4",
        "1 2 3 4"
    ]

solvedSoln :: SudokuBoard
solvedSoln = makeSudokuBoard 4 2 
    $ makeSudokuTable [
       "1 2 3 4",
       "3 4 1 2", 
       "2 3 4 1", 
       "4 1 2 3"
    ]

solvedKiller :: KillerSudokuBoard
solvedKiller = makeKillerSudokuBoard 4 2 
    (makeSudokuTable [
       "1 2 3 4",
       "3 4 1 2", 
       "2 3 4 1", 
       "4 1 2 3"
    ]) example2x2KillerGroups example2x2KillerGroupTargets


main :: IO ()
main = do
    putStrLn $ show exampleBoard
    putStrLn $ "Solved: " ++ (show $ isSolved exampleBoard)
    putStrLn $ "Solvable: " ++ (show $ isSolvable exampleBoard)
