module Main where

import qualified Data.Map as Map
import Data.Either
import Data.String
import Data.List.Split (splitOn)
import System.IO.Unsafe

import Solvers.DFS (solveBoard, boardTree, extractFinishedBoards, StepData)
import BoardTypes
import Sudoku
import KillerSudoku
import Kakuro

wordAsMaybeInt :: String -> Maybe Int
wordAsMaybeInt cs
    | all (\x -> x `elem` "123456789") cs = Just $ (read :: String -> Int) cs
    | otherwise = Nothing

makeSudokuTable :: [String] -> [Maybe Int]
makeSudokuTable ls = 
    let ws = concat [words l | l <- ls]
    in map wordAsMaybeInt ws
              
makeKillerSudokuGroups :: [String] -> [String]
makeKillerSudokuGroups ls = concat [words l | l <- ls]

makeKakuroTable :: [String] -> [ReadCell]
makeKakuroTable ls =
    let ws = concat [words l | l <- ls]
    in map wordAsReadCell ws
    where wordAsReadCell :: String -> ReadCell
          wordAsReadCell cs
              | cs == "x" || cs == "X" = Unused
              | cs == "?" = Unknown
              | all (\x -> x `elem` "0123456789/-") cs = let [down, right] = splitOn "/" cs
                                                        in Director {downwardsSum = wordAsMaybeInt down, rightSum = wordAsMaybeInt right}
              | otherwise = error $ "Couldn't parse " ++ cs ++ " into a cell"

example2x2Table = makeSudokuTable [
        "1 x x 4",
        "x x x x",
        "x x x x",
        "4 x x 3"
    ]
    
    {-
example2x2KillerGroups = makeKillerSudokuGroups [
        "a a a b",
        "c b b b",
        "c c d d",
        "e e d d"
    ]
    
example2x2KillerGroupTargets =  (Map.fromList [("a", 6), ("b", 11), ("c", 8), ("d", 10), ("e", 5)])
    -}

example2x2KillerGroups = makeKillerSudokuGroups [
        "d c b b",
        "d c c b",
        "e e a b",
        "e a a a"
    ]
    
example2x2KillerGroupTargets =  (Map.fromList [("a", 10), ("b", 10), ("c", 7), ("d", 4), ("e", 9)])
    
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

    
example2x2Kakuro :: KakuroBoard
example2x2Kakuro = kakuroBoardFromSquare 3 $
    makeKakuroTable [
        "X 7/- 3/-",
        "-/5 ? ?",
        "-/5 ? ?"
    ]
    
exampleLargeKakuro :: IO KakuroBoard
exampleLargeKakuro = do
    puzzle <- readFile "ex_9x9_kakuro.txt"
    return $ kakuroBoardFromSquare 9 $ makeKakuroTable $ lines puzzle
    
momKakuro :: IO KakuroBoard
momKakuro = do
    puzzle <- readFile "FULL_KAKURO.csv"
    return $ kakuroBoardFromSquare 21 $ makeKakuroTable $ lines puzzle
    
solveAndPrint :: (SlotValue v, BoardType t v) => BoardState t v -> IO ()
solveAndPrint board = mapM_ (putStrLn . showBoardContents) $ solveBoard board

solveAndPrintFirst board = putStrLn $ showBoardContents $ head $ solveBoard board

nextStatesStats nextStates = let percentagesFilled = map (\(b,_,_) -> ( * length $ filter isSolved $ map snd $ Map.toList $ slots b) / (length $ slots b)) nextStates
                             in (percentagesFilled, 0)

solveAndPrintProgress :: [[StepData t v]] -> IO ()
solveAndPrintProgress (boards:nextBoards) = do
    let fs = extractFinishedBoards boards
        (percentagesFilledList, _) = nextStatesStats nextBoards
    if null fs
          then do
            putStr $ "No full solns yet, testing " ++ (show $ length $ head $ nextBoards) ++ " partial solutions next "
            putStrLn $ "with " ++ (show $ 100.0 * (min percentagesFilledList)) ++ "% - " ++ (show $ 100.0 $ (max percentagesFilledList)) ++ "% of filled spaces"
            solveAndPrintProgress nextBoards
          else do
            putStrLn $ "Found solution!"
            putStrLn $ showBoardContents $ head fs
solveAndPrintProgress [] = putStrLn "No board"

main :: IO ()
main = do
    putStrLn $ show exampleBoard
    putStrLn $ "Solved: " ++ (show $ isBoardSolved exampleBoard)
    putStrLn $ "Solvable: " ++ (show $ isBoardSolvable exampleBoard)
    putStrLn $ "Solving kakuro..."
    solveAndPrintProgress $ boardTree (unsafePerformIO exampleLargeKakuro)--momKakuro)
