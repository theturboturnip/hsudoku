module Main where

import Lib
import Data.Either
import Solver
import DFSSolver

exampleSoln :: PartialSolution
exampleSoln = PartialSolution { 
    size = 4,
    squareSize = 2,
    slots = [
        Right 1,     Left [1..4], Left [1..4], Right 4,
        Left [1..4], Left [1..4], Left [1..4], Left [1..4],
        Left [1..4], Left [1..4], Left [1..4], Left [1..4],
        Right 4,     Left [1..4], Left [1..4], Right 3
    ]
}

invalidSolvedSoln :: PartialSolution
invalidSolvedSoln = PartialSolution { 
    size = 4,
    squareSize = 2,
    slots = [
        Right 1, Right 2, Right 3, Right 4,
        Right 1, Right 2, Right 3, Right 4,
        Right 1, Right 2, Right 3, Right 4,
        Right 1, Right 2, Right 3, Right 4  
    ]
}

solvedSoln :: PartialSolution
solvedSoln = PartialSolution { 
    size = 4,
    squareSize = 2,
    slots = map Right [
       1, 2, 3, 4,
       3, 4, 1, 2, 
       2, 3, 4, 1, 
       4, 1, 2, 3
    ]
}



main :: IO ()
main = do
    putStrLn $ show exampleSoln
    putStrLn $ "Solved: " ++ (show $ isSolved exampleSoln)
    putStrLn $ "Solvable: " ++ (show $ isSolvable exampleSoln)
