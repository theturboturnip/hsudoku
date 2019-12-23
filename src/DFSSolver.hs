module DFSSolver where

import Data.Either
import Data.Maybe
import Data.List
import Lib
import Solver


coords :: PartialSolution -> [Coord]
coords s = [(x,y) | y <- [0..(size s) - 1], x <- [0..(size s) - 1]]

solveSudoku :: PartialSolution -> [PartialSolution]
solveSudoku s = let s' = constrainUnsolvedSlots s in
                    solveSudokuImpl (sortBestCoordsForResolve s') s'
                    
-- Takes a solution and a list of coords sorted by least possibilities
solveSudokuImpl :: [([Int], Coord)] -> PartialSolution -> [PartialSolution]
solveSudokuImpl [] s = if (isSolved s && isValidlySolved s) then [s] else []
solveSudokuImpl ((ps, c):cs) s = -- ps = stack of possible values, c = cood
    let sWithPs = [setCoordSlotValue s c p | p <- ps]
    in foldr ((++) . (solveSudokuImpl cs)) [] sWithPs
                    
sortBestCoordsForResolve :: PartialSolution -> [([Int], Coord)]
sortBestCoordsForResolve s = (sortOn (length . fst) [(fromLeft [] slot, c) | c <- coords s, let slot = fromJust $ indexSoln s c, isLeft slot])



-- Sort coordinates by lowest amount of possibilities
-- Set the top coord to it's first value
-- Check if solveSudokuImpl can solve that
-- Set the top coord to the next possible value, check
-- etc.
-- If a solution is found, return it
-- If none are found, return []
    
