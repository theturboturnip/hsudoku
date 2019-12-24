module DFSSolver where

import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Lib
import Solutions


{-coords :: PartialSolution -> [Coord]
coords s = [(x,y) | y <- [0..(size s) - 1], x <- [0..(size s) - 1]]-}

solveSudoku :: (Eq v) => BoardState t v -> [BoardState t v]
solveSudoku s = let s' = constrainUnsolvedSlots s in
                    solveSudokuImpl (sortBestCoordsForResolve s') s'
                    
-- Takes a solution and a list of coords sorted by least possibilities
solveSudokuImpl :: (Eq v) => [([v], Coord)] -> BoardState t v -> [BoardState t v]
solveSudokuImpl [] board = if (isSolved board && isValidlySolved board) then [board] else []
solveSudokuImpl ((ps, c):cs) board = -- ps = stack of possible values, c = cood
    let sWithPs = [constrainUnsolvedSlots $ updateBoardStateSlot board c p | p <- ps]
    in foldr ((++) . (\x -> solveSudokuImpl (sortBestCoordsForResolve x) x)) [] sWithPs
                    
sortBestCoordsForResolve :: BoardState t v -> [([v], Coord)]
sortBestCoordsForResolve board = (sortOn (length . fst) [(fromLeft [] slot, c) | (c,slot) <- Map.toList (slots board), isLeft slot])

updateBoardStateSlot :: BoardState t v -> Coord -> v -> BoardState t v
updateBoardStateSlot board c value = board { slots = Map.insert c (Right value) (slots board)}

constrainUnsolvedSlot :: (Eq v) => (Coord, Slot v) -> BoardState t v -> Slot v
constrainUnsolvedSlot (c, slot) board = 
    let impossibleValues = findImpossibleValues c board
    in subtractFromSlot impossibleValues slot

constrainUnsolvedSlots :: (Eq v) => BoardState t v -> BoardState t v
constrainUnsolvedSlots board = 
    board {  slots = Map.mapWithKey (\c slot -> constrainUnsolvedSlot (c, slot) board) (slots board) }


-- Sort coordinates by lowest amount of possibilities
-- Set the top coord to it's first value
-- Check if solveSudokuImpl can solve that
-- Set the top coord to the next possible value, check
-- etc.
-- If a solution is found, return it
-- If none are found, return []
    
