module Solvers.DFS where

import BoardTypes

import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map as Map

solveBoard :: (SlotValue v) => BoardState t v -> [BoardState t v]
solveBoard s = let s' = constrainUnsolvedSlots s in
                    solveBoardImpl (sortBestCoordsForResolve s') s'
                    
-- Takes a solution and a list of coords sorted by least possibilities
solveBoardImpl :: (SlotValue v) => [([v], Coord)] -> BoardState t v -> [BoardState t v]
solveBoardImpl [] board = if (isSolved board && isValidlySolved board) then [board] else []
solveBoardImpl ((ps, c):cs) board = -- ps = stack of possible values, c = cood
    let sWithPs = [constrainUnsolvedSlots $ updateBoardStateSlot board c p | p <- ps]
    in foldr ((++) . (\x -> solveBoardImpl (sortBestCoordsForResolve x) x)) [] sWithPs
                    
sortBestCoordsForResolve :: BoardState t v -> [([v], Coord)]
sortBestCoordsForResolve board = (sortOn (length . fst) [(fromLeft [] slot, c) | (c,slot) <- Map.toList (slots board), isLeft slot])

updateBoardStateSlot :: BoardState t v -> Coord -> v -> BoardState t v
updateBoardStateSlot board c value = board { slots = Map.insert c (Right value) (slots board)}

constrainUnsolvedSlot :: (SlotValue v) => (Coord, Slot v) -> BoardState t v -> Slot v
constrainUnsolvedSlot (c, slot) board = findConstrainedSlot c board

constrainUnsolvedSlots :: (SlotValue v) => BoardState t v -> BoardState t v
constrainUnsolvedSlots board = 
    board {  slots = Map.mapWithKey (\c slot -> constrainUnsolvedSlot (c, slot) board) (slots board) }
