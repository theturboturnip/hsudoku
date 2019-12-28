module Solvers.DFS where

import BoardTypes

import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

type CoordConnections = Map.Map Coord ([Coord])

buildConnectionSet :: Coord -> BoardState t v -> ([Coord])
buildConnectionSet c board = Set.toList $ Set.fromList $ concat [(coordConnectionFunc constr) c board| constr <- constraints board]

solveBoard :: (SlotValue v, BoardType t v) => BoardState t v -> [BoardState t v]
solveBoard s = let s' = constrainUnsolvedSlots s (validCoords $ boardType s)
                   coordConnections = Map.fromList [(c, buildConnectionSet c s) | c <- validCoords $ boardType s]
               in solveBoardImpl coordConnections (sortBestCoordsForResolve s') s'
                    
-- Takes a solution and a list of coords sorted by least possibilities
solveBoardImpl :: (SlotValue v, BoardType t v) => CoordConnections -> [([v], Coord)] -> BoardState t v -> [BoardState t v]
solveBoardImpl _ [] board = if (isSolved board && isValidlySolved board) then [board] else []
solveBoardImpl coordConnections ((ps, c):cs) board = -- ps = stack of possible values, c = cood
    let sWithPs = [constrainUnsolvedSlots (updateBoardStateSlot board c p) (coordConnections Map.! c) | p <- ps]
    in foldr ((++) . (\x -> solveBoardImpl coordConnections (sortBestCoordsForResolve x) x)) [] sWithPs
                    
sortBestCoordsForResolve :: BoardState t v -> [([v], Coord)]
sortBestCoordsForResolve board = (sortOn (length . fst) [(fromLeft [] slot, c) | (c,slot) <- Map.toList (slots board), isLeft slot])

updateBoardStateSlot :: BoardState t v -> Coord -> v -> BoardState t v
updateBoardStateSlot board c value = board { slots = Map.insert c (Right value) (slots board)}

constrainUnsolvedSlot :: (SlotValue v) => Coord -> BoardState t v -> Slot v
constrainUnsolvedSlot c board = findConstrainedSlot c board

constrainUnsolvedSlots :: (SlotValue v, BoardType t v) => BoardState t v -> [Coord] -> BoardState t v
constrainUnsolvedSlots board cs = 
    board { slots = foldr (\c oldSlots -> Map.update (const $ Just $ constrainUnsolvedSlot c board) c oldSlots) (slots board) cs } 
    --board {  slots = Map.mapWithKey (\c slot -> constrainUnsolvedSlot (c, slot) board) (slots board) }
