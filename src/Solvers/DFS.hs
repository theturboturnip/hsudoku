module Solvers.DFS where

import BoardTypes

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

type CoordConnections = Map.Map Coord (Set.Set Coord)
type CoordStack v = [([v], Coord)]
data StepData t v = NeedsStep (BoardState t v, CoordConnections, CoordStack v) | Finished (BoardState t v)
    deriving (Show)

extractFinishedBoards :: [StepData t v] -> [BoardState t v]
extractFinishedBoards (x:xs) = case x of
                                  (Finished state) -> state:extractFinishedBoards xs
                                  _ -> extractFinishedBoards xs
extractFinishedBoards [] = []

buildConnectionSet :: (BoardType t v) => Coord -> BoardState t v -> (Set.Set Coord)
buildConnectionSet c board = Set.fromList $ (touchingConnections c board) \\ [c]

allConnections c board = validCoords $ boardType board
touchingConnections c board = Set.toList $ Set.fromList $ concat [(coordConnectionFunc constr) c board| constr <- constraints board]

validateBoard :: (SlotValue v, BoardType t v) => BoardState t v -> StepData t v
validateBoard s = let s' = constrainUnsolvedSlots s (Set.fromList $ validCoords $ boardType s) coordConnections
                      coordConnections = Map.fromList [(c, buildConnectionSet c s) | c <- validCoords $ boardType s]
                      coordStack = sortBestCoordsForResolve s'
                  in NeedsStep (s', coordConnections, coordStack)

solveBoard :: (SlotValue v, BoardType t v) => BoardState t v -> [BoardState t v]
solveBoard board = concat (map extractFinishedBoards $ boardTree board)
                  
boardTree :: (SlotValue v, BoardType t v) => BoardState t v -> [[StepData t v]]
boardTree board = let root = [validateBoard board]
                  in root : (nextSteps root)
    where nextSteps xs = 
            let next = concat (map stepSolveBoard xs)
            in next : nextSteps next

stepSolveBoard :: (SlotValue v, BoardType t v) => StepData t v -> [StepData t v]
stepSolveBoard (Finished _) = []
stepSolveBoard (NeedsStep (board, _, [])) = if (isBoardSolved board && isBoardValidlySolved board) then [Finished board] else []
stepSolveBoard (NeedsStep (board, coordConnections, ((ps, c):_))) = -- ps = stack of possible values, c = cood
    let sWithPs = [constrainUnsolvedSlots (updateBoardStateSlot board c p) (coordConnections Map.! c) coordConnections | p <- ps]
    in [NeedsStep (x, coordConnections, sortBestCoordsForResolve x) | x <- sWithPs]
--solveBoards ss = 
--               in solveBoardImpl coordConnections (sortBestCoordsForResolve s') s'
                    
-- Takes a solution and a list of coords sorted by least possibilities
{-solveBoardImpl :: (SlotValue v, BoardType t v) => CoordConnections -> CoordStack -> BoardState t v -> [BoardState t v]
solveBoardImpl _ [] board = if (isBoardSolved board && isBoardValidlySolved board) then [Finished board] else []
solveBoardImpl coordConnections ((ps, c):_) board = -- ps = stack of possible values, c = cood
    let sWithPs = [constrainUnsolvedSlots (updateBoardStateSlot board c p) (coordConnections Map.! c) coordConnections | p <- ps]
    in foldr ((++) . (\x -> solveBoardImpl coordConnections (sortBestCoordsForResolve x) x)) [] sWithPs-}   
                    
sortBestCoordsForResolve :: (SlotValue v) => BoardState t v -> CoordStack v
sortBestCoordsForResolve board = (sortOn (length . fst) [(listFromUnsolved [] slot, c) | (c,slot) <- Map.toList (slots board), isUnsolved slot])

updateBoardStateSlot :: BoardState t v -> Coord -> v -> BoardState t v
updateBoardStateSlot board c value = board { slots = Map.insert c (Solved value) (slots board)}

constrainUnsolvedSlot :: (SlotValue v) => Coord -> BoardState t v -> (Slot v, Bool)
constrainUnsolvedSlot c board = findConstrainedSlot c board

constrainUnsolvedSlots :: (SlotValue v, BoardType t v) => BoardState t v -> Set.Set Coord -> CoordConnections -> BoardState t v
constrainUnsolvedSlots board cs conns = constrainUnsolvedSlotsImpl board cs conns Set.empty 0
    where constrainUnsolvedSlotsImpl board cs conns touchedCoords n = if not $ Set.null connectedChangedCs 
                                                        then if n > 500 
                                                                then error "Maximum recursion limit on constrainUnsolvedSlots reached"
                                                                else constrainUnsolvedSlotsImpl newBoard connectedChangedCs conns (Set.union touchedCoords connectedChangedCs) (n+1)
                                                                else newBoard
                                                        where oldSlots = slots board
                                                              reconstrainCoord c (currSlots, changedCs) = let (constrainedSlot, hasChanged) = constrainUnsolvedSlot c board
                                                                                                              newChangedCs = if hasChanged then c:changedCs else changedCs
                                                                                                          in (Map.update (const $ Just $ constrainedSlot) c currSlots, newChangedCs)
                                                              (newSlots, changedCs) = foldr reconstrainCoord (oldSlots, []) cs
                                                              connectedChangedCs = (foldr Set.union Set.empty (map (conns Map.!) changedCs)) Set.\\ touchedCoords
                                                              newBoard = board { slots = newSlots }
    --board {  slots = Map.mapWithKey (\c slot -> constrainUnsolvedSlot (c, slot) board) (slots board) }
