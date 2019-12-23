module Solver where

import Data.Either
import Data.Maybe
import Data.List
import Lib

solvedValsAtIndices :: PartialSolution -> [Coord] -> [Int]
solvedValsAtIndices s cs = foldr addSolvedVal [] cs
    where 
        addSolvedVal :: Coord -> [Int] -> [Int]
        addSolvedVal c xs = case (indexSoln s c) of
                                Just (Right x) -> x : xs
                                _ -> xs
        
        
-- Find the coordinates on the board in the specified direction, append to final argument
coordsInDirection :: PartialSolution -> Coord -> Coord -> [Coord] -> [Coord]
coordsInDirection s (x, y) (dx, dy) cs = 
        if (x' < 0 || y' < 0) then cs
        else if x' >= (size s) || y' >= (size s) then cs
        else coordsInDirection s c' (dx, dy) (c' : cs)
    where x' = x + dx
          y' = y + dy
          c' = (x', y')
        
coordsInSquare :: PartialSolution -> Coord -> [Coord] -> [Coord]
coordsInSquare s (x, y) xs = 
    let sqS = squareSize s
        sqX = x - (x `mod` sqS)
        sqY = y - (y `mod` sqS)
    in [(x', y') | x' <- [sqX..sqX + sqS - 1], y' <- [sqY..sqY + sqS - 1], (x',y') /= (x,y)] ++ xs
        
-- Find all coordinates on the board that constrain the given coordinate
coordsConstrainingCoord :: PartialSolution -> Coord -> [Coord]
coordsConstrainingCoord s c =
    coordsInSquare s c $ foldr (coordsInDirection s c) [] [(1, 0), (0, 1), (-1, 0), (0, -1)]
    
impossibleValuesForCoord :: PartialSolution -> Coord -> [Int]
impossibleValuesForCoord s c = solvedValsAtIndices s $ coordsConstrainingCoord s c

constrainUnsolvedSlot :: PartialSolution -> Coord -> Slot
constrainUnsolvedSlot s c = 
    let slot = fromJust $ indexSoln s c 
        impossibleValues = impossibleValuesForCoord s c in
        case slot of
            Right _ -> slot
            Left xs -> let newValues = [x | x <- xs, not $ elem x impossibleValues] in
                           if (length newValues) == 1 then Right (head newValues) else Left newValues

constrainUnsolvedSlots :: PartialSolution -> PartialSolution
constrainUnsolvedSlots s = 
    s {  slots = [constrainUnsolvedSlot s (x,y) | y <- [0..si - 1], x <- [0..si - 1]] }
    where si = size s
          
                    
isValidlySolved :: PartialSolution -> Bool
isValidlySolved s = all hasPossibleValue [(x,y) | x <- [0..si - 1], y <- [0..si - 1]]
    where 
        si = size s
        hasPossibleValue c = 
            let slot = fromJust $ indexSoln s c in
            case slot of
                Right v -> not $ elem v $ impossibleValuesForCoord s c
                Left _ -> False

isSolved :: PartialSolution -> Bool
isSolved ss = all isRight (slots ss)

isSolvable :: PartialSolution -> Bool
isSolvable ss = all notEmpty (slots ss)
    where notEmpty s = case s of
            Left [] -> False
            _ -> True
            
