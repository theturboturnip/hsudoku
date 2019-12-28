{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module KillerSudoku where

import BoardTypes
import Sudoku

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Either
import Data.String

type KillerSudokuBoard = SudokuBoard
makeKillerSudokuBoard :: (Eq a) => Int -> Int -> [Maybe Int] -> [a] -> Map.Map a Int -> KillerSudokuBoard
makeKillerSudokuBoard size squareSize knownParts coordGroups groupTotals = 
    let ps = [1..size]
        boardType = SquareBoardType { size = size, possibleValues = ps }
        possibleWhenNotSet = Left ps
        groupCoords = zip coordGroups (validCoords boardType)
        coordConstraints = [KillerSudokuSetTotalConstraint coords target | (key, target) <- Map.toList groupTotals, let coords = map snd $ filter (\(k, c) -> k == key) groupCoords]
    in BoardState { boardType = boardType
                  , slots = Map.fromList $ zip (validCoords boardType) (map (\x -> if (isNothing x) then possibleWhenNotSet else Right $ fromJust x) knownParts)
                  , constraints = map makeErasedConstraint coordConstraints ++ [
                        makeErasedConstraint $ SudokuSquaresConstraint squareSize, 
                        makeErasedConstraint $ SudokuHorizontalConstraint,
                        makeErasedConstraint $ SudokuVerticalConstraint
                        ]
                  }
                  
                  
-- Say a group has 4 items expecting X, with 2 slots (s1, s2) already filled in
-- The possibilities for the other items are equivalent to the possiblites for (4 - 2) items adding up to (X - s1 - s2)
                  
sumPossWithRepeats :: Int -> Int -> [Int]
sumPossWithRepeats target slotCount
    | target > slotCount = [1..(target - slotCount + 1)]
    | target == slotCount = [1]
    | otherwise = []
    
                  
data KillerSudokuSetTotalConstraint t v = KillerSudokuSetTotalConstraint [Coord] v
    deriving (Show)
instance ErasableConstraint KillerSudokuSetTotalConstraint t Int where
    erasedFunc (KillerSudokuSetTotalConstraint coords target) c board = 
        if elem c coords then
            let solvedValues = getSolvedSlotValues (coords \\ [c]) board
                currentSum = sum solvedValues
                remainingTarget = target - currentSum
            in Allow $ sumPossWithRepeats remainingTarget (length coords - length solvedValues) 
        else Unchanged
