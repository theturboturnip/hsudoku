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
makeKillerSudokuBoardRepeats :: (Eq a) => Int -> Int -> [Maybe Int] -> [a] -> Map.Map a Int -> KillerSudokuBoard
makeKillerSudokuBoardRepeats size squareSize knownParts coordGroups groupTotals = 
    let ps = [1..size]
        boardType = SquareBoardType { size = size, possibleValues = ps }
        possibleWhenNotSet = Left ps
        groupCoords = zip coordGroups (validCoords boardType)
        coordConstraints = [KillerSudokuSetRepeatsConstraint coords target | (key, target) <- Map.toList groupTotals, let coords = map snd $ filter (\(k, c) -> k == key) groupCoords]
    in BoardState { boardType = boardType
                  , slots = Map.fromList $ zip (validCoords boardType) (map (\x -> if (isNothing x) then possibleWhenNotSet else Right $ fromJust x) knownParts)
                  , constraints = map makeErasedConstraint coordConstraints ++ [
                        makeErasedConstraint $ SudokuSquaresConstraint squareSize, 
                        makeErasedConstraint $ SudokuHorizontalConstraint,
                        makeErasedConstraint $ SudokuVerticalConstraint
                        ]
                  }
       
makeKillerSudokuBoard :: (Eq a) => Int -> Int -> [Maybe Int] -> [a] -> Map.Map a Int -> KillerSudokuBoard
makeKillerSudokuBoard size squareSize knownParts coordGroups groupTotals = 
    let ps = [1..size]
        boardType = SquareBoardType { size = size, possibleValues = ps }
        possibleWhenNotSet = Left ps
        groupCoords = zip coordGroups (validCoords boardType)
        coordConstraints = [KillerSudokuSetUniqueConstraint coords target 1 size | (key, target) <- Map.toList groupTotals, let coords = map snd $ filter (\(k, c) -> k == key) groupCoords]
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
    
{-maxForNChoices :: [Int] -> Int -> [Int]
maxForNChoices ps n = take n $ reverse $ sort $ ps

minForNChoices :: [Int] -> Int -> [Int]
minForNChoices ps n = take n $ sort ps-}
    
-- This might be solvable recursively?
sumPossUnique :: Int -> Int -> Int -> Int -> [Int]
sumPossUnique target slotCount minVal maxVal = 
    let smallestNextSum = sum [minVal..min (minVal + slotCount - 1 - 1) (maxVal - 1)]
        largestNextSum  = sum [max (minVal + 1) (maxVal - slotCount + 1 + 1)..maxVal]
        upperBound = reverse [x | x <- [minVal..maxVal], x + smallestNextSum <= target]
        lowerBound = [x | x <- [minVal..maxVal], x + largestNextSum  >= target]
    in if upperBound == [] || lowerBound == [] then [] else [head lowerBound..head upperBound] -- TODO: If the number is even, you can't have the middle one. Probably negligible
        
        
    {-| slotCount == 1 = if target `elem` ps then [target] else []
    | target > sum [maxVal..(maxVal - slotCount + 1)] = []
    | target < sum [minVal..(minVal + slotCount - 1)] = [] -- Shouldn't happen in practice, because -}
data KillerSudokuSetTotalConstraint t v = KillerSudokuSetRepeatsConstraint [Coord] v | KillerSudokuSetUniqueConstraint [Coord] v v v
    deriving (Show)
instance ErasableConstraint KillerSudokuSetTotalConstraint t Int where
    erasedFunc (KillerSudokuSetRepeatsConstraint coords target) c board = 
        if elem c coords then
            let solvedValues = getSolvedSlotValues (coords \\ [c]) board
                currentSum = sum solvedValues
                remainingTarget = target - currentSum
            in Allow $ sumPossWithRepeats remainingTarget (length coords - length solvedValues) 
        else Unchanged
        
    erasedFunc (KillerSudokuSetUniqueConstraint coords target minVal maxVal) c board = 
        if elem c coords then
            let solvedValues = getSolvedSlotValues (coords \\ [c]) board
                currentSum = sum solvedValues
                remainingTarget = target - currentSum
            in Allow $ sumPossUnique remainingTarget (length coords - length solvedValues) minVal maxVal
        else Unchanged
