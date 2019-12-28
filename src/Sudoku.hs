{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Sudoku where

import BoardTypes

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Either
import Data.String

type SudokuBoard = BoardState SquareBoardType Int
makeSudokuBoard :: Int -> Int -> [Maybe Int] -> SudokuBoard
makeSudokuBoard size squareSize knownParts = 
    let ps = [1..size]
        boardType = SquareBoardType { size = size, possibleValues = ps }
        possibleWhenNotSet = Left ps
    in BoardState { boardType = boardType
                  , slots = Map.fromList $ zip (validCoords boardType) (map (\x -> if (isNothing x) then possibleWhenNotSet else Right $ fromJust x) knownParts)
                  , constraints = [
                        makeErasedConstraint $ SudokuSquaresConstraint squareSize, 
                        makeErasedConstraint $ SudokuHorizontalConstraint,
                        makeErasedConstraint $ SudokuVerticalConstraint
                        ]
                  }
 
squareCoords :: Coord -> Int -> [Coord]
squareCoords (ox,oy) size = [(x,y) | y <- [oy..oy + size - 1], x <- [ox..ox + size - 1]]

data SudokuSquaresConstraint t v = SudokuSquaresConstraint Int
    deriving Show
instance (SlotValue v) => ErasableConstraint SudokuSquaresConstraint SquareBoardType v where
    erasedFunc (SudokuSquaresConstraint squareSize) c board = 
        let (x,y) = c
            squareOrigin = (x - x `mod` squareSize, y - y `mod` squareSize)
        -- If the slot is a Left, it should not be allowed to have any of the values in the line
        in Disallow $ getSolvedSlotValues ((squareCoords squareOrigin squareSize) \\ [c]) board
    
data SudokuLineConstraint t v = SudokuVerticalConstraint | SudokuHorizontalConstraint
    deriving Show
instance (SlotValue v) => ErasableConstraint SudokuLineConstraint SquareBoardType v where
    erasedFunc SudokuVerticalConstraint c board = 
        let (x,y) = c
            s = size (boardType board)
            csInDir = [(x,y') | y' <- [0..s - 1], y /= y']
        -- If the slot is a Left, it should not be allowed to have any of the values in the line
        in Disallow $ getSolvedSlotValues csInDir board
        
    erasedFunc SudokuHorizontalConstraint c board = 
        let (x,y) = c
            s = size (boardType board)
            csInDir = [(x',y) | x' <- [0..s - 1], x' /= x]
        -- If the slot is a Left, it should not be allowed to have any of the values in the line
        in Disallow $ getSolvedSlotValues csInDir board
