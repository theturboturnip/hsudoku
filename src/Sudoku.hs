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
    let boardType = SquareBoardType { size = size }
        possibleWhenNotSet = Left [1..size]
    in BoardState { boardType = boardType
                  , slots = Map.fromList $ zip (validCoords boardType) (map (\x -> if (isNothing x) then possibleWhenNotSet else Right $ fromJust x) knownParts)
                  , constraints = [sudokuSquaresConstraint squareSize, sudokuHorizontalConstraint, sudokuVerticalConstraint]
                  }
 
squareCoords :: Coord -> Int -> [Coord]
squareCoords (ox,oy) size = [(x,y) | y <- [oy..oy + size - 1], x <- [ox..ox + size - 1]]

sudokuSquaresConstraint :: Int -> Constraint SquareBoardType Int
sudokuSquaresConstraint squareSize c board = 
    let (x,y) = c
        squareOrigin = (x - x `mod` squareSize, y - y `mod` squareSize)
    -- If the slot is a Left, it should not be allowed to have any of the values in the line
    in getSolvedSlotValues ((squareCoords squareOrigin squareSize) \\ [c]) board
    
sudokuVerticalConstraint :: Constraint SquareBoardType Int
sudokuVerticalConstraint c board = 
    let (x,y) = c
        s = size (boardType board)
        csInDir = [(x,y') | y' <- [0..s - 1], y /= y']
    -- If the slot is a Left, it should not be allowed to have any of the values in the line
    in getSolvedSlotValues csInDir board
    
sudokuHorizontalConstraint :: Constraint SquareBoardType Int
sudokuHorizontalConstraint c board = 
    let (x,y) = c
        s = size (boardType board)
        csInDir = [(x',y) | x' <- [0..s - 1], x' /= x]
    in getSolvedSlotValues csInDir board
