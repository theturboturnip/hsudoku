{-# LANGUAGE TypeOperators, MultiParamTypeClasses, GADTs, FlexibleInstances, UndecidableInstances #-}
module Solutions where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Either
import Data.String

type Coord = (Int, Int)

-- TODO: Introduce SlotValue typeclass as an alias for Eq 
{-class SlotValue a
instance SlotValue a => Eq a where
    (==) = 

    instance (Eq a) => SlotValue a -}

--data SlotValue a = forall. a (Eq a) => (SlotValue a)

-- TODO: Constrain Slot to only take SlotValue a
type Slot a = Either [a] a -- Left implies unsolved slot with list of possible values, Right is solved to a single value.

    
class (Show a) => BoardType a where
    --type Value a; -- Declare a type Value which depends on what a is.
    validCoords :: a -> [Coord]
    
--class (BoardType t, SlotValue v) => ConstraintC a t v where 
--    applyConstraintC :: a -> Coord -> BoardState t v -> [v] 

-- returns list of values the slot cannot have
type Constraint t v = Coord -> BoardState t v -> [v]

subtractFromSlot :: (Eq v) => [v] -> Slot v -> Slot v
subtractFromSlot _ (Right v) = Right v
subtractFromSlot vs (Left oldVs) = case (oldVs \\ vs) of
                                        [v] -> Right v
                                        newVs -> Left newVs

data BoardState t v = 
    (BoardType t, Eq v) => BoardState 
    { boardType :: t
    , slots :: Map.Map Coord (Slot v)
    , constraints :: [Constraint t v]
    }

instance (BoardType t, Show t, Eq v, Show v) => Show (BoardState t v) where
    show board = (show $ boardType board) ++ '\n':(show $ slots board)

    
findImpossibleValues :: (Eq v) => Coord -> BoardState t v -> [v]
findImpossibleValues c board = nub $ foldr (\constr vs -> (constr c board) ++ vs) [] (constraints board)
    
{-makeBoardState :: (BoardType t, SlotValue v) => t -> [Constraint t v] -> BoardState t v
makeBoardState boardType constraints = BoardState 
    { boardType = boardType
    , slots = Map.fromList [(c, ]
    }-}
    
getSlotValue :: Coord -> BoardState t v -> Maybe (Slot v)
getSlotValue c board = (slots board) Map.!? c 

getSlotValues :: [Coord] -> BoardState t v -> [Maybe (Slot v)]
getSlotValues cs board = [(slots board) Map.!? c | c <- cs]

fromRightOrError (Right x) = x
fromRightOrError leftVal = error ("fromRightOrError")

getSolvedSlotValues :: [Coord] -> BoardState t v -> [v]
getSolvedSlotValues cs board = [fromRightOrError sJ | s <- getSlotValues cs board, let sJ = fromJust s, isRight sJ]

isValidlySolved :: (Eq v) => BoardState t v -> Bool
isValidlySolved board = Map.foldrWithKey (\c slot solved -> solved && hasPossibleValue c slot) True (slots board)
    where 
        hasPossibleValue c slot = 
            case slot of
                Right v -> not $ elem v $ findImpossibleValues c board
                Left _ -> False

isSolved :: BoardState t v -> Bool
isSolved board = all isRight (slots board)

isSolvable :: BoardState t v -> Bool
isSolvable board = all notEmpty (slots board)
    where notEmpty s = case s of
            Left [] -> False
            _ -> True
            

data SquareBoardType = SquareBoardType { size :: Int }
    deriving (Show)
instance BoardType SquareBoardType where
    validCoords SquareBoardType { size = s } = [(x,y) | y <- [0..s-1], x <- [0..s-1]]
    
showSquareContents :: (Show v) => BoardState SquareBoardType v -> [String] 
showSquareContents board = [showLine y | y <- [0..(size $ boardType board) - 1]]
    where showLine y = unwords [showVal v | x <- [0..(size $ boardType board) - 1], let v = fromJust $ getSlotValue (x,y) board]
          showVal v = case v of 
                           Right x -> (show x)
                           Left _ -> "x"
    
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

--makeSudokuBoard :: Int -> Int -> BoardState SquareBoardType Int
--makeSudokuBoard 

{-

data BoardData = BoardData {
    validCoords :: [Coord],
}        

data PartialSolution = PartialSolution { size :: Int, squareSize :: Int, slots :: [Slot] }
    deriving (Show)

-}
    

{-indexSoln :: PartialSolution -> Coord -> Maybe Slot
indexSoln ss (x, y) = 
    if i >= (size ss * size ss) || i < 0 then
        Nothing
    else
        Just $ (slots ss) !! i
    where i = x + (y * (size ss))
          
setIndexTo :: Int -> a -> [a] -> [a]
setIndexTo i a as = let (start,lost:end) = splitAt i as in
                        start ++ (a : end)

setCoordSlotValue :: PartialSolution -> Coord -> Int -> PartialSolution
setCoordSlotValue s (x,y) n = s { slots = setIndexTo i (Right n) (slots s) } 
    where i = x + (y * (size s))
    -}
