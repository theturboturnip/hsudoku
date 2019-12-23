module Lib where

import Data.Either
import Data.Maybe
import Data.List

type Slot = Either [Int] Int -- Left implies unsolved slot with list of possible values, Right is solved to a single value.
type Coord = (Int, Int)

--data Slot = Solved Int | Unsolved [Int];


data PartialSolution = PartialSolution { size :: Int, squareSize :: Int, slots :: [Slot] }
    deriving (Show)
    

indexSoln :: PartialSolution -> Coord -> Maybe Slot
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
