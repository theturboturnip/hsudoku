{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Kakuro where

import BoardTypes
import KillerSudoku

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.String

type KakuroBoard = BoardState KakuroBoardType Int

data ReadCell = Unused | Unknown | Director { downwardsSum :: Maybe Int, rightSum :: Maybe Int }
    deriving (Show, Eq)

-- TODO: If kakuro can only handle ints, it shouldn't have to be generic in v
data KakuroBoardType v = KakuroBoardType { unknownCoords :: [Coord], bottomRight :: Coord }
    deriving (Show)
instance BoardType KakuroBoardType Int where
    validCoords = unknownCoords
    initialPossibilities = const [1..9]
    bounds board = ((0,0), bottomRight board)
    
type KakuroRunTotalConstraint = KillerSudokuSetTotalConstraint (KakuroBoardType Int) Int

kakuroBoardFromSquare :: Int -> [ReadCell] -> KakuroBoard
kakuroBoardFromSquare size cells = 
    let indexer = index2D cells (size, size) 
        unknownCoords = [coord | coord <- squareCoords size, indexer coord == Unknown]
        constraints = foldr (addConstraintsForDirector indexer (size,size)) [] (squareCoords size)
        boardType = KakuroBoardType { unknownCoords = unknownCoords, bottomRight = (size,size) }
    in BoardState { boardType = boardType
                  , slots = Map.fromList $ zip unknownCoords (repeat $ Left $ initialPossibilities boardType)
                  , constraints = map makeErasedConstraint constraints
                  }
    
index2D :: [a] -> (Int, Int) -> Coord -> a
index2D xs (w, h) (x, y)
    | x >= w || y >= h || x < 0 || y < 0 = error $ "Out-of-bounds access to " ++ (show (x,y)) ++ " of array size " ++ (show(w,h))
    | otherwise = xs !! (x + y * h)
    
takeCoordUntilNotUnknown :: [(Coord, ReadCell)] -> [Coord]
takeCoordUntilNotUnknown xs = map fst $ takeWhile (\(c, r) -> case r of 
                                                                   Unknown -> True
                                                                   _ -> False) xs
    
data DirectorGroupDir = DirectorDown | DirectorRight
coordsInDirection :: Coord -> (Int, Int) -> DirectorGroupDir -> [Coord]
coordsInDirection (x,y) (w,h) DirectorDown = [(x,y') | y' <- [y + 1..h-1]]
coordsInDirection (x,y) (w,h) DirectorRight= [(x',y) | x' <- [x + 1..w-1]]

getDirectorGroup :: Coord -> (Int, Int) -> DirectorGroupDir -> (Coord -> ReadCell) -> [Coord]
getDirectorGroup origin size dir indexer = takeCoordUntilNotUnknown [(c, indexer c) | c <- coordsInDirection origin size dir]

-- TODO: Ugh, this is messy 
addConstraintsForDirector :: (Coord -> ReadCell) -> (Int, Int) -> Coord -> [KakuroRunTotalConstraint] -> [KakuroRunTotalConstraint]
addConstraintsForDirector indexer size coord cs = 
    let directorCell = indexer coord
        downwardsConstraint = getDirectorCoords directorCell DirectorDown
        rightConstraint = getDirectorCoords directorCell DirectorRight
        csPlusDown = if isNothing downwardsConstraint then cs else (fromJust downwardsConstraint) : cs
        csPlusDownPlusRight = if isNothing rightConstraint then csPlusDown else (fromJust rightConstraint) : csPlusDown
    in csPlusDownPlusRight
    where getDirectorCoords :: ReadCell -> DirectorGroupDir -> Maybe KakuroRunTotalConstraint
          getDirectorCoords Director{downwardsSum = ds, rightSum = rs} dir =
              let groupSum = (case dir of
                                DirectorDown  -> ds
                                DirectorRight -> rs)
              in groupSum >>= (\s -> return $ KillerSudokuSetUniqueConstraint (getDirectorGroup coord size dir indexer) s 1 9)
          getDirectorCoords _ _ = Nothing
