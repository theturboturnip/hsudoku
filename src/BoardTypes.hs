{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module BoardTypes where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Either
import Data.String

type Coord = (Int, Int)

-- TODO: Introduce SlotValue typeclass as an alias for Eq
-- TODO: Constrain Slot to only take SlotValue a
type Slot a = Either [a] a -- Left implies unsolved slot with list of possible values, Right is solved to a single value.

    
class (Show (a v), Eq v) => BoardType (a :: * -> *) v where
    --type Value a; -- Declare a type Value which depends on what a is.
    validCoords :: (a v) -> [Coord]
    initialPossibilities :: (a v) -> [v]
    
-- returns list of values the slot cannot have
--type Constraint t v = Coord -> BoardState t v -> [v]
type ConstraintFunc (t :: * -> *) v = Coord -> BoardState t v-> [v]

data ErasedConstraint (t :: * -> *) v = Constraint { constraintName :: String, constraintFunc :: (ConstraintFunc t v) }
instance (BoardType t v, Eq v) => Show (ErasedConstraint t v) where
    show = constraintName
    
class (Show (a (t v) v), Eq v) => ErasableConstraint (a :: * -> * -> *) (t :: * -> *) v where
    erasedName :: (a (t v) v) -> String
    erasedName = show
    
    erasedFunc :: (BoardType t v) => (a (t v) v) -> ConstraintFunc t v
    
{-instance (Show (a (t v) v), Eq v) => Show (ErasedConstraint t v) where
show = erasedName-}
    
makeErasedConstraint :: (BoardType t v, ErasableConstraint a t v) => (a (t v) v) -> ErasedConstraint t v
makeErasedConstraint e = Constraint (erasedName e) (erasedFunc e)

subtractFromSlot :: (Eq v) => [v] -> Slot v -> Slot v
subtractFromSlot _ (Right v) = Right v
subtractFromSlot vs (Left oldVs) = case (oldVs \\ vs) of
                                        [v] -> Right v
                                        newVs -> Left newVs

data BoardState (t :: * -> *) v = 
    (BoardType t v, Eq v) => BoardState 
    { boardType :: (t v)
    , slots :: Map.Map Coord (Slot v)
    , constraints :: [ErasedConstraint t v]
    }

instance (BoardType t v, Show (t v), Eq v, Show v) => Show (BoardState t v) where
    show board = (show $ boardType board) ++ '\n':(show $ slots board) ++ '\n':(show $ constraints board)

    
findImpossibleValues :: (Eq v) => Coord -> BoardState t v -> [v]
findImpossibleValues c board = nub $ foldr (\constr vs -> (constr c board) ++ vs) [] (map constraintFunc $ constraints board)
    
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
            

-- TODO: If we include the Show constraint in the data type we can't derive Show without "a standalone deriving declaration"
data SquareBoardType v = {-(Show v) => -}SquareBoardType { size :: Int, possibleValues :: [v] }
    deriving (Show)
instance (Show v, Eq v) => BoardType SquareBoardType v where
    validCoords SquareBoardType { size = s } = [(x,y) | y <- [0..s-1], x <- [0..s-1]]
    
    initialPossibilities = possibleValues
    
showSquareContents :: (Show v) => BoardState SquareBoardType v -> [String] 
showSquareContents board = [showLine y | y <- [0..(size $ boardType board) - 1]]
    where showLine y = unwords [showVal v | x <- [0..(size $ boardType board) - 1], let v = fromJust $ getSlotValue (x,y) board]
          showVal v = case v of 
                           Right x -> (show x)
                           Left _ -> "x"
    
