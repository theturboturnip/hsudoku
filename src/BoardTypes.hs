{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module BoardTypes where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Either
import Data.String

type Coord = (Int, Int)

class (Show a, Eq a) => SlotValue a
instance SlotValue Int

-- TODO: Introduce SlotValue typeclass as an alias for Eq
-- TODO: Constrain Slot to only take SlotValue a
type Slot a = Either [a] a -- Left implies unsolved slot with list of possible values, Right is solved to a single value.
slotToList :: Slot a -> [a]
slotToList (Left xs) = xs
slotToList (Right x) = [x]

listToSlot :: [a] -> Slot a
listToSlot [x] = Right x
listToSlot xs = Left xs
    
class (Show (a v), SlotValue v) => BoardType (a :: * -> *) v where
    --type Value a; -- Declare a type Value which depends on what a is.
    validCoords :: (a v) -> [Coord]
    initialPossibilities :: (a v) -> [v]
    
data ConstraintResult a = Allow [a] | Exactly [a] | Disallow [a] | Unchanged
    deriving (Show, Eq)
    
-- returns list of values the slot cannot have
--type Constraint t v = Coord -> BoardState t v -> [v]
type ConstraintFunc (t :: * -> *) v = Coord -> BoardState t v-> ConstraintResult v

data ErasedConstraint (t :: * -> *) v = Constraint { constraintName :: String, constraintFunc :: (ConstraintFunc t v) }
instance (BoardType t v, SlotValue v) => Show (ErasedConstraint t v) where
    show = constraintName
    
class (Show (a (t v) v), SlotValue v) => ErasableConstraint (a :: * -> * -> *) (t :: * -> *) v where
    erasedName :: (a (t v) v) -> String
    erasedName = show
    
    erasedFunc :: (BoardType t v) => (a (t v) v) -> ConstraintFunc t v
    
{-instance (Show (a (t v) v), SlotValue v) => Show (ErasedConstraint t v) where
show = erasedName-}
    
makeErasedConstraint :: (BoardType t v, ErasableConstraint a t v) => (a (t v) v) -> ErasedConstraint t v
makeErasedConstraint e = Constraint (erasedName e) (erasedFunc e)

applyConstraintResToSlot :: (SlotValue v) => ConstraintResult v -> Slot v -> Slot v
applyConstraintResToSlot Unchanged slot = slot
applyConstraintResToSlot (Disallow vs) slot = 
    let oldVs = slotToList slot 
    in listToSlot $ oldVs \\ vs
applyConstraintResToSlot (Exactly vs) slot =
    let oldVs = slotToList slot 
    in 
        if (length $ intersect vs oldVs) == (length vs) then
            listToSlot vs
        else error $ "Exact constraint " ++ (show vs) ++ " weaker than previous " ++ (show oldVs)
applyConstraintResToSlot (Allow vs) slot = 
    let oldVs = slotToList slot
    in listToSlot (intersect vs oldVs)
    
data BoardState (t :: * -> *) v = 
    (BoardType t v, SlotValue v) => BoardState 
    { boardType :: (t v)
    , slots :: Map.Map Coord (Slot v)
    , constraints :: [ErasedConstraint t v]
    }

instance (BoardType t v, Show (t v), SlotValue v) => Show (BoardState t v) where
    show board = (show $ boardType board) ++ '\n':(show $ slots board) ++ '\n':(show $ constraints board)

    
findConstraintResults :: (SlotValue v) => Coord -> BoardState t v -> [ConstraintResult v]
findConstraintResults c board = foldr (\constr vs -> (constr c board) : vs) [] (map constraintFunc $ constraints board)
    
findConstrainedSlot :: (SlotValue v) => Coord -> BoardState t v -> Slot v
findConstrainedSlot c board = 
    let slot = fromJust $ getSlotValue c board
        cs = findConstraintResults c board
    in foldr applyConstraintResToSlot slot cs
    
findPossibleValues :: (SlotValue v) => Coord -> BoardState t v -> [v]
findPossibleValues c board = slotToList (findConstrainedSlot c board)
    
getSlotValue :: Coord -> BoardState t v -> Maybe (Slot v)
getSlotValue c board = (slots board) Map.!? c 

getSlotValues :: [Coord] -> BoardState t v -> [Maybe (Slot v)]
getSlotValues cs board = [(slots board) Map.!? c | c <- cs]

fromRightOrError (Right x) = x
fromRightOrError leftVal = error ("fromRightOrError")

getSolvedSlotValues :: [Coord] -> BoardState t v -> [v]
getSolvedSlotValues cs board = [fromRightOrError sJ | s <- getSlotValues cs board, let sJ = fromJust s, isRight sJ]

isValidlySolved :: (SlotValue v) => BoardState t v -> Bool
isValidlySolved board = Map.foldrWithKey (\c slot solved -> solved && hasPossibleValue c slot) True (slots board)
    where 
        hasPossibleValue c slot = 
            case slot of
                Right v -> elem v $ findPossibleValues c board
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
instance (SlotValue v) => BoardType SquareBoardType v where
    validCoords SquareBoardType { size = s } = [(x,y) | y <- [0..s-1], x <- [0..s-1]]
    
    initialPossibilities = possibleValues
    
showSquareContents :: (SlotValue v) => BoardState SquareBoardType v -> [String] 
showSquareContents board = [showLine y | y <- [0..(size $ boardType board) - 1]]
    where showLine y = unwords [showVal v | x <- [0..(size $ boardType board) - 1], let v = fromJust $ getSlotValue (x,y) board]
          showVal v = case v of 
                           Right x -> (show x)
                           Left _ -> "x"
    
