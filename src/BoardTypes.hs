{-# LANGUAGE TypeOperators, MultiParamTypeClasses, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module BoardTypes where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Either
import Data.String
import Data.Bits

type Coord = (Int, Int)

class (Show a, Eq a, Enum a) => SlotValue a
instance SlotValue Int

type SlotBitfieldHolder = Word
--data SlotValues a = (SlotValue a) => SlotValues (FiniteBits SlotBitfieldHolder) Int
type SlotValues = SlotBitfieldHolder
slotBitfieldForOne :: (SlotValue a) => a -> SlotValues
slotBitfieldForOne x = bit (fromEnum x)

slotValuesForOne :: (SlotValue a) => a -> SlotValues
slotValuesForOne = slotBitfieldForOne

lengthOfValues :: SlotValues -> Int
lengthOfValues = popCount

data Slot a = Unsolved SlotValues | Solved a deriving (Show, Eq)

listFromUnsolved :: (SlotValue a) => [a] -> Slot a -> [a]
listFromUnsolved xs (Solved _) = xs
listFromUnsolved _ (Unsolved bits) = bitsToList bits

fromSolved :: Slot a -> a
fromSolved (Solved x) = x
fromSolved _ = error ("fromSolved")

isSolved :: Slot a -> Bool
isSolved (Solved _) = True
isSolved _ = False

isUnsolved :: Slot a -> Bool
isUnsolved = not . isSolved

listToBits :: (SlotValue a) => [a] -> SlotValues
listToBits [] = zeroBits
listToBits (x:xs) = (bit $ fromEnum x) .|. listToBits xs

bitsToList :: (SlotValue a) => SlotValues -> [a]
bitsToList bits = [toEnum x | x <- [0..finiteBitSize bits], testBit bits x]

slotToList :: (SlotValue a) => Slot a -> [a]
slotToList (Solved x) = [x]
slotToList (Unsolved bits) = bitsToList bits

slotToBits :: (SlotValue a) => Slot a -> SlotValues
slotToBits (Unsolved xs) = xs
slotToBits (Solved x) = slotBitfieldForOne x

bitsToSlot :: (SlotValue a) => SlotValues -> Slot a
bitsToSlot bits
    | lengthOfValues bits == 1 = Solved (toEnum $ countTrailingZeros bits)
    | otherwise = Unsolved bits
    
listToSlot :: (SlotValue a) => [a] -> Slot a
listToSlot = bitsToSlot . listToBits

data ConstraintResult a = Allow SlotValues | Exactly SlotValues | Disallow SlotValues | Unchanged
    deriving (Show, Eq)
    
applyConstraintResToSlot :: (SlotValue v) => ConstraintResult v -> Slot v -> Slot v
applyConstraintResToSlot Unchanged slot = slot
applyConstraintResToSlot (Disallow vs) slot = 
    let oldVs = slotToBits slot
        newVs = oldVs .&. (complement vs)
    in (bitsToSlot newVs)
applyConstraintResToSlot (Exactly vs) slot =
    let oldVs = slotToBits slot 
    in 
        if (lengthOfValues $ vs .&. oldVs) == (lengthOfValues vs) then
            (bitsToSlot vs)
        else error $ "Exact constraint " ++ (show vs) ++ " weaker than previous " ++ (show oldVs)
applyConstraintResToSlot (Allow vs) slot = 
    let oldVs = slotToBits slot
        newVs = (vs .&. oldVs)
    in (bitsToSlot newVs)
    
    
    
class (Show (a v), SlotValue v) => BoardType (a :: * -> *) v where
    --type Value a; -- Declare a type Value which depends on what a is.
    validCoords :: (a v) -> [Coord]
    initialPossibilities :: (a v) -> SlotValues
    bounds :: (a v) -> (Coord, Coord)
    
-- returns list of values the slot cannot have
--type Constraint t v = Coord -> BoardState t v -> [v]
type ConstraintFunc (t :: * -> *) v = Coord -> BoardState t v -> ConstraintResult v

data ErasedConstraint (t :: * -> *) v = Constraint { constraintName :: String, constraintFunc :: (ConstraintFunc t v), coordConnectionFunc :: Coord -> BoardState t v -> [Coord] }
instance (BoardType t v, SlotValue v) => Show (ErasedConstraint t v) where
    show = constraintName
    
class (Show (a (t v) v), SlotValue v) => ErasableConstraint (a :: * -> * -> *) (t :: * -> *) v where
    erasedName :: (a (t v) v) -> String
    erasedName = show
    
    erasedFunc :: (BoardType t v) => (a (t v) v) -> ConstraintFunc t v
    
    erasedConnectionFunc :: (a (t v) v) -> Coord -> BoardState t v -> [Coord]
    
{-instance (Show (a (t v) v), SlotValue v) => Show (ErasedConstraint t v) where
show = erasedName-}
    
makeErasedConstraint :: (BoardType t v, ErasableConstraint a t v) => (a (t v) v) -> ErasedConstraint t v
makeErasedConstraint e = Constraint (erasedName e) (erasedFunc e) (erasedConnectionFunc e)


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
    
findConstrainedSlot :: (SlotValue v) => Coord -> BoardState t v -> (Slot v, Bool)
findConstrainedSlot c board = 
    let slot = fromJust $ getSlotValue c board
        cs = findConstraintResults c board
        newSlot = foldr applyConstraintResToSlot slot cs
    in (newSlot, newSlot == slot)
    
findPossibleValues :: (SlotValue v) => Coord -> BoardState t v -> [v]
findPossibleValues c board = slotToList $ fst (findConstrainedSlot c board)
    
getSlotValue :: Coord -> BoardState t v -> Maybe (Slot v)
getSlotValue c board = (slots board) Map.!? c 

getSlotValues :: [Coord] -> BoardState t v -> [Maybe (Slot v)]
getSlotValues cs board = [(slots board) Map.!? c | c <- cs]

getSolvedSlotValues :: (SlotValue v) => [Coord] -> BoardState t v -> SlotValues
getSolvedSlotValues cs board = listToBits [fromSolved sJ | s <- getSlotValues cs board, let sJ = fromJust s, isSolved sJ]

isBoardValidlySolved :: (SlotValue v) => BoardState t v -> Bool
isBoardValidlySolved board = Map.foldrWithKey (\c slot solved -> solved && hasPossibleValue c slot) True (slots board)
    where 
        hasPossibleValue c slot = 
            case slot of
                Solved v -> elem v $ findPossibleValues c board
                _ -> False

isBoardSolved :: BoardState t v -> Bool
isBoardSolved board = all isSolved (slots board)

isBoardSolvable :: BoardState t v -> Bool
isBoardSolvable board = all notEmpty (slots board)
    where notEmpty s = case s of
            Unsolved zeroBits -> False
            _ -> True
            
squareCoords :: Int -> [Coord]
squareCoords s = [(x,y) | y <- [0..s-1], x <- [0..s-1]]

-- TODO: If we include the Show constraint in the data type we can't derive Show without "a standalone deriving declaration"
data SquareBoardType v = {-(Show v) => -}SquareBoardType { size :: Int, possibleValues :: [v] }
    deriving (Show)
instance (SlotValue v) => BoardType SquareBoardType v where
    validCoords SquareBoardType { size = s } = squareCoords s
    
    bounds SquareBoardType { size = s } = ((0,0), (s-1,s-1))
    
    initialPossibilities = listToBits . possibleValues
    
showSquareContents :: (SlotValue v) => BoardState SquareBoardType v -> String
showSquareContents = showBoardContents
    
showBoardContents :: (SlotValue v, BoardType t v) => BoardState t v -> String
showBoardContents board = 
    let ((xMin, yMin), (xMax, yMax)) = bounds $ boardType board
    in unlines [showLine [xMin..xMax] y | y <- [yMin..yMax]]
    where showLine xs y = unwords [showVal v | x <- xs, let v = getSlotValue (x,y) board]
          showVal v = case v of 
                        Nothing -> " "
                        Just (Solved x) -> (show x)
                        Just (_) -> "?"
