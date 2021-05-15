module MancalaTypes where

data Mancala  = Mancala {
  firstSlot :: [Int]
, firstMancala :: Int
, secondSlot :: [Int]
, secondMancala :: Int
} deriving (Eq, Show)


data Player = First | Second deriving (Eq, Show)
type Slot = Int

check :: Slot -> [Int] -> String
check slot l | (l !! (slot - 1)) > 0 = "correct step"
             | otherwise = "0 Incorrect step"

data Out = Out String Mancala deriving (Eq, Show)
data Info = Info String Out deriving (Eq, Show)
type Sum = Int
type NextSlot = Int