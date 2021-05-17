module MancalaTypes where

data Mancala  = Mancala {
  firstSlot :: [Int]
, firstMancala :: Int
, secondSlot :: [Int]
, secondMancala :: Int
} deriving (Eq, Show)


data Player = First | Second deriving (Eq, Show)
type Slot = Int


data Out = Out String Mancala deriving (Eq, Show)
data Info = Info String Out deriving (Eq, Show)
type Sum = Int
type NextSlot = Int