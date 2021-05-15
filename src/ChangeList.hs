module ChangeList where

import MancalaTypes

toZero :: Slot -> [Slot] -> [Slot]
toZero n [] = []
toZero 1 (x1 : xs) = (0 : xs)
toZero n (x1 : xs) = (x1 : (toZero (n - 1) xs))

toOne :: Slot -> [Slot] -> [Slot]
toOne n [] = []
toOne 1 (x1 : xs) = (1 : xs)
toOne n (x1 : xs) = (x1 : (toOne (n - 1) xs))

plusOne :: Slot -> [Slot] -> [Slot]
plusOne n [] = []
plusOne 1 (x1 : xs) = ((x1+1) : xs)
plusOne n (x1 : xs) = (x1 : (plusOne (n - 1) xs))