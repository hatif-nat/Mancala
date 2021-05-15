module Main where

import OneStep
import PlayGame
import Draw
import MancalaTypes
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  putStrLn "Start game!"
  let mancala = Mancala { firstSlot = [4, 4, 4, 4, 4, 4], firstMancala = 0, secondSlot = [4, 4, 4, 4, 4, 4], secondMancala = 0}
  play display (dark orange) fps (Info "1 First player move" (Out "0 Start game" mancala)) drawDesk handleEvent updateApp
