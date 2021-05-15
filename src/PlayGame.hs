module PlayGame where

import MancalaTypes
import OneStep
import Graphics.Gloss.Interface.Pure.Game

handleEvent :: Event -> Info -> Info
handleEvent (EventKey (Char '1') Down _ _) info = playingGame 1 info
handleEvent (EventKey (Char '2') Down _ _) info = playingGame 2 info
handleEvent (EventKey (Char '3') Down _ _) info = playingGame 3 info
handleEvent (EventKey (Char '4') Down _ _) info = playingGame 4 info
handleEvent (EventKey (Char '5') Down _ _) info = playingGame 5 info
handleEvent (EventKey (Char '6') Down _ _) info = playingGame 6 info
handleEvent _ info = info

playingGame:: Slot -> Info -> Info
playingGame slot (Info str1 (Out str2 mancala)) = do
    if (head str1) == '1'
    then playerMove First (step First slot mancala)
    else 
      if (head str1) == '2'
      then playerMove Second (step Second slot mancala)
      else (Info str1 (Out str2 mancala))

playerMove:: Player -> Out -> Info
playerMove player (Out str2 m) | (head str2) == '0' && player == First = Info "1 First player move" (Out str2 m)
playerMove player (Out str2 m) | (head str2) == '0' && player == Second = Info "2 Second player move" (Out str2 m)
playerMove player (Out str2 m) | (head str2) == '1' && player == First = Info "2 Second player move" (Out str2 m)
playerMove player (Out str2 m) | (head str2) == '1' && player == Second = Info "1 First player move" (Out str2 m)
                               | otherwise = Info "3 End of game" (Out str2 m)

fps :: Int
fps = 60

updateApp :: Float -> Info -> Info
updateApp _ x = x