module OneStep where

import ChangeList
import MancalaTypes
import Graphics.Gloss.Interface.Pure.Game

step :: Player -> Slot -> Mancala -> Out
step First slot (Mancala s1 m1 s2 m2) | foldl1 (+) s1 == 0 && m1 > (m2 + (foldl1 (+) s2)) = Out "2 First player wins" (Mancala s1 m1 s1 (m2 + (foldl1 (+) s2)))
step First slot (Mancala s1 m1 s2 m2) | foldl1 (+) s1 == 0 && m1 < (m2 + (foldl1 (+) s2)) = Out "2 Second player wins" (Mancala s1 m1 s1 (m2 + (foldl1 (+) s2)))
step Second slot (Mancala s1 m1 s2 m2) | foldl1 (+) s2 == 0 && (m1 + (foldl1 (+) s1)) > m2 = Out "2 First player wins" (Mancala s2 (m1 + (foldl1 (+) s1)) s2 m2)
step Second slot (Mancala s1 m1 s2 m2) | foldl1 (+) s2 == 0 && (m1 + (foldl1 (+) s1)) < m2 = Out "2 Second player wins" (Mancala s2 (m1 + (foldl1 (+) s1)) s2 m2)
step player slot mancala | (slot > 6) || (slot < 0) = Out "0 Incorrect number of slot" mancala
step First slot (Mancala s1 m1 s2 m2) = make_step First (check slot s1)  slot (Mancala s1 m1 s2 m2)
step Second slot (Mancala s1 m1 s2 m2) = make_step Second (check slot s2) slot (Mancala s1 m1 s2 m2)

check :: Slot -> [Int] -> String
check slot l | (l !! (slot - 1)) > 0 = "correct step"
             | otherwise = "0 Incorrect step"

make_step :: Player -> String -> Slot -> Mancala -> Out
make_step p "0 Incorrect step" s m = Out "0 Incorrect step" m
make_step First str n (Mancala s1 m1 s2 m2) = put_out First (s1 !! (n - 1)) (n+1) (Mancala (toZero n s1) m1 s2 m2)
make_step Second str n (Mancala s1 m1 s2 m2) = put_out Second (s2 !! (n - 1)) (n + 1) (Mancala s1 m1 (toZero n s2) m2)

put_out :: Player -> Sum -> NextSlot -> Mancala -> Out
put_out First 1 slot m = put_out_end First slot m
put_out First s slot m = put_out First (s - 1) ((mod slot 13) + 1)(add_one First slot m)
put_out Second 1 slot m = put_out_end Second slot m
put_out Second s slot m = put_out Second (s - 1) ((mod slot 13) + 1)(add_one Second slot m)

put_out_end :: Player ->NextSlot -> Mancala -> Out
put_out_end First 13 (Mancala s1 m1 s2 m2) = Out "0 One more step for first player" (Mancala s1 (m1 + 1) s2 m2)
put_out_end First slot (Mancala s1 m1 s2 m2) | slot < 7 && (s1 !! (slot - 1) == 0) = Out "1 Ha-ha i add many rocks in first mancala" (put_to_mancala First slot (Mancala s1 m1 s2 m2))
                                             | otherwise = Out "1 Common end first player step" (add_one First slot (Mancala s1 m1 s2 m2))
put_out_end Second 13 (Mancala s1 m1 s2 m2) = Out "0 One more step for second player" (Mancala s1 m1 s2 (m2 + 1))
put_out_end Second slot (Mancala s1 m1 s2 m2) | slot < 7 && (s2 !! (slot - 1) == 0) = Out "1 Ha-ha i add many rocks in second mancala" (put_to_mancala Second slot (Mancala s1 m1 s2 m2))
                                              | otherwise = Out "1 Common end second player step" (add_one Second slot (Mancala s1 m1 s2 m2))

add_one :: Player -> Slot -> Mancala -> Mancala
add_one First n (Mancala s1 m1 s2 m2) | n > 0 && n < 7 = Mancala (plusOne n s1) m1 s2 m2
                                      | n > 6 && n < 13 = Mancala s1 m1 (plusOne (n - 6) s2) m2
                                      | n == 13 = Mancala s1 (m1 + 1) s2 m2
add_one Second n (Mancala s1 m1 s2 m2) | n > 0 && n < 7 = Mancala s1 m1 (plusOne n s2) m2
                                       | n > 6 && n < 13 = Mancala (plusOne (n - 6) s1) m1 s2 m2
                                       | n == 13 = Mancala s1 m1 s2 (m2 + 1)

put_to_mancala :: Player -> Slot -> Mancala -> Mancala
put_to_mancala First n (Mancala s1 m1 s2 m2) = Mancala (toOne n s1) (m1 + (s2 !! (6 - n))) (toZero (7 - n) s2)  m2
put_to_mancala Second n (Mancala s1 m1 s2 m2) = Mancala (toZero (7 - n) s1) m1 (toOne n s2)  (m2 + (s1 !! (6 - n)))
