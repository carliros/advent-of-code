module Ex02 where
import Data.List

main :: IO()
main = do content <- readFile "ex02_input.txt"
          let rawList = lines content
          let result = foldr fshape 0 rawList
          print result

fshape :: String -> Int -> Int
fshape values acc = let [p1, p2] = words values
                        p2' = selectTwo p1 p2
                    in valueOf p2' + play4two p1 p2' + acc

valueOf :: String -> Int
valueOf "X" = 1
valueOf "Y" = 2
valueOf "Z" = 3

-- 0 if you lost, 3 if the round was a draw, and 6 if you won
-- A for Rock, B for Paper, and C for Scissors
-- X for Rock, Y for Paper, and Z for Scissors
play4two :: String -> String -> Int
play4two "A" "X" = 3
play4two "A" "Y" = 6
play4two "A" "Z" = 0
play4two "B" "Y" = 3
play4two "B" "X" = 0
play4two "B" "Z" = 6
play4two "C" "Z" = 3
play4two "C" "X" = 6
play4two "C" "Y" = 0

-- 0 if you lost, 3 if the round was a draw, and 6 if you won
-- the second column says how the round needs to end: 
--      X means you need to lose, 
--      Y means you need to end the round in a draw, and 
--      Z means you need to win.
selectTwo :: String -> String -> String
selectTwo "A" "X" = "Z"
selectTwo "B" "X" = "X"
selectTwo "C" "X" = "Y"
selectTwo "A" "Y" = "X"
selectTwo "B" "Y" = "Y"
selectTwo "C" "Y" = "Z"
selectTwo "A" "Z" = "Y"
selectTwo "B" "Z" = "Z"
selectTwo "C" "Z" = "X"
