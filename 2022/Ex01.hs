module Ex01 where
import Data.List

main :: IO()
main = do content <- readFile "ex01_input.txt"
          let rawList = lines content
          let (sum, ls) = foldr f (0, []) rawList
          print $ foldr (+) 0 . take 3 . reverse . sort $ sum:ls

getBig :: Int -> Int -> Int
getBig val big = if val >= big then val else big

f :: String -> (Int, [Int]) -> (Int, [Int])
f val (acc, ls) = if null val 
                   then (0, acc:ls)
                   else (readInt val + acc, ls)

readInt :: String -> Int
readInt = read

