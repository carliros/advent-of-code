module Ex04 where
import Data.List

main :: IO()
main = do content <- readFile "ex04.txt"
          let rawList = lines content
          let result = filter middleIntersected rawList
          --mapM_ putStrLn result
          print $ length result

fullyIntersected :: String -> Bool
fullyIntersected field
    = let (as, _:bs) = span (/=',') field
          (a1', _:a2') = span (/='-') as
          (b1', _:b2') = span (/='-') bs
          (a1, a2) = (readInt a1', readInt a2')
          (b1, b2) = (readInt b1', readInt b2')
      in (a1 <= b1) || (b1 <= a1 && b2 >= a2)

middleIntersected :: String -> Bool
middleIntersected field
    = let (as, _:bs) = span (/=',') field
          (a1', _:a2') = span (/='-') as
          (b1', _:b2') = span (/='-') bs
          (a1, a2) = (readInt a1', readInt a2')
          (b1, b2) = (readInt b1', readInt b2')
      in not . null $ intersect [a1..a2] [b1..b2]


readInt :: String -> Int
readInt = read
