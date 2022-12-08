module Ex03 where
import Data.List
import Data.Char

main :: IO()
main = do content <- readFile "ex03.txt"
          let rawList = lines content
          print $ part2 rawList

part1 :: [String] -> Int
part1 rawList = foldr fpriority 0 rawList

part2 :: [String] -> Int
part2 rawList
    = let g3List = groupAt 3 rawList
      in foldr fpriority3 0 g3List
          

groupAt :: Int -> [a] -> [[a]]
groupAt n [] = []
groupAt n ls = let (e, es) = splitAt n ls
               in e : groupAt n es

fpriority3 :: [String] -> Int -> Int
fpriority3 ls acc = let l = shared ls
                        p = priority l
                    in p + acc

fpriority :: String -> Int -> Int
fpriority pack acc = let (p1, p2) = splitAt (div (length pack) 2) pack
                         l = shared [p1, p2]
                         p = priority l
                    in p + acc

priority :: String -> Int
priority (c:cs) = if isUpper c
                  then ord c - 38
                  else ord c - 96

shared :: [String] -> String
shared = foldr1 intersect
