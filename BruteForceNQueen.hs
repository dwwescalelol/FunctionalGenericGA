import Control.Monad
import Data.List

type Size = Int
type Row = Int
type Column = Int
type Board = [Int]

queens :: Size -> [Board]
queens n = foldl (updateBoards n) [[]] [1..n]

updateBoards :: Size -> [Board] -> Int -> [Board]
updateBoards n boards c = concatMap (nextBoards n) boards

nextBoards :: Size -> Board -> [Board]
nextBoards n ys = [x : ys | x <- [1..n] \\ ys, safe x (zip [1..] ys)]

safe :: Int -> [(Row,Column)] -> Bool
safe x [] = True
safe x ((r,c):y) = (x /= c + r) && (x /= c - r) && safe x y

qfitness :: Board -> Int
qfitness xs = length $ concatMap (\(y:ys) -> filter (takes y) ys) (init . tails $ zip [1..] xs)

takes :: (Row, Column) -> (Row, Column) -> Bool 
takes (r1,c1) (r2,c2) = abs (r1-r2) == abs (c1-c2)

main = mapM_ print $ queens 8