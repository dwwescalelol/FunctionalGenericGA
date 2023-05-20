import Data.List
import CaseStudies.NQueen.GAforQueens ( Board, Column, Row, NQueen )

queens :: NQueen -> [Board]
queens n = foldl (updateBoards n) [[]] [1..n]

updateBoards :: NQueen -> [Board] -> Int -> [Board]
updateBoards n boards c = concatMap (nextBoards n) boards

nextBoards :: NQueen -> Board -> [Board]
nextBoards n ys = [x : ys | x <- [1..n] \\ ys, safe x (zip [1..] ys)]

safe :: Int -> [(Row,Column)] -> Bool
safe x [] = True
safe x ((r,c):y) = (x /= c + r) && (x /= c - r) && safe x y

main :: IO ()
main = mapM_ print $ queens 8