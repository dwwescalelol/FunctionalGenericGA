{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
import GAFramework
import GAforQueens hiding(gaForQueens,main)
import GAUtility
import Data.List

type Coord = (Row,Column)
type QueenPair = (Coord, Coord)

queenMutation :: Mutation Board
queenMutation _ _ [xs]
  | length collisions < 2 = []
  | otherwise = mutateBoard xs (head collisions) (collisions!!1)
  where
    collisions = take 2 $ collidingPairs xs

mutateBoard :: Board -> QueenPair -> QueenPair -> [Board]
mutateBoard b ((c1,_),(c2,_)) ((c3,_),(c4,_)) =
  [swapItemAt (c1-1,c3-1) b,
  swapItemAt (c1-1,c4-1) b,
  swapItemAt (c2-1,c3-1) b,
  swapItemAt (c2-1,c4-1) b]

collidingPairs :: Board -> [QueenPair]
collidingPairs b = concatMap collidingPairs' (tails (to2DBoard b))
  where
    collidingPairs' [] = []
    collidingPairs' (x:xs) = map (x,) (filter (takes x) xs)

gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Board)]
gaForQueens nQueens maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize nQueens (randQueen nQueens) qfitness rselection
  (permCrossover, 2, 1, xProb) (queenMutation, 1, 4, mProb) orderedMerge (stopFit 0)

customDisplay :: DisplayPop Board
customDisplay solutions window = do
  putStrLn " --  All Generations --"
  let myprint (x, ys, n) = do
                        putStrLn ("Generation " ++ show x)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   ")) ys
                        print n
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn (" --  Last Generation -- " ++ show (length solutions - 1))
  mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) $ filter (\(f,bs) -> f==0)(last solutions)


main :: IO ()
main = do
  let n = 1000
  let seed = 123456
  let maxGen = 10000
  let popSize = 100
  let xProb = 0.0
  let mProb = 0.8
  let solutions = gaForQueens n maxGen  popSize (xProb, mProb) seed
  customDisplay solutions 3
