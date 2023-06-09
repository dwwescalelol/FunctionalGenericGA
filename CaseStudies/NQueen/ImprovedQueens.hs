{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
import GAFramework
import CaseStudies.NQueen.GAforQueens hiding(gaForQueens,main)
import GAUtility
import Data.List

type Coord = (Row,Column)
type QueenPair = (Coord, Coord)

queenMutation :: Mutation Board
queenMutation _ _ [xs]
  | null xs = [xs]
  | length collisions == 1 = [swapItemAt singlePair xs]
  | otherwise = mutateBoard xs (head collisions) (collisions!!1)
  where
    collisions = take 2 $ collidingPairs xs
    collision = head collisions
    singlePair = (fst (fst collision) - 1 ,fst (snd collision) -1)

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

main :: IO ()
main = do
  let n = 25
  let seed = 12345678
  let maxGen = 1000
  let popSize = 500
  let xProb = 0.2
  let mProb = 0.7
  let solutions = gaForQueens n maxGen popSize (xProb, mProb) seed
  writeToFile solutions 5

