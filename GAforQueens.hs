import GeneticAlgorithm
import GAUtility
import Data.List

type Board = [Int]
type NQueen = Int

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]

qfitness :: Fitness Board
qfitness xs = length $ concatMap (\(y:ys) -> filter (takes y) ys) (tails $ zip xs [1..])
  where
    takes (x1,x2) (y1,y2) = x1 == y1 || abs (x1-x2) == abs (y1-y2)
    tails [x] = [[x]]
    tails (x:xs) = (x:xs) : tails xs

qstop :: Stop Board
qstop evalPop = null evalPop || fst (head evalPop) == 0
  
queenCrossover2 :: Crossover Board
queenCrossover2 size seed [xs,ys] = [c1 ++ (ys \\ c1), c2 ++ (xs \\ c2)]
  where 
    c1 = take i xs
    c2 = take i ys
    i = mod seed size

gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> (Pop (Eval Board), Pop (Eval Board))
gaForQueens nQueens maxGenerations popSize (xProb,mProb) seed
  = geneticAlgorithm maxGenerations popSize nQueens (randQueen nQueens) qfitness eliteSelection
  (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) orderedMerge qstop seed

qmerge :: Ord a => [a] -> [a] -> [a]
qmerge popA [] = popA
qmerge [] popB = popB
qmerge (x:popA) (y:popB)
  | x == y = qmerge (x:popA) popB
  | x < y = x : qmerge popA (y:popB)
  | otherwise = y : qmerge (x:popA) popB

main :: IO ()
main = do
  let n = 20
  let seed = 2342123
  let maxGen = 5
  let popSize = 10000
  let xProb = 0.4
  let mProb = 0.6
  let (solution,hallOfFame) = gaForQueens n maxGen popSize (xProb,mProb) seed
  mapM_ print (take 20 $ sortPop hallOfFame)
  print "-----------------------------------------------------"
  print $ length solution
  print $ head solution

