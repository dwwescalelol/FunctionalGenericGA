import GeneticAlgorithm
import GAUtility
import Data.List

type Board = [Int]
type NQueen = Int

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]

qfitness :: Fitness Board
qfitness xs = length $ concatMap (\(y:ys) -> filter (takes y) ys) ((init . tails) $ zip xs [1..])
  where
    takes (r1,c1) (r2,c2) = abs (r1-r2) == abs (c1-c2)

qstop :: Stop Board
qstop evalPop = null evalPop || fst (head evalPop) == 0
  
queenCrossover2 :: Crossover Board
queenCrossover2 size seed [xs,ys] = [c1 ++ (ys \\ c1), c2 ++ (xs \\ c2)]
  where 
    c1 = take i xs
    c2 = take i ys
    i = mod seed size

gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Board)]
gaForQueens nQueens maxGenerations popSize (xProb,mProb)
  = gga maxGenerations popSize nQueens (randQueen nQueens) qfitness rselection
  (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) qmerge qstop

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
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.6
  let mProb = 0.2
  putStrLn " --  All Generations --"
  let solutions = gaForQueens n maxGen popSize (xProb, mProb) seed
  let window = 12
  let myprint (x, ys, z) = do
                        putStrLn ("Generation " ++ show x ++ ", Size " ++ show z)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"  
  print (length solutions)
