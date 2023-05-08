import GeneticAlgorithm
import GAUtility
import Data.List

type NQueen = Int
type Row = Int; 
type Column = Int
type Board = [Column]
type Board2D = [(Row, Column)]; 

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]

qfitness :: Fitness Board
qfitness b = length $ concatMap (\(q:qs) -> filter (takes q) qs) ((init . tails) board2D)
  where
    board2D = zip [1..] b

takes :: (Row, Column) -> (Row, Column) -> Bool 
takes (r1,c1) (r2,c2) = abs (r1-r2) == abs (c1-c2)

qstop :: Stop Board
qstop evalPop = null evalPop || fst (head evalPop) == 0

gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Board)]
gaForQueens nQueens maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize nQueens (RandChrom (randQueen nQueens)) qfitness rselection
  (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) orderedMerge qstop

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
