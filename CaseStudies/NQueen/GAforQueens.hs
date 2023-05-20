module CaseStudies.NQueen.GAforQueens where
import GAFramework
import GAUtility
import Data.List

type NQueen = Int
type Row = Int
type Column = Int
type Board = [Column]
type Board2D = [(Row, Column)]

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]

qfitness :: Fitness Board
qfitness b = length $ concatMap (\(q:qs) -> filter (takes q) qs) ((init . tails) (to2DBoard b))

to2DBoard :: Board -> Board2D
to2DBoard = zip [1..]

takes :: (Row, Column) -> (Row, Column) -> Bool 
takes (r1,c1) (r2,c2) = abs (r1-r2) == abs (c1-c2)

gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Board)]
gaForQueens nQueens maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize nQueens (randQueen nQueens) qfitness rselection
  (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) orderedMerge (stopFit 0)

mainn :: Int -> IO ()
mainn n = do
  -- let n = 20
  let seed = 123456
  let maxGen = 500
  let popSize = 5000
  let xProb = 0.4
  let mProb = 0.4
  let solutions = gaForQueens n maxGen popSize (xProb, mProb) seed
  writeToFile solutions 1
  let best = foldl min (1000000000, []) (map head solutions)
  print $ fst best
  print $ snd best
  print $ fst $ head $ head solutions
  print $ length solutions