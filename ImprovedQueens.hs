import GeneticAlgorithm
import GAUtility
import Data.List

type Board = [Int]
type NQueen = Int

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]

qfitness :: Fitness Board
qfitness xs = length $ concatMap (\(y:ys) -> filter (takes y) ys) ((init . tails) $ zip [1..] xs)

takes :: (Eq a, Num a) => (a, a) -> (a, a) -> Bool
takes (r1,c1) (r2,c2) = abs (r1-r2) == abs (c1-c2)

qstop :: Stop Board
qstop evalPop = null evalPop || fst (head evalPop) == 0
  
queenCrossover2 :: Crossover Board
queenCrossover2 size seed [xs,ys] = [c1 ++ (ys \\ c1), c2 ++ (xs \\ c2)]
  where 
    c1 = take i xs
    c2 = take i ys
    i = mod seed size

queenMutation :: Mutation Board
queenMutation _ _ [xs] = concatMap mutatedBoards ts
  where
    ts = (init . tails) (collidingPairs xs)

    mutatedBoards [] = []
    mutatedBoards (y:ys) = concatMap (mutateBoard xs y) ys

type Coord = (Int,Int)

mutateBoard :: Board -> (Coord,Coord) -> (Coord,Coord) -> [Board]
mutateBoard b ((c1,_),(c2,_)) ((c3,_),(c4,_)) = 
  [swapItemAt (c1-1,c3-1) b,
  swapItemAt (c1-1,c4-1) b,
  swapItemAt (c2-1,c3-1) b,
  swapItemAt (c2-1,c4-1) b]

collidingPairs :: Board -> [(Coord,Coord)]
collidingPairs xs =  concatMap collidingPairs' (tails (zip [1..] xs))
  where
    collidingPairs' [] = []
    collidingPairs' (x:xs) = zip (repeat x) (filter (takes x) xs)
    
gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Board)]
gaForQueens nQueens maxGenerations popSize (xProb,mProb)
  = gga maxGenerations popSize nQueens (randQueen nQueens) qfitness rselection
  (permCrossover, 2, 1, xProb) (queenMutation, 1, 1, mProb) qmerge qstop

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
  let seed = 2342345
  let maxGen = 3
  let popSize = 500
  let xProb = 0.1
  let mProb = 0.8
  putStrLn " --  All Generations --"
  let solutions = gaForQueens n maxGen  popSize (xProb, mProb) seed
  let window = 12
  let myprint (x, ys, n) = do
                        putStrLn ("Generation " ++ show x)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
                        print n
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"  
  print (length solutions)
