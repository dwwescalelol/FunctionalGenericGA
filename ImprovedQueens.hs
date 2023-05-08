import GeneticAlgorithm
import GAUtility
import Data.List

type Board = [Int]
type NQueen = Int
type Coord = (Int,Int)
type Board2D = [Coord]
type QueenPair = (Coord, Coord)

randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]

qfitness :: Fitness Board
qfitness xs = length $ concatMap (\(y:ys) -> filter (takes y) ys) (init . tails $ to2DBoard xs)

takes :: (Eq a, Num a) => (a, a) -> (a, a) -> Bool
takes (c1,r1) (c2,r2) = abs (c1-c2) == abs (r1-r2)

qstop :: Stop Board
qstop evalPop = null evalPop || fst (head evalPop) == 0

to2DBoard :: Board -> Board2D
to2DBoard = zip [1..]

-- mutation

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
  = gga maxGenerations popSize nQueens (randQueen nQueens) qfitness rselection
  (permCrossover, 2, 1, xProb) (queenMutation, 1, 4, mProb) orderedMerge qstop

main :: IO ()
main = do
  let n = 350
  let seed = 1234567
  let maxGen = 1000
  let popSize = 50
  let xProb = 0.0
  let mProb = 0.7
  putStrLn " --  All Generations --"
  let solutions = gaForQueens n maxGen  popSize (xProb, mProb) seed
  let window = 15
  let myprint (x, ys, n) = do
                        putStrLn ("Generation " ++ show x)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   ")) ys
                        print n
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"
  print (length solutions)
  mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) $ last solutions
