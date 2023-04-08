import GeneticAlgorithm
import GAUtility
import System.Random (randomR, mkStdGen, Random (randomRs))

type Node = (String,Int,Int)
type Route = [Node]
type Distance = Double

cities1 :: Route
cities1 = [("A",1,1),("B",6,5),("C",97,2),("D",43,2),("E",32,1),("F",20,20)]

mkRandCities :: Int -> Seed -> Route
mkRandCities size seed = zip3 (take size (map singleton ['A'..'Z'])) (take size seeds) (take size (drop size seeds))
  where
    seeds = randomRs (2,98) (mkStdGen seed)
    singleton x = [x]
    

mkRandRoute :: Route -> MkRand Route
mkRandRoute cities seed = head cities : shuffle seed (tail cities)

distance :: Node -> Node -> Distance
distance (n1,x1,y1) (n2,x2,y2) = sqrt $ fromIntegral((x2 - x1)^2 + (y2 - y1)^2)

fitness :: Fitness Route
fitness route = round $ sum $ zipWith distance route (tail route ++ [head route])

legs :: Route -> [(Node, Node)]
legs route = zip route (tail route ++ [head route])
  
stop :: Stop Route
stop = const False

tspMutate :: Mutation Route
tspMutate size seeds [route] = map (head route :) (mutationBySwap size seeds [tail route])

gaForTSP :: Route -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> (Pop (Eval Route), Pop (Eval Route))
gaForTSP cities maxGenerations popSize (xProb,mProb) seed
  = geneticAlgorithm maxGenerations popSize (length cities) (mkRandRoute cities) fitness eliteSelection
  (permCrossover, 2, 1, xProb) (tspMutate, 1, 1, mProb) orderedMerge stop seed

main :: IO ()
main = do
  let cities = mkRandCities 26 2314
  let seed = 1234324
  let maxGen = 20
  let popSize = 6000
  let xProb = 0.4
  let mProb = 0.6
  let (solution,hallOfFame) = gaForTSP cities maxGen popSize (xProb,mProb) seed
  mapM_ print (take 20 hallOfFame)
  print "-----------------------------------------------------"
  print $ length solution
  print $ head solution
  

