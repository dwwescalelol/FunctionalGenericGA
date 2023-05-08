import GeneticAlgorithm
import GAUtility
import System.Random

type City = (String,Int,Int)
type Route = [City]
type Distance = Double

mkRandCities :: Int -> Seed -> Route
mkRandCities size seed = zip3 (take size (map singleton ['A'..'Z'])) (take size seeds) (take size (drop size seeds))
  where
    seeds = randomRs (2,98) (mkStdGen seed)
    singleton x = [x]

mkRandRoute :: Route -> MkRand Route
mkRandRoute cities seed = head cities : shuffle seed (tail cities)

distance :: (City, City) -> Distance
distance ((n1,x1,y1),(n2,x2,y2)) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

fitness :: Fitness Route
fitness route = round $ sum $ map distance (legs route)

legs :: Route -> [(City, City)]
legs route = zip route (tail route ++ [head route])

stop :: Stop Route
stop = const False

tspMutate :: Mutation Route
tspMutate size seeds [route] = map (head route :) (mutationBySwap size seeds [tail route])

gaForTSP :: Route -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Route)]
gaForTSP cities maxGenerations popSize (xProb,mProb) = geneticAlgorithm maxGenerations popSize (length cities) (mkRandRoute cities) fitness rselection
  (permCrossover, 2, 1, xProb) (tspMutate, 1, 1, mProb) orderedMerge stop

main :: IO ()
main = do
  let cities = mkRandCities 10 2314
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.6
  let mProb = 0.2
  putStrLn " --  All Generations --"
  let solutions = gaForTSP cities maxGen popSize (xProb,mProb) seed
  let window = 12
  let myprint (x, ys, z) = do
                        putStrLn ("Generation " ++ show x ++ ", Size " ++ show z)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"  
  print (length solutions)