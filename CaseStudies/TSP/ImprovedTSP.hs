import GAFramework
import CaseStudies.TSP.GAforTSP hiding(gaForTSP, main)
import Data.List
import System.Random
import GAUtility (removeItemAt)

dist :: City -> City -> Distance
dist x y = distance (x,y)

dist2AllCities :: City -> [City] -> [Distance]
dist2AllCities start = map (dist start)

nearestCity :: City -> [City]  -> City
nearestCity start [c] = c
nearestCity start cs = head [c | c <- cs, dist start c == minimum (dist2AllCities start cs)]

prGreedyRoute' :: City -> [City] -> [Int] -> Route
prGreedyRoute' start [c] (s:seeds) =[start, c] 
prGreedyRoute' start cs (s:seeds) = start:prGreedyRoute' nextCity (cs\\[nextCity]) seeds
     where  
      closest = nearestCity start cs
      nextCity
        | 1 == s = closest 
        | otherwise = nearestCity start (cs\\[closest])

mkGreedyRoute' :: [City] -> MkRand Route
mkGreedyRoute' cities seed = prGreedyRoute' (head newcities) (tail newcities) seeds 
  where 
    seeds = randomRs(1,2 :: Int) (mkStdGen seed )     
    n = length cities
    i = seed `mod` n
    newcities = drop i cities ++ take i cities 

smartGaForTSP :: Route -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Route)]
smartGaForTSP cities maxGenerations popSize (xProb,mProb) 
  = geneticAlgorithm maxGenerations popSize (length cities) (mkGreedyRoute' cities) fitness rselection
    (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) orderedMerge dontStop

mainn :: [City] -> IO ()
mainn cs = do
  let seed = 1234567
  let maxGen = 50
  let popSize = 500
  let xProb = 0.4
  let mProb = 0.4
  let solutions = smartGaForTSP cs maxGen popSize (xProb, mProb) seed
  writeToFile solutions 1

