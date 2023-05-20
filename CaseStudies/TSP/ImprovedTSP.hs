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

-- probablistic greedy algorthm, choses either nearest or occasionally second nearest city
prGreedyRoute :: City -> [City] -> Int -> [Int] -> Route
prGreedyRoute start [c] p (s:seeds) =[start, c] 
prGreedyRoute start cs p (s:seeds) = start:prGreedyRoute nextCity (cs\\[nextCity]) p seeds
     where  
      closest = nearestCity start cs
      nextCity 
        | p <= s = nearestCity start (cs\\[closest])
        | otherwise = closest

mkGreedyRoute :: Int -> [City] -> MkRand Route
mkGreedyRoute rate cities seed = prGreedyRoute (head newcities) (tail newcities) rate seeds 
  where 
    seeds = randomRs(1,99 :: Int) (mkStdGen seed )     
    n = length cities
    i = seed `mod` n
    newcities = drop i cities ++ take i cities 

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
    where
      rate = round ((r-1)/r * 100.0)
      r = numChildren popSize cities

-- first hte number of cities (x) is given and also the number of population is given (y)
-- find a such that a ^ x = y
-- the path should be of length x starting from the root to leaf and a is the average num children

logOfa :: Int -> Int -> Double
logOfa popSize nrCities = logBase 2 (fromIntegral popSize)/fromIntegral nrCities

binSearchLog :: (Double, Double) -> Double -> Int -> Double
binSearchLog (x,y) b count
  | count == 0 = mid
  | b < logBase 2 mid = binSearchLog (x,mid) b (count-1)
  | b > logBase 2 mid = binSearchLog (mid,y) b (count-1)
  | otherwise = mid
  where mid = (x+y)/2

-- given b, aim is to find a such that b = log a
inverseLog :: Double -> Double
inverseLog b = binSearchLog (1.0, 2.0) b 100

numChildren :: Int -> [City] -> Double
numChildren popSize cities = inverseLog (logOfa popSize (length cities))

mainn :: [City] -> IO ()
mainn cs = do
  let seed = 1234567
  let maxGen = 50
  let popSize = 500
  let xProb = 0.4
  let mProb = 0.4
  let solutions = smartGaForTSP cs maxGen popSize (xProb, mProb) seed
  writeToFile solutions 1
  let best = foldl min (100000, []) (map head solutions)
  print (fst (head $ head solutions))
  print $ fst best
  print $ snd best

