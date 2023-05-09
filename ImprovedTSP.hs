import GeneticAlgorithm
import GAforTSP hiding(gaForTSP, main)
import Data.List
import System.Random

dist2AllCities :: City -> [City] -> [Distance]
dist2AllCities start = map (dist start)

dist :: City -> City -> Distance
dist x y = distance (x,y)

-- first hte number of cities (x) is given and also the number of population is given (y)
-- find a such that a ^ x = y
-- the path should be of length x starting from the root to leaf and a is the average num children
nearestCity :: City -> [City]  -> City
nearestCity start [c] = c
nearestCity start cs = head [c | c <- cs, dist start c == minimum (dist2AllCities start cs)]

prGreedyRoute :: City -> [City] -> Int -> [Int] -> Route
prGreedyRoute start [c] p (s:seeds) =[start, c] 
prGreedyRoute start cs p (s: seeds)   = start : prGreedyRoute z (cs\\[z]) p seeds
     where  c = nearestCity start cs
            z | p <= s = nearestCity start (cs\\[c])
              | otherwise = c

mkGreedyRoute :: Int -> [City] -> MkRand Route
mkGreedyRoute rate cities seed = prGreedyRoute (head newcities) (tail newcities) rate seeds 
  where seeds = randomRs(1,99 :: Int) (mkStdGen seed )     
        n = length cities
        i = seed `mod` n
        newcities = drop i cities ++ take i cities 

-- Calc num to make for intial pop
smartGaForTSP :: Route -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Route)]
smartGaForTSP cities maxGenerations popSize (xProb,mProb) 
  = geneticAlgorithm maxGenerations popSize (length cities) (mkGreedyRoute rate cities) fitness rselection
    (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) distinctOrderedMerge dontStop
    where
      rate = round ((r-1)/r * 100.0)
      r = numChildren popSize cities

logOfa :: Int -> Int -> Double
logOfa popSize nrCities = logBase 2 (fromIntegral popSize)/fromIntegral nrCities

binSearchLog :: (Double, Double) -> Double -> Int -> Double
binSearchLog (x,y)  b  count
  | count == 0 = mid
  | b < logBase 2 mid = binSearchLog (x,mid) b (count-1)
  | b > logBase 2 mid = binSearchLog (mid,y) b (count-1)
  | otherwise = mid
  where mid = (x+y)/2

inverseLog :: Double -> Double
inverseLog b = binSearchLog (1.0, 2.0) b 100

numChildren :: Int -> [City] -> Double
numChildren popSize cities = inverseLog (logOfa popSize (length cities))

main :: IO ()
main = do
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.6
  let mProb = 0.2
  putStrLn " --  All Generations --"
  let solutions = smartGaForTSP cities26 maxGen popSize (xProb, mProb) seed
  let window = 2
  let myprint (x, ys, n) = do
                        putStrLn ("Generation " ++ show x ++ ", Size " ++ show n)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"
  print (length solutions)
  print (foldl min (1000, []) (map head solutions))

