import GeneticAlgorithm
import Data.List
import System.Random

type City = (String,Int,Int)
type Route = [City]
type Distance = Double

cities10 :: Route
cities10 = [("A",86,22),("B",63,50),("C",17,46),("D",66,28),("E",76,49),("F",47,40),("G",11,63),("H",36,27),("I",29,14),("J",2,9)]
cities26 :: Route
cities26 = [("A",36,4),("B",63,30),("C",22,20),("D",66,63),
            ("E",76,14),("F",47,25),("G",11,24),("H",86,35),
            ("I",29,28),("J",5,10),("K",10,71),("L",78,45),
            ("M",25,63),("N",88,3),("O",89,73),("P",90,51),
            ("Q",53,40),("R",7,59),("S",54,50),("T",34,51),
            ("U",38,41),("V",80,20),("W",91,31),("X",42,75),
            ("Y",57,12),("Z",23,43)]

distance :: (City, City) -> Distance
distance ((n1,x1,y1),(n2,x2,y2)) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

fitness :: Fitness Route
fitness route = round $ sum $ map distance (legs route)

legs :: Route -> [(City, City)]
legs route = zip route (tail route ++ [head route])

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

