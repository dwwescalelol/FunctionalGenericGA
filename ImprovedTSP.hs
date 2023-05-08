import GeneticAlgorithm
import GAUtility
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
            
mkRandCities :: Int -> Seed -> Route
mkRandCities size seed = zip3 (take size (map singleton ['A'..'Z'])) (take size seeds) (take size (drop size seeds))
  where
    seeds = randomRs (2,98) (mkStdGen seed)
    singleton x = [x]

distance :: (City, City) -> Distance
distance ((n1,x1,y1),(n2,x2,y2)) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

fitness :: Fitness Route
fitness route = round $ sum $ map distance (legs route)

legs :: Route -> [(City, City)]
legs route = zip route (tail route ++ [head route])

mkGreedyRoute :: [City] -> Int -> MkRand Route
mkGreedyRoute cities rate seed = prGreedyRoute rate (head newcities) (tail newcities) seeds 
  where 
    seeds = randomRs(1,99 :: Int) (mkStdGen seed )     
    n = length cities
    i = seed `mod` n
    newcities = drop i cities ++ take i cities 

prGreedyRoute ::  Int -> City -> [City] ->[Int] -> Route
prGreedyRoute prob start [] (s:seeds) = [start] 
prGreedyRoute prob start cities (s:seeds) = start : prGreedyRoute prob nextCity (cities\\[nextCity]) seeds
  where
    closest = closestCity start cities
    sndClosest = closestCity start (cities\\[closest])
    nextCity
      | prob > s = closest
      | otherwise = sndClosest

myGreedy :: City -> [City] -> Route
myGreedy start [] = [start] 
myGreedy start cities = start : myGreedy closest (cities\\[closest])
  where
    closest = closestCity start cities
    sndClosest = closestCity start (cities\\[closest])

closestCity :: City -> [City]  -> City
closestCity start [c] = c
closestCity start cs = head [c | c <- cs, dist start c == minimum dist2AllCities]
  where
    dist2AllCities = map (dist start) cs
    dist x y = distance (x,y)

mkRandRoute :: MkRand Route
mkRandRoute seed = myGreedy c (cities10\\[c])
  where
    index = seed `mod` 9
    c = cities10 !! index 

mkInitialPop :: [City] -> Pop Route
mkInitialPop cities = map (\c -> myGreedy c (cities\\[c])) cities

gaForTSP :: Route -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Route)]
gaForTSP cities maxGenerations popSize (xProb,mProb) 
  = geneticAlgorithm maxGenerations popSize (length cities) (InitialisedPop (mkInitialPop cities)) fitness rselection
    (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) orderedMerge dontStop

main :: IO ()
main = do
  let seed = 123456
  let maxGen = 1
  let popSize = 500
  let xProb = 0.6
  let mProb = 0.2
  putStrLn " --  All Generations --"
  let solutions = gaForTSP cities26 maxGen popSize (xProb, mProb) seed
  let window = 12
  let myprint (x, ys, n) = do
                        putStrLn ("Generation " ++ show x)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
                        print n
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"
  print (length solutions)
