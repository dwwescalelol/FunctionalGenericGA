module CaseStudies.TSP.GAforTSP where
import GAFramework
import GAUtility
import System.Random
import Data.Char

type City = (String,Int,Int)
type Route = [City]
type Distance = Double



-- This function converts a number to the corresponding uppercase letter
numToLetter :: Int -> Char
numToLetter n = chr (ord 'A' + n)

numToString :: Int -> String
numToString n
  | q == 0 = [r'] 
  | otherwise = numToString (q - 1) ++ [r']
  where
    (q, r) = n `divMod` 26
    r' = numToLetter r

-- unbounded list
cityNames :: [String]
cityNames = [ numToString n | n <- [0..]]

cities10,cities20,cities30,cities50,cities100 :: Route
cities10 = 
  [("A",60,38),("B",36,40),("C",17,55),("D",60,58),("E",63,52),("F",90,54),("G",3,51),("H",88,53),("I",14,18),("J",35,30)]
cities20 = 
  [("A",60,51),("B",36,53),("C",17,18),("D",60,30),("E",63,31),("F",90,2),("G",3,61),("H",88,7),("I",14,72),("J",35,10),("K",91,26),("L",38,61),("M",40,21),("N",87,66),("O",55,45),("P",58,2),("Q",52,30),("R",94,31),("S",54,25),("T",90,4)]
cities30 = 
  [("A",60,61),("B",36,7),("C",17,72),("D",60,10),("E",63,26),("F",90,61),("G",3,21),("H",88,66),("I",14,45),("J",35,2),("K",91,30),("L",38,31),("M",40,25),("N",87,4),("O",55,51),("P",58,18),("Q",52,71),("R",94,29),("S",54,17),("T",90,11),("U",97,51),("V",51,67),("W",53,30),("X",18,31),("Y",88,24),("Z",30,58),("AA",31,76),("AB",2,14),("AC",93,54),("AD",90,14)]
cities50 = 
  [("A",60,51),("B",36,18),("C",17,71),("D",60,29),("E",63,17),("F",90,11),("G",3,51),("H",88,67),("I",14,30),("J",35,31),("K",91,24),("L",38,58),("M",40,76),("N",87,14),("O",55,54),("P",58,14),("Q",52,28),("R",94,43),("S",54,40),("T",90,19),("U",97,77),("V",51,55),("W",53,56),("X",18,76),("Y",88,15),("Z",30,71),("AA",31,44),("AB",2,35),("AC",93,50),("AD",90,8),("AE",61,27),("AF",7,37),("AG",72,22),("AH",88,58),("AI",97,59),("AJ",10,58),("AK",26,54),("AL",61,39),("AM",21,65),("AN",95,45),("AO",85,7),("AP",66,74),("AQ",45,58),("AR",81,11),("AS",2,11),("AT",30,72),("AU",31,51),("AV",98,28),("AW",25,70),("AX",4,39)]
cities100 = 
  [("A",60,11),("B",36,72),("C",17,51),("D",60,28),("E",63,70),("F",90,39),("G",3,21),("H",88,13),("I",14,13),("J",35,62),("K",91,33),("L",38,60),("M",40,47),("N",87,64),("O",55,59),("P",58,42),("Q",52,23),("R",94,27),("S",54,63),("T",90,49),("U",97,47),("V",51,52),("W",53,47),("X",18,43),("Y",88,70),("Z",30,2),("AA",31,24),("AB",2,31),("AC",93,22),("AD",90,69),("AE",61,8),("AF",7,63),("AG",72,23),("AH",88,48),("AI",97,2),("AJ",10,32),("AK",26,11),("AL",61,3),("AM",21,20),("AN",95,69),("AO",85,18),("AP",66,3),("AQ",45,44),("AR",81,77),("AS",2,67),("AT",30,7),("AU",31,22),("AV",98,12),("AW",25,26),("AX",4,41),("AY",82,41),("AZ",51,12),("BA",18,62),("BB",71,40),("BC",29,20),("BD",17,49),("BE",11,55),("BF",51,48),("BG",67,64),("BH",30,5),("BI",31,57),("BJ",24,17),("BK",58,70),("BL",76,50),("BM",14,28),("BN",54,25),("BO",94,13),("BP",14,67),("BQ",28,56),("BR",43,23),("BS",40,31),("BT",19,4),("BU",77,33),("BV",55,15),("BW",56,29),("BX",76,7),("BY",15,21),("BZ",71,76),("CA",44,39),("CB",35,50),("CC",92,57),("CD",91,15),("CE",50,19),("CF",97,36),("CG",8,38),("CH",27,64),("CI",37,66),("CJ",22,43),("CK",58,8),("CL",59,56),("CM",58,49),("CN",54,23),("CO",39,66),("CP",65,3),("CQ",45,44),("CR",7,14),("CS",74,57),("CT",58,9),("CU",11,16),("CV",89,65)]

mkRandCities :: Int -> Seed -> Route
mkRandCities size seed = zip3 (take size cityNames) (take size seeds) (take size (filter (79>) (drop size seeds)))
  where
    seeds = randomRs (2,98) (mkStdGen seed)

mkRandRoute :: Route -> MkRand Route
mkRandRoute cities seed = head cities : shuffle seed (tail cities)

distance :: (City, City) -> Distance
distance ((n1,x1,y1),(n2,x2,y2)) = sqrt $ fromIntegral ((x2 - x1)^2 + (y2 - y1)^2)

fitness :: Fitness Route
fitness route = round $ sum $ map distance (legs route)

legs :: Route -> [(City, City)]
legs route = zip route (tail route ++ [head route])

tspMutate :: Mutation Route
tspMutate size seeds [route] = map (head route :) (mutationBySwap size seeds [tail route])

gaForTSP :: Route -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Route)]
gaForTSP cities maxGenerations popSize (xProb,mProb) = geneticAlgorithm maxGenerations popSize (length cities - 1) (mkRandRoute cities) fitness rselection
  (permCrossover, 2, 1, xProb) (tspMutate, 1, 1, mProb) orderedMerge dontStop

main :: IO ()
main = do
  let cities = cities20
  let seed = 123456
  let maxGen = 5000
  let popSize = 500
  let xProb = 0.4
  let mProb = 0.4
  let solutions = gaForTSP cities maxGen popSize (xProb,mProb) seed
  writeToFile solutions 1
  let best = foldl min (1000000000, []) (map head solutions)
  print $ fst best
  print $ snd best