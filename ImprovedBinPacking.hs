import GeneticAlgorithm
import GAUtility
import Data.List
import System.Random

type Weight = Int
type Bin = [Weight]
type Bins = [Bin]
type NumBins = Int

-- assume: first bin is lightes, last is heaviest. First item in each bin is lightes, end item is heaviest
mkRandWeights :: Int -> (Int,Int) -> Seed -> [Weight]
mkRandWeights size (lowerB,upperB) seed = take size (randomRs (lowerB,upperB) (mkStdGen seed))

-- representation: each item in the starting weights must appear exactly once in the binPacked
-- this means the concat of all binpacks must be permutation of the initial weights 
mkRandBins :: NumBins -> [Weight] -> MkRand Bins
mkRandBins numBins weights seed = orderBins $ foldl addWeightToBin binsInit (zip binIndicies weights) 
  where
    binsInit = replicate numBins []
    binIndicies = randomRs (0,numBins-1) (mkStdGen seed)
    addWeightToBin bins (i,w) = updateItemAt i (w : bins !! i) bins

orderBins :: Bins -> Bins 
orderBins bins = map snd (sortBy binOrder (zip (map sum bins) (map sort bins)))
  where
    binOrder (w1,b1) (w2,b2)       
      | w1 < w2 = LT
      | w1 > w2 = GT
      | w1 == w2 && b1 < b2 = LT
      | w1 == w2 && b1 > b2 = GT
      | otherwise = EQ

calcTotalWeight :: Bins -> Weight
calcTotalWeight bins = sum (map sum bins)

fitness :: Fitness Bins
fitness bins = maximum binWeights - minimum binWeights
  where
    binWeights = map sum bins

stop :: Int -> Stop Bins
stop minFitness ebins = (fst . head) ebins <= minFitness

binMutate :: Mutation Bins
binMutate _ s [bins] = [orderBins (binA : tail (init bins) ++ [binB])]
  where
    (binA, binB) = balancedWasteSwap (head bins) (last bins)

-- where binB > binA
balancedWasteSwap :: Bin -> Bin -> (Bin,Bin)
balancedWasteSwap binA binB
  | (a,b) == (0,0) = (binA,binB)
  | a == 0 = (b:binA, binB \\ [b])
  | otherwise = ((b:binA) \\ [a], (a:binB) \\ [b])
  where
    target = sum binB - sum binA
    midTarget = div target 2

    potentialSwaps = [abs (y - x - midTarget) | y <- binB, x <- binA, y - x < target, y > x]
    potentialTransfer = [abs (y - midTarget) | y <- binB, y < target]

    -- returns singleton list so take head
    bestTransfer = head [y | y <- binB, abs (y - midTarget) == minimum potentialTransfer]
    (bestSwapA, bestSwapB) = head [(x,y) | x <- binA, y <- binB, abs (y - x - midTarget) == minimum potentialSwaps]

    (a,b)
      | null potentialSwaps && null potentialTransfer = (0,0)
      | null potentialSwaps = (0,bestTransfer)
      | null potentialTransfer = (bestSwapA, bestSwapB)
      | abs (bestTransfer - midTarget) <= abs (bestSwapB - bestSwapA - midTarget) = (0,bestTransfer)
      | otherwise = (bestSwapA, bestSwapB)

binCrossover :: Crossover Bins
binCrossover numWeights seed [xs,ys] = [orderBins childBins]
  where
    binSizes = map length ys
    [childWeights] = permCrossover numWeights seed [concat xs, concat ys]
    childBins = splitList childWeights binSizes

gaForBP :: [Weight] -> NumBins -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Bins)]
gaForBP weights numBins maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize numBins (RandChrom (mkRandBins numBins weights)) fitness rselection
  (binCrossover, 2, 1, xProb) (binMutate, 1, 1, mProb) orderedMerge (stop minWaste)
    where
      totalSum = sum weights
      average = div totalSum numBins
      minWaste = numBins - (totalSum - (average * numBins)) 

main :: IO ()
main = do
  let weights = mkRandWeights 1000 (10,50) 34
  let numBins = 15
  let seed = 123456
  let maxGen = 1000
  let popSize = 500
  let xProb = 0.0
  let mProb = 0.9
  putStrLn " --  All Generations --"
  let solutions = gaForBP weights numBins maxGen popSize (xProb, mProb) seed
  let window = 3
  let myprint (x, ys, n) = do
                        putStrLn ("Generation " ++ show x)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
                        print n
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"  
  print (length solutions)