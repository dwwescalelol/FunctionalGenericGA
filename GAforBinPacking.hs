import GeneticAlgorithm
import GAUtility
import System.Random (randomR, mkStdGen, Random (randomRs))



type Weight = Int
type Bin = [Weight]
type Bins = [Bin]
type NumBins = Int

-- testing
weights :: [Weight]
weights = [25,12,33,10,47,9,21,43,30,20,10,10]
binsa :: Bins
binsa = mkRandBins 4 weights 324
binsb :: Bins
binsb = mkRandBins 4 weights 3789123

mkRandWeights :: Int -> (Int,Int) -> Seed -> [Weight]
mkRandWeights size (lowerB,upperB) seed = take size (randomRs (lowerB,upperB) (mkStdGen seed))

-- representation: each item in the starting weights must appear exactly once in the binPacked
-- this means the concat of all binpacks must be permutation of the initial weights 
mkRandBins :: NumBins -> [Weight] -> MkRand Bins
mkRandBins numBins weights seed =  foldl addWeightToBin binsInit (zip binIndicies weights) 
  where
    binsInit = replicate numBins []
    binIndicies = randomRs (0,numBins-1) (mkStdGen seed)
    addWeightToBin bins (i,w) = take i bins ++  [w : bins !! i] ++ drop (i + 1) bins

calcTotalWeight :: Bins -> Weight
calcTotalWeight bins = sum (map sum bins)

fitness :: Int -> Fitness Bins
fitness average bins = sum (map (abs . (average -) . sum) bins)

stop :: Int -> Stop Bins
stop minFitness ebins = (fst . head) ebins <= minFitness

binMutate :: Mutation Bins
binMutate numBins seed [bins]
  | binIndexA == binIndexB = [bins]
  | otherwise = [updateItemAt binIndexA newBinA (updateItemAt binIndexB newBinB bins)]
  where
    [binIndexA, binIndexB, newSeed] = take 3 $ randomRs (0,numBins-1) (mkStdGen seed)
    (newBinA, newBinB) = swap1Waste (bins !! binIndexA) (bins !! binIndexB) newSeed

swap1Waste :: Bin -> Bin -> Seed -> (Bin, Bin)
swap1Waste binA binB seed
  | null binA && null binB = ([],[])
  | null binB = ([itemA],removedA)
  | null binA = ([itemB],removedB)
  | otherwise = (updateItemAt i (binB !! j) binA, updateItemAt j (binA !! i) binB)
  where
    seeds = randomRs (minBound,maxBound) (mkStdGen seed)
    i = fst $ randomR (0,length binA-1) (mkStdGen (seeds !! 0))
    j = fst $ randomR (0,length binB-1) (mkStdGen (seeds !! 1))
    (itemA, removedA) = removeItemAt i binA
    (itemB, removedB) = removeItemAt j binB

binCrossover :: Crossover Bins
binCrossover numWeights seed [xs,ys] = [childBins]
  where
    binSizes = map length ys
    [childWeights] = permCrossover numWeights seed [concat xs, concat ys]
    childBins = splitList childWeights binSizes

naiveBinMutate :: Mutation Bins
naiveBinMutate numBins seed [bins] = [splitList (head updatedWeights) windows]
  where
    windows = map length bins
    updatedWeights = mutationBySwap numBins seed [concat bins]
    
gaForBP :: [Weight] -> NumBins -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> (Pop (Eval Bins), Pop (Eval Bins))
gaForBP weights numBins maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize numBins (mkRandBins numBins weights) (fitness average) eliteSelection
  (binCrossover, 2, 1, xProb) (binMutate, 1, 1, mProb) orderedMerge (stop minWaste)
    where
      totalSum = sum weights
      average = div totalSum numBins
      minWaste = numBins - (totalSum - (average * numBins)) 

main :: IO ()
main = do
  let weights = mkRandWeights 30 (2,50) 4234
  let numBins = 7
  let seed = 4892
  let maxGen = 20
  let popSize = 6000
  let xProb = 0.4
  let mProb = 0.6
  let (solution,hallOfFame) = gaForBP weights numBins maxGen popSize (xProb,mProb) seed
  mapM_ print (take 20 hallOfFame)
  print "-----------------------------------------------------"
  print $ length solution
  print $ head solution
  print $ map sum (snd $ head solution)