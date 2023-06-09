module CaseStudies.BalancedBinPacking.GAforBinPacking where
import GAFramework
import GAUtility
import System.Random

type Weight = Int
type Bin = [Weight]
type Bins = [Bin]
type NumBins = Int

mkRandWeights :: Int -> (Int,Int) -> Seed -> [Weight]
mkRandWeights size (lowerB,upperB) seed = take size (randomRs (lowerB,upperB) (mkStdGen seed))

mkRandBins :: NumBins -> [Weight] -> MkRand Bins
mkRandBins numBins weights seed = foldl addWeightToBin binsInit (zip binIndicies weights) 
  where
    binsInit = replicate numBins []
    binIndicies = randomRs (0,numBins-1) (mkStdGen seed)
    addWeightToBin bins (i,w) = updateItemAt i (w : bins !! i) bins

fitness :: Int -> Fitness Bins
fitness average bins = sum (map (abs . (average -) . sum) bins)

stop :: Int -> Stop Bins
stop minFitness ebins = (fst . head) ebins <= minFitness
  
binMutate :: Mutation Bins
binMutate numBins seed [bins]
  | index1 == index2 = [bins]
  | null bins1 && null bins2 = [bins]
  | null bins1 = updateBins (transferWeight bins2 newSeed)
  | null bins2 = updateBins (transferWeight bins1 newSeed)
  | otherwise = updateBins (swapWeight (bins1,bins2) newSeed)
  where
    (bins1, bins2) = (bins !! index1, bins !! index2)
    [index1, index2, newSeed] = take 3 $ randomRs (0,numBins-1) (mkStdGen seed)
    updateBins (bin1, bin2) = [updateItemAt index1 bin1 (updateItemAt index2 bin2 bins)]

transferWeight :: Bin -> Seed -> (Bin, Bin)
transferWeight bin seed = ([weight], reducedBin)
  where
    (weight, reducedBin) = removeItemAt i bin
    i = fst $ randomR (0, length bin-1) (mkStdGen seed)

swapWeight :: (Bin,Bin) -> Seed -> (Bin, Bin)
swapWeight (bin1,bin2) seed = (updateItemAt i (bin2 !! j) bin1, updateItemAt j (bin1 !! i) bin2)
  where
    [seed1, seed2] = take 2 $ randomRs (minBound,maxBound) (mkStdGen seed)
    i = fst $ randomR (0,length bin1-1) (mkStdGen seed1)
    j = fst $ randomR (0,length bin2-1) (mkStdGen seed2)

unpack :: Bins -> [Weight]
unpack = concat

repack :: [Size] -> Bin -> Bins
repack [s] bin = [take s bin]
repack (s:ss)  bin = take s bin : repack ss (drop s bin)

naiveBinMutate :: Mutation Bins
naiveBinMutate n seed [bins] = map (repack binSizes) (mutationBySwap (length bins) seed [unpack bins])
    where
        binSizes = map length bins

binCrossover :: Crossover Bins
binCrossover numWeights seed [bins1,bins2] = [childBins]
  where
    binSizes = map length bins2
    [childWeights] = permCrossover numWeights seed [unpack bins1, unpack bins2]
    childBins = repack (map length bins2) childWeights 

gaForBP :: [Weight] -> NumBins -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Bins)]
gaForBP weights numBins maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize numBins (mkRandBins numBins weights) (fitness average) rselection
  (binCrossover, 2, 1, xProb) (binMutate, 1, 1, mProb) orderedMerge (stop minWaste)
    where
      totalSum = sum weights
      average = div totalSum numBins
      minWaste = numBins - (totalSum - (average * numBins)) 

weights12,weights20,weights30,weights50,weights100 :: [Weight]
weights12 = [41,39,20,29,46,6,27,17,38,30,40,41]
weights20 = [41,39,20,29,46,6,27,17,38,30,40,41,40,43,26,33,29,36,21,49]
weights30 = [41,39,20,29,46,6,27,17,38,30,40,41,40,43,26,33,29,36,21,49,27,33,34,5,32,29,10,11,27,49]
weights50 = [41,39,20,29,46,6,27,17,38,30,40,41,40,43,26,33,29,36,21,49,27,33,34,5,32,29,10,11,27,49,36,13,48,29,24,34,24,49,45,5,48,20,5,33,34,37,28,7,21,47]
weights100 = [41,39,20,29,46,6,27,17,38,30,40,41,40,43,26,33,29,36,21,49,27,33,34,5,32,29,10,11,27,49,36,13,48,29,24,34,24,49,45,5,48,20,5,33,34,37,28,7,21,47,21,10,32,20,14,6,33,34,38,27,15,17,33,17,31,46,43,22,16,15,18,45,10,47,38,31,30,36,11,30,40,41,25,42,48,10,13,14,44,28,49,14,11,24,19,24,48,19,31,40]

main :: IO ()
main = do
  let weights = weights20
  let numBins = 6
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.4
  let mProb = 0.4
  let solutions = gaForBP weights numBins maxGen popSize (xProb,mProb) seed
  writeToFile solutions 4
