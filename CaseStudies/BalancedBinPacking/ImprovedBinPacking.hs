import GAFramework
import CaseStudies.BalancedBinPacking.GAforBinPacking hiding(gaForBP,binMutate,fitness,main)
import Data.List

mkRandOrderedBins :: NumBins -> [Weight] -> MkRand Bins
mkRandOrderedBins numBins weights seed = orderBins (mkRandBins numBins weights seed)

orderBins :: Bins -> Bins 
orderBins bins = map snd (sortBy binOrder (zip (map sum bins) (map sort bins)))
  where
    binOrder (w1,b1) (w2,b2)       
      | w1 < w2 = LT
      | w1 > w2 = GT
      | w1 == w2 && b1 < b2 = LT
      | w1 == w2 && b1 > b2 = GT
      | otherwise = EQ

fitness :: Fitness Bins
fitness bins = sum (last bins) - sum (head bins)

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

gaForBP :: [Weight] -> NumBins -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Bins)]
gaForBP weights numBins maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize numBins (mkRandOrderedBins numBins weights) fitness rselection
  (binCrossover, 2, 1, xProb) (binMutate, 1, 1, mProb) orderedMerge (stopFit 3)
    where
      totalSum = sum weights
      average = div totalSum numBins
      minWaste = numBins - (totalSum - (average * numBins)) 

main :: IO ()
main = do
  let weights = mkRandWeights 5000 (5,50) 123456
  let numBins = 500
  let seed = 123456
  let maxGen = 500
  let popSize = 100
  let xProb = 0.0
  let mProb = 1
  let solutions = gaForBP weights numBins maxGen popSize (xProb, mProb) seed
  display solutions 1