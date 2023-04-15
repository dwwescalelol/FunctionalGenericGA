import GeneticAlgorithm
import GAUtility
import Data.List ( sort, sortBy )
import System.Random (randomR, mkStdGen, Random (randomRs))

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
binMutate _ _ [bins] = [orderBins (binA : tail (init bins) ++ [binB])]
  where
    (binA, binB) = balancedWasteSwap (head bins) (last bins)

-- where binB > binA
balancedWasteSwap :: Bin -> Bin -> (Bin,Bin)
balancedWasteSwap binA binB
  | minSwap < transferIndex = (updateItemAt swapIndexA (binB !! swapIndexB) binA, updateItemAt swapIndexB (binA !! swapIndexA) binB) 
  | otherwise = (transferItem : binA, transferedBinB)
  where
    target = div (sum binB - sum binA) 2
    evaledAllSwaps = map (\x -> map (\y -> abs(y - x - target)) binB) binA
    indexedSwaps = concatMap (\(x,y) -> zip3 x (repeat y) [0..]) (zip evaledAllSwaps [0..])
    (minSwap,swapIndexA,swapIndexB) = minimum indexedSwaps

    evaledAllTransfers =  map (\x -> abs(x - target)) binB
    (minTransfer,transferIndex) = minimum (zip evaledAllTransfers [0..])
    (transferItem,transferedBinB) = removeItemAt transferIndex binB

binCrossover :: Crossover Bins
binCrossover numWeights seed [xs,ys] = [childBins]
  where
    binSizes = map length ys
    [childWeights] = permCrossover numWeights seed [concat xs, concat ys]
    childBins = splitList childWeights binSizes

binCrossover2 :: Crossover Bins
binCrossover2 numWeights seed [xs,ys] = [childA,childB]
  where
    (binSizesA,binSizesB) = (map length xs, map length ys)
    [childWeightsA,childWeightsB] = permCrossover2 numWeights seed [concat xs, concat ys]
    childA = splitList childWeightsA binSizesA
    childB = splitList childWeightsB binSizesB

naiveBinMutate :: Mutation Bins
naiveBinMutate numBins seed [bins] = [splitList (head updatedWeights) windows]
  where
    windows = map length bins
    updatedWeights = mutationBySwap numBins seed [concat bins]

gaForBP :: [Weight] -> NumBins -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Bins)]
gaForBP weights numBins maxGenerations popSize (xProb,mProb)
  = gga maxGenerations popSize numBins (mkRandBins numBins weights) fitness rselection
  (binCrossover, 2, 1, xProb) (binMutate, 1, 1, mProb) orderedMerge (stop minWaste)
    where
      totalSum = sum weights
      average = div totalSum numBins
      minWaste = numBins - (totalSum - (average * numBins)) 

main :: IO ()
main = do
  let weights = mkRandWeights 1000 (1,400) 34
  let numBins = 30
  let seed = 2342345
  let maxGen = 40
  let popSize = 1000
  let xProb = 0.1
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