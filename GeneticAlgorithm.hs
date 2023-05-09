module GeneticAlgorithm where 
import System.Random (randomR, mkStdGen, Random (randomRs))
import Data.List ( (\\), sortBy )
import GAUtility ( indexOf, segment, swapItemAt )
import Data.Tuple (swap)

type Seed = Int
type Size = Int
type Index = Int
type Prob = Double
type MaxGenerations = Int
type PopSize = Int
type ChromSize = Int
type FitnessValue = Int

type Eval c = (FitnessValue, c)
type Pop c = [c]

type Selection c = Seed -> Pop (Eval c) -> Pop (Eval c)
type Crossover c = ChromSize -> Seed -> Pop c -> Pop c
type Mutation c = ChromSize -> Seed -> Pop c -> Pop c
type Fitness c = c -> FitnessValue
type Merge c = Pop (Eval c) -> Pop (Eval c) -> Pop (Eval c)
type Stop c = Pop (Eval c) -> Bool
type MkRand c = Seed -> c

--
-- Selection
--

-- assume population is sorted
-- Selection returns an infinate list, must use take when called
rselection :: Selection c
rselection seed evalPop = map (evalPop !!) indices
  where
    highestFitness = 1 + fst (last evalPop)
    accFitness = scanl1 (+) $ map ((highestFitness -) . fst) evalPop
    highestAccFitness = last accFitness
    randomNumbers = randomRs (0, highestAccFitness - 1) (mkStdGen seed)
    indices = map (indexOf accFitness) randomNumbers

tournementSelection :: Ord c => Selection c
tournementSelection seed evalPop = map (fitterChrom evalPop) (segment 2 indicies)
  where
    indicies = take (length evalPop * 2) $ randomRs (0, length evalPop - 1) (mkStdGen seed)
    fitterChrom evaledPop [i,j] = min (evaledPop !! i) (evaledPop !! j)

eliteSelection :: Selection c
eliteSelection seed evalPop = evalPop

--
-- Crossover
--

onePointCrossover :: Crossover [a]
onePointCrossover size seed [xs,ys] = [take i xs ++ drop i ys]
  where i = mod seed size

permCrossover :: Eq a => Crossover [a]
permCrossover size seed [xs,ys] = [cs ++ (ys \\ cs)]
  where
    cs = take i xs
    i = mod seed size

permCrossover2 :: Eq a => Crossover [a]
permCrossover2 size seed [xs,ys] = [csA ++ (ys \\ csA),csB ++ (xs \\ csB)]
  where
    csA = take i xs
    csB = take i ys
    i = mod seed size

--
-- Mutation
--

mutationBySwap :: Mutation [a]
mutationBySwap size seed [xs] = [swapItemAt (i,j) xs]
    where 
      rs = randomRs (0,size-1) (mkStdGen seed)
      i = head rs
      j = rs !! 1

--
-- Merge
--

concatMerge :: Merge c
concatMerge = (++)

orderedMerge :: Ord c => Merge c
orderedMerge xs [] = xs
orderedMerge [] ys = ys
orderedMerge (x:xs) (y:ys)
  | x <= y = x : orderedMerge xs (y:ys)
  | otherwise = y : orderedMerge (x:xs) ys

distinctOrderedMerge :: Ord c => Merge c
distinctOrderedMerge xs ys = orderedMerge (distinct xs) (distinct ys)

--
-- Stop
--

dontStop :: Stop c
dontStop = const False

stopFit :: Int -> Stop c
stopFit f evalPop = null evalPop || fst (head evalPop) == f

--
-- Population
--

initPop :: Size -> MkRand c -> Seed -> Pop c
initPop popSize mkRandChrom seed = map mkRandChrom seeds
  where
    rnds = randomRs (0 , maxBound :: Seed) (mkStdGen seed)
    seeds = take popSize rnds

evalPop :: Ord c => Fitness c -> Pop c -> Pop (Eval c)
evalPop fitFunc pop = distinct $ mySort $ zip (map fitFunc pop) pop

sortPop :: Pop (Eval c) -> Pop (Eval c)
sortPop = sortBy (\(fitnessA, _) (fitnessB, _) -> compare fitnessA fitnessB)

mySort :: Ord c => Pop (Eval c) -> Pop (Eval c)
mySort = sortBy myCompare
  where
    myCompare (f1,c1) (f2,c2)
      | f1 < f2 = LT
      | f1 > f2 = GT
      | f1 == f2 && c1 < c2 = LT
      | f1 == f2 && c1 > c2 = GT
      | otherwise = EQ

distinct :: Eq c => Pop (Eval c) -> Pop (Eval c)
distinct [] = []
distinct [x] = [x]
distinct (x:y:xs)
  | x == y = distinct (x:xs)
  | otherwise = x : distinct (y:xs)

evolve :: Ord c => PopSize -> ChromSize -> Fitness c -> Selection c ->
  (Crossover c, Int, Int, Prob) -> (Mutation c, Int, Int, Prob) -> Merge c -> Seed -> Pop (Eval c) -> Pop (Eval c)
evolve popSize chromSize fitness selection (crossover, xNumParents, xNumChildren, crossoverProb) 
  (mutation, mNumParents, mNumChildren, mutationProb) merge seed evaledPop = 
  take popSize $ childrenOfCrossover `merge` childrenOfMutation `merge` grannies
  where
    numParentsCrossover = xNumParents * floor ((fromIntegral popSize * crossoverProb) / fromIntegral xNumChildren)
    numParentsMutation = mNumParents * floor ((fromIntegral popSize * mutationProb) / fromIntegral mNumChildren)
    numGrannies = popSize - (numParentsCrossover `div` xNumChildren + numParentsMutation `div` mNumChildren)
    numParents = (xNumParents * numParentsCrossover) + (mNumParents * numParentsMutation) + numGrannies

    selectedParents = take numParents (selection (seeds !! 0) evaledPop)
    parentsForMutation = take numParentsMutation selectedParents
    parentsForCrossover = take numParentsCrossover $ drop numParentsMutation selectedParents
    grannies = distinct $ mySort $ drop (numParentsCrossover + numParentsMutation) selectedParents

    childrenOfCrossover = evalPop fitness $ concatMap (crossover chromSize (seeds !! 1)) (segment xNumParents (map snd parentsForCrossover))
    childrenOfMutation = evalPop fitness $ concatMap (mutation chromSize (seeds !! 2)) (segment mNumParents (map snd parentsForMutation))

    seeds = randomRs (0,maxBound) (mkStdGen seed)

loop :: [Pop (Eval c) -> Pop (Eval c)] -> Stop c -> Pop (Eval c) -> [Pop (Eval c)]
loop [] stop x = [x] 
loop (f:fs) stop x
  | stop x = [x]
  | otherwise =  x: loop fs stop (f x)

geneticAlgorithm :: Ord c => MaxGenerations -> PopSize -> ChromSize -> MkRand c -> Fitness c -> Selection c ->
  (Crossover c, Int, Int, Prob) -> (Mutation c, Int, Int, Prob) -> Merge c -> Stop c -> Seed -> [Pop (Eval c)]
geneticAlgorithm maxGenerations popSize chromSize mkRandChrom fitness selection (crossover, xNumParents, xNumChildren, xProb) (mutation, mNumParents, mNumChildren, mProb) merge  stop seed
   = map (take popSize) evolvedPop
       where
       seeds = randomRs (0, maxBound) (mkStdGen seed)
       initialPop = evalPop fitness (initPop popSize mkRandChrom (seeds !! (maxGenerations + 1)))
       evolvedPop = loop (map (evolve popSize chromSize fitness selection (crossover, xNumParents, xNumChildren, xProb) (mutation, mNumParents, mNumChildren, mProb) merge) (take maxGenerations seeds)) stop initialPop
