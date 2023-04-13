module GeneticAlgorithm where 
import System.Random (randomR, mkStdGen, Random (randomRs))
import Data.List
import GAUtility

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
mutationBySwap size seed [xs]
  | i == j = [xs]
  | i < j = [mutationBySwap' (i,j) xs]
  | otherwise = [mutationBySwap' (j,i) xs]
    where 
      rs = randomRs (0,size-1) (mkStdGen seed)
      i = head rs
      j = rs !! 1

-- where i is 0..n-1 and i < j
mutationBySwap' :: (Index, Index) -> [a] -> [a]
mutationBySwap' (i,j) xs = l1 ++ [xs !! j] ++ l2 ++ [xs !! i] ++ l3
  where
    l1 = take i xs
    l2 = drop (succ i) (take j xs)
    l3 = drop (succ j) xs

mutationBySwap2' :: Index -> Pop [a] -> Pop [a]
mutationBySwap2' i [c1,c2] = [m1,m2]
  where
    m1 = take i c1 ++ [c2 !! i] ++ drop (i+1) c1
    m2 = take i c2 ++ [c1 !! i] ++ drop (i+1) c2

--
-- Merge
--

concatMerge :: Merge c
concatMerge = (++)

orderedMerge :: Ord a => [a] -> [a] -> [a]
orderedMerge xs [] = xs
orderedMerge [] ys = ys
orderedMerge (x:xs) (y:ys)
  | x <= y = x : orderedMerge xs (y:ys)
  | otherwise = y : orderedMerge (x:xs) ys
  
--
-- Population
--

initPop :: Size -> Size -> MkRand c -> Seed -> Pop c
initPop popSize chromSize mkRandChrom seed = map mkRandChrom seeds
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
  childrenOfCrossover `merge` childrenOfMutation `merge` grannies

  where
    numParentsCrossover = xNumChildren * (floor $ (fromIntegral popSize * crossoverProb) / fromIntegral xNumChildren)
    numParentsMutation = mNumChildren * (floor $ (fromIntegral popSize * mutationProb) / fromIntegral mNumChildren)
    numGrannies = popSize - (numParentsCrossover `div` xNumChildren + numParentsMutation `div` mNumChildren)
    numParents = (xNumParents * numParentsCrossover) + (mNumParents * numParentsMutation) + numGrannies

    selectedParents = take numParents (selection (seeds !! 0) evaledPop)
    parentsForCrossover = take numParentsCrossover $ drop numParentsMutation selectedParents
    parentsForMutation = take numParentsMutation selectedParents
    grannies = sortPop $ drop (numParentsCrossover + numParentsMutation) selectedParents

    childrenOfCrossover = evalPop fitness $ concatMap (crossover chromSize (seeds !! 1)) (segment xNumParents (map snd parentsForCrossover))
    childrenOfMutation = evalPop fitness $ concatMap (mutation chromSize (seeds !! 2)) (segment mNumParents (map snd parentsForMutation))

    seeds = randomRs (0,maxBound) (mkStdGen seed)

geneticAlgorithm :: Ord c => MaxGenerations -> PopSize -> ChromSize -> MkRand c -> Fitness c -> Selection c ->
  (Crossover c, Int, Int, Prob) -> (Mutation c, Int, Int, Prob) -> Merge c -> Stop c -> Seed -> (Pop (Eval c), Pop (Eval c))
geneticAlgorithm maxGenerations popSize chromSize randChrom fitness selection
  (crossover, xNumParents, xNumChildren, crossoverProb) (mutation, mNumParents, mNumChildren, mutationProb) merge stop seed =
    geneticAlgorithm' maxGenerations seed (initialPop,[])

    where
      initialPop = evalPop fitness (initPop popSize chromSize randChrom (seeds !! (maxGenerations + 1)))
      seeds = randomRs (0,maxBound) (mkStdGen seed)

      geneticAlgorithm' numGeneration currentSeed (evaluatedPop,hallOfFame)
        | numGeneration == 0 || stop evaluatedPop = (evaluatedPop, hallOfFame) 
        | otherwise = geneticAlgorithm' (numGeneration - 1) (seeds !! (numGeneration -1))
          ((evolve popSize chromSize fitness selection (crossover, xNumParents, xNumChildren, crossoverProb) 
            (mutation, mNumParents, mNumChildren, mutationProb) merge seed evaluatedPop), (take 5 evaluatedPop ++ hallOfFame))
