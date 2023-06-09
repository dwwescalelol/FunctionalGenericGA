module GAFramework where 
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

tournementSelection :: Selection c
tournementSelection seed evalPop = map (fitterChrom evalPop) (pairIndices indicies)
  where
    indicies = randomRs (0, length evalPop - 1) (mkStdGen seed)
    fitterChrom evaledPop [i,j] 
      | fst c1 < fst c2 = c1
      | otherwise = c2
        where [c1,c2] = [evalPop !! i, evalPop !! j]
    pairIndices [] = []
    pairIndices [_] = []
    pairIndices (x:y:zs) = [x, y] : pairIndices zs

eliteSelection :: Selection c
eliteSelection _ = concat . repeat

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

distinct :: Eq c => Pop (Eval c) -> Pop (Eval c)
distinct [] = []
distinct [x] = [x]
distinct (x:y:xs)
  | x == y = distinct (x:xs)
  | otherwise = x : distinct (y:xs)

--
-- Stop
--

dontStop :: Stop c
dontStop = const False

stopFit :: Int -> Stop c
stopFit f evalPop = null evalPop || fst (head evalPop) <= f

--
-- Population
--

initPop :: Size -> MkRand c -> Seed -> Pop c
initPop popSize mkRandChrom seed = take popSize $ map mkRandChrom seeds
  where
    seeds = randomRs (0 , maxBound :: Seed) (mkStdGen seed)

evalPop :: Ord c => Fitness c -> Pop c -> Pop (Eval c)
evalPop fitFunc pop = sortPop $ zip (map fitFunc pop) pop

sortPop :: Pop (Eval c) -> Pop (Eval c)
sortPop = sortBy (\(fitnessA, _) (fitnessB, _) -> compare fitnessA fitnessB)

--
-- GA
--

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
    grannies = sortPop $ drop (numParentsCrossover + numParentsMutation) selectedParents

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

--
-- Display
--
type DisplayPop c = (Ord c, Show c) => [Pop (Eval c)] -> Int -> IO ()

display :: DisplayPop c
display solutions window = do
  putStrLn " --  All Generations --"
  let myprint (x, ys, z) = do
                        putStrLn ("Generation " ++ show x ++ ", Size " ++ show z)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn (" Last Generation, " ++ show (length solutions - 1))
  let best = minimum (map head solutions)
  print $ fst best
  print $ snd best

writeToFile :: DisplayPop c
writeToFile solutions window = do
  let filename = "Results.txt"
  writeFile filename ""
  let myprint (x, ys, z) = do
                        appendFile filename ("Generation " ++ show x ++ ", Size " ++ show z ++ "\n")
                        mapM_ (\(f, bs) -> appendFile filename (show f ++ "   " ++ show bs ++ "\n")) ys
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  appendFile filename (" Last Generation, " ++ show (length solutions - 1) ++ "\n")
  let best = minimum (map head solutions)
  print $ fst best
  print $ snd best
  print ("Last Generation: " ++ show (length solutions - 1))
