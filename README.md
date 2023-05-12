# FunctionalGenericGA
FunctionalGenericGA is a Haskell generic Genetic Algorithm framework which allows for rapid development of GA solutions for a wide range of problmes. It comes with a variaty of pre defined components that can be deployed to any problem. 
The project comes with demonstartions of how to deploy the framework in the form of GA solutions to the following problems:
+ N Queens
+ Traveling Salesman Problem
+ Balanced Bin Packing 

The project comes with 3 improved exampels of GAs, showing the steps of how to improve a GA solution to the same problems listed above.

This framework is unorthadox in its approach to GAs in how the populations evolve, the population is split into three. The population to merge, to crossover and to persist into the next generation. In each evolution only crossover or mutation can be applied to a chromosome, not both as seen in more orthadox frameworks.
A new operator merge is defined which controlls the behavior in how the 3 populations are merged together in each evolution.

# Using The Framework
N Queens will be used as an example of how to deploy the framework.
When creating a solutoin the following components must be defined by the user as they are problem specific:
+ Representation
+ Random Chromosome
+ Fitness
All other components are not problem specific and thus pre defined components are avalible for use.
## Representation
The GA is defined in terms of the generic type 'c', when the user deploys the framework 'c' must be defined. 
In this example the chromosome is represented as the type Board.
```haskell
type NQueen = Int
type Row = Int
type Column = Int
type Board = [Column]
```
## Random Chromosome
Now that the representation has been defined, the method of creating new instances of a chromosome must be defined by using MkRand.
```haskell
type MkRand c = Seed -> c
```
In this example the fucntion requires an extra input for the number of queens.
```haskell
randQueen :: NQueen -> MkRand Board
randQueen size seed = shuffle seed [1..size]
```
## Fitness
The merit of each chromosome must be evaluated by defining a fitness function.
```haskell
type FitnessValue = Int
type Fitness c = c -> FitnessValue
```
In this example the number of collisions on the board are calculated.
```haskell
type Board2D = [(Row, Column)]

qfitness :: Fitness Board
qfitness b = length $ concatMap (\(q:qs) -> filter (takes q) qs) ((init . tails) (to2DBoard b))
to2DBoard :: Board -> Board2D
to2DBoard = zip [1..]
takes :: (Row, Column) -> (Row, Column) -> Bool 
takes (r1,c1) (r2,c2) = abs (r1-r2) == abs (c1-c2)
```

## Crossover 
In order to evolve the population a crossover function must be chosen. In crossover either one or two offspring are outputted, to accomidate this and for further inginuity crossover is defined such that it takes any amount of parents and can produce any amount of children.
```haskell
type Crossover c = ChromSize -> Seed -> Pop c -> Pop c
```
In the framework the following have been defined:
+ OnePoint
+ Permutation Crossover

In the example permutation crossover will be used.

## Mutation
In order to evolve the population a mutation function must be chosen. In mutation usually one chromosome is mutated and outputted, however to accomidate for potential insight mutation is defined such that it takes any amount of parents and can produce any amount of children.
```haskell
type Mutation c = ChromSize -> Seed -> Pop c -> Pop c
```
In the framework the following have been defined:
+ Mutation by Swap (Allele Swap)

In the example mutation by swap will be used.

## Selection
To select the next generation a selection method must be chosen. Selection returns an unbounded list. The list becomes bounded in the evolution function in the GA. 
```haskell
type Selection c = Seed -> Pop (Eval c) -> Pop (Eval c)
```
In the framework the following have been defined:
+ Roulette
+ Tournement
+ Elite

In this example roulette will be used.

## Merge
This operator defines the behavior of how the populations of crossover, merge and the persisting populaiton will interact. The function takes two population and merges them to create a new population.
```haskell
type Merge c = Pop (Eval c) -> Pop (Eval c) -> Pop (Eval c)
```
In the framework the following have been defined:
+ Concat Merge
+ Ordered Merge (removes some duplicates)
+ Distinct Ordered Merge (removes all duplicates)

In this example ordered merge will be used.

## Stop
The stop function describes when the GA should stop evolving. 
```haskell
type Stop c = Pop (Eval c) -> Bool
```
In the framework the following have been defined:
+ Dont Stop
+ Fitness Stop

In this example we will use fitness stop, where fitness to stop at is 0 as this represents a perfect solution.

## GA
Here all of the components are used to create an instance of the GA. The GA returns a list of all of the populations produced by a run of the GA.
```haskell
geneticAlgorithm :: Ord c => MaxGenerations -> PopSize -> ChromSize -> MkRand c -> Fitness c -> Selection c ->
  (Crossover c, Int, Int, Prob) -> (Mutation c, Int, Int, Prob) -> Merge c -> Stop c -> Seed -> [Pop (Eval c)]
```

In the example of N Queens, an insatnce called gaForQueens can be created. It takes the problem specific parameters of N Queen. In this the number of parents and children are defined for both mutation and crossover. 

```haskell
gaForQueens :: NQueen -> MaxGenerations -> PopSize -> (Prob,Prob) -> Seed -> [Pop (Eval Board)]
gaForQueens nQueens maxGenerations popSize (xProb,mProb)
  = geneticAlgorithm maxGenerations popSize nQueens (randQueen nQueens) qfitness rselection
  (permCrossover, 2, 1, xProb) (mutationBySwap, 1, 1, mProb) orderedMerge (stopFit 0)
```

## Displaying
To display the outputs to the console, a quick main monad can be defined. 

```haskell
main :: IO ()
main = do
  let n = 20
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.6
  let mProb = 0.2
  putStrLn " --  All Generations --"
  let solutions = gaForQueens n maxGen popSize (xProb, mProb) seed
  let window = 12
  let myprint (x, ys, z) = do
                        putStrLn ("Generation " ++ show x ++ ", Size " ++ show z)
                        mapM_ (putStrLn . (\ (f, bs) -> show f ++ "   " ++ show bs)) ys
  mapM_ myprint (zip3 [0..] (map (take window) solutions) (map length solutions))
  putStrLn " --  Last Generation --"  
  print (length solutions)

```

