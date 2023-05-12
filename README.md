# FunctionalGenericGA
FunctionalGenericGA is a Haskell generic Genetic Algorithm framework which allows for rapid development of GA solutions for a wide range of problmes. It comes with a variaty of pre defined components that can be deployed to any problem. 
The project comes with demonstartions of how to deploy the framework in the form of GA solutions to the following problems:
+ N Queens
+ Traveling Salesman Problem
+ Balanced Bin Packing 

The project comes with 3 improved exampels of GAs, showing the steps of how to improve a GA solution to the same problems listed above. For each example there is also a transpiler file that generates latex code for each case study solution type.

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
To display the outputs to the console, a generic type has been defined such that either a user can implement their own display or use one of the pre defined displays. Display takes a population of evaluated chromsomes and a window, then returns a monad to display the contents to the console.
```haskell
type DisplayPop c = Show c => [Pop (Eval c)] -> Int -> IO ()
```
There is a pre defined function called display that returns windows of each generation, displaying first the fitness and then the chromosome. Each generation has its generation number and population size displayed.

In our example, we can call the generic display function within our main function. Here is where we define the mutation/crossover probabilities, the population size, max generations, seed and any other problem specific values needed for the GA
```haskell
main :: IO ()
main = do
  let n = 20
  let seed = 123456
  let maxGen = 50
  let popSize = 500
  let xProb = 0.7
  let mProb = 0.15
  let solutions = gaForQueens n maxGen popSize (xProb, mProb) seed
  display solutions 5
```
When called it gives the results:
```ghci
ghci> main  
 --  All Generations --
Generation 0, Size 500
4   [3,17,14,6,13,20,10,7,2,16,11,4,1,19,5,8,18,12,15,9]
4   [19,5,1,4,20,15,9,3,8,12,18,2,17,10,16,7,6,11,13,14]   
6   [1,3,15,6,7,14,4,13,12,16,5,19,11,10,8,18,2,17,20,9]   
6   [9,2,6,13,20,15,1,14,4,12,18,3,7,19,11,8,16,5,10,17]   
6   [9,6,14,20,19,5,8,10,4,11,3,16,2,12,15,7,17,1,13,18]
Generation 1, Size 500
4   [3,17,14,6,13,20,10,7,2,16,11,4,1,19,5,8,18,12,15,9]   
4   [4,15,3,10,13,5,12,7,14,2,20,8,11,16,1,17,19,9,6,18]   
4   [19,5,1,4,20,15,9,3,8,12,18,2,17,10,16,7,6,11,13,14]   
6   [6,12,11,13,8,17,3,10,18,16,4,20,7,9,14,15,5,2,19,1]   
6   [9,1,15,10,18,14,5,11,6,2,4,13,20,16,19,12,8,17,7,3] 
...
...
Generation 37, Size 500
0   [5,12,15,1,20,6,9,13,17,3,18,7,14,10,2,19,16,4,11,8]   
1   [5,12,15,1,20,6,9,13,17,3,18,7,14,10,2,19,11,16,4,8]   
1   [5,12,15,18,20,6,9,13,17,1,3,7,14,10,2,19,16,11,4,8]   
1   [5,12,15,18,20,6,9,13,17,1,3,7,14,10,2,19,16,11,4,8]   
1   [5,15,12,3,20,6,9,13,17,1,18,7,19,10,16,14,11,2,4,8]   
 Last Generation, 37
```

