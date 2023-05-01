module GAUtility where
import System.Random
import GHC.Stack

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs [x] = []
adjacentPairs (x:y:xs) = (x,y) : adjacentPairs xs

shuffle :: Eq a => Int -> [a] -> [a]
shuffle seed xs = shuffle' seeds xs
  where
    seeds = randomRs (0, length xs - 1) (mkStdGen seed)
    shuffle' _ [] = []
    shuffle' _ [x] = [x]
    shuffle' (s:ss) qs = q : shuffle' ss remaining
      where
        index = s `mod` length qs
        q = qs !! index
        remaining = take index qs ++ drop (index + 1) qs

segment :: Int -> [a] -> [[a]]
segment n xs
  | null ys = ys
  | length (last ys) == n = ys
  | otherwise = init ys
  where 
    ys = segment' n xs

segment' :: Int -> [a] -> [[a]]
segment' n [] = []
segment' n xs = take n xs : segment' n (drop n xs)

indexOf :: Ord a => [a] -> a -> Int
indexOf xs v = length $ takeWhile (<v) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs

removeItemAt :: Int -> [a] -> (a, [a])
removeItemAt i xs = (xs !! i, take i xs ++ drop (i + 1) xs)

updateItemAt :: Int -> a -> [a] -> [a]
updateItemAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

swapItemAt :: (Int, Int) -> [a] -> [a]
swapItemAt (i,j) xs
  | i < j = swapItemAt' (i,j) xs
  | i == j = xs
  | otherwise =  swapItemAt' (j,i) xs
  where
    swapItemAt' (i,j) xs = l1 ++ [xs !! j] ++ l2 ++ [xs !! i] ++ l3
      where
        l1 = take i xs
        l2 = drop (succ i) (take j xs)
        l3 = drop (succ j) xs

splitList :: [a] -> [Int] -> [[a]]
splitList xs [y] = [take y xs]
splitList xs (y:ys) = take y xs : splitList (drop y xs) ys

safeElement :: HasCallStack => [a] -> Int -> a
safeElement xs i
  | i >= 0 && i < length xs = xs !! i
  | otherwise = error $ "Index " ++ show i ++ " out of bounds in safeElement at " ++ prettyCallStack callStack