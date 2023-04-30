nQueens' :: Int -> Int -> [Board2D]
nQueens' n 1  = map (\c -> [(1,c)]) [1 ..n]
nQueens' n r  = [ (r,c) : qs | c <- [1..n], qs <- nQueens' n (r-1), 
                  and (map (not . takes (r,c) ) qs) ]

type Row = Int; 
type Column = Int
type Board2D = [(Row, Column)]; 

takes :: (Row, Column) -> (Row, Column) -> Bool
takes (r1, c1) (r2, c2) =   
    r1 == r2 || c1 == c2 || abs (r1-r2) == abs (c1-c2)

nQueens :: Int -> [Board2D]
nQueens n = nQueens' n n