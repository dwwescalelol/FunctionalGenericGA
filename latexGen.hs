type City = (String,Int,Int)
type Route = [City]
type Distance = Double

cities1 :: Route
cities1 = [("A",86,72),("B",63,46),("C",17,21),("D",66,4),("E",76,47)]
cities2 :: Route
cities2 = [("A",86,22),("B",63,50),("C",17,46),("D",66,28),("E",76,49),("F",47,40),("G",11,63),("H",36,27),("I",29,14),("J",2,9)]
cities3 :: Route
cities3 = [("A",86,68),("B",63,40),("C",17,61),("D",66,6),("E",76,17),("F",47,23),("G",11,35),("H",36,30),("I",29,20),("J",2,63),
           ("K",7,14),("L",90,25),("M",26,24),("N",88,4),("O",89,28),("P",90,13),("Q",53,71),("R",7,35),("S",54,25),("T",34,1)]
cities4 :: Route
cities4 = [("A",36,4),("B",63,30),("C",22,20),("D",66,63),("E",76,14),("F",47,25),("G",11,24),("H",86,35),("I",29,28),("J",5,10),
           ("K",10,71),("L",78,45),("M",25,63),("N",88,3),("O",89,73),("P",90,51),("Q",53,40),("R",7,59),("S",54,50),("T",34,51),
           ("U",38,41),("V",80,20),("W",91,31),("X",42,75),("Y",57,12),("Z",23,43)]

plotPoint :: City -> String
plotPoint (s, x, y) = show (x,y) ++ "/" ++s 

join :: String -> String -> String
join s t = s++"," ++ t

plotEdge :: (Show a1, Show a2, Show a3, Show a4) => ((a5, a1, a2), (a6, a3, a4)) -> String
plotEdge ((n1, x1, y1), (n2, x2, y2)) = 
    "     \\" ++ "draw[dashed] (" ++show x1 ++ ","++ show y1 ++")--("++show x2 ++ "," ++ show y2++ "); \n" 

drawPath :: Route -> [Char]
drawPath cs = beforeNodes ++ concatMap plotEdge ps ++ 
 "      %Points \n" ++ 
  "      \\" ++ "foreach " ++ "\\" ++ "Point/\\" ++ "PointLabel in \n      {" ++ 
       foldr1 join (map (plotPoint.fst) ps) ++"} \n" ++ afterNodes
       where
        ps = zip cs (tail cs ++ [head cs])

drawNodes :: [City] -> String
drawNodes cs = beforeNodes ++
  "      %Points \n" ++ 
  "      \\" ++ "foreach " ++ "\\" ++ "Point/\\" ++ "PointLabel in \n      {" ++ 
       foldr1 join (map plotPoint cs) ++"} \n" ++ afterNodes

beforeNodes :: String
beforeNodes = 
  "\\" ++ "begin{tikzpicture}[x=0.15cm,y=0.12cm] \n \n" ++
    "      % help lines \n" ++
    "       \\" ++ "draw[step=0.1,help lines,black!20] (1,1) grid (99,79); \n" ++
    "      % axis \n" ++
    "      \\" ++ "draw[thick,->] (0,0) -- (100,0); \n" ++
    "       \\" ++ "draw[thick,->] (0,0) -- (0,80); \n \n"  :: String

afterNodes :: String
afterNodes =
 "      \\" ++ "draw[fill=black] \\" ++ "Point circle (0.5) node[above right] {$\\" ++
   "PointLabel$}; \n \n" ++
       "\\" ++ "end{tikzpicture} \n"

main :: IO ()
main = do
  putStrLn (drawPath [("U",38,41),("T",34,51),("X",42,75),("M",25,63),("K",10,71),("R",7,59),("Z",23,43),("I",29,28),("C",22,20),("G",11,24),("J",5,10),("A",36,4),("Y",57,12),("E",76,14),("N",88,3),("V",80,20),("W",91,31),("H",86,35),("L",78,45),("P",90,51),("O",89,73),("D",66,63),("S",54,50),("Q",53,40),("B",63,30),("F",47,25)])