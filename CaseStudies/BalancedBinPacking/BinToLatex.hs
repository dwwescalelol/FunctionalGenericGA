import System.Random (Random(randomRs), Random(randomR), mkStdGen)
import Data.List
import Text.Printf

type NumBins = Int
type Weight  = Int
type Bin = [Weight]
type Bins = [Bin]

beforeBins :: String
beforeBins = 
  "\\" ++ "begin{tikzpicture}[x=0.15cm,y=0.15cm] \n \n" ++
    "      % help lines \n" ++
    "%       \\" ++ "draw[step=0.1,help lines,black!20] (1,1) grid (99,79); \n" ++
    "      % axis \n" ++
    "%      \\" ++ "draw[thick,->] (0,0) -- (100,0); \n" ++
    "%       \\" ++ "draw[thick,->] (0,0) -- (0,80); \n \n"  :: String

drawLabels :: [Int] -> [(Double, Double)] -> String
drawLabels bs cs = beforeLabels ++
  "      %Points \n" ++ 
  "      \\" ++ "foreach " ++ "\\" ++ "Point/\\" ++ "PointLabel in \n      {" ++ 
       foldr1 join (zipWith (curry plotPoint) bs cs) ++"} \n" ++ afterLabels
  where
    plotPoint (b, (x, y)) = "("++ printf "%.1f" x ++ "," ++ printf "%.1f" y ++ ")/" ++ show b 
    join s t = s++", " ++ t
    beforeLabels = ""
    afterLabels = "      \\" ++ "draw[fill=black] \\" ++ "Point circle (0.01) node[above right] {$\\" ++
      "PointLabel$}; \n \n"

afterBins :: String
afterBins = "\\" ++ "end{tikzpicture} \n"

fillcolour :: (Show a, Num a) => a -> String
fillcolour w = "red!" ++ show(20 +w) ++ "!white"

drawColour :: String
drawColour = "black"

drawWeight :: (Int, (Double, Double), ( Double, Double)) -> String
drawWeight (w, (c1x,c1y), (c2x, c2y)) = 
  "      \\" ++ "filldraw[fill=" ++ fillcolour w ++ ", draw=" ++ 
  drawColour ++ "] (" ++  printf "%.2f" c1x ++ 
  "," ++ printf "%.2f" c1y  ++ ") rectangle (" ++ printf "%.2f" c2x ++ "," ++ 
  printf "%.2f" c2y ++ "); \n"  

dim :: Int -> (Double, Double)
dim w = (h*1.25, h) 
  where h = sqrt (fromIntegral w/1.56)

drawOneBin :: (Double, Double) -> (Double, Double) -> [Int] -> String
drawOneBin (xscale, yscale) (cx, cy) bs = concatMap drawWeight rectangles ++ drawLabels bs (map (\(x,y) -> (x-2,y)) centres)
  where
    sbs = reverse (sort bs)
    sizes = map ((\(x, y) -> (x*xscale, y*yscale)) . dim)  sbs
    centres  = zip (repeat cx) (scanl (+) cy (map snd (init sizes)))
    rectangles = zipWith ($) (zipWith recCoordinates bs centres) sizes  
    recCoordinates label (cx,cy) (len, wid) = (label, (cx-len/2, cy), (cx+len/2, cy+wid))

drawBin :: (Double, Double) -> [Int] ->  String
drawBin (cx, cy) bs = beforeBins ++ drawOneBin (1,1) (cx, cy) bs ++ afterBins

drawBins ::[[Int]]-> String 
drawBins bs = beforeBins ++
  concat (zipWith (drawOneBin (1.4*xscale, 1.2*yscale)) centres sbs) ++ 
  drawTotalWeights (map sum bs) (zip xCentres (replicate nB  (-6.0))) ++
  afterBins
  where 
    s = map sum bs
    sbs = map (reverse.sort) bs
    nB = length bs
    heaviest = maximum s
    yscale = 1.0 :: Double -- 78.0 / fromIntegral heaviest :: Double
    sep = 2
    half = sep `div` 2
    binWidth = 98.0/fromIntegral nB
    xscale = binWidth / fst (dim heaviest) :: Double
    halfWidth = binWidth /2
    xCentres = scanl (+) halfWidth (replicate (nB -1) binWidth)
    centres = zip xCentres [0,0..]
    drawTotalWeights bs centres = drawLabels bs (map (\(x,y) -> (x-2,y)) centres)

dr1 :: String
dr1 = drawBins [[36,29,34],[46,50],[11,44,42],[24,40,16,18],[35,37,27],[30,11,37,19],[35,22,39],[35,31,32],[46,29,22],[46,42]]

main :: IO ()
main = do
  putStrLn dr1