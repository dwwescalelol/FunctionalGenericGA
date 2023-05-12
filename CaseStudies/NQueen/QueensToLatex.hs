import CaseStudies.NQueen.GAforQueens ( Board, NQueen )

-- The chess drawing package must be included in the master file.
-- anything above 20 put \resizebox{0.8\textwidth}{!}{...}

beforeBoard :: String
beforeBoard = 
  "\\" ++  "begin{figure}\n" ++
  "\\" ++ "centering\n" 

afterBoard :: NQueen -> String
afterBoard n =
  "\\" ++ "caption{\\" ++ "label{fig:Queens" ++ show n ++"} Solution for " ++ show n ++ "queens} \n" ++
  "\\" ++ "end{figure}\n"

chessLabels :: Board -> String
chessLabels b = foldr1 join (map (\(f,r) -> ('q':f : (show r))) (zip ['a' .. 'z'] b))
  where
    join s t = s++ "," ++ t

showBoard :: Board -> String
showBoard b = 
  "\\" ++ "storechessboardstyle{" ++ show n ++ "x"++ show n ++ "}{\n"++
  "       maxfield=y" ++show n ++ ",\n" ++
  "       borderwidth=1mm, \n"  ++ 
  "       % color=white,\n"  ++
  "       %colorwhitebackfields,\n" ++
  "       % color=black,\n" ++
  "       %$ colorblackbackfields, \n" ++
  "       % blackfieldmaskcolor=white, \n" ++
  "       % whitepiececolor=cyan, \n" ++
  "       % whitepiecemaskcolor=red, \n" ++
  "       % blackpiececolor=cyan, \n" ++
  "       % blackpiecemaskcolor=blue, \n" ++
  "       addfontcolors, \n" ++
  "       pgfstyle=border, \n" ++
  "       color=white,  \n" ++
  "       markregion=a1-y" ++ show n ++ ",  \n" ++
  "       showmover=false,  \n" ++
  "       hlabelwidth=18pt, \n" ++
  "       vlabellift=10pt}  \n" ++
  "       \\" ++ "chessboard[   \n" ++
  "       style=" ++ show n ++ "x"++ show n ++ ", \n" ++
  "       setpieces={" ++ chessLabels b ++ "\n" ++
  "       },  \n" ++
  "       padding=1ex, \n" ++
  "       ] \n" ++
  "} \n"
          where n = length b

resize :: Board  -> String
resize b =   "\\" ++ "resizebox{" ++ show r ++ "\\" ++ "textwidth}{!}{"
  where
      n = length b
      r | n < 20 = 1.0 :: Double
        | otherwise =  1- (fromIntegral (n-20) * 0.04) 

displayBoard :: Board -> String
displayBoard b = beforeBoard ++ resize b ++ showBoard b ++ afterBoard (length b)

latexQueens :: Board -> IO ()
latexQueens b = writeFile ("LatexFigures" ++ "\\" ++ "drawQueens" ++ show (length b) ++ ".tex") (displayBoard fb)
  where fb = map (1+) b

qSolution25, qSolution25a :: [Int]
qSolution25a =  [16,5,12,9,18,22,24,6,20,10,23,4,14,11,17,3,21,0,7,1,15,2,19,13,8]
qSolution25 =  [11,22,15,21,4,7,14,12,6,24,5,18,1,19,13,16,25,20,8,23,2,9,3,17,10]