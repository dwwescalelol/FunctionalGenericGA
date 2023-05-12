import CaseStudies.NQueen.GAforQueens ( Board, NQueen )

-- The chess drawing package must be included in the master file.
-- anything above 20 put \resizebox{0.8\textwidth}{!}{...}

beforeBoard :: String
beforeBoard = 
  "\\" ++  "begin{figure}\n" ++
  "\\" ++ "centering\n" ++
  "\\" ++ "noindent \n" 

afterBoard :: NQueen -> String
afterBoard n =
  "\\" ++ "caption{\\" ++ "label{fig:Queens" ++ show n ++"} Solution for " ++ show n ++ "queens} \n" ++
  "\\" ++ "end{figure}\n"

chessLabels :: Board -> [Char]
chessLabels b = init $ concatMap (\(f,r) -> ('q':f : show(r) ++ ",")) (zip ['a' .. 'z'] b)

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
  "\\" ++ "chessboard[\n" ++
  "       style=" ++ show n ++ "x"++ show n ++ ", \n" ++
  "       setpieces={\n" ++
  "       " ++ "[" ++ chessLabels b ++ "]\n" ++
  "       },  \n" ++
  "       padding=1ex,\n]\n" 
          where n = length b

displayBoard :: Board -> String
displayBoard b = beforeBoard ++ showBoard b ++ afterBoard (length b)

latexQueens :: Board -> IO ()
latexQueens b = writeFile ("LatexFigures" ++ "\\" ++ "drawQueens" ++ show (length b) ++ ".tex") (displayBoard b)

qSolution25 :: Board
qSolution25 =  [16,5,12,9,18,22,24,6,20,10,23,4,14,11,17,3,21,0,7,1,15,2,19,13,8]
