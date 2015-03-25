import Data.Array.Unboxed as U
import Data.List

main = do
  putStrLn "How many iterations of sierpinski would you like to see?\n"
  putStrLn "Enter an integer between 0 and 5, inclusive\n"
  n <- readLn
  putStr $ printArray $ sierpinski n

-- Note that throughout this code tl refers to top-left, br to bottom-right, and so on.

bl = (1,1)
tr = (63,32)
height = 32
width = 63
    
data Orientation = Corner | BotLeft | TopRight deriving (Eq)
type Index = (Int, Int)
type SquareBounds = (Index, Index)

printArray :: UArray Index Char -> String
printArray array = intercalate "\n" rows 
  where
    rows = [[array ! (i, j) | i <- [1..width]] | j <- [height,height-1..1]]

sierpinski :: Int -> UArray Index Char
sierpinski n = amap toDisplay $ straighten sierpinski' 
  where
    toDisplay   = \x -> if x then '1' else '_'
    sierpinski' = filledInBox `without` safety (cutOut n BotLeft (bl, tr)) 
      where
        safety = filter (/= (width,1))

straighten :: UArray Index Bool -> UArray Index Bool
straighten arr = array (bl, tr) new
  where
    new = [((newX x y,y), arr ! (x,y)) | (x,y) <- range (bl,tr)] 
    newX x y = (((x-1) + (y-1)) `mod` width) + 1

without :: UArray Index Bool -> [Index] -> UArray Index Bool
without original negated = original // [(x, False) | x <- negated]

filledInBox :: UArray (Int, Int) Bool
filledInBox = U.array (bl, tr) [(x, True) | x <- range (bl, tr)]

cutOut :: Int -> Orientation -> SquareBounds -> [Index]
cutOut recursionLvl orientation (bl,tr)
  | recursionLvl < 0        = []
  | orientation == BotLeft  = cutTopRightDiagonal (bl,tr) ++ recursiveRemovals
  | orientation == Corner   = recursiveRemovals
  | otherwise               = []
  where
    recursiveRemovals             = tlRec ++ brRec ++ blRec
    tlRec                         = cutOut (recursionLvl-1) Corner topLeft
    brRec                         = cutOut (recursionLvl-1) Corner botRight
    blRec                         = cutOut (recursionLvl-1) BotLeft botLeft
    (topLeft, botLeft, botRight)  = sector (bl,tr)

-- Takes the bottom-left and top-right indices of a square and returns the 
-- respective bottom-left and top-right indices of the top-left, bottom-left,
-- and bottom-right sub-squares
sector :: SquareBounds -> (SquareBounds, SquareBounds, SquareBounds)
sector ((x1, y1), (x2, y2))   = (topLeft, botLeft, botRight) 
  where
    topLeft     = ((x1, halfY + 1),   (halfX, y2))
    botLeft     = ((x1, y1),          (halfX, halfY))
    botRight    = ((halfX + 1, y1),   (x2, halfY - 1))
    halfX       = x1 + ((x2-x1) `div` 2)
    halfY       = y1 + ((y2-y1) `div` 2)

-- Takes the bottom-left and top-right indices of a sqare, and returns a list
-- of all squares resting above the top-left to bottom-right diagonal to
-- to nullify.
cutTopRightDiagonal :: SquareBounds -> [Index]
cutTopRightDiagonal (bl,tr) = extra:negatedSquares
  where
    extra           = (fst tr, snd bl)
    negatedSquares  = filter (isTopRight (bl,tr)) $ range (bl,tr)
    isTopRight ((x1, y1), (x2, y2)) (x, y) = x' + y' > 1
      where 
        x' = fromIntegral (x - x1) / fromIntegral (x2 - x1)
        y' = fromIntegral (y - y1) / fromIntegral (y2 - y1)
