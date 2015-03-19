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

printArray :: UArray (Int, Int) Char -> String
printArray array = intercalate "\n" rows where
  rows = [[array ! (i, j) | i <- [1..width]] | j <- [height,height-1..1]]

sierpinski :: Int -> UArray (Int, Int) Char
sierpinski n = amap toDisplay $ straighten sierpinski' where
  toDisplay = \x -> if x then '1' else '_'
  sierpinski' = blackBox // safety (cutOut n BotLeft (bl, tr)) where
  blackBox :: UArray (Int, Int) Bool
  blackBox = U.array (bl, tr) [(x, True) | x <- range (bl, tr)]
  safety = filter (\(ix, _) -> ix /= (width,1))
  straighten arr = array (bl,tr) straightened where
    straightened = [((newX x y,y), arr ! (x,y)) | (x,y) <- range (bl,tr)] where
      newX x y = (((x-1) + (y-1)) `mod` width) + 1

cutOut :: Int -> Orientation -> -- recursion level, orientation 
          ((Int, Int), (Int, Int)) -> -- bot left, top right
          [((Int, Int), Bool)] -- indices to change
cutOut (-1) _ _ = []
cutOut n pos (bl,tr)
  | pos == BotLeft = cutTopRight (bl,tr) ++ recursiveRemoves
  | pos == Corner  = recursiveRemoves
  | otherwise      = []
  where
    recursiveRemoves              = tlRec ++ brRec ++ blRec
    tlRec                         = cutOut (n-1) Corner topLeft
    brRec                         = cutOut (n-1) Corner botRight
    blRec                         = cutOut (n-1) BotLeft botLeft
    (topLeft, botLeft, botRight)  = sector (bl,tr)
    sector ((x1, y1), (x2, y2))   = (topLeft, botLeft, botRight) where
      topLeft     = ((x1, halfY + 1), (halfX, y2))
      botLeft     = ((x1, y1), (halfX, halfY))
      botRight    = ((halfX + 1, y1), (x2, halfY - 1))
      halfX       = x1 + ((x2-x1) `div` 2)
      halfY       = y1 + ((y2-y1) `div` 2)
    cutTopRight (bl,tr) = extra:[(x, False) | x <- negatedSquares] where
      extra           = ((fst tr, snd bl), False)
      negatedSquares  = filter (isTopRight (bl,tr)) $ range (bl,tr)
      isTopRight ((x1, y1), (x2, y2)) (x, y) = x' + y' > 1 where
        x' = fromIntegral (x - x1) / fromIntegral (x2 - x1)
        y' = fromIntegral (y - y1) / fromIntegral (y2 - y1)
