import Data.List
import Text.Printf

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans

type Point = (Int, Int)
fI = fromIntegral

solve :: [Point] -> Double
solve points = perimeter . toPairs . solvePath $ points
  where
    toPairs (p1:p0:ps)  = (p1, p0) : toPairs (p0:ps)
    toPairs _           = []
    perimeter hull      = foldl (addPath) 0.0 hull
    addPath p x         = p + uncurry distance x

solvePath :: [Point] -> [Point]
solvePath points = foldl nextInHull [x] sorted
  where
    x:sorted                    = sortPoints points
    nextInHull [x0] p           = p:[x0]
    nextInHull (x2:x1:xs) p
      | clockwise x1 x2 p == LT = p:x2:x1:xs
      | otherwise               = nextInHull (x1:xs) p

sortPoints :: [Point] -> [Point]
sortPoints points = y:start ++ end ++ [y]
  where
    (end, y:start)  = span (/= yMax) sorted
    sorted          = sortBy (clockwise yMax) points
    yMax            = maximumBy (\(_,y0) (_,y1) -> compare y0 y1) points

distance :: Point -> Point -> Double
distance (x0,y0) (x1,y1) = sqrt $ (fI x1 - fI x0)**2 + (fI y1 - fI y0)**2

clockwise :: Point -> Point -> Point -> Ordering
clockwise (x0,y0) (x1,y1) (x2,y2) = ((x1-x0)*(y2-y0) - (x2-x0)*(y1-y0)) `compare` 0
