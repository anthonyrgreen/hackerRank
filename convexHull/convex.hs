import Data.List
import Data.Monoid ((<>))
import Text.Printf

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map ((\[x, y] -> (x, y)) . map (read::String->Int) . words) . lines $ content
    ans = solve points
  printf "%.1f\n" ans

type Point = (Int, Int)
fI = fromIntegral

solve :: [(Int, Int)] -> Double
solve points = perimeter . toPairs . solvePath $ points
  where
    toPairs (p1:p0:ps)  = (p1, p0) : toPairs (p0:ps)
    toPairs _           = []
    perimeter hull      = foldl (addPath) 0.0 hull
    addPath p x         = p + uncurry distance x

solvePath :: [Point] -> [Point]
solvePath points = upperHull ++ tail lowerHull
  where
    upperHull     = orientedHull LT sortedPoints
    lowerHull     = reverse . orientedHull GT $ sortedPoints
    sortedPoints  = sortBy pointComparison points

orientedHull :: Ordering -> [Point] -> [Point]
orientedHull orientation (p:points) = reverse . foldl nextInHull [p] $ points
  where
    nextInHull [p] x                          = x:[p]
    nextInHull (p2:p1:ps) x
      | rightHandRule p1 p2 x == orientation  = x:p2:p1:ps
      | otherwise                             = nextInHull (p1:ps) x

pointComparison :: Point -> Point -> Ordering
pointComparison (x0,y0) (x1,y1) = x0 `compare` x1 <> y1 `compare` y0

rightHandRule :: Point -> Point -> Point -> Ordering
rightHandRule (x0,y0) (x1,y1) (x2,y2) = z `compare` 0
  where
    z = (x1-x0)*(y2-y0) - (y1-y0)*(x2-x0)

distance :: Point -> Point -> Double
distance (x0,y0) (x1,y1) = sqrt $ (fI x1 - fI x0)**2 + (fI y1 - fI y0)**2
