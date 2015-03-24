import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
data Cell = On | Off deriving (Show, Eq)

main = example
example = putStrLn . show . parse $ "((. X (. . .)) . (X . (. X X)))"

parse :: String -> Tree Cell
parse = parseTree . filter (/= ' ')

parseTree :: String -> Tree Cell
parseTree = makeTree . segment 

makeTree :: (String, String, String) -> Tree Cell
makeTree (ls,x:[],rs) = Node (charToCell x) (makeBranch ls) (makeBranch rs)
  where
    makeBranch (x:xs)
      | x == '('  = parseTree (x:xs)
      | otherwise = Node (charToCell x) Empty Empty

segment :: String -> (String, String, String)
segment = fst . runState extractSegments . tail . init
  where
    extractSegments = do
      let extractBranchSt = state extractBranch
      left  <- extractBranchSt
      value <- extractBranchSt
      right <- extractBranchSt
      return (left, value, right)

extractBranch :: String -> (String, String)
extractBranch (i:input)
  | i /= '('  = (i:[], input)
  | otherwise = getTree (i:[], input) 1
  where
    getTree (parsed, rest) 0 = (reverse parsed, rest)
    getTree (parsed, r:rest) n
      | r == '('  = getTree (r:parsed, rest) (n+1)
      | r == ')'  = getTree (r:parsed, rest) (n-1)
      | otherwise = getTree (r:parsed, rest) n

charToCell :: Char -> Cell
charToCell '.' = Off
charToCell 'X' = On
