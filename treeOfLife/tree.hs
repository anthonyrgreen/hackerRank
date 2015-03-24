import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
data Cell = On | Off deriving (Show, Eq)

parseTree :: String -> Tree Cell
parseTree = parse . segment . filter (/= ' ')

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
  | i == '('    = foo' (i:[], input) 1
  | otherwise   = (i:[], input)
  where
    foo' (parsed, rest) 0 = (reverse parsed, rest)
    foo' (parsed, r:rest) n
      | r == '('  = foo' (r:parsed, rest) (n+1)
      | r == ')'  = foo' (r:parsed, rest) (n-1)
      | otherwise = foo' (r:parsed, rest) n

parse :: (String, String, String) -> Tree Cell
parse (ls, x:[], rs) = Node (charToCell x) (parse' ls) (parse' rs)
  where
    parse' (x:xs)
      | x == '('  = parseTree (x:xs)
      | otherwise = Node (charToCell x) Empty Empty

charToCell :: Char -> Cell
charToCell '.' = Off
charToCell 'X' = On
