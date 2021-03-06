import Control.Monad.State (state, runState)
import Data.Vector (Vector, generate, (!))
import Data.Bits (testBit, (.|.), shift)

main = do
  ruleNum <- readLn :: IO Int
  initialTree <- getLine >>= return . parseTree
  trees <- return $ steps ruleNum initialTree
  numQueries <- readLn :: IO Int
  nQueries numQueries 0 trees
  
-- Interaction functionality

nQueries :: Int -> Int -> [Tree Cell] -> IO ()
nQueries 0 _           _     = return ()
nQueries n currentStep trees = do 
  (nextStep, directions) <- getLine >>= return . parseQuery
  let currentStep' = currentStep + nextStep
  let currentTree = trees !! currentStep'
  putStrLn . cellToString . getEltAt directions $ currentTree
  nQueries (n-1) currentStep' trees

cellToString :: Cell -> String
cellToString True  = "X"
cellToString False = "."

-- Cellular Automaton functionality

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)
data Direction = L | R deriving (Show, Eq)
type Cell = Bool

steps :: Int -> Tree Cell -> [Tree Cell]
steps ruleNum t = t : (steps ruleNum $ step ruleNum t)

step :: Int -> Tree Cell -> Tree Cell
step ruleNum tree = step' False tree
  where
    step' _         Empty         = Empty
    step' upperNode (Node x l r)  = Node x' (step' x l) (step' x r)
      where 
        x' = getRule ruleNum [upperNode, getElt l, x, getElt r]

getRule :: Int -> [Cell] -> Cell
getRule ruleNum = (!) (getRules ruleNum) . foldl checkBit 0 . zip [3,2..]
  where
    checkBit n (i, cell)
      | cell      = n .|. (shift 1 i)
      | otherwise = n

getRules :: Int -> Vector Bool
getRules = generate 16 . testBit

getElt :: Tree Cell -> Cell
getElt Empty        = False
getElt (Node x _ _) = x

getEltAt :: [Direction] -> Tree Cell -> Cell
getEltAt _      Empty               = getElt Empty
getEltAt []     tree                = getElt tree
getEltAt (d:ds) (Node _ left right) = case d of
  L -> getEltAt ds left
  R -> getEltAt ds right

-- Parsing functions:

parseQuery :: String -> (Int, [Direction])
parseQuery = (\[x,y] -> (read x, parseDirections y)) . words

parseDirections :: String -> [Direction]
parseDirections (']':ds) = []
parseDirections ('[':ds) = parseDirections ds
parseDirections ('<':ds) = L : parseDirections ds
parseDirections ('>':ds) = R : parseDirections ds

parseTree :: String -> Tree Cell
parseTree = head . foldl stackManip []

stackManip :: [Tree Cell] -> Char -> [Tree Cell]
stackManip ts x
  | x == ' '  = ts
  | x == '('  = ts
  | x == ')'  = formTree ts
  | otherwise = toTree x : ts
  where
    toTree '.' = Node False Empty Empty
    toTree 'X' = Node True  Empty Empty

formTree :: [Tree Cell] -> [Tree Cell]
formTree (r:c:l:ts) = (Node (node c) l r) : ts
  where
    node (Node x _ _) = x
