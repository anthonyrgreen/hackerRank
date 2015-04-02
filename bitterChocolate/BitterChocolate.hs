-- IDEA :: We can think of the set of all possible games of bitterChocolate
-- as being a directed acyclic graph, where nodes are partially eaten choc-
-- olate bars represented as triples (row1,row2,row3) corresponding to row1
-- remaining squares in the last row of chocolate, row2 remaining squares in
-- the middle, and row3 in the top. Hence, the node (4, 2, 1) represents:
--  __ 
-- |__|__
-- |__|__|__ __
-- |__|__|__|__|
--
-- node1 has an edge pointed at node2 iff it is possible to transform node1
-- into node2 by a single bite, ie turn. Hence, precisely the following 
-- nodes point to the above node:
--
--    (4,2,2)         (4,3,1)         (4,4,1)         (5..25,2,1)
--  __ __           __              __              __ 
-- |__|__|         |__|__ __       |__|__ __ __    |__|__
-- |__|__|__ __    |__|__|__|__    |__|__|__|__|   |__|__|__ __ __
-- |__|__|__|__|   |__|__|__|__|   |__|__|__|__|   |__|__|__|__|__|...
-- where the ... refers to the fact that the bottom row (row1) may extend as
-- far as the original length of the chocolate bar (in this game, 25).
-- You lose this game when you're forced to eat the bitter bottom-left
-- square: (1,0,0). We're trying to figure out which initial configurations
-- inevitable loss for the player who finds him or herself forced to play
-- from them, and which initial configurations will always result in
-- victory. This is all assuming optimal play from both sides.
--
-- We should think of this problem in the following recursive manner:
-- node1 is a winning configuration if it points to any losing config-
-- uration. Otherwise, it is a losing configuration. We know that (1,0,0)
-- is a losing configuration, so that will be our base case.
--
-- We will flow backwards through our graph from child to root, recursively,
-- starting with (1,0,0) as our first child. Treating winning as True and 
-- losing as False, we will set the value of every root to be 
-- root' = root || not child
-- since a root is a winner if one of its children is a guaranteed loser.
--
-- We reverse the flow of our acyclic graph using the function
-- rootConfigurations, defined by:
-- rootConfigurations node2 :=  [node1 | node1 points at node2]
-- so that, in the above example,
-- rootConfigurations (4,2,1) = 
--      [(4,2,2),(4,3,1),(4,4,1),(5,2,1),(6,2,1),..,(25,2,1)]
-- We begin with all nodes set to losing.
-- We maintain an "inspection stack" whose first value is (1,0,0)
-- At every step of the algorithm, we pop the top off the inspection stack.
-- and calculate "roots = rootConfigurations top". We only want to inspect
-- roots which haven't already been inspected and found to be winning, so
-- we declare roots' to be only those nodes which are currently losing.
-- We add roots' to the inspection stack.
-- If top is a losing node we convert all of roots' to winning nodes.
-- If top is a winning node, we do nothing, since all nodes are initially
-- losing.
-- We terminate this process when we have no more values left in the
-- inspection stack.

module BitterChocolate (victoryPossible, rootConfigurations) where

--import Data.Array.Unboxed (UArray, (//), (!), array, range)
--import Data.Array.Unboxed (range)
import Data.Array (Array, (//), (!), array, range)
import Data.Set (insert, notMember, Set, empty)
import Data.Ix (range)
import Debug.Trace (traceShow, trace)
import Data.Heap (union, view, singleton, MinHeap, fromList, toList)
import Data.Ix (Ix)

maxVal = 25

type Config = (Int, Int, Int)
type Array' = Data.Array.Array Config (Bool, Maybe Config)

-- c1 < c2 if we can get to c2 via c1->c1'->c1''->...->c2. 
-- c1 == c2 if there is no path.
-- (this is with regard to our flow-reversed graph)

victoryPossible :: Config -> (Bool, [Config])
victoryPossible c = case solutionMatrix ! c of
  (True,   Just x)  -> (True, [x])
  (False, Nothing)  -> (False, rootConfigurations c)

solutionMatrix :: Array'
solutionMatrix = solve (inspectionList, initialSolns) 
  where
    inspectionList   = topoSort rootConfigurations (1,0,0)
    valid (r1,r2,r3) = r1 >= r2 && r2 >= r3 && r1 /=0
    bnds             = ((0,0,0), (maxVal,maxVal,maxVal))
    initialSolns     = array bnds [(ix, (False, Nothing)) | ix <- range bnds]

-- INVARIANT: there exists no x <- configs such that there is a path x->x1->...->minConfig
solve :: ([Config], Array') -> Array'
solve ([],          solns) = solns
solve (min:configs, solns) = solve (configs, solns')
  where
    solns' = solns // changedSquares
    newConfigs = filter (not . fst . (!) solns) . rootConfigurations $ min
    changedSquares
      | fst $ solns ! min = []
      | otherwise         = [(c', (True, Just min)) | c' <- newConfigs]
    --solns' = if (r1,r2,r3) == (7,2,2) then trace (b ++ c ++ d) a else a
    --a = solns // changedSquares
    --b = "At (7,2,2): " ++ (if (6,2,2) `elem` map toTuple (toList configs) then "(6,2,2) in heap\n" else "(6,2,2) not in heap\n") ++ "\n"
    --c =  "newConfigs (7,2,2): " ++ show x ++ "\n"
    --d = "Solns ! (14,2,2): " ++ show (solns ! Config (14,2,2)) ++ "\n"
    --x = filter (not . fst . (!) solns) . rootConfigurations $ minConfig
    --x' = "newConfigs (6,2,2): " ++ show x ++ "\n"
    --y = "Solns ! (14,2,2): " ++ show (solns ! Config (14,2,2)) ++ "\n"
    --z = "At (6,2,2): " ++ (if (7,2,2) `elem` map toTuple (toList configs) then "(7,2,2) in heap\n" else "(7,2,2) not in heap\n") ++ "\n"
    --newConfigs = if (r1,r2,r3) == (6,2,2) then trace (z ++ y ++ x') x else x

rootConfigurations :: Config -> [Config]
rootConfigurations (r1,r2,r3) = a ++ b ++ c ++ d ++ e ++ f
  where
    a = [(r1',r2,r3) | r1' <- [r1+1..maxVal]]
    b = [(r1,r2',r3) | r2' <- [r2+1..r1]]
    c = [(r1,r2,r3') | r3' <- [r3+1..r2]]
    d = [(r1',r1',r3) | r1 == r2, r1' <- [r1+1..maxVal]]
    e = [(r1,r2',r2') | r2 == r3, r2' <- [r2+1..r1]]
    f = [(r1',r1',r1') | r1 == r2 && r2 == r3, r1' <- [r1+1..maxVal]]

elt |+ set = insert elt set
elt !> set = notMember elt set

topoSort :: (Ord a) => (a -> [a]) -> a -> [a]
topoSort childFunc head = snd $ topoSort' childFunc head (empty, [])

topoSort' :: (Ord a) => (a -> [a]) -> a -> (Set a, [a]) -> (Set a, [a])
topoSort' childFunc head (visited, tail)
  | head !> visited = (visited', head : depthSort)
  | otherwise       = (visited, tail)
  where
    (visited', depthSort) = foldl recurse (head |+ visited, tail) (childFunc head)
    recurse (visited, tail) head = topoSort' childFunc head (visited, tail)

simpleChild :: Int -> [Int]
simpleChild 0 = [1,2,3]
simpleChild 1 = [3]
simpleChild 2 = [3,1]
simpleChild 3 = []

--solutionMatrix :: Array'
--solutionMatrix = solve (lossSquares, initialSolns) 
--  where
--    lossSquares  = Data.Heap.singleton (Config (1,0,0)) :: MinHeap Config
--    bnds         = (Config (0,0,0), Config (maxVal,maxVal,maxVal))
--    initialSolns = array bnds [(ix, (False, Nothing)) | ix <- range bnds]

--solve :: (MinHeap Config, Array') -> Array'
--solve (inspectionHeap, solns) = case view inspectionHeap of
--  Nothing            -> solns
--  Just (minConfig, configs)  -> solve (configs', solns')
--    where
--      solns' = solns // changedSquares
--      configs' = union configs . fromList $ newConfigs
--      newConfigs = filter (not . fst . (!) solns) . rootConfigurations $ minConfig
--      changedSquares
--        | fst $ solns ! minConfig = []
--        | otherwise               = [(c', (True, Just minConfig)) | c' <- newConfigs]

--import Data.Array (Array, (//), (!), array, range)
--import Data.Set (insert, notMember, Set)
--import Data.Ix (range)
--import Debug.Trace (traceShow, trace)
--import Data.Heap (union, view, singleton, MinHeap, fromList, toList)
--import Data.Ix (Ix)
--
--maxVal = 25
--
--newtype Config = Config (Int, Int, Int) deriving (Ix, Show)
--type Array' = Data.Array.Array Config (Bool, Maybe Config)
--
---- c1 < c2 if we can get to c2 via c1->c1'->c1''->...->c2. 
---- c1 == c2 if there is no path.
---- (this is with regard to our flow-reversed graph)
--instance Ord Config where
--  Config (x1, x2, x3) `compare` Config (y1, y2, y3)
--    | x1 == y1 && x2 == y2 && x3 == y3 = EQ
--    | x1 <= y1 && x2 <= y2 && x3 <= y3 = LT
--    | x1 >= y1 && x2 >= y2 && x3 >= y3 = GT
--    | otherwise                        = EQ
--
--instance Eq Config where
--  a == b = a `compare` b == EQ
--
--toTuple :: Config -> (Int, Int, Int)
--toTuple (Config x) = x
--
--victoryPossible :: (Int, Int, Int) -> (Bool, [(Int, Int, Int)])
--victoryPossible c = case solutionMatrix ! (Config c) of
--  (True,   Just x)  -> (True, [toTuple x])
--  (False, Nothing)  -> (False, map toTuple . rootConfigurations . Config $ c)
--
--solutionMatrix :: Array'
--solutionMatrix = solve (inspectionHeap, initialSolns) 
--  where
--    inspectionHeap  = fromList [x|x<-(range bnds), valid x] :: MinHeap Config
--    valid (Config (r1,r2,r3)) = r1 >= r2 && r2 >= r3 && r1 /=0
--    bnds         = (Config (0,0,0), Config (maxVal,maxVal,maxVal))
--    initialSolns = array bnds [(ix, (False, Nothing)) | ix <- range bnds]
--
---- INVARIANT: there exists no x <- configs such that there is a path x->x1->...->minConfig
--solve :: (MinHeap Config, Array') -> Array'
--solve (inspectionHeap, solns) = case view inspectionHeap of
--  Nothing                    -> solns
--  Just (minConfig@(Config (r1,r2,r3)), configs)  -> solve (configs, solns')
--    where
--      --solns' = solns // changedSquares
--      --newConfigs = filter (not . fst . (!) solns) . rootConfigurations $ minConfig
--      solns' = if (r1,r2,r3) == (7,2,2) then trace (b ++ c ++ d) a else a
--      a = solns // changedSquares
--      b = "At (7,2,2): " ++ (if (6,2,2) `elem` map toTuple (toList configs) then "(6,2,2) in heap\n" else "(6,2,2) not in heap\n") ++ "\n"
--      c =  "newConfigs (7,2,2): " ++ show x ++ "\n"
--      d = "Solns ! (14,2,2): " ++ show (solns ! Config (14,2,2)) ++ "\n"
--      x = filter (not . fst . (!) solns) . rootConfigurations $ minConfig
--      x' = "newConfigs (6,2,2): " ++ show x ++ "\n"
--      y = "Solns ! (14,2,2): " ++ show (solns ! Config (14,2,2)) ++ "\n"
--      z = "At (6,2,2): " ++ (if (7,2,2) `elem` map toTuple (toList configs) then "(7,2,2) in heap\n" else "(7,2,2) not in heap\n") ++ "\n"
--      newConfigs = if (r1,r2,r3) == (6,2,2) then trace (z ++ y ++ x') x else x
--      changedSquares
--        | fst $ solns ! minConfig = []
--        | otherwise               = [(c', (True, Just minConfig)) | c' <- newConfigs]
---- RIDDLE:
---- *BitterChocolate> victoryPossible (14,12,2)  => (14,2,2) was False when popped off heap
---- (True,[(14,2,2)])
---- *BitterChocolate> victoryPossible (14,2,2)   => (7,2,2) was False when popped off heap
---- (True,[(7,2,2)])
---- *BitterChocolate> victoryPossible (7,2,2)    => (6,2,2) was False when popped off heap
---- (True,[(6,2,2)])
---- *BitterChocolate> victoryPossible (6,2,2)    => should have turned (6,2,2),(7,2,2) to True
---- (False,[(7,2,2),(8,2,2),(9,2,2),(10,2,2),(11,2,2),(12,2,2),(13,2,2),(14,2,2),(15,2,2),(16,2,2),(17,2,2),(18,2,2),(19,2,2),(20,2,2),(21,2,2),(22,2,2),(23,2,2),(24,2,2),(25,2,2),(6,3,2),(6,4,2),(6,5,2),(6,6,2),(6,3,3),(6,4,4),(6,5,5),(6,6,6)])
--
--rootConfigurations :: Config -> [Config]
--rootConfigurations (Config (r1,r2,r3)) = map Config $ a ++ b ++ c ++ d ++ e ++ f
--  where
--    a = [(r1',r2,r3) | r1' <- [r1+1..maxVal]]
--    b = [(r1,r2',r3) | r2' <- [r2+1..r1]]
--    c = [(r1,r2,r3') | r3' <- [r3+1..r2]]
--    d = [(r1',r1',r3) | r1 == r2, r1' <- [r1+1..maxVal]]
--    e = [(r1,r2',r2') | r2 == r3, r2' <- [r2+1..r1]]
--    f = [(r1',r1',r1') | r1 == r2 && r2 == r3, r1' <- [r1+1..maxVal]]
--
----topoSort :: (a -> [a]) -> Set a -> a -> [a]
----topoSort childFunc visitedNodes node = node : descendants
----  where
----    foldl depthFirst ([], visitedNodes) (childFunc nodes)
----    depthFirst ([a], Set a) -> a -> 
--
--elt |+ set = insert elt set
--elt !> set = notMember elt set
--
---- We do not assume that head is unvisited
--topoSort :: (Ord a) => (a -> [a]) -> (Set a, a) -> (Set a, [a])
--topoSort childFunc (visited, head)
--  | head !> visited = (visited', head : depthSort)
--  | otherwise       = (visited, [])
--  where
--    (visited', depthSort) = foldl foo (head |+ visited, []) (childFunc head)
--    foo (visited, soFar) nextNode = (visited', soFar ++ nextList)
--      where
--        (visited', nextList) = topoSort childFunc (visited, nextNode) 
