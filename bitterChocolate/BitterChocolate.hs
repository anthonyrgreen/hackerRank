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

import Data.Array.Unboxed (UArray, (//), (!), array, range)
import Data.Dequeue (BankersDequeue, pushBack, fromList, popFront, length)
import Debug.Trace (traceShow)

type Config = (Int, Int, Int)
type Queue = BankersDequeue Config
type Array = UArray Config Bool
maxVal = 25

victoryPossible :: Config -> Bool
victoryPossible = (!) solutionMatrix

solutionMatrix :: Array
solutionMatrix = solve (lossSquare, initialSolns) 
  where
    lossSquare   = [(1,0,0)]
    bnds         = ((0,0,0),(maxVal,maxVal,maxVal))
    initialSolns = array bnds [(ix, False) | ix <- range bnds]

solve :: ([Config], Array) -> Array
solve ([],        solns) = solns
solve (c:configs, solns) = solve (newConfigs ++ configs, solns')
  where
    newConfigs = filter (not . (!) solns) . rootConfigurations $ c
    solns'
      | solns ! c = solns
      | otherwise = solns // [(c', True) | c' <- newConfigs]

rootConfigurations :: Config -> [Config]
rootConfigurations (r1,r2,r3) = a ++ b ++ c ++ d ++ e ++ f
  where
    a = [(r1',r2,r3) | r1' <- [r1+1..maxVal]]
    b = [(r1,r2',r3) | r2' <- [r2+1..r1]]
    c = [(r1,r2,r3') | r3' <- [r3+1..r2]]
    d = [(r1',r1',r3) | r1 == r2, r1' <- [r1+1..maxVal]]
    e = [(r1,r2',r2') | r2 == r3, r2' <- [r2+1..r1]]
    f = [(r1',r1',r1') | r1 == r2 && r2 == r3, r1' <- [r1+1..maxVal]]
