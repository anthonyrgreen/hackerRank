-- IDEA :: return a list of configurations to test w/ (|| . not) 
-- maintain queue of configurations.
-- pop item off queue, determine all possible root configs
-- or . not popped with possible configs
-- put possible configs on back of queue
module BitterChocolate (victoryPossible, rootConfigurations) where

--import Prelude hiding (length)
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
