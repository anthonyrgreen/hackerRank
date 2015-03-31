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

--solutionMatrix :: Array
--solutionMatrix = snd $ solve (initialQueue, initialSolns) 
--  where
--    initialQueue = fromList [(1,0,0)]
--    bnds         = ((0,0,0),(maxVal,maxVal,maxVal))
--    initialSolns = array bnds [(ix, False) | ix <- range bnds]

solutionMatrix :: Array
solutionMatrix = solve (initialQueue, initialSolns) 
  where
    initialQueue = [(1,0,0)]
    bnds         = ((0,0,0),(maxVal,maxVal,maxVal))
    initialSolns = array bnds [(ix, False) | ix <- range bnds]

--solve :: (Queue, Array) -> (Queue, Array)
--solve (queue, solns) = case popFront queue of
--  (Nothing, queue) -> (queue, solns)
--  --(Just x,  queue) -> traceShow (length queue') (solve (queue', solns'))
--  (Just x,  queue) -> solve (queue', solns')
--    where
--      -- Only re-add to queue if array isn't yet True at that configuration
--      queue'          = foldl pushBack' queue possibleRoots
--        where
--          pushBack' queue root
--            | solns ! root = queue
--            | otherwise    = pushBack queue root
--      opponentWin     = solns ! x
--      possibleRoots   = rootConfigurations x
--      solns'          = if opponentWin 
--                        then solns
--                        else solns // [(root, True) | root <- possibleRoots]

solve :: ([Config], Array) -> Array
solve (configs, solns) = case configs of
  []          -> solns
  (c:configs) -> traceShow (Prelude.length configs) (solve (newConfigs ++ configs, solns'))
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

--import qualified Data.Map as M
---- 25 >= row1 >= row2 >= row3 >= 0
---- row3
---- row2
---- row1
---- (row1, row2, row3)
--
--type Configuration = (Int, Int, Int)
--maxVal = 25
--
--victoryPossible :: M.Map -> Configuration -> (Bool, M.Map)
--victoryPossible dict (1,0,0) = (False, dict)
--victoryPossible dict config  = case M.lookup dict config of
--  Nothing   -> (result, dict')
--  Just ans  -> (ans, dict)
--  where
--    (result, dict') = any (not . victoryPossible) $ possibleMoves config
--
--possibleMoves :: Configuration -> [Configuration]
--possibleMoves config = map (leftOver config) $ eatableSquares config
--
--leftOver :: Configuration -> (Int, Int) -> Configuration
--leftOver (r1,r2,r3) (x,y)
--  | y == 3 = (r1, r2, x')
--  | y == 2 = (r1, x', min r3 x')
--  | y == 1 = (x', min r2 x', min r3 x')
--  where
--    x' = x - 1
--
--eatableSquares :: Configuration -> [(Int, Int)]
--eatableSquares (r1,r2,r3) = byRow 2 r1 1 ++ byRow 1 r2 2 ++ byRow 1 r3 3
--  where
--    byRow l u row = zip [l..u] (repeat row)
