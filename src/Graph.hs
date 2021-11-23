module Graph where

import Control.Monad
import qualified Data.List as List
import Data.Map (Map, insert)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck


newtype Vertices = V {v :: [Int]}
newtype Edges = E {e :: [(Int, Int)]}
newtype Graph = G {rows :: [Vector]} 

newtype Palette = P {c :: [Int]}
newtype Color = c :: Int
newtype Coloring = C {p :: [(Int, Int)]}


-- ColoringAlgorithm(G, ∆): A meta-algorithm for finding a (∆+1)-coloring in a graph G(V, E) with
-- maximum degree ∆.
-- 1. Sample Θ(log n) colors L(v) uniformly at random for each vertex v ∈ V (as in Theorem 1).
-- 2. Define, for each color c ∈ [∆ + 1], a set χc ⊆ V where v ∈ χc iff c ∈ L(v).
-- 3. Define Econflict as the set of all edges (u, v) where both u, v ∈ χc for some c ∈ [∆ + 1].
-- 4. Construct the conflict graph Gconflict(V, Econflict).
-- 5. Find a proper list-coloring of Gconflict(V, Econflict) with L(v) being the color list of vertex v ∈

-- sample \Theta(log n) colors L(v) uniformly at random
createPalette :: Int -> Palette
createPalette = undefined

getSample :: Int -> Palette -> Palette
getSample lgN colors = undefined

-- run getSample for each vertex
sample :: Vertices -> Palette -> [Palette]
sample = undefined

-- for a given color, find all vertices that sample that color
vertexColor :: Color -> Vertices -> [Palette] -> Vertices
vertexColor = undefined

-- find all vertices that sample a given color for all colors
vertexColorAll :: Palette -> Vertices -> [Palette] -> [Vertices]  
vertexColorAll = undefined

-- all edges that have a conflict
eConflict :: Palette -> [Vertices] -> Edges -> Edges
eConflict = undefined

-- construct a graph given vertices and edges
constructGraph :: Vertices -> Edges -> Graph
constructGraph = undefined

colorGraph :: Graph -> Palette -> Coloring 
colorGraph = undefined

-- make sure coloring is correct
colorVerifier :: Graph -> Coloring -> Bool
colorVerifier = undefined 

-- check if AB = C then the matrixEval agrees at each v
propertyCorrectColor ::  Graph -> Coloring -> Bool
propertyCorrectColor graph coloring = 
    property (colorVerifier graph coloring)

instance Arbitrary Vertices where
  arbitrary = genVertex 
  shrink v = undefined

genVertex :: Int -> Vector 
genVertex n = undefined

instance Arbitrary Edges where
  arbitrary = genEdges 
  shrink m = undefined

genEdges :: Int -> Matrix 
genEdges n = undefined 




