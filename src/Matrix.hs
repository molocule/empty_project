module Matrix where

import Control.Monad
import qualified Data.List as List
import Data.Map (Map, insert)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck

newtype Vector = V {v :: [Int]}
newtype Matrix = M {rows :: [Vector]} 

dim :: Matrix -> (Int, Int) 
dim (M rows) = undefined

isSquare :: Matrix -> Bool 
isSquare (M rows) = undefined 

-- if the dimensions are wrong, return Nothing
-- here, we include a deterministic implementation
-- that will be helpful in quickCheck
matrixMult :: Matrix -> Matrix -> Maybe Matrix 
matrixMult = undefined

matrixVecMult :: Matrix -> Vector -> Maybe Matrix 
matrixVecMult = undefined

matrixEval :: Matrix -> Vector -> Int 
matrixEval = undefined 

-- verifier takes in matrices A, B, C
-- and determines if A*B = C
-- the algorithm will choose O(log n) random boolean vectors
-- and check that matrixEval A (matrixEval B v) = matrixEval C v
verifier :: Matrix -> Matrix -> Matrix -> Boolean
verifier = undefined 

-- check if AB = C then the matrixEval agrees at each v
propertyCorrectMatrixMult :: Matrix -> Matrix -> Vector -> Property
propertyCorrectMatrixMult m1 m2 v = 
    property (matrixEval m1 (matrixEval m2 v) == matrixEval (matrixMult m1 m2) v)

-- if AB = C then verifier should always return True (one sided error)
propertyMatrixMult :: Matrix -> Matrix -> Property
propertyMatrixMult m1 m2 =
    property (verifier m1 m2 (matrixMult m1 m2))

instance Arbitrary Vector where
  arbitrary = genVector 
  shrink v = undefined

genVector :: Int -> Vector 
genVector n = undefined
-- only elements in {0, 1}

instance Arbitrary Matrix where
  arbitrary = genMatrix 
  shrink m = undefined

genMatrix :: Int -> Matrix 
genMatrix n = undefined 


-- call genVector multiple times



