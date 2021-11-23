module Sat where

import Control.Monad
import qualified Data.List as List
import Data.Map (Map, insert)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses. We store these clauses in the conjunction in a list.
newtype CNF = Conj {clauses :: [Clause]} deriving (Eq, Ord, Show)

-- | A clause is a disjunction of a number of literals, again storing
-- each literal in a list.
newtype Clause = Disj {lits :: [Lit]} deriving (Eq, Ord, Show)

-- | A literal is either a positive or a negative variable
data Lit = Lit {polarity :: Bool, var :: Var} deriving (Eq, Ord, Show)

newtype Var = Var Char
  deriving (Eq, Ord, Show)

-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

instance Semigroup CNF where
  Conj c1 <> Conj c2 = Conj (c1 <> c2)

instance Monoid CNF where
  mempty = Conj mempty

instance Semigroup Clause where
  Disj c1 <> Disj c2 = Disj (c1 <> c2)

instance Monoid Clause where
  mempty = Disj mempty

instance Enum Var where
  toEnum i = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

-- | A long list of variables
allVars :: [Var]
allVars = [vA ..]

type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var, Bool)] -> Valuation
fromList = Map.fromList

-- | creates random valuation for given CNF
randomVal :: CNF -> Valuation
randomVal = undefined

-- | counts number of clauses satisfied by valuation
score :: CNF -> Valuation -> Int
score cnf v = undefined

-- | to get a good valuation, run f several times
-- | and take best scoring valuation

-- Generator Section

-- | Generate a random variable (limited to the first `n` variables).
genVar :: Int -> Gen Var
genVar n | n < 1 = error "Must supply a positive number to genVar"
genVar n = elements (List.map toEnum [0 .. n -1])

-- | Generate a random literal with `n` distinct variables.
genLit :: Int -> Gen Lit
genLit n = liftM2 Lit (choose (True, False)) (genVar n)

-- | Generate a random Clause with `n` distinct variables.
genClause :: Int -> Gen Clause
genClause n = fmap Disj $ listOf $ genLit n

-- | Generate a random CNF with `n` distinct variables.
genCNF :: Int -> Gen CNF
genCNF n = fmap Conj $ listOf $ genClause n

defaultNumVariables :: Int
defaultNumVariables = 7

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v
    | v == vA = []
    | otherwise = [vA .. pred v]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) =
    map (`Lit` v) (shrink b)
      ++ map (Lit b) (shrink v)

instance Arbitrary Clause where
  arbitrary = genClause defaultNumVariables
  shrink (Disj l) = map Disj (shrink l)

instance Arbitrary CNF where
  arbitrary = genCNF defaultNumVariables
  shrink (Conj x) = map Conj (shrink x)

-- Question: what are reasonable properties for two-sided error algorithms?
