module Main where
    
import Data.List (singleton)
import Data.Bifunctor (Bifunctor(first))
import qualified Data.Matrix as Mat

type Term = [Rational]
type Inequation = (Term, Rational) -- we assume <= here, as this is the type of inequation commonly used for the simplex algorithm

main :: IO ()
main = print (simplex [([1, 2], 1000), ([3, 4], 3500)] [5, 20])

-- use matrix & vector
simplex :: [Inequation] -> Term -> [Rational]
simplex inequations toMaximize = error "simplex currently unimplemented!" where
    table = buildSimplexTable inequations toMaximize

buildSimplexTable :: [Inequation] -> Term -> Mat.Matrix Rational
buildSimplexTable inequations toMaximize = leftSide Mat.<|> helperVariables Mat.<|> rightSide where
    variableCount = maximum . map (length . fst) $ inequations
    inequations' = map (fill variableCount 0 . fst) inequations
    inequationCount = length inequations'
    toMaximize' = fill variableCount 0 . map negate $ toMaximize

    leftSide = Mat.fromLists (inequations' ++ [toMaximize'])
    rightSide = Mat.setSize 0 (inequationCount + 1) 1 . Mat.fromLists $ map (singleton . snd) inequations
    helperVariables = Mat.setSize 0 (inequationCount + 1) inequationCount $ Mat.identity inequationCount

fill :: Int -> a -> [a] -> [a]
fill len val toFill = take len $ toFill ++ repeat val