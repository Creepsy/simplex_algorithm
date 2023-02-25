module Main where
    
import Data.List (singleton)
import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec
import Debug.Trace (trace)

type Term = [Rational]
type Inequation = (Term, Rational) -- we assume <= here, as this is the type of inequation commonly used for the simplex algorithm

main :: IO ()
main = print (simplex [([1, 2], 1000), ([3, 4], 3500)] [5, 20])

-- use matrix & vector
simplex :: [Inequation] -> Term -> [Rational]
simplex inequations toMaximize = trace (Mat.prettyMatrix finalTable) error "simplex currently unimplemented!" where
    inequationCount = length inequations
    isFinished table =Vec.all (>= 0) . Mat.getRow (Mat.nrows table) $ table

    initialTable = buildSimplexTable inequations toMaximize
    finalTable = head . dropWhile (not . isFinished) . iterate simplexStep $ initialTable

simplexStep :: Mat.Matrix Rational -> Mat.Matrix Rational
simplexStep simplexTable = simplexTable'' where
    pivotPos@(pivotRow, pivotColumn) = pivotElement simplexTable
    pivot = simplexTable Mat.! pivotPos

    simplexTable' = Mat.mapRow (\_ e -> e / pivot) pivotRow simplexTable
    simplexTable'' = foldr zeroPivotColumn simplexTable' [r | r <- [1..Mat.nrows simplexTable'], r /= pivotRow]

    zeroPivotColumn row table = Mat.mapRow (\col e -> e - multiplier * table Mat.! (pivotRow, col)) row table where
        multiplier = table Mat.! (row, pivotColumn)

pivotElement :: Mat.Matrix Rational -> (Int, Int)
pivotElement simplexTable = (pivotRow, pivotColumn) where
    toMaximizeRow = Vec.init . Mat.getRow (Mat.nrows simplexTable) $ simplexTable
    bounds = Vec.init . Mat.getCol (Mat.ncols simplexTable) $ simplexTable

    pivotColumn = Vec.minIndex toMaximizeRow + 1
    pivotRow = Vec.minIndex (Vec.zipWith (/) bounds (Mat.getCol pivotColumn simplexTable)) + 1

buildSimplexTable :: [Inequation] -> Term -> Mat.Matrix Rational
buildSimplexTable inequations toMaximize = inequationsTable Mat.<|> helperVariables Mat.<|> bounds where
    variableCount = maximum . map (length . fst) $ inequations
    inequations' = map (fill variableCount 0 . fst) inequations
    inequationCount = length inequations'
    toMaximize' = fill variableCount 0 . map negate $ toMaximize

    inequationsTable = Mat.fromLists (inequations' ++ [toMaximize'])
    bounds = Mat.setSize 0 (inequationCount + 1) 1 . Mat.fromLists $ map (singleton . snd) inequations
    helperVariables = Mat.setSize 0 (inequationCount + 1) inequationCount $ Mat.identity inequationCount

fill :: Int -> a -> [a] -> [a]
fill len val toFill = take len $ toFill ++ repeat val