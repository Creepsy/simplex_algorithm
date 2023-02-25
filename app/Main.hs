module Main where
    
import Data.List (singleton)
import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec

type Term a = [a]
type Inequation a = (Term a, a) -- we assume <= here, as this is the type of inequation commonly used for the simplex algorithm

main :: IO ()
main = print (simplex [([1, 2], 1000), ([3, 4], 3500)] [5, 20])

simplex :: (Fractional a, Ord a, Eq a) => [Inequation a] -> Term a -> [a]
simplex inequations toMaximize = extractSolution finalTable where
    inequationCount = length inequations
    isFinished table =Vec.all (>=0) . Mat.getRow (Mat.nrows table) $ table

    initialTable = buildSimplexTable inequations toMaximize
    finalTable = head . dropWhile (not . isFinished) . iterate simplexStep $ initialTable

simplexStep :: (Fractional a, Ord a) => Mat.Matrix a -> Mat.Matrix a
simplexStep simplexTable = simplexTable'' where
    pivotPos@(pivotRow, pivotColumn) = pivotElement simplexTable
    pivot = simplexTable Mat.! pivotPos

    simplexTable' = Mat.mapRow (\_ e -> e / pivot) pivotRow simplexTable
    simplexTable'' = foldr zeroPivotColumn simplexTable' [r | r <- [1..Mat.nrows simplexTable'], r /= pivotRow]

    zeroPivotColumn row table = Mat.mapRow (\col e -> e - multiplier * table Mat.! (pivotRow, col)) row table where
        multiplier = table Mat.! (row, pivotColumn)

extractSolution :: (Fractional a, Eq a) => Mat.Matrix a -> [a]
extractSolution simplexTable = [if partOfSolution var then getVariableValue var else 0 | var <- [1..variableCount]] where
    variableCount = Mat.ncols simplexTable - Mat.nrows simplexTable

    partOfSolution column = Vec.sum colElements == 1 && Vec.all (\e -> e == 0 || e== 1) colElements where
        colElements = Mat.getCol column simplexTable 

    getVariableValue column = simplexTable Mat.! (row, Mat.ncols simplexTable) where
        (Just row) = (+1) <$> Vec.findIndex (==1) (Mat.getCol column simplexTable)

pivotElement :: (Fractional a, Ord a) => Mat.Matrix a -> (Int, Int)
pivotElement simplexTable = (pivotRow, pivotColumn) where
    toMaximizeRow = Vec.init . Mat.getRow (Mat.nrows simplexTable) $ simplexTable
    bounds = Vec.init . Mat.getCol (Mat.ncols simplexTable) $ simplexTable

    pivotColumn = Vec.minIndex toMaximizeRow + 1
    pivotRow = Vec.minIndex (Vec.zipWith (/) bounds (Mat.getCol pivotColumn simplexTable)) + 1

buildSimplexTable :: Fractional a => [Inequation a] -> Term a -> Mat.Matrix a
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