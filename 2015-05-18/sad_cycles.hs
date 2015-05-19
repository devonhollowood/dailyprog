import Data.Char (digitToInt)
import Data.List (intercalate)

main = do
    b <- readLn :: IO Integer
    n <- readLn :: IO Integer
    let cycle = sadCycle b n
    putStrLn $ intercalate ", " $ map show cycle

sadCycle b n = runCycle b n []

runCycle b n l
    | next `elem` l = reverse $ next : takeWhile (/=next) l
    | otherwise     = runCycle b next (next:l)
    where next = nextSad b n

nextSad b = sum . map ((^b) . toInteger) . digits

digits = map digitToInt . show
