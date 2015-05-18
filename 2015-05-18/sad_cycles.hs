import Data.Char (intToDigit, digitToInt)
import Data.List (intercalate)

digits :: Integer -> [Int]
digits = map digitToInt . show

undigits :: [Int] -> Integer
undigits = foldl acc 0
    where acc m n = 10*m + toInteger n

nextSad :: Integer -> Integer -> Integer
nextSad b = sum . map ((^b) . toInteger) . digits

runCycle b n l
    | next `elem` l = reverse $ next : takeWhile (/=next) l
    | otherwise     = runCycle b next (next:l)
    where next = nextSad b n

sadCycle b n = runCycle b n []

main = do
    b <- readLn :: IO Integer
    n <- readLn :: IO Integer
    let cycle = sadCycle b n
    putStrLn $ intercalate ", " $ map show cycle
