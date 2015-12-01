import Data.List (find)
import Control.Monad (liftM)
import System.Environment (getArgs)

main = liftM (read . head) getArgs >>= print . abundance

abundance :: Integer -> Abundance
abundance n =
    case factor_sum `compare` n of
        GT -> Abundant (factor_sum-n)
        LT -> Deficient (n-factor_sum)
        EQ -> Perfect
    where factor_sum = sum $ factors n
          factors p = filter (\q -> p `mod` q == 0) [1..p-1]

data Abundance = Abundant Integer | Deficient Integer | Perfect deriving Show
