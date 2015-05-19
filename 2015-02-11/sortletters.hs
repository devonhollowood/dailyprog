import System.Environment (getArgs)
import Data.Char (isAlpha)
import Data.List (sort)

main = getArgs >>= putStrLn . sort . filter (isAlpha) . head
