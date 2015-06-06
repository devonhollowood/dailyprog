import Data.Time
import Control.Monad
import System.Environment

today :: IO Day
today = getCurrentTime >>= return . utctDay

readDay :: [String] -> Day
readDay (y:m:d:[]) = fromGregorian (read y) (read m) (read d)
readDay _          = error "Invalid Arguments"

output :: Day -> Day -> String
output now target 
    | diff==(-1) = concat [show target, " was yesterday!"]
    | diff < 0   = concat ["It has been ", show (-diff), 
                           " days since ", show target, "."]
    | diff== 1   = concat [show target, " is tomorrow!"] 
    | diff > 0   = concat ["It will be ", show diff, 
                           " days until ", show target, "."] 
    | otherwise  = concat [show target, " is today!"]
    where diff = diffDays target now

main = getArgs >>= liftM2 output today . return . readDay >>= putStrLn
