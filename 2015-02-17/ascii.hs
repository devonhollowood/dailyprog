import System.Environment (getArgs)
import Data.Char (chr)
import Data.List.Split (chunksOf)

readBin = foldl step 0
    where l `step` r = l*2 + to_int r
          to_int = read . return

toAscii = map (chr . readBin) . chunksOf 8

main = getArgs >>= mapM_  (putStrLn . toAscii)
