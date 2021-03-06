import Data.Vector (Vector, fromList, toList, (//), (!), thaw, freeze)
import qualified Data.Vector.Mutable as M
import System.Environment (getArgs)
import System.IO (openFile, IOMode (..), hClose, hGetLine)
import Control.Exception (bracket)
import Control.Monad (replicateM)
import Control.Monad.ST (runST)

type Switch = (Int, Int)
type SwitchBoard = [Switch]
type Elements = Vector Int

main = do
    file:junk <- getArgs
    (nwires, board) <- readBoard file
    --display nwires board --enable this if you want to see outputs
    let valid = all isSorted . map (`apply` board) . generateTests $ nwires
    putStrLn $ if valid then "Valid network" else "Invalid network"

-- Applies SwitchBoard to Elements
apply :: Elements -> SwitchBoard -> Elements
apply els board = runST $ do
    mels <- thaw els
    mapM_ (maybe_swap mels) board
    freeze mels
    where maybe_swap mels (posA, posB) = do
              elA <- mels `M.read` posA
              elB <- mels `M.read` posB
              if elA > elB then M.swap mels posA posB else return ()

-- Generates tests for an switchboard with nwires wires
generateTests :: Int -> [Elements]
generateTests nwires = map fromList . combinations . take nwires $ repeat [0,1]
    where combinations []     = []
          combinations [xs]    = [[x] | x <- xs]
          combinations (xs:xss) = [x:cs | x<-xs, cs <- combinations xss]

-- Tests whether Elements are sorted
isSorted :: Elements -> Bool
isSorted = check . toList
    where check []       = True
          check [x]      = True
          check (x:y:xs) = x <= y && check (y:xs)

-- Reads a board from a file
readBoard :: String -> IO (Int, SwitchBoard)
readBoard file = bracket (openFile file ReadMode) (hClose) $ \h -> do
    (nwires, nlines) <- read_pair h
    switches <- replicateM nlines $ read_pair h
    return (nwires, switches)
    where read_pair h = do
              line <- hGetLine h
              let elements = map read . words $ line
              return (elements !! 0, elements !! 1)

-- Displays results of testing a board
display :: Int -> SwitchBoard -> IO ()
display nwires board = do
    let tests = generateTests nwires
        results = map (toList . (`apply` board)) tests
        tuples = zip (map toList tests) results
        lines = map (\(a, b) -> concat [show a, " -> ", show b]) tuples
    mapM_ putStrLn lines
