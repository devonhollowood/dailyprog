module Main where

import Data.List (minimumBy)
import Data.Maybe
import qualified Data.Map as Map
import Text.Regex.Posix
import System.IO
import System.IO.Error
import System.Environment

type Time = Int
type Node = Char
data Street = Street {
    start   :: Node,
    end     :: Node,
    name    :: String,
    times   :: [Time]
    }
type Path = [Street]
type Graph = Map.Map Node [Street]

instance Show Street where
    show s = concat [(name s), " (", (return $ start s), "->", 
                     (return $ end s), ")"]

--streetTime t s = time at which you finish going down s if you start at t
streetTime :: Time -> Street -> Time
streetTime t street
    | 360  <= t && t < 600  = t + times street !! 0
    | 600  <= t && t < 900  = t + times street !! 1
    | 900  <= t && t < 1140 = t + times street !! 2
    | 1140 <= t || t < 360  = t + times street !! 3

--pathTime t p = time at which you finish going down p if you start at t
pathTime :: Time -> Path -> Time
pathTime = foldl streetTime

--next g t paths = path to next node in g under Djikstra's algorithm
next :: Graph -> Time -> Map.Map Node Path -> Maybe Path
next graph start_t node_paths = externals node_paths 
                                >>= return . map attach 
                                >>= earliest
    where externals    = nullguard . filter is_external . concat 
                         . map (graph Map.!) . Map.keys
          nullguard [] = Nothing
          nullguard a  = Just a 
          is_external  = flip Map.notMember node_paths . end
          attach st    = node_paths Map.! start st ++ [st]
          earliest     = return . minimumBy arrival
          arrival a b  = compare (pathTime start_t a) (pathTime start_t b)

--gives shortest distance to dest within graph if you start at t
djikstra :: Graph -> Node -> Time -> Map.Map Node Path -> Maybe Path
djikstra graph dest start_t node_paths = case Map.lookup dest node_paths of
    Just path -> Just path 
    Nothing   -> next graph start_t node_paths >>= return . updateMap
                 >>= djikstra graph dest start_t
    where updateMap path = Map.insert (end $ last path) path node_paths

--gives shortest path from a to b within g if you start at time t
shortestPath :: Graph -> Node -> Node -> Time -> Maybe Path
shortestPath graph a b time = djikstra graph b time (Map.singleton a [])

--converts an input line to a Street
lineToStreet :: String -> Maybe Street
lineToStreet line = case listToMaybe (line =~ line_regex :: [[String]]) of
    Just (match:a:b:n:ts:[]) -> Just $ Street (head a) (head b) n 
                                     (map read $ words ts)
    _                        -> Nothing
    where line_regex = "(.) (.) \"([A-Za-z ]+)\" ([0-9 ]+)"

--parses command line arguments to get (start, end, start_time)
parseArgs :: [String] -> Maybe (Node, Node, Time)
parseArgs ((a:as):(b:bs):t:[]) = Just (a, b, (*60) . (`div` 100) $ read t)
parseArgs _                    = (Nothing)

--converts a list of streets to a graph.
streetsToGraph :: [Street] -> Graph
streetsToGraph = foldl insert_street Map.empty
    where insert_street m s =   Map.insertWith (++) (start s) [s]
                              . Map.insertWith (++) (start $ rev s) [rev s] $ m
          rev s             = Street (end s) (start s) (name s) (times s)

main = do
    args <- getArgs
    (a, b, t) <- case parseArgs args of
        Just tup -> return tup
        Nothing  -> ioError $ userError "Invalid args"
    mapfile <- openFile "data.txt" ReadMode
    contents <- hGetContents mapfile
    let streets = mapMaybe lineToStreet . lines $ contents
    let g = streetsToGraph streets
    p <- case shortestPath g a b t of
        Just path -> return path
        Nothing   -> ioError $ userError "No paths found"
    mapM_ (putStrLn . show) p
    putStrLn $ "Total time: " ++ show (pathTime t p - t) ++ " minutes."
