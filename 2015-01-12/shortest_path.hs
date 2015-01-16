import Data.List (minimumBy)
import qualified Data.Map as Map

type Time = Int
type Node = Char
data Street = Street {
    name    :: String
    start   :: Node
    end     :: Node
    times   :: [Time]
    }
type Path = [Street]
type Graph = Map.map Node [Street]

arrivalTime :: Time -> Street -> Time
arrivalTime t street
    | 360  <= t && t < 600  = t + street !! 1
    | 600  <= t && t < 900  = t + street !! 2
    | 900  <= t && t < 1140 = t + street !! 3
    | 1140 <= t || t < 360  = t + street !! 4

pathTime :: Time -> Path -> Time
pathTime = foldl arrivalTime

next :: Graph -> Map.Map Node Path -> Street
next graph node_paths = earliest . concat . elems . externals $ node_paths
    where earliest  = minimumBy (compare . uncurry arrivalTime)
          externals = Map.map $ filter $ notMember node_paths . end

djikstra :: Graph -> Map.Map Node Path -> Node -> Path
djikstra graph node_paths dest = case map.lookup dest node_paths
    | Just path = path
    | Nothing   = djikstra (Map.insert next_node next_path nodepaths)
    where next_street = next node_paths
          next_path   = lookup node_paths (start next_street) ++ next_street
          next_node   = end . last next_path

shortest_path graph a b = djikstra graph (Map.singleton a []) b
