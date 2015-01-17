import Data.List (minimumBy)
import Data.Maybe
import qualified Data.Map as Map

type Time = Int
type Node = Char
data Street = Street {
    name    :: String,
    start   :: Node,
    end     :: Node,
    times   :: [Time]
    }
type Path = [Street]
type Graph = Map.Map Node [Street]

--streetTime t s = time at which you finish going down s if you start at t
streetTime :: Time -> Street -> Time
streetTime t street
    | 360  <= t && t < 600  = t + times street !! 1
    | 600  <= t && t < 900  = t + times street !! 2
    | 900  <= t && t < 1140 = t + times street !! 3
    | 1140 <= t || t < 360  = t + times street !! 4

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

djikstra :: Graph -> Node -> Time -> Map.Map Node Path -> Maybe Path
djikstra graph dest start_t node_paths = case Map.lookup dest node_paths of
    Just path -> Just path 
    Nothing   -> next graph start_t node_paths >>= return . updateMap
                 >>= djikstra graph dest start_t
    where updateMap path = Map.insert (end $ last path) path node_paths

shortest_path :: Graph -> Node -> Node -> Time -> Maybe Path
shortest_path graph a b time = djikstra graph b time (Map.singleton a [])
