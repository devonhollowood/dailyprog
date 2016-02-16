import qualified Data.Map as M

main :: IO ()
main = interact atbash

atbash :: String -> String
atbash = map $ \c -> M.findWithDefault c c atbashDict

atbashDict :: M.Map Char Char
atbashDict =
    M.fromList $ zip ['a'..'z'] ['z','y'..'a'] ++ zip ['A'..'Z'] ['Z','Y'..'A']
