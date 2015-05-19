import Data.Char (toLower)

rovarlet :: Char -> [Char]
rovarlet c
    | consonant c = [c, 'o', toLower c]
    | otherwise   = [c]
    where consonant = (`elem` "bcdfghjklmnpqrstvwxz") . toLower

rovarspraket :: String -> String
rovarspraket = concat . map rovarlet

main = interact rovarspraket
