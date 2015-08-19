import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = undefined

create_bigrams :: String -> IO BigramTable
create_bigrams filename = do
    contents <- TIO.readFile filename
    let content_bigrams = bigrams contents
    let table = M.fromList [(Bigram (a, b), 0) | a <- ['a'..'z'],
                                                 b <- ['a'..'z']]
    return $ foldl' (M.adjust (+1)) content_bigrams

bigrams :: Text -> [Bigram]
bigrams = undefined

newtype Bigram = Bigram (Char, Char)
type BigramTable = (Map Bigram Int)
