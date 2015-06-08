import qualified Text.PrettyPrint.Boxes as B
import Text.PrettyPrint.Boxes (Box, Alignment)
import Text.Parsec
import Text.Parsec.Text (Parser)
import qualified Data.Text.IO as IO
import Control.Applicative ((*>), (<*))
import Data.Monoid
import System.Environment (getArgs)

main = do
    [infile] <- getArgs
    contents <- IO.readFile infile
    case parse latex "" contents of
        Left err -> putStrLn $ show err
        Right lc -> B.printBox $ chunkBox lc

latex :: Parser LatexChunk
latex = do
    chunks <- many latexChunk
    return $ mconcat chunks

latexChunk :: Parser LatexChunk
latexChunk = command <|> plaintex

command :: Parser LatexChunk
command = backSlashCommand <|> subscript <|> superscript

arg = char '{' *> latex <* char '}' >>= return . chunkBox

backSlashCommand = do
    char '\\'
    frac <|> psqrt <|> root <|> ppi

frac = do
    string "frac" 
    num <- arg
    denom <- arg
    let width = max (B.cols num) (B.cols denom)
    let new_box = B.vcat B.center1 [num, B.text (replicate width '-'), denom]
    return $ LatexChunk Inline new_box

psqrt = do
    string "sqrt"
    contents <- arg
    let width = B.cols contents
    let height = B.cols contents
    let underroot = B.vcat B.center1 [contents, B.text (replicate width '_')]
    let new_box = B.hcat B.bottom [makeRadical height, underroot]
    return $ LatexChunk Inline new_box

root = do
    string "root"
    power <- arg
    contents <- arg
    let width = B.cols contents
    let height = B.cols contents
    let underroot = B.vcat B.center1 [contents, B.text (replicate width '_')]
    let radical = B.vcat B.right [power, makeRadical height]
    let new_box = B.hcat B.bottom [radical, underroot]
    return $ LatexChunk Inline new_box

ppi = string "pi" *> return (LatexChunk Inline (B.char '\x03c0'))

subscript = do
    char '_'
    sub <- arg
    return $ LatexChunk Below sub

superscript = do
    char '^'
    super <- arg
    return $ LatexChunk Above super

plaintex = do
    contents <- many $ noneOf "\\_^"
    return $ LatexChunk Inline (B.text contents)

makeRadical 1 = B.char '\x221a'
makeRadical height = B.hcat B.top [B.moveDown 1 $ makeRadical (height-1),
                                   B.char '/']

data ChunkPosition = Inline | Above | Below

data LatexChunk = LatexChunk {
    chunkPos :: ChunkPosition,
    chunkBox :: Box
}

--TODO: Add rules for Above Below mixing, change associativity

instance Monoid LatexChunk where
    mempty = LatexChunk Inline B.nullBox
    c1 `mappend` c2 = let new_pos = chunkPos c2 in
        case chunkPos c1 of
            Inline -> LatexChunk new_pos $
                          B.hcat B.center1 [chunkBox c1, chunkBox c2]
            Above -> let height  = B.rows (chunkBox c2)
                         new_box = B.hcat B.bottom
                                       [B.moveUp height (chunkBox c1),
                                        chunkBox c2]
                         in LatexChunk new_pos new_box
            Below -> let height  = B.rows (chunkBox c2)
                         new_box = B.hcat B.top
                                       [B.moveDown height (chunkBox c1),
                                        chunkBox c2]
                         in LatexChunk new_pos new_box
