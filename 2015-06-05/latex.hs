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
        Right bx -> B.printBox bx

latex :: Parser Box
latex = do
    chunks <- many latexChunk
    return . toBox $ mconcat chunks

latexChunk :: Parser LatexChunk
latexChunk = command <|> plaintex

command :: Parser LatexChunk
command = backSlashCommand <|> try subsuper <|> subscript <|> superscript
          <?> "valid command"

arg = between (char '{') (char '}') latex <?> "bracketed argument"

backSlashCommand = do
    char '\\'
    frac <|> psqrt <|> root <|> ppi <?> "backslash command"

frac = do
    string "frac" 
    num <- arg
    denom <- arg
    let width = max (B.cols num) (B.cols denom)
    let new_box = B.vcat B.center1 [num, B.text (replicate width '-'), denom]
    return $ chunkInLine new_box

psqrt = do
    string "sqrt"
    contents <- arg
    let width = B.cols contents
    let height = B.rows contents
    let ceiling = B.text (replicate width '_')
    let aligned_ceil = B.alignHoriz B.right (width + height) ceiling
    let radical = makeRadical height
    return $ chunk aligned_ceil (B.hcat B.center1 [radical, contents]) B.nullBox

root = do
    string "root"
    power <- arg
    contents <- arg
    let width = B.cols contents
    let height = B.rows contents
    let ceiling = B.text (replicate width '_')
    let radical = makeRadical height
    return $ chunk (B.hcat B.center1 [B.alignHoriz B.right height power, 
                                      ceiling])
                   (B.hcat B.center1 [radical, contents])
                   B.nullBox

ppi = string "pi" *> return (chunkInLine (B.char '\x03c0'))

subsuper = do
    char '_'
    sub <- arg
    char '^'
    super <- arg
    return $ chunk super B.nullBox sub

subscript = do
    char '_'
    sub <- arg
    return $ chunkBelow sub

superscript = do
    char '^'
    super <- arg
    return $ chunkAbove super

plaintex = do
    contents <- many1 $ noneOf "\\_^{}"
    return $ chunkInLine (B.text $ filter (/= '\n') contents)

makeRadical 1 = B.char '\x221a'
makeRadical height = B.hcat B.top [B.moveDown 1 $ makeRadical (height-1),
                                   B.char '/']

data ChunkPosition = Inline | Above | Below

data LatexChunk = LatexChunk {
    above  :: Box,
    inLine :: Box,
    below  :: Box
}

instance Monoid LatexChunk where
    mempty = LatexChunk B.nullBox B.nullBox B.nullBox
    mappend a b = let combine f = B.hcat B.center1 [f a, f b]
                  in LatexChunk (combine above) (combine inLine) (combine below)

chunk :: Box -> Box -> Box -> LatexChunk
chunk top mid bot = let width = maximum $ map B.cols [top, mid, bot]
                        align = B.alignHoriz B.left width
                    in LatexChunk (align top) (align mid) (align bot)

chunkAbove :: Box -> LatexChunk
chunkAbove top = chunk top B.nullBox B.nullBox
chunkInLine :: Box -> LatexChunk
chunkInLine mid = chunk B.nullBox mid B.nullBox
chunkBelow :: Box -> LatexChunk
chunkBelow bot = chunk B.nullBox B.nullBox bot

toBox :: LatexChunk -> Box
toBox lc = B.vcat B.left [above lc, inLine lc, below lc]
