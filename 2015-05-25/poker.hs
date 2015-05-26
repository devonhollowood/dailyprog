import System.IO (hFlush, stdout)
import System.Random
import Data.List (intersperse)
import Text.Read (readEither)
import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Trans.Either
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main = do
    nplayers <- prompt validate "How many players?"
    gen <- getStdGen
    let deck = evalState (shuffle newDeck) gen
    let deal = runEitherT (dealTexasHoldem nplayers)
    case evalState deal deck of
        Left _  -> putStrLn "Error: Not enough cards for that many players!"
        Right d -> output d
    where validate s = readEither s >>= check_positive
          check_positive n = if n >= 2 && n <= 8
                             then Right n
                             else Left "Must be 2-8 players."

-- Outputs a TexasHoldemDeal
output :: TexasHoldemDeal -> IO ()
output deal = do
    putStrLn $ "Your hand: " ++ showmany (head $ hands deal)
    putStrLn $ "Flop: " ++ showmany (flop deal)
    putStrLn $ "Turn: " ++ show (turn deal)
    putStrLn $ "River: " ++ show (river deal)
    where showmany = concat . intersperse ", " . map show

-- Repeatedly prompts the user using query until parser returns a Right value
prompt :: Show e => (String -> Either e a) -> String -> IO a
prompt parser query = do
    putStr query
    putStr " "
    hFlush stdout
    response <- getLine
    case parser response of
        Right val  -> return val
        Left e -> do
                       putStrLn ("Invalid response: " ++ show e)
                       prompt parser query

-- Deals a game of Texas Hold'em, using deck stored in a SafeDeckState
dealTexasHoldem :: Int -> SafeDeckState TexasHoldemDeal
dealTexasHoldem nplayers = do
    hs <- replicateM nplayers (replicateM 2 dealCard)
    dealCard -- burn one
    f  <- replicateM 3 dealCard
    dealCard -- burn one
    t  <- dealCard
    dealCard -- burn one
    r  <- dealCard
    return $ TexasHoldemDeal hs f t r

-- Deals a sing card, using deck stored in a SafeDeckState
dealCard :: SafeDeckState Card
dealCard = do
    cards <- get
    case cards of
        []     -> left NotEnoughCards
        (x:xs) -> put xs >> right x

-- Create new deck
newDeck :: Deck
newDeck = [Card r s | r <- [Two ..], s <- [Diamonds ..]]

-- Shuffles a list using a RandomGen stored in a state
shuffle :: RandomGen g => [a] -> State g [a]
shuffle xs = do
    swaps <- fyswaps (length xs)
    return $ V.toList $ runST $ do
        list <- V.thaw $ V.fromList xs
        mapM_ (uncurry $ MV.swap list) swaps
        V.freeze list

-- List of swaps for Fisher-Yates algorithm for list of length n
fyswaps :: RandomGen g => Int -> State g [(Int, Int)]
fyswaps n = forM [n-1, n-2..0] $ \i -> do
    j <- getRandomR 0 i
    return (i, j)

-- Get a random number between lo and hi, inclusive
getRandomR :: RandomGen g => Random a => a -> a -> State g a
getRandomR lo hi = do
    gen <- get
    let (val, gen') = randomR (lo, hi) gen
    put gen'
    return val

-- Below are a number of data/type declarations, which build up the
-- fundamentals of Texas Hold'em

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
            Jack | Queen | King | Ace
            deriving (Eq, Enum, Show)

data Suit = Diamonds | Clubs | Hearts | Spades
            deriving (Eq, Enum, Show)

data Card = Card {
      rank :: Rank,
      suit :: Suit
    } deriving Eq

instance Show Card where
    show card = concat [show (rank card), " of ", show (suit card)]

type Deck = [Card]
type Hand = [Card]

data TexasHoldemDeal = TexasHoldemDeal {
      hands :: [Hand],
      flop  :: [Card],
      turn  :: Card,
      river :: Card
    } deriving Show

data NotEnoughCards = NotEnoughCards

type SafeDeckState a = EitherT NotEnoughCards (State Deck) a
