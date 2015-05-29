import System.IO (hFlush, stdout)
import System.Random
import Data.List (intersperse, sortBy, sort, tails, groupBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.Read (readEither)
import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Trans.Either
import Control.Applicative ((<|>))
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

-- Deals a single card, using deck stored in a SafeDeckState
dealCard :: SafeDeckState Card
dealCard = do
    cards <- get
    case cards of
        []     -> left NotEnoughCards
        (x:xs) -> put xs >> right x

-- Create new deck
newDeck :: Deck
newDeck = [Card r s | r <- [Two ..], s <- [Diamonds ..]]

bestHand :: [Card] -> HandRank
bestHand cards =
    case
        bestStraightFlush cards <|>
        bestFourOfAKind   cards <|>
        bestFullHouse     cards <|>
        bestFlush         cards <|>
        bestStraight       cards <|>
        bestThreeOfAKind  cards <|>
        bestTwoPair       cards <|>
        bestPair          cards
    of
        Just hand_rank -> hand_rank
        Nothing        -> highCard cards

hand1 = [Card Ace Spades, Card Two Hearts, Card King Diamonds, Card King Spades, Card Two Clubs, Card King Clubs, Card Jack Diamonds]
hand2 = [Card Two Clubs, Card Six Hearts, Card Seven Hearts, Card Eight Hearts, Card Nine Hearts, Card Ten Hearts, Card Ace Spades]
hand3 = [Card Six Hearts, Card Seven Hearts, Card Eight Hearts, Card Nine Hearts, Card Ten Hearts, Card Jack Spades, Card Jack Hearts]
hand4 = [Card Two Clubs, Card Two Hearts, Card King Hearts, Card Nine Hearts, Card King Clubs, Card Two Spades, Card Two Diamonds]
hand5 = [Card Three Clubs, Card Three Hearts, Card King Hearts, Card Nine Hearts, Card King Clubs, Card King Spades, Card Three Diamonds]

-- Returns best straight-flush in given list of cards, or nothing
bestStraightFlush :: [Card] -> Maybe HandRank
bestStraightFlush cards =
    case descending $ straight_flushes cards of
        best:rest -> Just $ Flush best
        _         -> Nothing
    where straight_flushes = map descending
                             . filter (\cs -> isFlush cs && isStraight cs)
                             . combinations 5

-- Returns best four-of-a-kind in given list of cards, or nothing
bestFourOfAKind :: [Card] -> Maybe HandRank
bestFourOfAKind cards =
    case descending . filter is_quartet $ combinations 4 cards of
        best:rest -> Just $ FourOfAKind best (kickers best)
        _         -> Nothing
    where is_quartet xss@(x:xs) = length xss == 4 && all ((==rank x) . rank) xs
          kickers four = take 1 . filter (`notElem` four) $ descending cards

-- Returns best full house in given list of cards, or nothing
bestFullHouse :: [Card] -> Maybe HandRank
bestFullHouse cards =
    case descending . catMaybes . map arrange $ combinations 5 cards of
        best:rest -> Just $ best
        _         -> Nothing
    where arrange xs =
              case groupBy (\a b -> rank a == rank b) (descending xs) of
                  [pair@[a,b], trip@[c,d,e]] -> Just $ FullHouse trip pair
                  [trip@[a,b,c], pair@[d,e]] -> Just $ FullHouse trip pair
                  _                          -> Nothing

-- Returns best flush in given list of cards, or nothing
bestFlush :: [Card] -> Maybe HandRank
bestFlush cards =
    case descending $ flushes cards of
        best:rest -> Just $ Flush best
        _         -> Nothing
    where flushes = map descending . filter isFlush . combinations 5

-- Tests whether a set of cards is a flush
isFlush :: [Card] -> Bool
isFlush xss@(x:xs) = length xss == 5 && all ((==suit x) . suit) xs
isFlush _ = False

-- Returns best straight in given list of cards, or nothing
bestStraight :: [Card] -> Maybe HandRank
bestStraight cards =
    case descending $ straights cards of
        best:rest -> Just $ Straight best
        _         -> Nothing
    where straights = map descending . filter isStraight . combinations 5

-- Tests whether a set of cards is a flush
isStraight :: [Card] -> Bool
isStraight xs = (length xs == 5) && (all successive . pairs $ descending xs)
    where pairs xs = zip xs (tail xs)
          successive (a,b)
              | rank a == Ace = False
              | otherwise     = rank a == succ (rank b)

-- Returns best three-of-a-kind in given list of cards, or nothing
bestThreeOfAKind :: [Card] -> Maybe HandRank
bestThreeOfAKind cards =
    case descending . filter is_triplet $ combinations 3 cards of
        best:rest -> Just $ ThreeOfAKind best (kickers best)
        _         -> Nothing
    where is_triplet xss@(x:xs) = length xss == 3 && all ((==rank x) . rank) xs
          kickers triple = take 2 . filter (`notElem` triple) $ descending cards

-- Returns best two-pair in given list of cards, or nothing
bestTwoPair :: [Card] -> Maybe HandRank
bestTwoPair cards =
    case elim_dups . descending . filter is_pair $ combinations 2 cards of
        best:next:rest -> Just $ TwoPair (best++next) (kickers $ best++next)
        _              -> Nothing
    where is_pair [a, b] = rank a == rank b
          elim_dups (best:rest) = best:filter (all (`notElem` best)) rest
          elim_dups [] = []
          kickers pairs = take 1 . filter (`notElem` pairs) $ descending cards

-- Returns best pair in given lsit of cards, or nothing
bestPair :: [Card] -> Maybe HandRank
bestPair cards =
    case reverse . sort . filter is_pair $ combinations 2 cards of
        best:rest -> Just $ Pair best (kickers best)
        _         -> Nothing
    where is_pair [a, b] = rank a == rank b
          kickers pair = take 3 . filter (`notElem` pair) $ descending cards

-- Returns the highest-rank card in given lsit of cards
highCard :: [Card] -> HandRank
highCard cards = HighCard best rest
    where best:rest = take 5 . descending $ cards

-- Gives all combinations of n elements of a list
combinations :: Int -> [a] -> [[a]]
combinations n xs
    | n <=0     = [[]]
    | otherwise = [hd:tl | hd:rest <- tails xs,
                           tl <- combinations (n-1) rest]

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

-- A few utility functions that I found myself typing a lot
descending :: Ord a => [a] -> [a]
descending = sortBy (flip compare)

-- Below are a number of data/type declarations, which build up the
-- fundamentals of Texas Hold'em

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
            Jack | Queen | King | Ace
            deriving (Eq, Ord, Enum, Show)

data Suit = Diamonds | Clubs | Hearts | Spades
            deriving (Eq, Ord, Enum, Show)

data Card = Card {
      rank :: Rank,
      suit :: Suit
    } deriving Eq

instance Ord Card where
    compare = comparing rank

instance Show Card where
    show card = concat [show (rank card), " of ", show (suit card)]

type Deck = [Card]
type Hand = [Card]
type Kickers = [Card]

data TexasHoldemDeal = TexasHoldemDeal {
      hands :: [Hand],
      flop  :: [Card],
      turn  :: Card,
      river :: Card
    } deriving Show

data NotEnoughCards = NotEnoughCards

type SafeDeckState a = EitherT NotEnoughCards (State Deck) a

data HandRank = HighCard Card Kickers
                | Pair [Card] Kickers
                | TwoPair [Card] Kickers
                | ThreeOfAKind [Card] Kickers
                | Straight [Card]
                | Flush [Card]
                | FullHouse [Card] [Card] --triplet, pair
                | FourOfAKind [Card] Kickers
                | StraightFlush [Card]
                deriving (Eq, Ord, Show)
