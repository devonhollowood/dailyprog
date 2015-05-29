import System.IO (hFlush, stdout)
import System.Random
import Data.List (intercalate, sortBy, sort, tails, groupBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Char (toLower)
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
    let player_types = Human:replicate (nplayers-1) CPU
    gen <- getStdGen
    let deck = evalState (shuffle newDeck) gen
    let try_deal = runEitherT (dealTexasHoldem player_types)
    case evalState try_deal deck of
        Left _  -> putStrLn "Error: Not enough cards for that many players!"
        Right (players, deal) -> evalStateT (runRound deal) players
    where validate s = readEither s >>= check_positive
          check_positive n = if n >= 2 && n <= 8
                             then Right n
                             else Left "Must be 2-8 players."

-- Run a round of texas hold'em
runRound :: TexasHoldemDeal -> StateT [Player] IO ()
runRound deal = do
    do_turn []
    liftIO . putStrLn $ "Flop: " ++ showMany (flop deal)
    do_turn (flop deal)
    liftIO . putStrLn $ "Turn: " ++ show (turn deal)
    do_turn (flop deal ++ [turn deal])
    liftIO . putStrLn $ "River: " ++ show (river deal)
    do_turn (flop deal ++ [turn deal, river deal])
    players <- get
    liftIO $ displayWinner players (flop deal ++ [turn deal, river deal])
    where not_folded = not . playerFolded
          do_turn cs = get >>= liftIO . mapM (takeTurn cs) >>= put

-- Displays the winner
displayWinner :: [Player] -> [Card] -> IO ()
displayWinner players table_cards = do
    let remaining = filter (not . playerFolded) players
    mapM put_hand remaining
    let player_ranks = descendingBy (comparing p_hand) players
    case winners player_ranks of
        [x] -> putStrLn $ concat [p_name (head player_ranks), " wins!"]
        xs  -> putStrLn $ intercalate ", " (map p_name xs)
                          ++ " tie and split the pot!"
    where put_hand p = putStrLn $ concat [p_name p, " has ", show (p_hand p)]
          p_name p = playerName p
          p_hand p = bestHand $ playerHand p ++ table_cards
          winners (x:xs) = x:takeWhile (\p -> comparing p_hand p x == EQ) xs

-- Take the appropriate Human/CPU turn for player
takeTurn :: [Card] -> Player -> IO Player
takeTurn table_cards player
    | playerFolded player = return player
    | otherwise           = do
          let name = playerName player
          putStrLn $ concat [name, "'s turn!"]
          action <- case playerType player of
                        Human -> humanTurn player table_cards
                        CPU   -> return $ cpuTurn player table_cards
          case action of
              Call -> putStrLn $ concat [name, " calls!"]
              Fold -> putStrLn $ concat [name, " folds!"]
          return $ (\(Player t n h f) -> Player t n h (action==Fold)) player

-- Take a Human turn
humanTurn :: Player -> [Card] -> IO PlayerAction
humanTurn player table_cards = do
    putStrLn $ "Your hand: " ++ showMany (playerHand player)
    putStrLn $ "Table cards: " ++ showMany table_cards
    prompt parseAction "Call/Fold? (c/f)"

-- Parse an action for a human turn.
parseAction :: String -> Either String PlayerAction
parseAction response
    | map toLower response `elem` ["call", "c"] = Right Call
    | map toLower response `elem` ["fold", "f"] = Right Fold
    | otherwise = Left "Must answer \"Call\" or \"Fold\""

-- Take a CPU turn
-- CPUs always call on their first turn.
-- On subsequent hands, CPUs call if their hand doesn't give them anything new
-- over what is already on the table
cpuTurn :: Player -> [Card] -> PlayerAction
cpuTurn player [] = Call
cpuTurn player table_cards =
    let own_cards = playerHand player in
    if bestHand (own_cards++table_cards) > bestHand table_cards
    then Call
    else Fold

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

-- Uses player_types to create a list of players, and deals a game of Texas
-- Hold'em to the players using a deck stored in a SafeDeckState
-- Returns list of created players, and the TexasHoldemDeal
dealTexasHoldem :: [PlayerType] -> SafeDeckState ([Player], TexasHoldemDeal)
dealTexasHoldem player_types = do
    players <- forM (zip [1..] player_types) $ \(pnum, ptype) -> do
                   hand <- replicateM 2 dealCard
                   return $ Player ptype pnum hand False
    dealCard -- burn a card
    flop  <- replicateM 3 dealCard
    dealCard -- burn a card
    turn  <- dealCard
    dealCard -- burn a card
    river  <- dealCard
    return (players, TexasHoldemDeal flop turn river)

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
        bestStraight      cards <|>
        bestThreeOfAKind  cards <|>
        bestTwoPair       cards <|>
        bestPair          cards
    of
        Just hand_rank -> hand_rank
        Nothing        -> highCard cards

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

descendingBy :: (a -> a -> Ordering) => [a] -> [a]
descendingBy f = sortBy (flip f)

showMany :: Show a => [a] -> String
showMany = intercalate ", " . map show

playerName :: Player -> String
playerName player =
    let pnum = show $ playerNum player
        ptype = show $ playerType player
    in
        concat ["Player ", pnum, " (", ptype, ")"]

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
                | FullHouse [Card] [Card] -- sets of cards are triplet, pair
                | FourOfAKind [Card] Kickers
                | StraightFlush [Card]
                deriving (Eq, Ord)

instance Show HandRank where
    show (HighCard c ks) = concat ["High Card (", show c, ") ", showKickers ks]
    show (Pair cs ks) = concat ["Pair (", showMany cs, ") ", showKickers ks]
    show (TwoPair cs k) = concat ["Two Pair (", showMany cs, ") ", showKicker k]
    show (ThreeOfAKind cs ks) = concat ["Three-of-a-Kind (", showMany cs, ") ",
                                      showKickers ks]
    show (Straight cs) = concat ["Straight (", showMany cs, ") "]
    show (Flush cs) = concat ["Flush (", showMany cs, ") "]
    show (FullHouse t p) = concat ["FullHouse (", showMany (t++p), ") "]
    show (FourOfAKind cs k) = concat ["Four-of-a-Kind (", showMany cs, ")",
                                    showKicker k]
    show (StraightFlush cs) = concat ["Straight Flush (", showMany cs, ") "]

showKicker :: Kickers -> String
showKicker k = concat ["(Kicker: ", showMany k, ")"]

showKickers :: Kickers -> String
showKickers k = concat ["(Kickers: ", showMany k, ")"]

data PlayerType = Human | CPU
                  deriving Show
data Player = Player {
      playerType   :: PlayerType,
      playerNum    :: Int,
      playerHand   :: Hand,
      playerFolded :: Bool
    }

data PlayerAction = Call | Fold
                    deriving Eq
