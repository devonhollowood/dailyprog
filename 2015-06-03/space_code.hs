import System.Environment (getArgs)
import Data.Char (ord, chr, toLower)
import Data.List (sortBy, nub)
import Data.Ord (comparing)
import Control.Monad (forM_)
import Data.Bits (xor)

main = do
    chars <- getArgs >>= return . map (chr . read)
    let ciphers = [omicronV, hoth, ryzaIV, htrae]
    let (decoded, cipher) = crack ciphers chars
    putStrLn $ concat [name cipher, ": ", decoded]

-- Decode using all ciphers, and return the best one
crack :: [Cipher] -> String -> (String, Cipher)
crack ciphers ciphertext =
    let best = head $ sortBy (comparing rank) ciphers
    in (decode best ciphertext, best)
    where rank = score . flip decode ciphertext

-- Score msg, with a lower score corresponding to being more likely to be
-- english text
score :: String -> Double
score msg = let f = frequencies msg in chi2 f / (fromIntegral $ length f)

-- get [(expected letter freq, observed letter freq)] for msg
frequencies msg = [(expected c , fromIntegral $ observed c)
                   | c <- nub $ prepared_msg]
    where expected c = englishFrequency c * (fromIntegral $ length prepared_msg)
          observed c = length $ filter (==c) prepared_msg
          prepared_msg = filter (`elem` allowed_chars) $ map toLower msg
          allowed_chars = ' ':['a'..'z']

-- Chi^2 for [(observed, expected)] values (assuming poisson distr)
chi2 :: [(Double, Double)] -> Double
chi2 = sum . map chi2'
    where chi2' (exp, obs) = (obs-exp)^2/exp

-- Define ciphers
data Cipher = Cipher {
    name :: String,
    encode :: String -> String,
    decode :: String -> String
}

omicronV = Cipher "Omicron V"
                  (map $ chr . (xor 16) . ord)
                  (map $ chr . (xor 16) . ord)

hoth = Cipher "Hoth"
              (map $ chr . (+10) . ord)
              (map $ chr . (subtract 10) . ord)

ryzaIV = Cipher "Ryza IV"
                (map $ chr . (subtract 1) . ord)
                (map $ chr . (+1) . ord)

htrae = Cipher "Htrae" reverse reverse

-- list of English letter frequencies from
-- http://www.data-compression.com/english.html
englishFrequency :: Char -> Double
englishFrequency 'a' = 0.0652
englishFrequency 'b' = 0.0124
englishFrequency 'c' = 0.0217
englishFrequency 'd' = 0.0350
englishFrequency 'e' = 0.1041
englishFrequency 'f' = 0.0198
englishFrequency 'g' = 0.0159
englishFrequency 'h' = 0.0493
englishFrequency 'i' = 0.0558
englishFrequency 'j' = 0.0009
englishFrequency 'k' = 0.0051
englishFrequency 'l' = 0.0331
englishFrequency 'm' = 0.0202
englishFrequency 'n' = 0.0564
englishFrequency 'o' = 0.0596
englishFrequency 'p' = 0.0138
englishFrequency 'q' = 0.0009
englishFrequency 'r' = 0.0498
englishFrequency 's' = 0.0516
englishFrequency 't' = 0.0729
englishFrequency 'u' = 0.0225
englishFrequency 'v' = 0.0083
englishFrequency 'w' = 0.0171
englishFrequency 'x' = 0.0014
englishFrequency 'y' = 0.0146
englishFrequency 'z' = 0.0007
englishFrequency ' ' = 0.1918
