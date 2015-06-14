--------------------------------------------------------------------------------
-- | 
-- File        : vigeneres_crack.hs
-- Note        : A decipherment of the Vigenere cipher, for BayHac 2015.
-- 
-- This module provides a function to decipher a message encoded, using the Vigenere cipher.
-- 
--------------------------------------------------------------------------------

import Data.List
import Data.Char
import CaesarCipher

pairSpacings :: Int -> String -> [Int]
pairSpacings l xs = concat [[n | n <- [l..(length xs' - l)], take l xs' == take l (drop n xs')]
                               | xs' <- [drop m xs | m <- [0..(length xs - 2 * l)]]]

commonFactors :: [Int] -> [Int]
commonFactors xs = foldl intersect (head xss) (tail xss)
                   where xss = [factors x | x <- xs]

vcrack :: String -> String
vcrack xs | null pair_spacings = "Sorry, couldn't find any pairs."
          | null com_facts     = "Sorry, couldn't find any common factors. Pair spacings found: " ++ intercalate ", " (map show pair_spacings)
          | otherwise = interleave [crack xs' | xs' <- [takeEvery n (drop m xs) | m <- [0..(n - 1)]]]
                                     where n = maximum com_facts
                                           com_facts = commonFactors pair_spacings
                                           pair_spacings = pairSpacings 5 xs

vcipher :: String -> String -> String
vcipher keyword plaintext = interleave [encipher shift text | (shift, text) <- zip (map let2int keyword)
                                                                                   [takeEvery n (drop m plaintext) | m <- [0..(n - 1)]]
                                       ] where n = length keyword

plaintext = "i am so glad to be back at bayhac! i really love seeing you all again and i wish that we did this more often than once a year. \
\ i look forward to hearing all the talks you have prepared and discussing them with you at length, until my head begins to smoke. \
\ this is a wonderful group of people, alive with creativity, acceptance, and community spirit, and i count myself very \
\ fortunate to be a part of it. i'm a little jealous of the baypiggies meetup, which is allowed to convene in a really nice linkedin \
\ conference room and is served dinner. how can we get ourselves a corporate sponsorship like that? i wish i could afford the cut in salary \
\ it would require, in order to get someone to hire me to do haskell professionally. alas, my kids are too demanding. maybe after retirement. \
\ i hope you all have a wonderful weekend at bayhac!"

main :: IO()
main = do
    putStrLn "Attempting to decode 'kdvnhoo lv ixq', using frequency analysis..."
    putStrLn $ crack "kdvnhoo lv ixq"
    let ciphertext = vcipher "bayhac" plaintext
    putStrLn ""
    putStrLn "Attempting to decode:"
    putStrLn ""
    putStrLn ciphertext
    putStrLn ""
    putStrLn "using frequency analysis..."
    putStrLn ""
    putStrLn $ crack ciphertext
    putStrLn ""
    putStrLn "using vcrack()..."
    putStrLn ""
    putStrLn $ vcrack ciphertext

-- Helper functions.
interleave :: [[a]] -> [a]
interleave = concat . transpose

factors :: Int -> [Int]
factors x = [n | n <- [2..x], x `mod` n == 0]

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = head xs : takeEvery n (drop n xs)

encipher :: Int -> String -> String
encipher n xs = [shift n x | x <- xs]

