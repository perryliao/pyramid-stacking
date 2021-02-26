module Deck where

import System.Random
import System.IO.Unsafe

specialChars = "!@#$%&"

-- pick a random value from a given list
rand :: [a] -> IO a
rand lst = do
  i <- randomRIO(0, (length lst) - 1)
  return $ lst !! i

-- generateHand n
-- creates a player's hand with n number of characters
generateHand :: Int -> [Char]
generateHand 0 = ""
generateHand n = [(unsafePerformIO (rand specialChars))] ++ (generateHand (n-1))

-- create all players hands
generateAllHands :: Int -> Int -> [[Char]]
generateAllHands 0 _ = []
generateAllHands numPlayers size = (generateHand size) : generateAllHands (numPlayers-1) size

-- generate the updated hand after placing a piece on the board
useHand :: Char -> [Char] -> [Char]
useHand val (x:xs)
  | val == x = xs
  | otherwise = x : (useHand val xs)

-- returns if it's possible to play the selected character in the hand
canPlayHand :: Char -> [Char] -> Bool
canPlayHand val [] = False
canPlayHand val (x:xs)
  | val == x = True
  | otherwise = (canPlayHand val xs)
