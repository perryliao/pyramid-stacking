module Game where

import BSTree
import Data.List
import Data.Maybe
import System.Random
import Control.Monad
import System.IO.Unsafe

alpha = ['A'..'Z']
dummyVar = "."
specialChars = "!@#$%&"

-- Current state of the game
-- [[Char]] a list of player's hands
-- BSTree the current state of the board
data State = State [[Char]] (BSTree [Char] [Char])
        deriving (Eq, Show)

data Result = EndOfGame State Bool
            | ContinueGame State
        deriving (Eq, Show)


-- getSlotLimit numTokens
-- given the amount of tokens players have, and the number of players,
-- calculate how big the play field should be.
getSlotLimit :: Int -> Int -> Int
getSlotLimit numTokens numPlayers = getSlotLimitHelper numTokens 0
  where
    getSlotLimitHelper numTokens currentDepth
      | (numTokens * numPlayers) > (2^currentDepth - 1) = getSlotLimitHelper numTokens (currentDepth + 1)
      | otherwise = (2^currentDepth - 1)

-- initTree numTokens numPlayers
-- given amount of tokens and number of players, generate a clean game board
initTree :: Int -> Int -> BSTree [Char] [Char]
initTree numTokens numPlayers = createNode 0 0 (getSlotLimit numTokens numPlayers)
  where
    createNode id currentDepth maxNodes
      | (2^currentDepth - 1) > maxNodes = Empty
      | otherwise = (Node ([alpha!!currentDepth] ++ (show id)) dummyVar (createNode (2 * id) (currentDepth + 1) maxNodes) (createNode (2 * id + 1) (currentDepth + 1) maxNodes))

-- calculates the depth of the given tree
findDepth :: BSTree k v -> Int
findDepth tree = findDepthHelper 0 tree
  where
    findDepthHelper depth Empty = depth
    findDepthHelper depth (Node k v lt rt) = findDepthHelper (depth+1) rt

-- Prints the state of the board at a given depth
printTreeAtDepth d tree = putStrLn (foldr (++) [] (printTreeAtDepthHelper d d (findDepth tree) tree))
  where
    printTreeAtDepthHelper d i maxDepth (Node k v lt rt) =
      do
        if i > 0
          then do
            printTreeAtDepthHelper d (i-1) maxDepth lt ++ printTreeAtDepthHelper d (i-1) maxDepth rt
          else do
            let spaces = (replicate ((2^(maxDepth - (d + 1))-1)) ' ')
            if (read (filter (`notElem` [alpha!!d]) k) :: Int) == 0
              then do
                return ([alpha!!d] ++ ": " ++ spaces ++ v ++ spaces ++ " ")
              else do
                return (spaces ++ v ++ spaces ++ " ")

-- Prints out the current state of the game board
printTree tree = printTreeHelper 0 (findDepth tree) tree
  where
    printTreeHelper d maxDepth tree
      | d < (maxDepth - 1) =
        do
          printTreeAtDepth d tree
          printTreeHelper (d+1) maxDepth tree
      | d == (maxDepth - 1) =
        do
          printTreeAtDepth d tree

-- Calculates the raw value of this node
determineOrder :: [Char] -> Int
determineOrder k = (2^(fromJust (elemIndex (k!!0) alpha))-1) + (read (tail k) :: Int) + 1

-- Calculate if the target key is on the left side of the current key
shouldSplitLeft :: [Char] -> [Char] -> Bool
shouldSplitLeft currentKey targetKey = do
  let diff = ((fromJust (elemIndex (targetKey!!0) alpha))-1) - ((fromJust (elemIndex (currentKey!!0) alpha))-1)
  let val = determineOrder currentKey
  let target = determineOrder targetKey
  target < (val * 2^diff) + 2^(diff - 1)

-- returns if it's possible to place the val on the selected key
canPlaceOnTree key val (Node k v Empty Empty) = v == dummyVar

canPlaceOnTree key val (Node k0 v0 lt@(Node k1 v1 lt1 rt1) rt@(Node k2 v2 lt2 rt2))
  | key == k0 = (v0 == dummyVar) && (((v1 == val) || (v2 == val)) && (v1 /= dummyVar) && (v2 /= dummyVar))
  | otherwise = do
      if (shouldSplitLeft k0 key)
        then
          canPlaceOnTree key val lt
        else
          canPlaceOnTree key val rt


-- insertBoard key val tree
-- returns the tree that results from inserting key with value into tree
insertBoard key val (Node k v Empty Empty)
  | (key == k) && (v == dummyVar) = Node key val Empty Empty
  | otherwise = Node k v Empty Empty
insertBoard key val (Node k0 v0 lt@(Node k1 v1 lt1 rt1) rt@(Node k2 v2 lt2 rt2))
  | (key == k0) && (v0 == dummyVar) && (((v1 == val) || (v2 == val)) && (v1 /= dummyVar) && (v2 /= dummyVar)) = Node key val lt rt
  | [k0!!0] < [key!!0] = do
    if (shouldSplitLeft k0 key)
      then
        (Node k0 v0 (insertBoard key val lt) rt)
      else
        (Node k0 v0 lt (insertBoard key val rt))
  | otherwise = Node k0 v0 lt rt

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

-- fixdel removes deleted elements from string
-- from A3 solutions
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r
