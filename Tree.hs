module Tree where

import BSTree
import Data.List
import Data.Maybe

alpha = ['A'..'Z']
dummyVar = "."

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
      | (2^currentDepth - 1) >= maxNodes = Empty
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
canPlaceOnTree :: [Char] -> [Char] -> BSTree [Char] [Char] -> Bool
canPlaceOnTree key val (Node k v Empty Empty) = v == dummyVar
canPlaceOnTree key val (Node k0 v0 lt@(Node k1 v1 lt1 rt1) rt@(Node k2 v2 lt2 rt2))
  | key == k0 = (v0 == dummyVar) && (((v1 == val) || (v2 == val)) && (v1 /= dummyVar) && (v2 /= dummyVar))
  | (shouldSplitLeft k0 key) = canPlaceOnTree key val lt
  | otherwise = canPlaceOnTree key val rt

-- returns if the given key exists in the given tree
keyExistsInTree :: [Char] -> BSTree [Char] [Char] -> Bool
keyExistsInTree key (Node k v Empty Empty) = key == k
keyExistsInTree key (Node k v lt rt)
  | key == k = True
  | (shouldSplitLeft k key) = keyExistsInTree key lt
  | otherwise = keyExistsInTree key rt

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
