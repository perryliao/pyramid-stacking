module Game where

import BSTree

alpha = ['A'..'Z']

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
      | otherwise = (Node ([alpha!!currentDepth] ++ (show id)) "*" (createNode (2 * id) (currentDepth + 1) maxNodes) (createNode (2 * id + 1) (currentDepth + 1) maxNodes))

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
