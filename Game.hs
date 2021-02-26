module Game where

import BSTree
import Tree
import Deck
import Data.List
import Data.Maybe
import Control.Monad

-- Current state of the game
-- [[Char]] a list of player's hands
-- BSTree the current state of the board
data State = State [[Char]] (BSTree [Char] [Char])
        deriving (Eq, Show)

data Result = EndOfGame State Bool
            | ContinueGame State
        deriving (Eq, Show)

type Game = [Char] -> [Char] -> State -> Result
game :: Game
game key val (State decks tree)
  | gameOver new_decks new_tree = EndOfGame (State new_decks new_tree) ((length (new_decks!!0)) == 0)
  | otherwise                   = ContinueGame (State new_decks new_tree)
  where
    (State new_decks new_tree) = updateGameState key val (State decks tree)

-- checks if the game is over
gameOver :: [[Char]] -> BSTree [Char] [Char] -> Bool
gameOver decks board = (foldr (||) False (map (noValidMoves board) decks))
  where
    -- returns True of there are no more valid moves, and game should be over.
    noValidMoves :: BSTree [Char] [Char] -> [Char] -> Bool
    noValidMoves _ [] = True
    noValidMoves (Node k v Empty Empty) deck = (v /= dummyVar)
    noValidMoves (Node k v lt@(Node kl vl llt lrt) rt@(Node kr vr rlt rrt)) deck
      | (v == dummyVar) && (((vl!!0) `elem` deck) || ((vr!!0) `elem` deck)) && (vl /= dummyVar) && (vr /= dummyVar) = False
      | otherwise = (noValidMoves lt deck) && (noValidMoves rt deck)


-- updateGameState creates the next iteration of the game state
updateGameState :: [Char] -> [Char] -> State -> State
updateGameState k v (State (playerHand:restOfDecks) t) = do
  let newTree = insertBoard k v t
  let newDeck = (useHand (v!!0) playerHand) : restOfDecks
  (State newDeck newTree)


-- fixdel removes deleted elements from string
-- from A3 solutions
fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r
