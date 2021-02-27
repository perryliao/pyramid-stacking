module Play where

import Game
import Tree
import Deck
import BSTree
import System.Random
import System.IO
import Data.Typeable
import Control.Exception
import Data.Char

numPlayers = 2

-- start the game
go :: IO ()
go = do main

main :: IO ()
main =
  do
    putStrLn "\nPyramid Stacking Game\n\n1 - Play\n2 - How To Play\nq - Quit game\n"
    option <- getLine
    if (fixdel option) == "q"
    then do
      putStrLn "\nSee you next time!\n"
    else do
      v <- start (fixdel option)
      main

start :: [Char] -> IO ()
start "1" =
  do
    handSize <- getMaxHand
    let hands = generateAllHands numPlayers (fromIntegral handSize)
    let board = initTree (fromIntegral handSize) numPlayers
    play (ContinueGame (State hands board))

start "2" =
  do
    putStrLn "\nStarting the game: Select the deck size. The larger the deck will generate a larger board.\nTo insert a value, type the coordinates and the character you wish to add, if it's in your hand.\nThe first player to exhaust their entire hand, or the player with the smallest hand at the end of the game wins!\n"

start _ =
  do
    putStrLn "\nInvalid option\n"


getMaxHand :: IO Integer
getMaxHand =
  do
    putStrLn "How many tokens should each player have?\n"
    res <- getLine
    n <- catch (convertInput res) $ \e -> errorHandler e
    return n
    where
      errorHandler :: SomeException -> IO Integer
      errorHandler _ = do
        putStrLn "Invalid response. Please enter a number.\n"
        getMaxHand
      convertInput :: [Char] -> IO Integer
      convertInput i = do
        let val = (read (fixdel i) :: Integer)
        if val `elem` [1..]
        then
          return val
        else
          -- numbers <1 go here
          return 1



play :: Result -> IO ()
play (ContinueGame (State decks@(playerDeck:opponentDecks) tree)) =
  do
    putStrLn ("\n")
    printTree tree
    putStrLn ("\nYour hand: " ++ playerDeck ++ "\nOpponent cards left: " ++ (foldr (\a b -> a ++ ", " ++ b) [] (map show (map length opponentDecks))))
    line <- getLine
    let cmd = words (fixdel line)
    let key = map toUpper (cmd!!0)
    if ((length cmd) == 2) && (keyExistsInTree key tree)
      then do
        if not (keyExistsInTree key tree) then do
          putStrLn(key ++ " is not a valid key on the board. Try a different value.\n")
          play (ContinueGame (State decks tree))
        else if not (canPlayHand ((cmd!!1)!!0) (decks!!0)) then do
          putStrLn((cmd!!1) ++ " cannot be played from your current hand. Try a different value.\n")
          play (ContinueGame (State decks tree))
        else if not (canPlaceOnTree key (cmd!!1) tree) then do
          putStrLn((cmd!!1) ++ " at position " ++ key ++ " cannot be placed on the board. Try a different value.\n")
          play (ContinueGame (State decks tree))
        else do
          -- play game
          play (game key (cmd!!1) (State decks tree))
      else if (length cmd == 1) && (cmd!!0 == "q") then do
        -- end of game
        play (EndOfGame (State decks tree) False)
      else do
        putStrLn("Invalid input. Please enter in the format of BOARD_LOCATION CHARACTER_TO_ADD\n")
        play (ContinueGame (State decks tree))

play (EndOfGame (State decks tree) outcome)
  | outcome = do
      putStrLn("YOU ARE A WINNER!\n")
  | otherwise = do
      putStrLn("GAME OVER\n")
