module Play where

import Game
import BSTree
import System.Random
import System.IO
import Data.Typeable
import Control.Exception

numPlayers = 2

-- start the game
go :: IO Integer
go = do main

main :: IO Integer
main =
  do
    putStrLn "\n\nPyramid Stacking Game\n\n1 - Play\n2 - How To Play\nq - Quit game\n"
    option <- getLine
    if (fixdel option) == "q"
    then do
      return 0
    else do
      v <- start (fixdel option)
      main

start :: [Char] -> IO Integer
start "1" =
  do
    handSize <- getMaxHand
    let hands = generateAllHands numPlayers (fromIntegral handSize)
    let board = initTree (fromIntegral handSize) numPlayers
    res <- play (ContinueGame (State hands board))
    return 0

start "2" =
  do
    putStrLn "\nStarting the game: Select the deck size. The larger the deck will generate a larger board.\nTo insert a value, type the coordinates and the character you wish to add, if it's in your hand."
    return 0

start _ =
  do
    putStrLn "\nInvalid option\n"
    return 0


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
          -- shouldn't get here... go to the error handler
          return val



play :: Result -> IO Integer
play (ContinueGame (State decks tree)) =
  do
    putStrLn ("\n")
    printTree tree
    putStrLn ("\nYour hand: " ++ decks!!0)
    line <- getLine
    let cmd = words (fixdel line)
    if ((length cmd) == 2) && (keyExistsInTree (cmd!!0) tree)
      then do
        if not (keyExistsInTree (cmd!!0) tree) then do
          putStrLn((cmd!!0) ++ " is not a valid key on the board. Try a different value.\n")
          play (ContinueGame (State decks tree))
        else if not (canPlayHand ((cmd!!1)!!0) (decks!!0)) then do
          putStrLn((cmd!!1) ++ " cannot be played from your current hand. Try a different value.\n")
          play (ContinueGame (State decks tree))
        else if not (canPlaceOnTree (cmd!!0) (cmd!!1) tree) then do
          putStrLn((cmd!!1) ++ " at position " ++ (cmd!!0) ++ " cannot be placed on the board. Try a different value.\n")
          play (ContinueGame (State decks tree))
        else do
          -- play game
          play (game (cmd!!0) (cmd!!1) (State decks tree))
      else if (length cmd == 1) && (cmd!!0 == "q") then do
        -- end of game
        play (EndOfGame (State decks tree) False)
      else do
        putStrLn("Invalid input. Please enter in the format of BOARD_LOCATION CHARACTER_TO_ADD\n")
        play (ContinueGame (State decks tree))

    return 0

play (EndOfGame (State decks tree) outcome)
  | outcome = do
      putStrLn("YOU ARE A WINNER!\n")
      return 0
  | otherwise = do
      putStrLn("GAME OVER\n")
      return 0
