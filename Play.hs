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
    putStrLn "\n\nPyramid Stacking Game\n\n1 - Play\n2 - How To Play\nq - quit game\n"
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
    let cmd = (fixdel line)
    return 0
