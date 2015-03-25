module NimGame (play) where
-- Contains all IO, logic is imported from NimGame.Internal module.

import NimGame.Internal
import Control.Applicative
import qualified Text.Read as R (readMaybe)

-- Ask player names.
askPlayers :: IO Players
askPlayers = do
  putStrLn "Player 1: "
  p1 <- getLine
  putStrLn "Player 2: "
  p2 <- getLine
  return (p1,p2)

-- Ask integer value. Repeat if invalid value.
askInt :: String -> IO Int
askInt msg = do
  putStrLn msg
  line <- getLine
  case R.readMaybe line of Just n  -> return n
                           Nothing -> askInt msg

-- Ask next move from player.
askMove :: Game -> IO Move
askMove g = do
  putStrLn $ "Next move is for: " ++ inTurn g
  row   <- askInt "Row number: "
  count <- askInt "Count of stars to remove: "
  return (row,count)

printTable :: Game -> IO ()
printTable g = putStrLn "" >> putStrLn (unlines $ printableTable g)

-- Handle a bad move
badMove :: Game -> String -> IO Game
badMove game msg = putStrLn msg >> playGame game

-- Handle a good move
goodMove :: Game -> IO Game
goodMove game | gameOver game = return game
              | otherwise     = playGame $ changeTurn game

-- Main game loop. Asks and applies moves. Return when game over.
playGame :: Game -> IO Game
playGame game = do
  printTable game
  newGame <- applyMove game <$> askMove game
  either (badMove game) goodMove newGame

-- Run the game and print winner.
play :: Table -> IO String
play t = do
  players <- askPlayers
  let game = Game (checkTable t) players
  result <- playGame game
  return $ inTurn result

