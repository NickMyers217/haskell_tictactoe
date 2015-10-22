module Tictactoe where

import Data.Char (digitToInt)
import Data.List (transpose)

-- A player can either be player One or player Two
data Player = One | Two deriving Show
-- A cell can either be an X, O, or Empty
data Cell   = X | O deriving (Eq, Show)
-- A Pos is an (x,y) pair
type Pos    = (Int, Int)
-- A board is a grid of Cells
type Board  = [[Maybe Cell]]


{-
 Pos Functions
-}
-- Parse a pos entered by a player as a string of two digits
parsePos :: String -> Pos
parsePos (x:xs) = (digitToInt x, digitToInt . head $ xs)
parsePos   _    = error "Invalid position entered."


-- Checks to see if a pos is valid
validPos :: Board -> Pos -> Bool
validPos b (x,y)
  | x < 0 || x > 2 || y < 0 || y > 2 = False
  | b !! y !! x == Nothing           = True
  | otherwise                        = False


{-
 Player Functions
-}
-- Converts a player to a String
showPlayer :: Player -> String
showPlayer p = case p of
  One -> "Player one"
  Two -> "Player two"


-- Gets the next Player
nextPlayer :: Player -> Player
nextPlayer p = case p of
  One -> Two
  Two -> One


-- Gets a players cell letter
getPlayerCell :: Player -> Cell
getPlayerCell p = case p of
  One -> X
  Two -> O


{-
 Cell Functions
-}
-- Converts a cell to a String
showCell :: Maybe Cell -> String
showCell c = case c of
  Just X  -> "X"
  Just O  -> "O"
  Nothing -> "#"


{-
 Board Functions
-}
-- A new empty 3x3 board
newBoard :: Board
newBoard = [ [ Nothing | x <- [0..2] ] | y <- [0..2] ]


-- Converts a board to a String
showBoard :: Board -> String
showBoard = unlines . map (concatMap showCell)


-- Replace an element of a list at an index
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (i,v) = take i xs ++ [v] ++ drop (i+1) xs


-- Modify a board
modifyBoard :: Board -> Pos -> Player -> Board
modifyBoard b (x,y) p = b !!= (y, b !! y !!= (x, Just . getPlayerCell $ p))


-- Gets all the threes on the board
boardThrees :: Board -> Board
boardThrees b = b ++ transpose b ++ map (map (\(x,y) -> b !! y !! x)) diags
  where diags = [[(0,0), (1,1), (2,2)]
                ,[(0,2), (1,1), (2,0)]]


-- Checks to see if a board is won
boardWon :: Board -> Bool
boardWon b = or . map matches $ boardThrees b
  where matches xs = xs == (replicate 3 (Just X)) || xs == (replicate 3 (Just O))


{-
 Impure Functions
-}
-- Gets a players move
getMove :: Board -> Player -> IO String
getMove b p = do
  putStr $ (showPlayer p) ++ "'s turn: "
  pos <- getLine
  if not . (validPos b) . parsePos $ pos
    then getMove b p
    else return pos


-- The main loop
gameLoop :: Board -> Player -> IO ()
gameLoop b p = do
  putStr "\n"
  putStrLn . showBoard $ b
  if boardWon b
     then putStrLn $ (showPlayer . nextPlayer $ p) ++ " wins!"
     else do
       pos <- getMove b p
       gameLoop (modifyBoard b (parsePos pos) p) (nextPlayer p)


-- Starts the game
main :: IO ()
main = putStrLn "Welcome to Tic Tac Toe!\n" >> gameLoop newBoard One
