module Tictactoe where

import Data.Char (digitToInt)
import Data.List (transpose)

-- A player can either be player One or player Two
data Player = One | Two deriving Show
-- A cell can either be an X, O, or Empty
data Cell   = X | O | Empty deriving (Eq, Show)
-- A Pos is an (x,y) pair
type Pos    = (Int, Int)
-- A board is a grid of Cells
type Board  = [[Cell]]


-- A new empty 3x3 board
newBoard :: Board
newBoard = [ [ Empty | x <- [0..2] ]
                     | y <- [0..2] ]

-- Parse a pos entered by a player as a string of two digits
parsePos :: String -> Pos
parsePos (x:xs) = (digitToInt x, digitToInt . head $ xs)
parsePos   _    = error "Invalid position entered."


-- Checks to see if a pos is valid
validPos :: Board -> Pos -> Bool
validPos b (x,y)
  | x < 0 || x > 2 || y < 0 || y > 2 = False
  | b !! y !! x == Empty             = True
  | otherwise                        = False


-- Converts a player to a String
showPlayer :: Player -> String
showPlayer One = "Player one"
showPlayer Two = "Player two"


-- Gets the next Player
nextPlayer :: Player -> Player
nextPlayer p = case p of
  One -> Two
  Two -> One


-- Converts a cell to a String
showCell :: Cell -> String
showCell c = case c of
  X     -> "X"
  O     -> "O"
  Empty -> "#"


-- Converts a board to a String
showBoard :: Board -> String
showBoard = unlines . map (concatMap showCell)


-- Replace an element of a list at an index
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) xs (i,v) = take i xs ++ [v] ++ drop (i+1) xs


-- Modify a board
modifyBoard :: Board -> Pos -> Player -> Board
modifyBoard b (x,y) One = b !!= (y, b !! y !!= (x, X))
modifyBoard b (x,y) Two = b !!= (y, b !! y !!= (x, O))


-- Gets all the threes on the board
boardThrees :: Board -> Board
boardThrees b = b ++ transpose b ++ map (map (\(x,y) -> b !! y !! x)) diags
  where diags = [[(0,0), (1,1), (2,2)]
                ,[(0,2), (1,1), (2,0)]]


-- Checks to see if a board is won
boardWon :: Board -> Bool
boardWon b = or . map (\xs -> (xs == [X,X,X]) || (xs == [O,O,O])) $ boardThrees b


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
