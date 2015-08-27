{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: (Eq a) => [a] -> [a] -> Int
exactMatches = (sum .) . zipWith (\a b -> if a == b then 1 else 0)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map count colors
    where count col = length . filter (==col) $ xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ zipWith min (countColors a) (countColors b)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = let exact = exactMatches secret guess
                           nonexact = matches secret guess - exact
                    in  Move guess exact nonexact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move m _ _) code = getMove code m == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
    | n == 0    = []
    | n == 1    = map (: []) colors
    | otherwise = concatMap (\code -> map (:code) colors) (allCodes (n - 1))

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = solver (allCodes 4) [Red, Red, Red, Red]
    where solver :: [Code] -> Code -> [Move]
          solver currentMoves nextMove
            | nextMove == secret  = []
            | otherwise           = let afterMove = getMove secret nextMove
                                        newMoves = filterCodes afterMove currentMoves
                                    in afterMove : solver (tail newMoves) (head newMoves)
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
