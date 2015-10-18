import System.IO (stdout, hFlush)

import Control.Applicative ((<$>))

import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)

showIntro :: IO ()
showIntro = do
    putStrLn "---------------------------------"
    putStrLn "Welcome to Tic Tac Toe Haskell!!"
    putStrLn "---------------------------------\n\n"

gameMenu :: IO ()
gameMenu = do
    choice <- getListInput ["One player", "Two player", "L"]

    case choice of
        0   -> undefined
        1   -> do
            playGame (humanPlayer "Player One") (humanPlayer "Player Two")
        _   -> return ()


type Grid = [Bool]
type Move = Int
type Player = Grid -> Grid -> IO Move

emptyGrid :: Grid
emptyGrid = replicate 9 False

humanPlayer :: String -> Player
humanPlayer name fGrid sGrid = do
    putStrLn $ name ++ "'s turn:"
    putStrLn "Current Grid:"
    putStrLn $ showGrids fGrid sGrid

    putStrLn "Choose position (1-9):"

    let validNum n = all ($ n) [ (> 0), (< 10) , not . (fGrid !!), not . (sGrid !!)]

    repeatUntil validNum
                (putStrLn "Try again - invalid number")
                getIntInput

showGrids :: Grid -> Grid -> String
showGrids = intercalate "---" .
            map ((++ "\n") . intersperse '|') .
            chunksOf 3 `applyTwice`
            zipWith (\a b -> if a then 'X' else if b then 'O' else '_')

playGame :: Player -> Player -> IO ()
playGame first second = iterateGame first second emptyGrid emptyGrid

iterateGame :: Player -> Player -> Grid -> Grid -> IO ()
iterateGame first second fGrid sGrid = do
    firstMove <- first fGrid sGrid
    secondMove <- second sGrid fGrid

    undefined

main :: IO ()
main = showIntro >> gameMenu

-- Utility functions

getListInput :: [String] -> IO Int
getListInput options = do
    putStrLn "Choose one of the following: (enter the number)"
    let beginnings = map ((++ ".) ") . show) ([1..] :: [Int])
        numbered = zipWith (++) beginnings options
    mapM_ putStrLn numbered

    repeatUntil (\x -> (x > 0) && (x <= length options))
                (putStrLn "Try again - invalid number")
                getIntInput

applyTwice :: (c -> d) -> (a -> b -> c) -> a -> b -> d
applyTwice f = (.) (fmap f)
infixl 8 `applyTwice`

repeatUntil :: (a -> Bool) -> IO () -> IO a -> IO a
repeatUntil condition err f = do
    a <- f
    if condition a then return a
    else err >> repeatUntil condition err f

getIntInput :: IO Int
getIntInput = do
    input <- fmap reads getStringInput :: IO [(Int, String)]

    if null input then do
        putStrLn "Try again - enter an integer"
        getIntInput
    else
        return $ (fst . head) input

getStringInput :: IO String
getStringInput = putStr ">>> " >> hFlush stdout >> getLine
