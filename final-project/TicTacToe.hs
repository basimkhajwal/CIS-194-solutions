import System.IO (stdout, hFlush)

import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)

showIntro :: IO ()
showIntro = do
    putStrLn "---------------------------------"
    putStrLn "Welcome to Tic Tac Toe Haskell!!"
    putStrLn "---------------------------------\n\n"

gameMenu :: IO ()
gameMenu = do
    putStrLn "--------- Game Menu ------------"
    choice <- getListInput ["One player", "Two player", "Leave Game"]

    case choice of
        1   -> undefined
        2   -> do
            putStrLn "\n-------------- Two Player Game ---------------"
            putStrLn "\nEnter player one's name:"
            firstName <- getStringInput

            putStrLn "\nEnter player two's name:"
            secondName <- getStringInput

            let player1 = Player firstName (humanPlayer firstName)
                player2 = Player secondName (humanPlayer secondName)

            playGame player1 player2
            gameMenu

        _   -> putStrLn "Thanks for playing!!"


type Grid = [Bool]
type Move = Int

type MoveCalculation = Grid -> Grid -> IO Move

data Player = Player {
        getName :: String,
        getMove :: MoveCalculation
    }


emptyGrid :: Grid
emptyGrid = replicate 9 False

countGrid :: Grid -> Int
countGrid = sum . map (\x -> if x then 1 else 0)

humanPlayer :: String -> MoveCalculation
humanPlayer name fGrid sGrid = do
    putStrLn $ "\n" ++ name ++ "'s turn:"
    putStrLn "Choose position (1-9):"

    let validNum n = all ($ (n - 1)) [ (>= 0), (< 9) , not . (fGrid !!), not . (sGrid !!)]

    repeatUntil validNum
                (putStrLn "Try again - invalid number")
                getIntInput

showGrids :: Grid -> Grid -> String
showGrids = intercalate "------\n" .
            map ((++ "\n") . intersperse '|') .
            chunksOf 3 `applyTwice`
            zipWith (\a b -> if a then 'X' else if b then 'O' else '_')

playGame :: Player -> Player -> IO ()
playGame first second = iterateGame first second emptyGrid emptyGrid

iterateGame :: Player -> Player -> Grid -> Grid -> IO ()
iterateGame first second fGrid sGrid = do
    putStrLn "\nCurrent Grid:"
    putStrLn $ showGrids fGrid sGrid

    firstMove <- getMove first fGrid sGrid
    let fGrid' = applyMove (firstMove - 1) fGrid

    if checkWin fGrid' then
        putStrLn $ "\n" ++ getName first ++ " wins!!"
    else
        if countGrid fGrid' + countGrid sGrid == 9 then
            putStrLn $ "\nGame Over - draw"
        else
            iterateGame second first sGrid fGrid'

applyMove :: Move -> Grid -> Grid
applyMove 0 (_:xs) = True:xs
applyMove n (x:xs) = x: applyMove (n-1) xs
applyMove _ []     = error "Error: grid index out of bounds"

winningCombinations :: [Grid]
winningCombinations =
    [
        [
            True,   True,   True,
            False,  False,  False,
            False,  False,  False
        ],

        [
            False,  False,  False,
            True,   True,   True,
            False,  False,  False
        ],

        [
            False,  False,  False,
            False,  False,  False,
            True,   True,   True
        ],

        [
            True,   False,  False,
            True,   False,  False,
            True,   False,  False
        ],

        [
            False,  True,   False,
            False,  True,   False,
            False,  True,   False
        ],

        [
            False,  False,  True,
            False,  False,  True,
            False,  False,  True
        ],

        [
            True,   False,  False,
            False,  True,   False,
            False,  False,  True
        ],

        [
            False,  False,  True,
            False,  True,   False,
            True,   False,  False
        ]
    ]

checkWin :: Grid -> Bool
checkWin grid = any (\win -> win == zipWith (&&) grid win) winningCombinations

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
