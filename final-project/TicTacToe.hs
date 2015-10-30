import System.IO (stdout, hFlush)

import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)

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

countHeuristic :: Grid -> Int
countHeuristic grid = 100 * threes + 10 * twos + ones
    where threes = length $ filter ((==3) . countGrid) $ map (zipWith (&&) grid) winningCombinations
          twos   = length $ filter ((==2) . countGrid) $ map (zipWith (&&) grid) winningCombinations
          ones   = countGrid grid - (3 * threes) - (2 * twos)

getHeuristicScore :: Grid -> Grid -> Int
getHeuristicScore fGrid sGrid = countHeuristic fGrid - countHeuristic sGrid

getMoves :: Grid -> Grid -> [Move]
getMoves fGrid sGrid = filter (not . (combined !!)) [0..8]
    where combined = zipWith (||) fGrid sGrid

minimax :: Grid -> Grid -> Int -> Move -> Int
minimax fGrid sGrid 0 move = getHeuristicScore (applyMove move fGrid) sGrid
minimax fGrid sGrid n move = gridScore - optimumNext
    where newGrid       = applyMove move fGrid
          gridScore     = getHeuristicScore newGrid sGrid
          nextMoves     = getMoves newGrid sGrid
          optimumNext   = if null nextMoves then 0 else minimum $ map (minimax sGrid newGrid (n - 1)) nextMoves

checkWin :: Grid -> Bool
checkWin grid = any (\win -> win == zipWith (&&) grid win) winningCombinations

computerPlayer :: MoveCalculation -> Player
computerPlayer calculation = Player "Computer" move
    where move fGrid sGrid = do
            putStrLn "\n------------------"
            putStrLn " Computer's Turn    "
            putStrLn "------------------"
            pos <- calculation fGrid sGrid
            putStrLn $ "Computer chose position " ++ show (pos + 1)
            return (pos + 1)

easyComputer :: MoveCalculation
easyComputer fGrid sGrid = return $ head $ dropWhile ((combined !!) . (pred)) moveOrders
                        where combined = zipWith (||) fGrid sGrid
                              moveOrders = [ 5, 1, 3, 7, 9, 2, 4, 6, 8] :: [Int]

hardComputer :: MoveCalculation
hardComputer fGrid sGrid = do
    let possibleMoves = getMoves fGrid sGrid
        moveValues = map (\m -> (m, minimax fGrid sGrid 5 m)) possibleMoves
        bestMove = fst $ foldr (\n@(_, new) o@(_, old) -> if new > old then n else o) (0, -1000) moveValues

    return bestMove

humanPlayer :: String -> MoveCalculation
humanPlayer name fGrid sGrid = do
    putStrLn "\n------------------------"
    putStrLn $ name ++ "'s turn:"
    putStrLn "-------------------------"
    putStrLn "\nCurrent Grid:"
    putStrLn $ showGrids fGrid sGrid
    putStrLn "Choose position (1-9):"

    let validNum n = all ($ (n - 1)) [ (>= 0), (< 9) , not . (fGrid !!), not . (sGrid !!)]

    pos <- repeatUntil validNum
                (putStrLn "Try again - invalid position")
                getIntInput

    putStrLn $ "You chose position " ++ show pos
    return pos

showGrids :: Grid -> Grid -> String
showGrids = intercalate "------\n" .
            map ((++ "\n") . intersperse '|') .
            chunksOf 3 `applyTwice`
            zipWith (\a b -> if a then 'X' else if b then 'O' else '_')

playGame :: Player -> Player -> IO ()
playGame first second = iterateGame first second emptyGrid emptyGrid

iterateGame :: Player -> Player -> Grid -> Grid -> IO ()
iterateGame first second fGrid sGrid = do
    firstMove <- getMove first fGrid sGrid
    let fGrid' = applyMove (firstMove - 1) fGrid

    if checkWin fGrid' then
        putStrLn $ "\n" ++ getName first ++ " wins!!"
    else
        if countGrid fGrid' + countGrid sGrid == 9 then
            putStrLn $ "\nGame Over - draw"
        else
            iterateGame second first sGrid fGrid'

showIntro :: IO ()
showIntro = do
    putStrLn "---------------------------------"
    putStrLn "Welcome to Tic Tac Toe Haskell!!"
    putStrLn "---------------------------------\n\n"

gameMenu :: IO ()
gameMenu = do
    putStrLn "--------- Game Menu ------------"
    choice <- getListInput ["One Player", "Two player", "Leave Game"]

    case choice of
        1   -> do
            putStrLn "\n-------------- One Player Game ---------------"
            putStrLn "Enter player name:"
            name <- getStringInput

            putStrLn "Choose difficulty:"
            difficulty <- getListInput ["Easy", "Hard"]

            let human = Player name (humanPlayer name)
            let computer = computerPlayer $
                            case difficulty of
                                1   -> easyComputer
                                2   -> hardComputer
                                _   -> undefined

            playGame human computer
            gameMenu

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
