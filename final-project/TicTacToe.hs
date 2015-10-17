import System.IO (stdout, hFlush)

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
        1   -> undefined
        _   -> return ()


type Grid = [Bool]
type Move = Int
type Player = Grid -> Grid -> IO Move

humanPlayer :: String -> Player
humanPlayer name fGrid sGrid = do
    undefined

showGrids :: Grid -> Grid -> String
showGrids = zipWith (\a b -> if a then 'X' else if b then 'O' else '_')

playGame :: Player -> Player -> Grid -> Grid -> IO ()
playGame first second fGrid sGrid = do
    firstMove <- first fGrid sGrid
    secondMove <- second sGrid fGrid

    undefined

main :: IO ()
main = showIntro >> gameMenu

getListInput :: [String] -> IO Int
getListInput options = do
    putStrLn "Choose one of the following: (enter the number)"
    let beginnings = map ((++ ".) ") . show) ([1..] :: [Int])
        numbered = zipWith (++) beginnings options
    mapM_ putStrLn numbered

    repeatUntil (\x -> (x > 0) && (x <= length options)) getIntInput

repeatUntil :: (a -> Bool) -> IO a -> IO a
repeatUntil condition f = do
    a <- f
    if condition a then return a
    else repeatUntil condition f

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
