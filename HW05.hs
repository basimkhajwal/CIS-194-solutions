{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret dogFile secretFile = do
    dog <- BS.readFile dogFile
    secret <- BS.readFile secretFile

    return $ BS.filter (/=0) $ BS.pack $ BS.zipWith xor dog secret

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key encryptedFile = do
    encryptedText <- BS.readFile (encryptedFile ++ ".enc")
    BS.writeFile encryptedFile $ BS.pack $ BS.zipWith xor (BS.cycle key) encryptedText

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = fmap decode . BS.readFile

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile = do
    victimData <- parseFile victimFile :: IO (Maybe [TId])
    transactionData <- parseFile transactionFile :: IO (Maybe [Transaction])

    return $ do
        a <- victimData
        b <- transactionData

        return $ filter ((`elem` a) . tid) b

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr addVictim Map.empty
    where addVictim (Transaction a b x _) = changePerson a (-x) . changePerson b x
          changePerson name money = Map.alter (checkPerson money) name
          checkPerson val (Just old) = Just (old + val)
          checkPerson val Nothing    = Just val

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldrWithKey (\k x curr -> if x > snd curr then (k, x) else curr) ("", 0)

-- Exercise 7 -----------------------------------------

splitTransactions :: Map String Integer -> ([(String, Integer)], [(String, Integer)])
splitTransactions = Map.foldrWithKey (\name money x@(more, less) -> if money == 0 then x else if money > 0 then ((name, money):more, less) else (more, (name,abs money):less) ) ([], [])

evenOutMoney :: [TId] -> ([(String, Integer)], [(String, Integer)]) -> [Transaction]
evenOutMoney [] _ = error "Not enough ID's supplied"
evenOutMoney (i:is) splitted = case splitted of
    (_, [])                   -> []
    ([], _)                   -> []
    ((a, j):as, (b, k):bs)    -> Transaction a b (min j k) i : evenOutMoney is (case compare j k of
        GT  -> ((a, j - k):as, bs)
        LT  -> (as, (b, k - j):bs)
        EQ  -> (as, bs) )

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs tranMap is = evenOutMoney is (splitTransactions tranMap)

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path value = BS.writeFile path (encode value)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
