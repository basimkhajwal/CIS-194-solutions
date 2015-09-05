{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Control.Applicative ((<$>), (<*>))

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
parseFile filePath = do
    fileString <- BS.readFile filePath
    return $ decode fileString

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile = do
    victimData <- parseFile victimFile :: IO (Maybe [TId])
    transactionData <- parseFile transactionFile :: IO (Maybe [Transaction])
    return $ do
        a <- victimData
        b <- transactionData
        return $ filter (\transaction -> tid transaction `elem` a) b

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

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
