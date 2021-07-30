{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Main where

import Control.Monad (when, void)
import Data.Text (isPrefixOf, toLower, Text, pack, unpack, strip)
import qualified Data.Text.IO as TIO

import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Control.Monad.IO.Class

import Data.Text.Manipulate

import System.IO
import Control.Exception

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
newtype Key = Key { getKey :: String } deriving (FromField, ToField, Show)
newtype Url = Url { getUrl :: String } deriving (FromField, ToField, Show)

data DBRow = DBRow { key :: Key
                   , url :: Url } deriving (Show)

instance FromRow DBRow where
  fromRow = DBRow <$> field <*> field

instance ToRow DBRow where
  toRow (DBRow key url) = toRow (key, url)


urlPath = "UrlFile.txt"
databasePath = "resources/storage.db"
tokenPath = "token"

main :: IO ()
main = pingPongExample

testDB :: IO ()
testDB = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 4" :: String))
  r <- query_ conn "SELECT * FROM test" :: IO [DBRow]
  mapM_ print r
  close conn

placeUrl :: Key -> Url -> IO ()
placeUrl key url = do
  conn <- open databasePath
  putStrLn "????1"
  result <- try $ execute conn "INSERT INTO data (key, url) VALUES(?, ?) ON CONFLICT(key) DO UPDATE SET url=excluded.url;" (DBRow key url) :: IO (Either SQLError ())
  putStrLn "????2"
  close conn
  case result of
    Left error -> putStrLn $ "Couldn't insert key! ERROR: " ++ unpack (sqlErrorDetails error)
    _ -> return ()

queryUrl :: Key -> IO (Maybe Url)
queryUrl key = do
  conn <- open databasePath
  result <- query_ conn (Query $ pack $ "SELECT key, url FROM data WHERE key = " ++ "\"" ++ getKey key ++ "\"") :: IO [DBRow]
  close conn
  if null result
    then return Nothing
    else return . Just . url . head $ result

queryKeys :: IO [Key]
queryKeys = do
  conn <- open databasePath
  result <- query_ conn (Query $ pack $ "SELECT key, url FROM data") :: IO [DBRow]
  return $ map key result

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageText

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> handleMessage m
  _ -> return ()

eventHandler2 :: Event -> DiscordHandler ()
eventHandler2 event = case event of
  MessageCreate m -> handleMessage m
  _ -> return ()

handleMessage :: Message -> DiscordHandler ()
handleMessage m
  | not (fromBot m) && isPing m =
    do
      void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
      threadDelay (2 * 10^6)
      void $ restCall (R.CreateMessage (messageChannel m) "Pong!")
  | hasAttachment m && isSave m =
    do
      liftIO $ putStrLn $ "SAVE dispatched with key " ++ getKey (getAttachmentKey m)
      liftIO $ placeUrl (getAttachmentKey m) (getAttachmentUrl m)
      void $ restCall (R.CreateMessage (messageChannel m) "Dodano wpis mordo")
  | isLoad m =
    do
      url <- liftIO $ queryUrl (Key . unpack . strip . pack . drop 5 . unpack $ messageText m)
      case url of
        Nothing -> void $ restCall (R.CreateMessage (messageChannel m) "Nie ma takiego wpisu kolego")
        Just value -> do
          void $ restCall (R.CreateMessage (messageChannel m) (pack . getUrl $ value))
  | isList m =
    do
      keys <- liftIO queryKeys
      void $ restCall (R.CreateMessage (messageChannel m) (pack (foldr ((\x y -> x ++ "\n" ++ y) . getKey) "" keys)))
  | otherwise = return ()

hasAttachment :: Message -> Bool
hasAttachment = not . null . messageAttachments

isSave :: Message -> Bool
isSave = ("!save" `isPrefixOf`) . toLower . messageText

isLoad :: Message -> Bool
isLoad = ("!load" `isPrefixOf`) . toLower . messageText

isList :: Message -> Bool
isList = ("!list" `isPrefixOf`) . toLower . messageText

getAttachmentUrl :: Message -> Url
getAttachmentUrl = Url . unpack . attachmentUrl . head . messageAttachments

getAttachmentKey :: Message -> Key
getAttachmentKey = Key . unpack . strip . pack . drop 5 . unpack . messageText

saveUrl :: Text -> IO ()
saveUrl url = withFile urlPath WriteMode (saveUrlToFile url)

saveUrlToFile :: Text -> Handle -> IO ()
saveUrlToFile url handle = hPutStr handle (unpack url)

-- getContent ::

pingPongExample :: IO ()
pingPongExample = do
  token <- loadToken
  userFacingError <- runDiscord $ def
                      { discordToken = pack token
                      , discordOnEvent = eventHandler }
  TIO.putStrLn userFacingError

loadToken :: IO String
loadToken = readFile tokenPath

test1 :: Int -> Int
test1 x
  | x == 5 = 5
  | otherwise = 6