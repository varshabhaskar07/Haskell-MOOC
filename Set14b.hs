module Set14b where

-- Importing necessary libraries and modules
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text.Encoding (encodeUtf8)
import Text.Read (readMaybe)

-- Importing HTTP server libraries
import Network.Wai (pathInfo, responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

-- Importing database libraries
import Database.SQLite.Simple (open, execute, execute_, query, query_, Connection, Query(..))

------------------------------------------------------------------------------
-- Exercise 1:

import qualified Data.Text as T
import Database.SQLite.Simple

-- Defining the SQL query to initialize the database schema
initQuery :: Query
initQuery = Query (T.pack "CREATE TABLE IF NOT EXISTS events (account TEXT NOT NULL, amount INTEGER NOT NULL);")

-- Defining the SQL query to insert a deposit record
depositQuery :: Query
depositQuery = Query (T.pack "INSERT INTO events (account, amount) VALUES (?, ?);")

-- Defining the SQL query to retrieve all deposit records
getAllQuery :: Query
getAllQuery = Query (T.pack "SELECT account, amount FROM events;")

-- Opening a database connection, initializing the schema, and returning the connection
openDatabase :: String -> IO Connection
openDatabase filename = do
  conn <- open filename
  execute_ conn initQuery
  return conn

-- Depositing an amount into an account
deposit :: Connection -> T.Text -> Int -> IO ()
deposit conn account amount = execute conn depositQuery (account, amount)

------------------------------------------------------------------------------
-- Exercise 2:

-- Defining the SQL query to retrieve the balance of an account
balanceQuery :: Query
balanceQuery = Query (T.pack "SELECT amount FROM events WHERE account = ?;")

-- Calculating the balance of an account
balance :: Connection -> T.Text -> IO Int
balance db account = do
  r <- query db balanceQuery [account] :: IO [[Int]]
  return $ sum $ map sum r

------------------------------------------------------------------------------
-- Exercise 3:

-- Defining the Command data type with possible actions
data Command = Deposit T.Text Int | Balance T.Text | Withdraw T.Text Int
  deriving (Show, Eq)

-- Parsing an Int from Text
parseInt :: T.Text -> Maybe Int
parseInt = readMaybe . T.unpack

-- Parsing a command from a list of Texts
parseCommand :: [T.Text] -> Maybe Command
parseCommand [cmd, acc, amt]
  | cmd == T.pack "deposit" = Deposit acc <$> parseInt amt
  | cmd == T.pack "withdraw" = Withdraw acc <$> parseInt amt
parseCommand [cmd, acc]
  | cmd == T.pack "balance" = Just $ Balance acc
parseCommand _ = Nothing

------------------------------------------------------------------------------
-- Exercise 4:

-- Performing an action based on the Command
perform :: Connection -> Maybe Command -> IO T.Text
perform _ Nothing = return (T.pack "ERROR")
perform conn (Just (Deposit acc amount)) = do
  deposit conn acc amount
  return (T.pack "OK")
perform conn (Just (Withdraw acc amount)) = do
  deposit conn acc (-amount)
  return (T.pack "OK")
perform conn (Just (Balance acc)) = do
  bal <- balance conn acc
  return (T.pack (show bal))

------------------------------------------------------------------------------
-- Exercise 5:

-- Encoding a Text response to ByteString
encodeResponse :: T.Text -> LB.ByteString
encodeResponse t = LB.fromStrict (encodeUtf8 t)

-- Defining a simple HTTP server that responds with "BANK"
simpleServer :: Application
simpleServer request respond =
  respond (responseLBS status200 [] (encodeResponse (T.pack "BANK")))

------------------------------------------------------------------------------
-- Exercise 6:

-- Defining the server application with database connection handling
server :: Connection -> Application
server db request respond = do
  let xs = pathInfo request
  r <- perform db (parseCommand xs)
  let resp = encodeResponse r
  respond (responseLBS status200 [] resp)

-- Setting the port for the server to run on
port :: Int
port = 3421

-- Running the server on the specified port
main :: IO ()
main = do
  db <- openDatabase "bank.db"
  putStr "Running on port: "
  print port
  run port (server db)
