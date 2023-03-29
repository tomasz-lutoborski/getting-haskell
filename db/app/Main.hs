{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesBorrowed :: Int
  }

data User = User
  { userId :: Int,
    userName :: String
  }

instance Show User where
  show user =
    mconcat
      [ show $ userId user,
        ".) ",
        userName user
      ]

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool,
        ".) ",
        name tool,
        "\n description: ",
        description tool,
        "\n last returned: ",
        show $ lastReturned tool,
        "\n times borrowed: ",
        show $ timesBorrowed tool,
        "\n"
      ]

instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field

instance FromRow Tool where
  fromRow =
    Tool
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn sql f = do
  conn <- open sql
  f conn
  close conn

addUser :: String -> IO ()
addUser userName = do
  withConn "tools.db" $ \conn -> do
    execute conn "INSERT INTO users (username) VALUES (?)" (Only name)
    print "User added"

checkout :: Int -> Int -> IO ()
checkout userId toolId = do
  withConn "tools.db" $ \conn -> do
    execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)" (userId, toolId)
    print "Tool checked out"

printUsers :: IO ()
printUsers = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

main :: IO ()
main = print "db-lesson"
