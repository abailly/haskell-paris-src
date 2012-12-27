{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Control.Monad (return)

data AppConfig = AppConfig { 
    adminLogin  :: ByteString,
    adminPass   :: ByteString,
    dbName      :: Text,
    dbServer    :: String
} deriving (Show,Eq)

hardcodedConfig :: AppConfig
hardcodedConfig = AppConfig "admin" "secret" "haskell-paris" "127.0.0.1"

readConfig :: IO (AppConfig)
readConfig = return hardcodedConfig
