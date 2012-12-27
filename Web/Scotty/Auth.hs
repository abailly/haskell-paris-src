{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Auth where

import Web.Scotty as W
import Network.HTTP.Types.Status
import Network.Wai
import Control.Applicative((<$>))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL

data AuthResult = Authorized | Unauthorized deriving (Show)

toBool :: AuthResult -> Bool
toBool Authorized   = True
toBool Unauthorized = False

askBasicAuthentication = do
    W.status status401
    header "WWW-Authenticate" "Basic realm=\"please-auth\""
    html $ "unauthorized" 

checkAuthentified test = do
    hs <- (requestHeaders <$> request) >>= (return . lookup "Authorization")
    return $ maybe Unauthorized (authResult test) hs

authResult test b64AuthVal = either (\_ -> Unauthorized) (\(l,p) -> test l (B.drop 1 p)) authVal
    where authVal = (B64.decode $ B.drop 6 b64AuthVal) >>= (return . B.break (== columnSep))
                    where columnSep = B.head ":"

isAuthentified test = (checkAuthentified test) >>= return . toBool

basicAuth test granted = do
    hs <- requestHeaders <$> request
    case lookup "Authorization" hs of
        Nothing     -> askBasicAuthentication
        (Just auth) -> case (authResult test auth) of
                        Authorized      -> granted
                        Unauthorized    -> askBasicAuthentication

