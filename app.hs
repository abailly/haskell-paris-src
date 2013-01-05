{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Config

import Database.MongoDB as M hiding (lookup)
import Web.Scotty as W
import Web.Scotty.Auth
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.BDoc
import Data.Bson hiding (lookup)
import Data.Maybe (catMaybes)

import Models.Meetup
import Models.Talk
import Views.Index
import Views.Meetup
import Views.Talk

main :: IO ()
main = do
    config <- readConfig

    pipe <- runIOE $ connect (host $ dbServer config)
    let db        = access pipe master $ dbName config
        isAdmin   = isAuthentified $ authTest config
        adminOnly = basicAuth $ authTest config 

    scotty 3000 $ do
        middleware logStdoutDev
        middleware (staticPolicy $ addBase "static/")

        W.get "/" $ do
            results <- liftIO $ db $ find' (select [] "meetup")
            let page = either (\_ -> degradedHomepage []) (\xs -> homepage $ map fst xs) results
            html $ TL.pack $ page

        W.post "/talk" $ do
            (who   :: String) <- param "talk[speaker]"
            (title :: String) <- param "talk[title]"
            (dur   :: Int)    <- param "talk[duration]" 
            (lvl   :: String) <- param "talk[difficulty]" 
            let talk = submittedTalk who title dur lvl
            insertAndRedirect db "/talk" talk

        W.get "/talk" $ do
            adminOnly $ do
                let query = (select [] "talk")
                findAndRender db query listTalkPage

        W.get "/talk/:id" $ do
            auth <- isAdmin
            (oId :: ObjectId) <- param "id" >>= return . read
            let query = (select ["_id" := ObjId oId] "talk") {limit = 1}
            findAndRender db query (if auth; then editTalkPage; else displayTalkPage)

        W.post "/talk/:id" $ do
            adminOnly $ do
                (oId :: ObjectId) <- param "id" >>= return . read
                (who   :: String) <- param "talk[speaker]"
                (title :: String) <- param "talk[title]"
                (dur   :: Int)    <- param "talk[duration]" 
                (lvl   :: String) <- param "talk[difficulty]" 
                (st    :: String) <- param "talk[status]" 
                let talk = Talk who title dur lvl st
                replaceAndRedirect db "/talk" talk oId

        W.get "/meetup" $ do
            adminOnly $ do
                let query = (select [] "meetup")
                findAndRender db query listMeetupPage

        W.get "/meetup/:id" $ do
            adminOnly $ do
                (oId :: ObjectId) <- param "id" >>= return . read
                let query = (select ["_id" := ObjId oId] "meetup") {limit = 1}
                findAndRender db query editMeetupPage

        W.post "/meetup" $ do
            adminOnly $ do
                (year       :: Int)     <- param "meetup[year]"
                (month      :: String)  <- param "meetup[month]"
                (place      :: String)  <- param "meetup[place]" 
                (summary    :: String)  <- param "meetup[summary]" 
                (sponsors   :: [String])<- param "meetup[sponsors]" >>= return . lines
                (links      :: [String])<- param "meetup[links]"    >>= return . lines
                (slides     :: [String])<- param "meetup[slides]"   >>= return . lines
                let m = newMeetup year month place sponsors summary links slides
                insertAndRedirect db "/meetup" m

        W.post "/meetup/:id" $ do
            adminOnly $ do
                (year       :: Int)     <- param "meetup[year]"
                (month      :: String)  <- param "meetup[month]"
                (place      :: String)  <- param "meetup[place]" 
                (summary    :: String)  <- param "meetup[summary]" 
                (sponsors   :: [String])<- param "meetup[sponsors]" >>= return . lines
                (links      :: [String])<- param "meetup[links]"    >>= return . lines
                (slides     :: [String])<- param "meetup[slides]"   >>= return . lines
                (oId        :: ObjectId)<- param "id" >>= return . read
                let m = newMeetup year month place sponsors summary links slides
                replaceAndRedirect db "/meetup" m oId

    close pipe


-- helpers --

-- Like MongoDB's find function except that it turns BDoc instances to their corresponding type.
-- return values are pairs of (instance/ObjectId) because BDoc instances should not worry about 
-- databases IDs.
-- This function omit all found Document which failed to be turned into a BDoc instance.
find' :: (Functor m,Monad m,MonadBaseControl IO m, MonadIO m,BDoc a) => Query -> M.Action m [(a,ObjectId)]
find' query = M.find query >>= rest >>= return . catMaybes . (map f)
        where f x = m (fromDocument x, x !? "_id")
              m (Just a, Just b) = Just (a,b)
              m _                = Nothing

-- helper to render a given page with a collection of BDoc instances
-- database errors are ignored and appear as there was no match for the query
findAndRender db query page = do
    queryResult <- liftIO $ db $ find' query
    let pairs = either (\_ -> []) id queryResult
    html $ TL.pack $ page pairs

-- Inserts a  BDoc instance into the DB and returns an HTTP redirect to the corresponding HTTP path.
-- Displays a simple error text in case of error.
insertAndRedirect db path obj = do
            act <- liftIO $ db $ insert' obj 
            either (\_ -> raise "could not save to DB") (redirect' path) act

-- Updates a BDoc instance into the DB and returns an HTTP redirect to the corresponding HTTP path.
-- Displays a simple error text in case of error.
replaceAndRedirect db path obj oId = do
            act <- liftIO $ db $ replace' obj  oId
            liftIO $ print act
            either (\_ -> raise "could not replace in DB") (\_ -> redirect' path (ObjId oId)) act

-- Like MongoDB's insert but on a BDoc instance
insert' doc         = M.insert (collection doc) (toDocument doc)

-- Like MongoDB's replace (i.e., item update) but on a BDoc instance.
-- Use the ObjectId field as a replacement key.
replace' doc oId    = M.replace (select ["_id" := ObjId oId] $ collection doc) (toDocument doc)

-- Like Scotty's redirect but redirects to a BDoc instance's corresponding HTTP path
redirect' x y       = redirect $ objPath x y
    where objPath path (ObjId objId) = TL.fromStrict $ T.append path (T.pack $ ('/':show objId))

-- Test function to authenticate admins against the config info
authTest :: AppConfig -> B.ByteString -> B.ByteString -> AuthResult
authTest cfg x y = if (x,y) == (adminLogin cfg, adminPass cfg)
               then Authorized
               else Unauthorized
