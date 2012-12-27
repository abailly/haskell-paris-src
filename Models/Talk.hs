{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Models.Talk (
    Talk(Talk),
    submittedTalk,speaker,title,duration,difficulty,status
    ) where

import Data.Bson
import Data.BDoc
import Data.Typeable (Typeable)
import Data.Maybe (isJust)

type Speaker    = String
type Status     = String
type Duration   = Int
type Difficulty = String

data Talk = Talk {
    speaker     :: Speaker,
    title       :: String,
    duration    :: Duration,
    difficulty  :: Difficulty,
    status      :: Status
    } deriving (Show, Eq, Typeable)

submittedTalk :: Speaker -> String -> Duration -> Difficulty -> Talk
submittedTalk sp t du di = Talk sp t du di "submitted"

instance Val Talk where
    cast' (Doc d) = fromDocument d
    cast' _       = Nothing
    val m         = Doc $ toDocument m

instance BDoc Talk where
    collection _ = "talk"
    toDocument (Talk sp t du di st) = [
                        "speaker-id"    =: sp, 
                        "title"         =: t, 
                        "duration"      =: du,
                        "difficulty"    =: di,
                        "status"        =: st
        ]

    fromDocument doc = do
        sp <- pick "speaker-id"
        t  <- pick "title"
        du <- pick "duration"
        di <- pick "difficulty"
        st <- pick "status"
        return $ Talk sp t du di st
            where pick f = look f doc >>= cast
