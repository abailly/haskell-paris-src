{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Models.Meetup (newMeetup,year,month,place,sponsors,summary,slides,links) where

import Data.Bson
import Data.BDoc
import Data.Typeable (Typeable)
import Data.Maybe (isJust)

type Year       = Int
type Month      = String
type Sponsor    = String
type Link       = String
type Slide      = String

data Meetup = Meetup {
    year        :: Year,
    month       :: Month,
    place       :: String,
    sponsors    :: [Sponsor],
    summary     :: String,
    links       :: [Link],
    slides      :: [Slide]
    } deriving (Typeable, Show, Eq)

newMeetup = Meetup

instance Val Meetup where
    cast' (Doc d) = fromDocument d
    cast' _       = Nothing
    val m         = Doc $ toDocument m

instance BDoc Meetup where
    collection _ = "meetup"
    toDocument (Meetup y m p sp su l sl) = [
        "year"      := val y,
        "month"     := val m,
        "place"     := val p,
        "sponsors"  := val sp,
        "summary"   := val su,
        "links"     := val l,
        "slides"    := val sl
        ]

    fromDocument doc = do
        y   <- pick "year"
        m   <- pick "month"
        p   <- pick "place"
        sp  <- pick "sponsors"
        su  <- pick "summary"
        l   <- pick "links"
        sl  <- pick "slides"
        return $ Meetup y m p sp su l sl
            where pick f = look f doc >>= cast
