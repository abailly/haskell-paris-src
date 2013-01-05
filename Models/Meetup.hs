{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Models.Meetup (newMeetup,year,month,month',place,sponsors,summary,slides,links) where

import Data.Bson
import Data.BDoc
import Data.Typeable (Typeable)
import Data.Maybe (isJust)
import qualified Data.Text as T

type Year       = Int
-- type Month      = String
type Sponsor    = String
type Link       = String
type Slide      = String

data Month = Jan | Feb | Mar | Apr 
    | May | Jun | Jul | Aug | Sep
    | Oct | Nov | Dec | NA
    deriving (Show, Typeable, Eq, Ord, Enum)

parseMonth :: T.Text -> Month
parseMonth "Janvier"    = Jan
parseMonth "Février"    = Feb
parseMonth "Mars"       = Mar
parseMonth "Avril"      = Apr
parseMonth "Mai"        = May
parseMonth "Juin"       = Jun
parseMonth "Juillet"    = Jul
parseMonth "Aout"       = Aug
parseMonth "Septembre"  = Sep
parseMonth "Octobre"    = Oct
parseMonth "Novembre"   = Nov
parseMonth "Décembre"   = Dec
parseMonth _            = NA

unparseMonth :: Month -> T.Text
unparseMonth Jan = "Janvier"
unparseMonth Feb = "Février"
unparseMonth Mar = "Mars"
unparseMonth Apr = "Avril"
unparseMonth May = "Mai"
unparseMonth Jun = "Juin"
unparseMonth Jul = "Juillet"
unparseMonth Aug = "Aout"
unparseMonth Sep = "Septembre"
unparseMonth Oct = "Octobre"
unparseMonth Nov = "Novembre"
unparseMonth Dec = "Décembre"
unparseMonth _   = "NA"

data Meetup = Meetup {
    year        :: Year,
    month       :: Month,
    place       :: String,
    sponsors    :: [Sponsor],
    summary     :: String,
    links       :: [Link],
    slides      :: [Slide]
    } deriving (Typeable, Show, Eq)

month' = unparseMonth . month

newMeetup y m p sp su l s = Meetup y m' p sp su l s
    where m' = parseMonth $ T.pack m

instance Val Month where
    cast' (String s)    = Just $ parseMonth s
    cast' _             = Nothing
    val m               = String $ unparseMonth m

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
