{-# LANGUAGE OverloadedStrings #-}

module Site.Map where

import Data.Text (Text)

data Page = Index | Talk | Meetup

render :: Page -> [(Text, Text)] -> Text
render Index    _ = "/"
render Talk     _ = "/talk"
render Meetup   _ = "/meetup"
