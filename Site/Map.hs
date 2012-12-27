{-# LANGUAGE OverloadedStrings #-}

module Site.Map where

import Data.Text (Text)

data Page = Index | Talk

render :: Page -> [(Text, Text)] -> Text
render Index    _ = "/"
render Talk     _ = "/talk"
