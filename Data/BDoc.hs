
module Data.BDoc where

import Data.Bson
import Data.Text(Text)

class BDoc a where
    model           :: a
    collection      :: a -> Text
    toDocument      :: a -> Document
    fromDocument    :: Document -> Maybe a
