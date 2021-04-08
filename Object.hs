module Object (
    Object (..),
    room
) where

import qualified Data.Map as Map

data Object = Object {
    name::String,
    location::String,
    adj::Maybe (Map.Map String String),
    contents::Maybe [String]  
} deriving (Show)

room n l a c = 
    Object n l (Just $ Map.fromList a) (Just c)