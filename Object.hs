{-# LANGUAGE TemplateHaskell #-}

module Object (
    Object (..),
    name,
    location,
    adj,
    contents,
    room
) where

import qualified Data.Map as Map
import Control.Lens

data Object = Object {
    _name::String,
    _location::Maybe String,
    _adj::Maybe (Map.Map String String),
    _contents::Maybe [String]  
} deriving (Show)

$(makeLenses ''Object)

room n l a c = 
    Object n l (Just $ Map.fromList a) (Just c)