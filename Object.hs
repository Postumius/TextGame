module Object (
    Object (..)
) where

data Object = Object {
    name::String,
    location::String,
    contents::Maybe [String]
} deriving (Show)

screwdriver = Object "screwdriver" "workshop" Nothing