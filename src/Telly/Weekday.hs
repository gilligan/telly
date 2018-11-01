{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telly.Weekday ( Weekday(..), weekdays ) where

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

data Weekday = Monday 
             | Tuesday 
             | Wednesday 
             | Thursday 
             | Friday 
             | Saturday 
             | Sunday
             deriving (Show, Eq, Generic)

weekdays :: [Weekday]
weekdays = enumFromTo minBound maxBound

instance Bounded Weekday where
    minBound = Monday
    maxBound = Sunday

instance Enum Weekday where
    toEnum 0 = Monday
    toEnum 1 = Tuesday
    toEnum 2 = Wednesday
    toEnum 3 = Thursday
    toEnum 4 = Friday
    toEnum 5 = Saturday
    toEnum 6 = Sunday
    toEnum _ = error "range outside of 0-6"

    fromEnum Monday     = 0
    fromEnum Tuesday    = 1
    fromEnum Wednesday  = 2
    fromEnum Thursday   = 3
    fromEnum Friday     = 4
    fromEnum Saturday   = 5
    fromEnum Sunday     = 6

fromName :: T.Text -> Maybe Weekday
fromName "Monday"    = Just Monday
fromName "Tuesday"   = Just Tuesday
fromName "Wednesday" = Just Wednesday
fromName "Thursday"  = Just Thursday
fromName "Friday"    = Just Friday
fromName "Saturday"  = Just Saturday
fromName "Sunday"    = Just Sunday
fromName _           = Nothing

instance ToJSON Weekday where
    toJSON x = String $ T.pack . show $ x

instance FromJSON Weekday where
    parseJSON (String s) = case fromName s of
        (Just d) -> return d
        Nothing  -> fail "Could not parse"
    parseJSON _ = fail "Could not parse"
