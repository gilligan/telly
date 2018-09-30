{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telly where

import Data.Aeson
import GHC.Generics
import Data.Text as T

days :: [(Text, Weekday)]
days = [ ("Monday", Monday)
       , ("Tuesday", Tuesday)
       , ("Wednesday", Wednesday)
       , ("Thursday", Thursday)
       , ("Friday", Friday)
       , ("Saturday", Saturday)
       , ("Sunday", Sunday) ]

data Weekday = Monday 
             | Tuesday 
             | Wednesday 
             | Thursday 
             | Friday 
             | Saturday 
             | Sunday
             deriving (Show, Eq, Generic)

instance ToJSON Weekday where
    toJSON x = String $ T.pack . show $ x


instance FromJSON Weekday where
    parseJSON (String s) = case (lookup s days) of
        (Just d) -> return d
        Nothing  -> fail "Could not parse"
    parseJSON _ = fail "Could not parse"

data TvSlot = TVSlot { name :: T.Text
                     , timeslot :: T.Text
                     } deriving (Show, Eq, Generic)

instance ToJSON TvSlot
instance FromJSON TvSlot

data ChannelProg = ChannelProg { channel :: T.Text
                               , programming :: [TvSlot]
                               } deriving (Show, Eq, Generic)

instance ToJSON ChannelProg
instance FromJSON ChannelProg

data DayProg = DayProg { channels :: [ChannelProg]
                       , day :: Weekday
                       } deriving (Show, Eq, Generic)

instance ToJSON DayProg
instance FromJSON DayProg
