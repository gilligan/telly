{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telly.Data where

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

import Telly.Weekday

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


data DayProg = DayProg { day :: Weekday
                       , channels :: [ChannelProg]
                       } deriving (Show, Eq, Generic)

instance ToJSON DayProg
instance FromJSON DayProg

tvChannels :: [(T.Text, T.Text)]
tvChannels = [ ("yle1", "Yle 1")
             , ("yle2", "Yle 2")
             , ("mtv3", "MTV 3")
             ]
