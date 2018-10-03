{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telly where

import Control.Lens hiding (elements, children)
import Data.List.Split (chunksOf)

import Text.Taggy.Lens
import Data.Aeson
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO

(||>) :: a -> (a -> b) -> b
(||>) x f = f x
infixl 0 ||>

tvChannels :: [(T.Text, T.Text)]
tvChannels = [ ("yle1", "Yle 1")
             , ("yle2", "Yle 2")
             , ("mtv3", "MTV 3")
             ]

weekdays :: [Weekday]
weekdays = enumFromTo minBound maxBound

fromName :: T.Text -> Maybe Weekday
fromName "Monday"    = Just Monday
fromName "Tuesday"   = Just Tuesday
fromName "Wednesday" = Just Wednesday
fromName "Thursday"  = Just Thursday
fromName "Friday"    = Just Friday
fromName "Saturday"  = Just Saturday
fromName "Sunday"    = Just Sunday
fromName _           = Nothing

data Weekday = Monday 
             | Tuesday 
             | Wednesday 
             | Thursday 
             | Friday 
             | Saturday 
             | Sunday
             deriving (Show, Eq, Generic)

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

instance ToJSON Weekday where
    toJSON x = String $ T.pack . show $ x

instance FromJSON Weekday where
    parseJSON (String s) = case (fromName s) of
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

data DayProg = DayProg { day :: Weekday
                       , channels :: [ChannelProg]
                       } deriving (Show, Eq, Generic)

instance ToJSON DayProg
instance FromJSON DayProg

mkPrism :: (Eq a, Foldable t, Applicative f, Choice p) =>
    t a -> p a (f a) -> p a (f a)
mkPrism xs = prism' id get
    where
        get x = if x `elem` xs then Just x else Nothing

buildSlots :: [Node] -> [TvSlot]
buildSlots ((NodeContent t):(NodeContent n):rest) = TVSlot t n : buildSlots rest
buildSlots ((NodeContent t):(NodeElement el):rest) = TVSlot t (el ^. contents) : buildSlots rest
buildSlots [] = []

getSlots :: Element -> [Node]
getSlots el = el ^.. allNamed (only "span") . children . traversed

getChannel :: Element -> T.Text -> [Element]
getChannel el ch = el ^.. allAttributed (ix "id" . only ch) . elements

getChannels :: Element -> [Element]
getChannels el = el ^.. allAttributed (ix "id" . mkPrism cs)
    where
        cs = (T.toLower . fst) <$> tvChannels

getDays :: LT.Text -> [Element]
getDays doc = doc ^.. html . allAttributed (ix "id" . mkPrism ds)
    where
        ds = T.pack . show <$> weekdays

sample :: IO LT.Text
sample = LTIO.readFile "./tv.html"

buildChannelProg :: Element -> [ChannelProg]
buildChannelProg e = [ChannelProg ch prog]
    where
        prog = buildSlots . getSlots $ e
        ch = e ^. attr "id" . _Just

extract :: LT.Text -> [DayProg]
extract htmlData = getDays htmlData
    ||> foldMap getChannels
    ||> foldMap buildChannelProg
    ||> chunksOf 3
    ||> zip (enumFromTo Monday Sunday)
    ||> fmap (uncurry DayProg)
