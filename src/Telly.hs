{-# LANGUAGE OverloadedStrings #-}

module Telly where

import Control.Lens
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

import qualified Data.Aeson.Encode.Pretty as AEP (encodePretty)
import qualified Text.Taggy.Lens as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB

import Telly.Weekday
import Telly.Data

(||>) :: a -> (a -> b) -> b
(||>) x f = f x
infixl 0 ||>

mkPrism :: (Eq a, Foldable t, Applicative f, Choice p) =>
    t a -> p a (f a) -> p a (f a)
mkPrism xs = prism' id get
    where
        get x = if x `elem` xs then Just x else Nothing

-- Turns a sequence of `time`, `name` containing nodes into a list of TvSlot
-- and unwraps the text nodes in the cases where they are wrapped in an <em>
-- (slightly ugly because of the non-exhaustiveness..)
buildSlots :: [TL.Node] -> [TvSlot]
buildSlots (TL.NodeContent t : TL.NodeContent n  : rest) = TVSlot n t : buildSlots rest
buildSlots (TL.NodeContent t : TL.NodeElement el : rest) = TVSlot (el ^. TL.contents) t : buildSlots rest
buildSlots [] = []

getSlots :: TL.Element -> [TL.Node]
getSlots el = el ^.. TL.allNamed (only "span") . TL.children . traversed

getChannels :: TL.Element -> [TL.Element]
getChannels el = el ^.. TL.allAttributed (ix "id" . mkPrism cs)
    where
        cs = T.toLower . fst <$> tvChannels

getDays :: LT.Text -> [TL.Element]
getDays doc = doc ^.. TL.html . TL.allAttributed (ix "id" . mkPrism ds)
    where
        ds = T.toLower . T.pack . show <$> weekdays

buildChannelProg :: TL.Element -> [ChannelProg]
buildChannelProg e = [ChannelProg (toName ch) prog]
    where
        prog = buildSlots . getSlots $ e
        ch = e ^. TL.attr "id" . _Just
        toName c = fromMaybe "" (lookup c tvChannels)

extract :: LT.Text -> [DayProg]
extract htmlData = getDays htmlData
    ||> foldMap getChannels
    ||> foldMap buildChannelProg
    ||> chunksOf 3
    ||> zip (enumFromTo Monday Sunday)
    ||> fmap (uncurry DayProg)

tvToJSON :: LT.Text -> LB.ByteString
tvToJSON = AEP.encodePretty . extract
