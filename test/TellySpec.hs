{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TellySpec where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as AE
import Data.Maybe 

import Test.Tasty.Hspec

import Telly

spec_Telly :: Spec
spec_Telly = do
    describe "Telly" $ do

        describe "Weekday" $ do
            it "should deserialize as expected" $ do
                AE.decode "\"Monday\"" `shouldBe` (Just Monday)
                AE.decode "\"Tuesday\"" `shouldBe` (Just Tuesday)
                AE.decode "\"Wednesday\"" `shouldBe` (Just Wednesday)
                AE.decode "\"Thursday\"" `shouldBe` (Just Thursday)
                AE.decode "\"Friday\"" `shouldBe` (Just Friday)
                AE.decode "\"Saturday\"" `shouldBe` (Just Saturday)
                AE.decode "\"Sunday\"" `shouldBe` (Just Sunday)
            it "should serialize as expected" $ do
                AE.encode Monday `shouldBe` "\"Monday\"" 
                AE.encode Tuesday `shouldBe` "\"Tuesday\""
                AE.encode Wednesday `shouldBe` "\"Wednesday\""
                AE.encode Thursday `shouldBe`"\"Thursday\""
                AE.encode Friday `shouldBe` "\"Friday\""
                AE.encode Saturday `shouldBe`"\"Saturday\""
                AE.encode Sunday `shouldBe` "\"Sunday\""

        describe "TVSlot" $ do
            it "should deserialize as expected" $
                AE.decode "{\"name\":\"disneyclub\",\"timeslot\":\"13:00\"}" `shouldBe` (Just $ TVSlot "disneyclub" "13:00")
            it "should serialize as expected" $
                AE.encode (Just $ TVSlot "disneyclub" "13:00") `shouldBe` "{\"name\":\"disneyclub\",\"timeslot\":\"13:00\"}"

        describe "ChannelProg" $ do
            it "should deserialize as expected" $ do
                AE.decode "{\"channel\":\"Kids\",\"programming\":[{\"name\":\"disneyclub\",\"timeslot\":\"13:00\"}]}" `shouldBe` (Just $ ChannelProg "Kids" [TVSlot "disneyclub" "13:00"])
            it "should serialize as expected" $ do
                AE.encode (Just $ ChannelProg "Kids" [TVSlot "disneyclub" "13:00"]) `shouldBe` "{\"channel\":\"Kids\",\"programming\":[{\"name\":\"disneyclub\",\"timeslot\":\"13:00\"}]}"

        describe "DayProg" $ do
            it "should deserialize as expected" $ do
                AE.decode "{\"channels\":[{\"channel\":\"Kids\",\"programming\":[{\"name\":\"disneyclub\",\"timeslot\":\"13:00\"}]}],\"day\":\"Monday\"}" `shouldBe` (Just (DayProg Monday [(ChannelProg "Kids" [TVSlot "disneyclub" "13:00"])]))
            it "should serialize as expected" $ do
                AE.encode (DayProg Monday [(ChannelProg "Kids" [TVSlot "disneyclub" "13:00"])]) `shouldBe` "{\"channels\":[{\"channel\":\"Kids\",\"programming\":[{\"name\":\"disneyclub\",\"timeslot\":\"13:00\"}]}],\"day\":\"Monday\"}"

        describe "sample.json" $ do
            it "can be deserialized" $ do
                sampleData <- BS.readFile "./sample.json"
                isJust (AE.decode sampleData :: Maybe [DayProg]) `shouldBe` True
            it "records program for 7 days" $ do
                sampleData <- BS.readFile "./sample.json"
                let program = (AE.decode sampleData :: Maybe [DayProg])
                length <$> program `shouldBe` (Just 7)
