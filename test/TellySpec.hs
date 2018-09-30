module TellySpec where

import Test.Tasty.Hspec
import Telly

spec :: Spec
spec = do
    describe "Telly" $ do
        it "exports a useless string" $
            Telly.telly `shouldBe` "telly"
