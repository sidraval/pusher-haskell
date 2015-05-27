module PusherSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Pusher" $ do
    describe "pleaseTestMe" $ do
      it "passes" $ do
        "" `shouldBe` ""

main :: IO ()
main = hspec spec
