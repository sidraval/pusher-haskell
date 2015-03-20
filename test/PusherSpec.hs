module PusherSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Pusher" $ do
    describe "urlWithoutSignature" $ do
      it "returns a correctly formatted string" $ do
        let p = Pusher "ID" "KEY" "SECRET"
        result <- urlWithoutSignature p "MD5BODY" (return "T")
        result `shouldBe` "http://api.pusherapp.com/apps/ID/events?body_md5=MD5BODY&auth_version=1.0&auth_key=KEY&auth_timestamp=T"
    describe "requestBody" $ do
      it "returns a correctly formatted string" $ do
        let body = requestBody "channel" (Event "name" "data")
        body `shouldBe` "{\"name\": \"name\", \"channel\": \"channel\", \"data\":data}"
    describe "unsignedAuthString" $ do
      it "returns a correctly formatted string" $ do
        let p = Pusher "ID" "KEY" "SECRET"
        string <- unsignedAuthString p (return "T") "MD5BODY"
        string `shouldBe` "POST\n/apps/ID/events\nauth_key=KEY&auth_timestamp=T&auth_version=1.0&body_md5=MD5BODY"
    describe "idKeyAndTimestamp" $ do
      it "returns a correctly formatted string" $ do
        idKeyAndTimestamp "i" "k" "t" `shouldBe` "POST\n/apps/i/events\nauth_key=k&auth_timestamp=t"
    describe "withVersionAndBody" $ do
      it "returns a correctly formatted string" $ do
        string <- withVersionAndBody "MD5BODY" "URL"
        string `shouldBe` "URL&auth_version=1.0&body_md5=MD5BODY"
    describe "contentType" $ do
      it "is application/json" $ do
        contentType `shouldBe` "application/json"

main :: IO ()
main = hspec spec
