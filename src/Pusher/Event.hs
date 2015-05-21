{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Pusher.Event where

import Network.HTTP
import Control.Applicative
import Data.Aeson.Encode (encode)
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Pusher.Base

class RequestBodyable a where
  requestBody :: a -> Event -> String

triggerEvent :: (RequestBodyable a) => Pusher -> a -> Event -> IO String
triggerEvent p c e = do
  let b = requestBody c e
  url <- generateUrl p b e
  response <- simpleHTTP $ postRequestWithBody url contentType b
  getResponseBody response

-- Generate full URL for posting to Pusher
generateUrl :: Pusher -> String -> Event -> IO String
generateUrl p b e = do
  let md5body = md5s . Str $ b
  let timestamp = authTimestamp
  withoutSignature <- urlWithoutSignature p md5body timestamp
  (++) (withoutSignature  ++ "&auth_signature=")
    <$> signedAuthString p timestamp md5body

urlWithoutSignature :: Pusher -> Md5Body -> Timestamp -> IO String
urlWithoutSignature p@(Pusher _ k _) b t = ((++) (baseUrl p
                                                  ++ "/events?body_md5="
                                                  ++ b
                                                  ++ "&auth_version=1.0&auth_key="
                                                  ++ k
                                                  ++ "&auth_timestamp="))
                                            <$> t

-- Encoding of data for POST to trigger an event
instance RequestBodyable Channel where
  requestBody c e = "{\"name\": \""
                    ++ (eventName e)
                    ++ "\", \"channel\": \""
                    ++ c
                    ++ "\", \"data\":"
                    ++ (B.unpack . encode . eventData $ e)
                    ++ "}"

instance RequestBodyable Channels where
  requestBody cs e = "{\"name\": \""
                     ++ (eventName e)
                     ++ "\", \"channels\": "
                     ++ show cs
                     ++ ", \"data\":"
                     ++ (B.unpack . encode . eventData $ e)
                     ++ "}"

-- Signed authentication string
signedAuthString :: Pusher -> Timestamp -> Md5Body -> IO String
signedAuthString p@(Pusher _ _ appSecret) t b = do
  signatureString <- unsignedAuthString p t b >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

-- Full string ready to be signed by the app secret
unsignedAuthString :: Pusher -> Timestamp -> Md5Body -> IO String
unsignedAuthString (Pusher appId appKey _) t b =
  idKeyAndTimestamp appId appKey
  <$> t
  >>= (\u -> return $ u ++ "&auth_version=1.0&body_md5=" ++ b)

-- Helper for unsignedAuthString
idKeyAndTimestamp :: String -> String -> String -> String
idKeyAndTimestamp i k t = "POST\n/apps/"
                          ++ i
                          ++ "/events\nauth_key="
                          ++ k
                          ++ "&auth_timestamp="
                          ++ t
