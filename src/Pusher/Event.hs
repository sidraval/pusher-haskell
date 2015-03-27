{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pusher.Event where

import Network.HTTP
import Control.Applicative
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Pusher.Base

class CanTriggerEvent a b where
  triggerEvent :: Pusher -> a -> Event -> b

instance CanTriggerEvent Channel (IO String) where
  triggerEvent p c e = do
    url <- generateUrl p c e
    response <- simpleHTTP $ postRequestWithBody url contentType (requestBody c e)
    getResponseBody response

instance CanTriggerEvent [Channel] [IO String] where
  triggerEvent p cs e = map (flip (generateUrl p) e) cs

-- Generate full URL for posting to Pusher
generateUrl :: Pusher -> Channel -> Event -> IO String
generateUrl p c e = do
  let md5body = md5s . Str $ requestBody c e
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
requestBody :: Channel -> Event -> String
requestBody c e = "{\"name\": \""
                  ++ (eventName e)
                  ++ "\", \"channel\": \""
                  ++ c
                  ++ "\", \"data\":"
                  ++ (eventData e)
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
  >>= withVersionAndBody b

-- Helper for unsignedAuthString
idKeyAndTimestamp :: String -> String -> String -> String
idKeyAndTimestamp i k t = "POST\n/apps/"
                          ++ i
                          ++ "/events\nauth_key="
                          ++ k
                          ++ "&auth_timestamp="
                          ++ t

-- Helper for unsignedAuthString
withVersionAndBody :: Md5Body -> String -> IO String
withVersionAndBody md5body url =
  return $ url ++ "&auth_version=1.0&body_md5=" ++ md5body
