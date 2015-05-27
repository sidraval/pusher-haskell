-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Pusher.Channel
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Sid Raval <sidsraval@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- The 'Network.Pusher.Channel' module provides an simple interface for interacting
-- with Pusher.com's @channel@ endpoints. In particular, one can fetch info
-- about every occupied channel for a given App ID, or more detailed information
-- about a channel specified by name.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Pusher.Channel (getChannelInfo) where

import Network.HTTP
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Data.List
import Network.Pusher.Base

-- | @getChannelInfo pusher channel info@ requests information about a
-- particular channel for the given 'Pusher' instance. The result is either an
-- error message returned by the Pusher server, or a 'ChannelInfo' data
-- structure.

getChannelInfo :: Pusher -> Channel -> [Info] -> IO (Either String ChannelInfo)
getChannelInfo p c is = do
  url <- generateUrl p c is
  response <- simpleHTTP $ getRequest url
  body <- getResponseBody response
  case (decode . B.pack $ body) of
    (Just c) -> return $ Right c
    Nothing -> return $ Left body

generateUrl :: Pusher -> Channel -> [Info] -> IO String
generateUrl p c is = do
  let timestamp = authTimestamp
  withoutSignature <- urlWithoutSignature p c is timestamp
  (++) (withoutSignature  ++ "&auth_signature=")
    <$> signedAuthString p c is timestamp

urlWithoutSignature :: Pusher -> Channel -> [Info] -> Timestamp -> IO String
urlWithoutSignature p@(Pusher _ k _) c is t = ((++) (baseUrl p
                                                  ++ "/channels/"
                                                  ++ c
                                                  ++ "?auth_version=1.0"
                                                  ++ "&auth_key="
                                                  ++ k
                                                  ++ queryParamFromInfo is
                                                  ++ "&auth_timestamp="))
                                            <$> t
queryParamFromInfo :: [Info] -> String
queryParamFromInfo [] = mzero
queryParamFromInfo xs = "&info=" ++ (intercalate "," $ map show xs)

signedAuthString :: Pusher -> Channel -> [Info] -> Timestamp -> IO String
signedAuthString p@(Pusher _ _ appSecret) c is t = do
  signatureString <- unsignedAuthString p c is t >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

unsignedAuthString :: Pusher -> Channel -> [Info] -> Timestamp -> IO String
unsignedAuthString (Pusher appId appKey _) c is t =
  idKeyAndTimestamp appId appKey c
  <$> t
  >>= (\u -> return $ u ++ "&auth_version=1.0" ++ queryParamFromInfo is)

idKeyAndTimestamp :: String -> String -> Channel -> String -> String
idKeyAndTimestamp i k c t = "GET\n/apps/"
                            ++ i
                            ++ "/channels/"
                            ++ c
                            ++ "\nauth_key="
                            ++ k
                            ++ "&auth_timestamp="
                            ++ t
