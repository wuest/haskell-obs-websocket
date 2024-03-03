{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.OBSWebSocket.API.Server where

{- |
Module      :  Web.OBSWebSocket.API.Server
Copyright   :  (c) Christina Wuest 2024
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Messages sent from OBS's WebSocket interface
-}

import Prelude

import qualified Data.Aeson.Types as JSON.Types

import Data.Aeson ( FromJSON(..), (.:), (.:?), withObject
                  , Object
                  )

import Control.Monad ( mzero )

type OBSMessageParser = Object -> JSON.Types.Parser ServerMessage

data AuthChallenge = AuthChallenge { challenge :: String
                                   , salt :: String
                                   }
instance FromJSON AuthChallenge where
    parseJSON = withObject "Authentication" $ \o -> do
        challenge <- o .: "challenge"
        salt <- o .: "salt"
        return AuthChallenge{..}

data RequestStatus = RequestStatus { requestType :: String
                                   , requestId :: String
                                   , requestStatus :: Object
                                   }
instance FromJSON RequestStatus where
    parseJSON = withObject "RequestStatus" $ \o -> do
        requestType <- o .: "requestType"
        requestId <- o .: "requestId"
        requestStatus <- o .: "requestStatus"
        return RequestStatus{..}

data ServerMessage = Hello { obsWebSocketVersion :: String
                           , remoteRPCVersion :: Integer
                           , authenticationChallenge :: Maybe AuthChallenge
                           }
                   | Identified { negotiatedRPCVersion :: Integer }
                   | Event { eventType :: String
                           , eventIntent :: Integer
                           , eventData :: Maybe Object
                           }
                   | RequestResponse { status :: RequestStatus
                                     , responseData :: Maybe Object
                                     }
                   | RequestBatchResponse { batchResponseId :: String
                                          , results :: [Object]
                                          }
instance FromJSON ServerMessage where
    parseJSON = withObject "OBSMessage" $ \o -> do
        o .: "op" >>= \(opcode :: Integer) ->
            case opcode of
              0 -> o .: "d" >>= hello
              2 -> o .: "d" >>= identified
              5 -> o .: "d" >>= event
              7 -> o .: "d" >>= response
              9 -> o .: "d" >>= batchResponse
              _ -> mzero

hello :: OBSMessageParser
hello o = do
    obsWebSocketVersion <- o .: "obsWebSocketVersion"
    remoteRPCVersion <- o .: "rpcVersion"
    authenticationChallenge <- o .:? "authentication"
    return Hello{..}

identified :: OBSMessageParser
identified o = do
    negotiatedRPCVersion <- o .: "negotiatedRPCVersion"
    return Identified{..}

event :: OBSMessageParser
event o = do
    eventType <- o .: "eventType"
    eventIntent <- o .: "eventIntent"
    eventData <- o .:? "eventData"
    return Event{..}

response :: OBSMessageParser
response o = do
    status <- o .: ""
    responseData <- o .:? "responseData"
    return RequestResponse{..}

batchResponse :: OBSMessageParser
batchResponse o = do
    batchResponseId <- o .: "requestId"
    results <- o .: "results"
    return RequestBatchResponse{..}
