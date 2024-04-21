{-# LANGUAGE RecordWildCards       #-}

module Web.OBSWebSocket.API.Client where

{- |
Module      :  Web.OBSWebSocket.API.Client
Copyright   :  (c) Christina Wuest 2024
License     :  BSD-style

Maintainer  :  tina@wuest.me
Stability   :  experimental
Portability :  non-portable

Messages sent to OBS's WebSocket interface
-}

import Prelude

import Data.Aeson ( ToJSON(..), (.=), object
                  , Object
                  )
import Data.Bits  ( (.|.) )

import Data.ByteString.Char8      ( fromStrict, toStrict )
import Data.ByteString.Lazy.Char8 ( append, pack, unpack )
import Data.Digest.Pure.SHA       ( sha256 )

import qualified Data.Binary as Binary            ( encode )
import qualified Data.ByteString.Base64 as Base64 ( encode )


data EventType = General
               | Config
               | Scenes
               | Inputs
               | Transitions
               | Filters
               | Outputs
               | SceneItems
               | MediaInputs
               | Vendors
               | Ui
               | InputVolumeMeters
               | InputActiveStateChanged
               | InputShowStateChanged
               | SceneItemTransformChanged
               deriving ( Show )
eventNum :: EventType -> Integer
eventNum General = 0x001
eventNum Config = 0x002
eventNum Scenes = 0x004
eventNum Inputs = 0x008
eventNum Transitions = 0x010
eventNum Filters = 0x020
eventNum Outputs = 0x040
eventNum SceneItems = 0x080
eventNum MediaInputs = 0x100
eventNum Vendors = 0x200
eventNum Ui = 0x400
eventNum InputVolumeMeters = 0x10000
eventNum InputActiveStateChanged = 0x20000
eventNum InputShowStateChanged = 0x40000
eventNum SceneItemTransformChanged = 0x80000

eventMask :: [EventType] -> Integer
eventMask = foldl (.|.) 0 . fmap eventNum

data ClientMessage = Identify { clientRPCVersion :: Integer
                              , authenticationRequest :: Maybe String
                              , eventSubscriptions :: [EventType]
                              }
                   | Reidentify { newEventSubscriptions :: [EventType] }
                   | Request { requestType :: String
                             , requestId :: String
                             , requestData :: Maybe Object
                             }
                   | RequestBatch { batchRequestId :: String
                                  , haltOnFailure :: Maybe Bool
                                  , executionType :: Maybe Integer
                                  , requests :: [Object]
                                  } deriving ( Show )

instance ToJSON ClientMessage where
    toJSON Identify{..} =
        object [ "op" .= (1 :: Integer)
               , "d" .= object [ "rpcVersion" .= clientRPCVersion
                               , "authentication" .= authenticationRequest
                               , "eventSubscriptions" .= eventMask eventSubscriptions
                               ]
               ]
    toJSON Reidentify{..} =
        object [ "op" .= (3 :: Integer)
               , "d" .= object [ "eventSubscriptions" .= eventMask newEventSubscriptions
                               ]
               ]
    toJSON Request{..} =
        object [ "op" .= (6 :: Integer)
               , "d" .= object [ "requestType" .= requestType
                               , "requestId" .= requestId
                               , "requestData" .= requestData
                               ]
               ]
    toJSON RequestBatch{..} =
        object [ "op" .= (8 :: Integer)
               , "d" .= object [ "requestId" .= batchRequestId
                               , "haltOnFailure" .= haltOnFailure
                               , "executiontype" .= executionType
                               , "requests" .= requests
                               ]
               ]

genAuthString :: String -> String -> String -> String
genAuthString salt challenge pass =
    let step1 = fromStrict . Base64.encode . toStrict . Binary.encode . sha256 . pack $ pass ++ salt
        step2 = Base64.encode . toStrict . Binary.encode . sha256 $ append step1 $ pack challenge
     in unpack $ fromStrict step2
