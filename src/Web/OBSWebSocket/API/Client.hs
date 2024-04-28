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

import Data.Aeson ( ToJSON(..), (.=), object )
import Data.Bits  ( (.|.) )

import Data.ByteString.Char8      ( fromStrict, toStrict )
import Data.ByteString.Lazy.Char8 ( append, pack, unpack )
import Data.Digest.Pure.SHA       ( sha256 )

import qualified Data.Aeson as JSON               ( Value(..) )
import qualified Data.Binary as Binary            ( encode )
import qualified Data.ByteString.Base64 as Base64 ( encode )
import qualified Data.HashMap.Strict as HM        ( insert )
import qualified Data.Text           as Text      ( Text, pack )

data Realm = Global | Profile
    deriving ( Show )
instance ToJSON Realm where
    toJSON Global = JSON.String "OBS_WEBSOCKET_DATA_REALM_GLOBAL"
    toJSON Profile = JSON.String "OBS_WEBSOCKET_DATA_REALM_PROFILE"

data MonitorType = NoMonitor | MonitorOnly | MonitorAndOutput
    deriving ( Show )
instance ToJSON MonitorType where
    toJSON NoMonitor = JSON.String "OBS_MONITORING_TYPE_NONE"
    toJSON MonitorOnly = JSON.String "OBS_MONITORING_TYPE_MONITOR_ONLY"
    toJSON MonitorAndOutput = JSON.String "OBS_MONITORING_TYPE_MONITOR_AND_OUTPUT"

data BlendMode = BlendNormal | BlendAdditive | BlendSubtract | BlendScreen | BlendMultiply | BlendLighten | BlendDarken
    deriving ( Show )
instance ToJSON BlendMode where
    toJSON BlendNormal = JSON.String "OBS_BLEND_NORMAL"
    toJSON BlendAdditive = JSON.String "OBS_BLEND_ADDITIVE"
    toJSON BlendSubtract = JSON.String "OBS_BLEND_SUBTRACT"
    toJSON BlendScreen = JSON.String "OBS_BLEND_SCREEN"
    toJSON BlendMultiply = JSON.String "OBS_BLEND_MULTIPLY"
    toJSON BlendLighten = JSON.String "OBS_BLEND_LIGHTEN"
    toJSON BlendDarken = JSON.String "OBS_BLEND_DARKEN"

data MediaState = NoMediaState | MediaPlaying | MediaOpening | MediaBuffering | MediaPaused | MediaStopped | MediaEnded | MediaError
    deriving ( Show )
instance ToJSON MediaState where
    toJSON NoMediaState = JSON.String "OBS_MEDIA_STATE_NONE"
    toJSON MediaPlaying = JSON.String "OBS_MEDIA_STATE_PLAYING"
    toJSON MediaOpening = JSON.String "OBS_MEDIA_STATE_OPENING"
    toJSON MediaBuffering = JSON.String "OBS_MEDIA_STATE_BUFFERING"
    toJSON MediaPaused = JSON.String "OBS_MEDIA_STATE_PAUSED"
    toJSON MediaStopped = JSON.String "OBS_MEDIA_STATE_STOPPED"
    toJSON MediaEnded = JSON.String "OBS_MEDIA_STATE_ENDED"
    toJSON MediaError = JSON.String "OBS_MEDIA_STATE_ERROR"

data MixType = MixPreview | MixProgram | MixMultiView
    deriving ( Show )
instance ToJSON MixType where
    toJSON MixPreview = JSON.String "OBS_WEBSOCKET_VIDEO_MIX_TYPE_PREVIEW"
    toJSON MixProgram = JSON.String "OBS_WEBSOCKET_VIDEO_MIX_TYPE_PROGRAM"
    toJSON MixMultiView = JSON.String "OBS_WEBSOCKET_VIDEO_MIX_TYPE_MULTIVIEW"

data MediaAction = MediaNone | MediaPlay | MediaPause | MediaStop | MediaRestart | MediaNext | MediaPrevious
    deriving ( Show )
instance ToJSON MediaAction where
    toJSON MediaNone = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_NONE"
    toJSON MediaPlay = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PLAY"
    toJSON MediaPause = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PAUSE"
    toJSON MediaStop = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_STOP"
    toJSON MediaRestart = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_RESTART"
    toJSON MediaNext = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_NEXT"
    toJSON MediaPrevious = JSON.String "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PREVIOUS"

type ProjectorGeometry = String

-- Ergonomics notes: fix before release
-- Types shared between client and server should be extracted
-- (Maybe String) (Maybe String) is an either in hiding.  Fix this
-- Some 2-tuples probably want to be extracted as more explicit types
-- Boolean overlay + a JSON.Value can be extracted as a type
-- Projector Geometry is a Qt object submitted in a base64 encoded format - no reason the developer should have to deal with that, this needs a type
data RequestData = GetVersion
                 | GetStats
                 | BroadcastCustomEvent JSON.Value
                 | CallVendorRequest String String (Maybe JSON.Value)
                 | GetHotkeyList
                 | TriggerHotkeyByName String (Maybe String)
                 | Sleep (Maybe Integer) (Maybe Integer)
                 | GetPersistentData Realm String
                 | SetPersistentData Realm String JSON.Value
                 | GetSceneCollectionList
                 | SetCurrentSceneCollection String
                 | CreateSceneCollection String
                 | GetProfileList
                 | SetCurrentProfile String
                 | CreateProfile String
                 | RemoveProfile String
                 | GetProfileParameter String String
                 | SetProfileParameter String String String
                 | GetVideoSettings
                 | SetVideoSettings (Maybe (Integer, Integer)) (Maybe (Integer, Integer)) (Maybe (Integer, Integer))
                 | GetStreamServiceSettings
                 | SetStreamServiceSettings String JSON.Value
                 | GetRecordDirectory
                 | SetRecordDirectory FilePath
                 | GetSourceActive (Maybe String) (Maybe String)
                 | GetSourceScreenshot String String (Maybe (Integer, Integer)) (Maybe Integer)
                 | SaveSourceScreenshot String String FilePath (Maybe (Integer, Integer)) (Maybe Integer)
                 | GetSceneList
                 | GetGroupList
                 | GetCurrentProgramScene
                 | SetCurrentProgramScene (Maybe String) (Maybe String)
                 | GetCurrentPreviewScene
                 | SetCurrentPreviewScene (Maybe String) (Maybe String)
                 | CreateScene String
                 | RemoveScene (Maybe String) (Maybe String)
                 | SetSceneName (Maybe String) (Maybe String) String
                 | GetSceneSceneTransitionOverride String
                 | SetSceneSceneTransitionOverride String (Maybe String) (Maybe Integer)
                 | GetInputList (Maybe String)
                 | GetInputKindList (Maybe Bool)
                 | GetSpecialInputs
                 | CreateInput String String String (Maybe JSON.Value) (Maybe Bool)
                 | RemoveInput (Maybe String) (Maybe String)
                 | SetInputName (Maybe String) (Maybe String) String
                 | GetInputDefaultSettings String
                 | GetInputSettings (Maybe String) (Maybe String)
                 | SetInputSettings (Maybe String) (Maybe String) JSON.Value (Maybe Bool)
                 | GetInputMute (Maybe String) (Maybe String)
                 | SetInputMute (Maybe String) (Maybe String) Bool
                 | ToggleInputMute (Maybe String) (Maybe String)
                 | GetInputVolume (Maybe String) (Maybe String)
                 | SetInputVolume (Maybe String) (Maybe String) (Maybe (Integer, Integer))
                 | GetInputAudioBalance (Maybe String) (Maybe String)
                 | SetInputAudioBalance (Maybe String) (Maybe String) Float
                 | GetInputAudioSyncOffset (Maybe String) (Maybe String)
                 | SetInputAudioSyncOffset (Maybe String) (Maybe String) Integer
                 | GetInputAudioMonitorType (Maybe String) (Maybe String)
                 | SetInputAudioMonitorType (Maybe String) (Maybe String) MonitorType
                 | GetInputAudioTracks (Maybe String) (Maybe String)
                 | SetInputAudioTracks (Maybe String) (Maybe String) JSON.Value
                 | GetInputPropertiesListPropertyItems (Maybe String) (Maybe String) String
                 | PressInputPropertiesButton (Maybe String) (Maybe String) String
                 | GetTransitionKindList
                 | GetSceneTransitionList
                 | GetCurrentSceneTransition
                 | SetCurrentSceneTransition String
                 | SetCurrentSceneTransitionDuration Integer
                 | SetCurrentSceneTransitionSettings JSON.Value (Maybe Bool)
                 | GetCurrentSceneTransitionCursor
                 | TriggerStudioModeTransition
                 | SetTBarPosition Float (Maybe Bool)
                 | GetSourceFilterKindList
                 | GetSourceFilterList (Maybe String) (Maybe String)
                 | GetSourceFilterDefaultSettings String
                 | CreateSourceFilter (Maybe String) (Maybe String) String String JSON.Value
                 | RemoveSourceFilter (Maybe String) (Maybe String) String
                 | SetSourceFilterName (Maybe String) (Maybe String) String String
                 | GetSourceFilter (Maybe String) (Maybe String) String
                 | SetSourceFilterIndex (Maybe String) (Maybe String) String Integer
                 | SetSourceFilterSettings (Maybe String) (Maybe String) String JSON.Value (Maybe Bool)
                 | SetSourceFilterEnabled (Maybe String) (Maybe String) String Bool
                 | GetSceneItemList (Maybe String) (Maybe String)
                 | GetGroupSceneItemList (Maybe String) (Maybe String)
                 | GetSceneItemId (Maybe String) (Maybe String) String (Maybe Integer)
                 | GetSceneItemSource (Maybe String) (Maybe String) Integer
                 | CreateSceneItem (Maybe String) (Maybe String) (Maybe String) (Maybe String) (Maybe Bool)
                 | RemoveSceneItem (Maybe String) (Maybe String) Integer
                 | DuplicateSceneItem (Maybe String) (Maybe String) Integer (Maybe String) (Maybe String)
                 | GetSceneItemTransform (Maybe String) (Maybe String) Integer
                 | SetSceneItemTransform (Maybe String) (Maybe String) Integer JSON.Value
                 | GetSceneItemEnabled (Maybe String) (Maybe String) Integer
                 | SetSceneItemEnabled (Maybe String) (Maybe String) Integer Bool
                 | GetSceneItemLocked (Maybe String) (Maybe String) Integer
                 | SetSceneItemLocked (Maybe String) (Maybe String) Integer Bool
                 | GetSceneItemIndex (Maybe String) (Maybe String) Integer
                 | SetSceneItemIndex (Maybe String) (Maybe String) Integer Integer
                 | GetSceneItemBlendMode (Maybe String) (Maybe String) Integer
                 | SetSceneItemBlendMode (Maybe String) (Maybe String) Integer BlendMode
                 | GetVirtualCamStatus
                 | ToggleVirtualCam
                 | StartVirtualCam
                 | StopVirtualCam
                 | GetReplayBufferStatus
                 | ToggleReplayBuffer
                 | StartReplayBuffer
                 | StopReplayBuffer
                 | SaveReplayBuffer
                 | GetLastReplayBufferReplay
                 | GetOutputList
                 | GetOutputStatus String
                 | ToggleOutput String
                 | StartOutput String
                 | StopOutput String
                 | GetOutputSettings String
                 | SetOutputSettings String JSON.Value
                 | GetStreamStatus
                 | ToggleStream
                 | StartStream
                 | StopStream
                 | SendStreamCaption String
                 | GetRecordStatus
                 | ToggleRecord
                 | StartRecord
                 | StopRecord
                 | ToggleRecordPause
                 | PauseRecord
                 | ResumeRecord
                 | GetMediaInputStatus (Maybe String) (Maybe String)
                 | SetMediaInputCursor (Maybe String) (Maybe String) Integer
                 | OffsetMediaInputCursor (Maybe String) (Maybe String) Integer
                 | TriggerMediaInputAction (Maybe String) (Maybe String) MediaAction
                 | GetStudioModeEnabled
                 | SetStudioModeEnabled Bool
                 | OpenInputPropertiesDialog (Maybe String) (Maybe String)
                 | OpenInputFiltersDialog (Maybe String) (Maybe String)
                 | OpenInputInteractDialog (Maybe String) (Maybe String)
                 | GetMonitorList
                 | OpenVideoMixProjector MixType (Maybe Integer) (Maybe ProjectorGeometry)
                 | OpenSourceProjector (Maybe String) (Maybe String) (Maybe Integer) (Maybe ProjectorGeometry)
                 deriving ( Show )

toRequestData :: RequestData -> String -> JSON.Value
toRequestData GetVersion rid = object [ "requestId" .= rid
                                      , "requestType" .= JSON.String "GetVersion"
                                      ]
toRequestData GetStats rid = object [ "requestId" .= rid
                                    , "requestType" .= JSON.String "GetStats"
                                    ]
toRequestData (BroadcastCustomEvent val) rid = object [ "requestId" .= rid
                                                      , "requestType" .= JSON.String "BroadcastCustomEvent"
                                                      , "eventData" .= val
                                                      ]
toRequestData (CallVendorRequest vendorName reqType (Just reqData)) rid = object [ "requestId" .= rid
                                                                                 , "requestType" .= JSON.String "CallVendorRequest"
                                                                                 , "requestData" .= object [ "vendorName" .= vendorName
                                                                                                           , "requestType" .= reqType
                                                                                                           , "requestData" .= reqData
                                                                                                           ]
                                                                                 ]
toRequestData (CallVendorRequest vendorName reqType Nothing) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "CallVendorRequest"
                                                                          , "requestData" .= object [ "vendorName" .= vendorName
                                                                                                    , "requestType" .= reqType
                                                                                                    ]
                                                                          ]
toRequestData GetHotkeyList rid = object [ "requestId" .= rid
                                         , "requestType" .= JSON.String "GetHotkeyList"
                                         ]
toRequestData (TriggerHotkeyByName hotkeyName (Just contextName)) rid = object [ "requestId" .= rid
                                                                               , "requestType" .= JSON.String "TriggerHotkeyByName"
                                                                               , "requestData" .= object [ "hotkeyName" .= hotkeyName
                                                                                                         , "contextName" .= contextName
                                                                                                         ]
                                                                               ]
toRequestData (TriggerHotkeyByName hotkeyName Nothing) rid = object [ "requestId" .= rid
                                                                    , "requestType" .= JSON.String "TriggerHotkeyByName"
                                                                    , "requestData" .= object [ "hotkeyName" .= hotkeyName ]
                                                                    ]
toRequestData (Sleep (Just millis) Nothing) rid = object [ "requestId" .= rid
                                                         , "requestType" .= JSON.String "Sleep"
                                                         , "requestData" .= object [ "sleepMillis" .= millis ]
                                                         ]
toRequestData (Sleep Nothing (Just frames)) rid = object [ "requestId" .= rid
                                                         , "requestType" .= JSON.String "Sleep"
                                                         , "requestData" .= object [ "sleepFrames" .= frames ]
                                                         ]
toRequestData (GetPersistentData realm slotName) rid = object [ "requestId" .= rid
                                                              , "requestType" .= JSON.String "GetPersistentData"
                                                              , "requestData" .= object [ "realm" .= realm
                                                                                        , "slotName" .= slotName
                                                                                        ]
                                                              ]
toRequestData (SetPersistentData realm slotName value) rid = object [ "requestId" .= rid
                                                                    , "requestType" .= JSON.String "SetPersistentData"
                                                                    , "requestData" .= object [ "realm" .= realm
                                                                                              , "slotName" .= slotName
                                                                                              , "slotValue" .= value
                                                                                              ]
                                                                    ]
toRequestData GetSceneCollectionList rid = object [ "requestId" .= rid
                                                  , "requestType" .= JSON.String "GetSceneCollectionList"
                                                  ]
toRequestData (SetCurrentSceneCollection sceneCollectionName) rid = object [ "requestId" .= rid
                                                                           , "requestType" .= JSON.String "SetCurrentSceneCollection"
                                                                           , "requestData" .= object [ "sceneCollectionName" .= sceneCollectionName ]
                                                                           ]
toRequestData (CreateSceneCollection sceneCollectionName) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "CreateSceneCollection"
                                                                       , "requestData" .= object [ "sceneCollectionName" .= sceneCollectionName ]
                                                                       ]
toRequestData GetProfileList rid = object [ "requestId" .= rid
                                          , "requestType" .= JSON.String "GetProfileList"
                                          ]
toRequestData (SetCurrentProfile profileName) rid = object [ "requestId" .= rid
                                                           , "requestType" .= JSON.String "SetCurrentProfile"
                                                           , "requestData" .= object [ "profileName" .= profileName ]
                                                           ]
toRequestData (CreateProfile profileName) rid = object [ "requestId" .= rid
                                                       , "requestType" .= JSON.String "CreateProfile"
                                                       , "requestData" .= object [ "profileName" .= profileName ]
                                                       ]
toRequestData (RemoveProfile profileName) rid = object [ "requestId" .= rid
                                                       , "requestType" .= JSON.String "RemoveProfile"
                                                       , "requestData" .= object [ "profileName" .= profileName ]
                                                       ]
toRequestData (GetProfileParameter parameterCategory parameterName) rid = object [ "requestId" .= rid
                                                                                 , "requestType" .= JSON.String "GetProfileParameter"
                                                                                 , "requestData" .= object [ "parameterCategory" .= parameterCategory
                                                                                                           , "parameterName" .= parameterName
                                                                                                           ]
                                                                                 ]
toRequestData (SetProfileParameter parameterCategory parameterName parameterValue) rid = object [ "requestId" .= rid
                                                                                                , "requestType" .= JSON.String "SetProfileParameter"
                                                                                                , "requestData" .= object [ "parameterCategory" .= parameterCategory
                                                                                                                          , "parameterName" .= parameterName
                                                                                                                          , "parameterValue" .= parameterValue
                                                                                                                          ]
                                                                                                ]
toRequestData GetVideoSettings rid = object [ "requestId" .= rid
                                            , "requestType" .= JSON.String "GetVideoSettings"
                                            ]
toRequestData (SetVideoSettings (Just (fpsNumerator, fpsDenominator)) Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                    , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                                              , "fpsDenominator" .= fpsDenominator
                                                                                                                              ]
                                                                                                    ]
toRequestData (SetVideoSettings Nothing (Just (baseWidth, baseHeight)) Nothing) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetVideoSettings"
                                                                                             , "requestData" .= object [ "baseWidth" .= baseWidth
                                                                                                                       , "baseHeight" .= baseHeight
                                                                                                                       ]
                                                                                             ]
toRequestData (SetVideoSettings Nothing Nothing (Just (outputWidth, outputHeight))) rid = object [ "requestId" .= rid
                                                                                                 , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                 , "requestData" .= object [ "outputWidth" .= outputWidth
                                                                                                                           , "outputHeight" .= outputHeight
                                                                                                                           ]
                                                                                                 ]
toRequestData (SetVideoSettings (Just (fpsNumerator, fpsDenominator)) (Just (baseWidth, baseHeight)) Nothing) rid = object [ "requestId" .= rid
                                                                                                                           , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                                           , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                                                                     , "fpsDenominator" .= fpsDenominator
                                                                                                                                                     , "baseWidth" .= baseWidth
                                                                                                                                                     , "baseHeight" .= baseHeight
                                                                                                                                                     ]
                                                                                                                           ]
toRequestData (SetVideoSettings (Just (fpsNumerator, fpsDenominator)) Nothing (Just (outputWidth, outputHeight))) rid = object [ "requestId" .= rid
                                                                                                                  , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                                  , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                                                            , "fpsDenominator" .= fpsDenominator
                                                                                                                                            , "outputWidth" .= outputWidth
                                                                                                                                            , "outputHeight" .= outputHeight
                                                                                                                                            ]
                                                                                                                  ]
toRequestData (SetVideoSettings Nothing (Just (baseWidth, baseHeight)) (Just (outputWidth, outputHeight))) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                                        , "requestData" .= object [ "baseWidth" .= baseWidth
                                                                                                                                                  , "baseHeight" .= baseHeight
                                                                                                                                                  , "outputWidth" .= outputWidth
                                                                                                                                                  , "outputHeight" .= outputHeight
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (SetVideoSettings (Just (fpsNumerator, fpsDenominator)) (Just (baseWidth, baseHeight)) (Just (outputWidth, outputHeight))) rid = object [ "requestId" .= rid
                                                                                                                                                      , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                                                                      , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                                                                                                , "fpsDenominator" .= fpsDenominator
                                                                                                                                                                                , "baseWidth" .= baseWidth
                                                                                                                                                                                , "baseHeight" .= baseHeight
                                                                                                                                                                                , "outputWidth" .= outputWidth
                                                                                                                                                                                , "outputHeight" .= outputHeight
                                                                                                                                                                                ]
                                                                                                                                                      ]
toRequestData GetStreamServiceSettings rid = object [ "requestId" .= rid
                                                    , "requestType" .= JSON.String "GetStreamServiceSettings"
                                                    ]
toRequestData (SetStreamServiceSettings streamServiceType streamServiceSettings) rid = object [ "requestId" .= rid
                                                                                              , "requestType" .= JSON.String "SetStreamServiceSettings"
                                                                                              , "requestData" .= object [ "streamServiceType" .= streamServiceType
                                                                                                                        , "streamServiceSettings" .= streamServiceSettings
                                                                                                                        ]
                                                                                              ]
toRequestData GetRecordDirectory rid = object [ "requestId" .= rid
                                              , "requestType" .= JSON.String "GetRecordDirectory"
                                              ]
toRequestData (SetRecordDirectory recordDirectory) rid = object [ "requestId" .= rid
                                                                , "requestType" .= JSON.String "SetRecordDirectory"
                                                                , "requestData" .= object [ "recordDirectory" .= recordDirectory ]
                                                                ]
toRequestData (GetSourceActive (Just sourceName) Nothing) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetSourceActive"
                                                                       , "requestData" .= object [ "sourceName" .= sourceName ]
                                                                       ]
toRequestData (GetSourceActive Nothing (Just sourceUuid) ) rid = object [ "requestId" .= rid
                                                                        , "requestType" .= JSON.String "GetSourceActive"
                                                                        , "requestData" .= object [ "sourceUuid" .= sourceUuid ]
                                                                        ]
toRequestData (GetSourceScreenshot sourceName sourceFormat (Just (imageWidth, imageHeight)) (Just imageCompressionQuality)) rid = object [ "requestId" .= rid
                                                                                                                                         , "requestType" .= JSON.String "GetSourceScreenshot"
                                                                                                                                         , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                                   , "sourceFormat" .= sourceFormat
                                                                                                                                                                   , "imageWidth" .= imageWidth
                                                                                                                                                                   , "imageHeight" .= imageHeight
                                                                                                                                                                   , "imageCompressionQuality" .= imageCompressionQuality
                                                                                                                                                                   ]
                                                                                                                                         ]
toRequestData (GetSourceScreenshot sourceName sourceFormat (Just (imageWidth, imageHeight)) Nothing) rid = object [ "requestId" .= rid
                                                                                                                  , "requestType" .= JSON.String "GetSourceScreenshot"
                                                                                                                  , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                            , "sourceFormat" .= sourceFormat
                                                                                                                                            , "imageWidth" .= imageWidth
                                                                                                                                            , "imageHeight" .= imageHeight
                                                                                                                                            ]
                                                                                                                  ]
toRequestData (GetSourceScreenshot sourceName sourceFormat Nothing (Just imageCompressionQuality)) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "GetSourceScreenshot"
                                                                                                                , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                          , "sourceFormat" .= sourceFormat
                                                                                                                                          , "imageCompressionQuality" .= imageCompressionQuality
                                                                                                                                          ]
                                                                                                                ]
toRequestData (GetSourceScreenshot sourceName sourceFormat Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                         , "requestType" .= JSON.String "GetSourceScreenshot"
                                                                                         , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                   , "sourceFormat" .= sourceFormat
                                                                                                                   ]
                                                                                         ]
toRequestData (SaveSourceScreenshot sourceName sourceFormat imageFilePath (Just (imageWidth, imageHeight)) (Just imageCompressionQuality)) rid = object [ "requestId" .= rid
                                                                                                                                                        , "requestType" .= JSON.String "SaveSourceScreenshot"
                                                                                                                                                        , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                                                  , "sourceFormat" .= sourceFormat
                                                                                                                                                                                  , "imageFilePath" .= imageFilePath
                                                                                                                                                                                  , "imageWidth" .= imageWidth
                                                                                                                                                                                  , "imageHeight" .= imageHeight
                                                                                                                                                                                  , "imageCompressionQuality" .= imageCompressionQuality
                                                                                                                                                                                  ]
                                                                                                                                                        ]
toRequestData (SaveSourceScreenshot sourceName sourceFormat imageFilePath (Just (imageWidth, imageHeight)) Nothing) rid = object [ "requestId" .= rid
                                                                                                                                 , "requestType" .= JSON.String "SaveSourceScreenshot"
                                                                                                                                 , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                           , "sourceFormat" .= sourceFormat
                                                                                                                                                           , "imageFilePath" .= imageFilePath
                                                                                                                                                           , "imageWidth" .= imageWidth
                                                                                                                                                           , "imageHeight" .= imageHeight
                                                                                                                                                           ]
                                                                                                                                                        ]
toRequestData (SaveSourceScreenshot sourceName sourceFormat imageFilePath Nothing (Just imageCompressionQuality)) rid = object [ "requestId" .= rid
                                                                                                                               , "requestType" .= JSON.String "SaveSourceScreenshot"
                                                                                                                               , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                         , "sourceFormat" .= sourceFormat
                                                                                                                                                         , "imageFilePath" .= imageFilePath
                                                                                                                                                         , "imageCompressionQuality" .= imageCompressionQuality
                                                                                                                                                         ]
                                                                                                                               ]
toRequestData (SaveSourceScreenshot sourceName sourceFormat imageFilePath Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "SaveSourceScreenshot"
                                                                                                        , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                  , "sourceFormat" .= sourceFormat
                                                                                                                                  , "imageFilePath" .= imageFilePath
                                                                                                                                  ]
                                                                                                        ]
toRequestData GetSceneList rid = object [ "requestId" .= rid
                                        , "requestType" .= JSON.String "GetSceneList"
                                        ]
toRequestData GetGroupList rid = object [ "requestId" .= rid
                                        , "requestType" .= JSON.String "GetGroupList"
                                        ]
toRequestData GetCurrentProgramScene rid = object [ "requestId" .= rid
                                                  , "requestType" .= JSON.String "GetCurrentProgramScene"
                                                  ]
toRequestData (SetCurrentProgramScene (Just sceneName) Nothing) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "SetCurrentProgramScene"
                                                                             , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                             ]
toRequestData (SetCurrentProgramScene Nothing (Just sceneUuid)) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "SetCurrentProgramScene"
                                                                             , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                             ]
toRequestData GetCurrentPreviewScene rid = object [ "requestId" .= rid
                                                  , "requestType" .= JSON.String "GetCurrentPreviewScene"
                                                  ]
toRequestData (SetCurrentPreviewScene (Just sceneName) Nothing) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "SetCurrentPreviewScene"
                                                                             , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                             ]
toRequestData (SetCurrentPreviewScene Nothing (Just sceneUuid)) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "SetCurrentPreviewScene"
                                                                             , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                             ]
toRequestData (CreateScene sceneName) rid = object [ "requestId" .= rid
                                                   , "requestType" .= JSON.String "CreateScene"
                                                   , "requestData" .= object [ "sceneName" .= sceneName ]
                                                   ]
toRequestData (RemoveScene (Just sceneName) Nothing) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "RemoveScene"
                                                                  , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                  ]
toRequestData (RemoveScene Nothing (Just sceneUuid)) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "RemoveScene"
                                                                  , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                  ]
toRequestData (SetSceneName (Just sceneName) Nothing newSceneName) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "SetSceneName"
                                                                                , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                          , "newSceneName" .= newSceneName
                                                                                                          ]
                                                                                ]
toRequestData (SetSceneName Nothing (Just sceneUuid) newSceneName) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "SetSceneName"
                                                                                , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                          , "newSceneName" .= newSceneName
                                                                                                          ]
                                                                                ]
toRequestData (GetSceneSceneTransitionOverride sceneName) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetSceneSceneTransitionOverride"
                                                                       , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                       ]
toRequestData (SetSceneSceneTransitionOverride sceneName (Just transitionName) (Just transitionDuration)) rid = object [ "requestId" .= rid
                                                                                                                       , "requestType" .= JSON.String "SetSceneSceneTransitionOverride"
                                                                                                                       , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                                 , "transitionName" .= transitionName
                                                                                                                                                 , "transitionDuration" .= transitionDuration
                                                                                                                                                 ]
                                                                                                                       ]
toRequestData (SetSceneSceneTransitionOverride sceneName (Just transitionName) Nothing) rid = object [ "requestId" .= rid
                                                                                                     , "requestType" .= JSON.String "SetSceneSceneTransitionOverride"
                                                                                                     , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                               , "transitionName" .= transitionName
                                                                                                                               ]
                                                                                                     ]
toRequestData (SetSceneSceneTransitionOverride sceneName Nothing (Just transitionDuration)) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "SetSceneSceneTransitionOverride"
                                                                                                         , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                   , "transitionDuration" .= transitionDuration
                                                                                                                                   ]
                                                                                                         ]
toRequestData (SetSceneSceneTransitionOverride sceneName Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                       , "requestType" .= JSON.String "SetSceneSceneTransitionOverride"
                                                                                       , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                                       ]
toRequestData (GetInputList (Just inputKind)) rid = object [ "requestId" .= rid
                                                           , "requestType" .= JSON.String "GetInputList"
                                                           , "requestData" .= object [ "inputKind" .= inputKind ]
                                                           ]
toRequestData (GetInputList Nothing) rid = object [ "requestId" .= rid
                                                  , "requestType" .= JSON.String "GetInputList"
                                                  ]
toRequestData (GetInputKindList (Just unversioned)) rid = object [ "requestId" .= rid
                                                                 , "requestType" .= JSON.String "GetInputKindList"
                                                                 , "requestData" .= object [ "unversioned" .= unversioned ]
                                                                 ]
toRequestData (GetInputKindList Nothing) rid = object [ "requestId" .= rid
                                                      , "requestType" .= JSON.String "GetInputKindList"
                                                      ]
toRequestData GetSpecialInputs rid = object [ "requestId" .= rid
                                            , "requestType" .= JSON.String "GetSpecialInputs"
                                            ]
toRequestData (CreateInput sceneName inputName inputKind (Just inputSettings) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                                    , "requestType" .= JSON.String "CreateInput"
                                                                                                                    , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                              , "inputName" .= inputName
                                                                                                                                              , "inputKind" .= inputKind
                                                                                                                                              , "inputSettings" .= inputSettings
                                                                                                                                              , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                              ]
                                                                                                                    ]
toRequestData (CreateInput sceneName inputName inputKind (Just inputSettings) Nothing) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "CreateInput"
                                                                                                    , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                              , "inputName" .= inputName
                                                                                                                              , "inputKind" .= inputKind
                                                                                                                              , "inputSettings" .= inputSettings
                                                                                                                              ]
                                                                                                    ]
toRequestData (CreateInput sceneName inputName inputKind Nothing (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                       , "requestType" .= JSON.String "CreateInput"
                                                                                                       , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                 , "inputName" .= inputName
                                                                                                                                 , "inputKind" .= inputKind
                                                                                                                                 , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                 ]
                                                                                                       ]
toRequestData (CreateInput sceneName inputName inputKind Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                       , "requestType" .= JSON.String "CreateInput"
                                                                                       , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                 , "inputName" .= inputName
                                                                                                                 , "inputKind" .= inputKind
                                                                                                                 ]
                                                                                       ]
toRequestData (RemoveInput (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "RemoveInput"
                                                                  , "requestData" .= object [ "inputName" .= inputName ]
                                                                  ]
toRequestData (RemoveInput Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "RemoveInput"
                                                                  , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                  ]
toRequestData (SetInputName (Just inputName) Nothing newInputName) rid = object [ "requestId" .= rid
                                                                               , "requestType" .= JSON.String "SetInputName"
                                                                               , "requestData" .= object [ "inputName" .= inputName ]
                                                                               ]
toRequestData (GetInputDefaultSettings inputKind) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetInputDefaultSettings"
                                                               , "requestData" .= object [ "inputKind" .= inputKind ]
                                                               ]
toRequestData (GetInputSettings (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetInputSettings"
                                                                       , "requestData" .= object [ "inputName" .= inputName ]
                                                                       ]
toRequestData (GetInputSettings Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetInputSettings"
                                                                       , "requestUuid" .= object [ "inputName" .= inputUuid ]
                                                                       ]
toRequestData (SetInputSettings (Just inputName) Nothing inputSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetInputSettings"
                                                                                                    , "requestData" .= object [ "inputName" .= inputName
                                                                                                                              , "inputSettings" .= inputSettings
                                                                                                                              , "overlay" .= overlay
                                                                                                                              ]
                                                                                                    ]
toRequestData (SetInputSettings (Just inputName) Nothing inputSettings Nothing) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetInputSettings"
                                                                                             , "requestData" .= object [ "inputName" .= inputName
                                                                                                                       , "inputSettings" .= inputSettings
                                                                                                                       ]
                                                                                             ]
toRequestData (SetInputSettings Nothing (Just inputUuid) inputSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetInputSettings"
                                                                                                    , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                              , "inputSettings" .= inputSettings
                                                                                                                              , "overlay" .= overlay
                                                                                                                              ]
                                                                                                    ]
toRequestData (SetInputSettings Nothing (Just inputUuid) inputSettings Nothing) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetInputSettings"
                                                                                             , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                       , "inputSettings" .= inputSettings
                                                                                                                       ]
                                                                                             ]
toRequestData (GetInputMute (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                   , "requestType" .= JSON.String "GetInputMute"
                                                                   , "requestData" .= object [ "inputName" .= inputName ]
                                                                   ]
toRequestData (GetInputMute Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                   , "requestType" .= JSON.String "GetInputMute"
                                                                   , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                   ]
toRequestData (SetInputMute (Just inputName) Nothing inputMuted) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "SetInputMute"
                                                                              , "requestData" .= object [ "inputName" .= inputName
                                                                                                        , "inputMuted" .= inputMuted
                                                                                                        ]
                                                                              ]
toRequestData (SetInputMute Nothing (Just inputUuid) inputMuted) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "SetInputMute"
                                                                              , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                        , "inputMuted" .= inputMuted
                                                                                                        ]
                                                                              ]
toRequestData (ToggleInputMute (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "ToggleInputMute"
                                                                      , "requestData" .= object [ "inputName" .= inputName ]
                                                                      ]
toRequestData (ToggleInputMute Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "ToggleInputMute"
                                                                      , "requestUuid" .= object [ "inputName" .= inputUuid ]
                                                                      ]
toRequestData (GetInputVolume (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "GetInputVolume"
                                                                     , "requestData" .= object [ "inputName" .= inputName ]
                                                                     ]
toRequestData (GetInputVolume Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "GetInputVolume"
                                                                     , "requestUuid" .= object [ "inputName" .= inputUuid ]
                                                                     ]
toRequestData (SetInputVolume (Just inputName) Nothing (Just (inputVolumeMul, inputVolumeDb))) rid = object [ "requestId" .= rid
                                                                                                            , "requestType" .= JSON.String "SetInputVolume"
                                                                                                            , "requestData" .= object [ "inputName" .= inputName
                                                                                                                                      , "inputVolumeMul" .= inputVolumeMul
                                                                                                                                      , "inputVolumeDb" .= inputVolumeDb
                                                                                                                                      ]
                                                                                                            ]
toRequestData (SetInputVolume (Just inputName) Nothing Nothing) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "SetInputVolume"
                                                                             , "requestData" .= object [ "inputName" .= inputName ]
                                                                             ]
toRequestData (SetInputVolume Nothing (Just inputUuid) (Just (inputVolumeMul, inputVolumeDb))) rid = object [ "requestId" .= rid
                                                                                                            , "requestType" .= JSON.String "SetInputVolume"
                                                                                                            , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                                      , "inputVolumeMul" .= inputVolumeMul
                                                                                                                                      , "inputVolumeDb" .= inputVolumeDb
                                                                                                                                      ]
                                                                                                            ]
toRequestData (SetInputVolume Nothing (Just inputUuid) Nothing) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "SetInputVolume"
                                                                             , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                             ]
toRequestData (GetInputAudioBalance (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                           , "requestType" .= JSON.String "GetInputAudioBalance"
                                                                           , "requestData" .= object [ "inputName" .= inputName ]
                                                                           ]
toRequestData (GetInputAudioBalance Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                           , "requestType" .= JSON.String "GetInputAudioBalance"
                                                                           , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                           ]
toRequestData (SetInputAudioBalance (Just inputName) Nothing inputAudioBalance) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetInputAudioBalance"
                                                                                             , "requestData" .= object [ "inputName" .= inputName
                                                                                                                       , "inputAudioBalance" .= inputAudioBalance
                                                                                                                       ]
                                                                                             ]
toRequestData (SetInputAudioBalance Nothing (Just inputUuid) inputAudioBalance) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetInputAudioBalance"
                                                                                             , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                       , "inputAudioBalance" .= inputAudioBalance
                                                                                                                       ]
                                                                                             ]
toRequestData (GetInputAudioSyncOffset (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "GetInputAudioSyncOffset"
                                                                              , "requestData" .= object [ "inputName" .= inputName ]
                                                                              ]
toRequestData (GetInputAudioSyncOffset Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "GetInputAudioSyncOffset"
                                                                              , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                              ]
toRequestData (SetInputAudioSyncOffset (Just inputName) Nothing inputAudioSyncOffset) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetInputAudioSyncOffset"
                                                                                                   , "requestData" .= object [ "inputName" .= inputName
                                                                                                                             , "inputAudioSyncOffset" .= inputAudioSyncOffset
                                                                                                                             ]
                                                                                                   ]
toRequestData (SetInputAudioSyncOffset Nothing (Just inputUuid) inputAudioSyncOffset) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetInputAudioSyncOffset"
                                                                                                   , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                             , "inputAudioSyncOffset" .= inputAudioSyncOffset
                                                                                                                             ]
                                                                                                   ]
toRequestData (GetInputAudioMonitorType (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                               , "requestType" .= JSON.String "GetInputAudioMonitorType"
                                                                               , "requestData" .= object [ "inputName" .= inputName ]
                                                                               ]
toRequestData (GetInputAudioMonitorType Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                               , "requestType" .= JSON.String "GetInputAudioMonitorType"
                                                                               , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                               ]
toRequestData (SetInputAudioMonitorType (Just inputName) Nothing monitorType) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetInputAudioMonitorType"
                                                                                           , "requestData" .= object [ "inputName" .= inputName
                                                                                                                     , "monitorType" .= monitorType
                                                                                                                     ]
                                                                                           ]
toRequestData (SetInputAudioMonitorType Nothing (Just inputUuid) monitorType) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetInputAudioMonitorType"
                                                                                           , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                     , "monitorType" .= monitorType
                                                                                                                     ]
                                                                                           ]
toRequestData (GetInputAudioTracks (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "GetInputAudioTracks"
                                                                          , "requestData" .= object [ "inputName" .= inputName ]
                                                                          ]
toRequestData (GetInputAudioTracks Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "GetInputAudioTracks"
                                                                          , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                          ]
toRequestData (SetInputAudioTracks (Just inputName) Nothing inputAudioTracks) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetInputAudioTracks"
                                                                                           , "requestData" .= object [ "inputName" .= inputName
                                                                                                                     , "inputAudioTracks" .= inputAudioTracks
                                                                                                                     ]
                                                                                           ]
toRequestData (SetInputAudioTracks Nothing (Just inputUuid) inputAudioTracks) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetInputAudioTracks"
                                                                                           , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                     , "inputAudioTracks" .= inputAudioTracks
                                                                                                                     ]
                                                                                           ]
toRequestData (GetInputPropertiesListPropertyItems (Just inputName) Nothing propertyName) rid = object [ "requestId" .= rid
                                                                                                       , "requestType" .= JSON.String "GetInputPropertiesListPropertyItems"
                                                                                                       , "requestData" .= object [ "inputName" .= inputName
                                                                                                                                 , "propertyName" .= propertyName
                                                                                                                                 ]
                                                                                                       ]
toRequestData (GetInputPropertiesListPropertyItems Nothing (Just inputUuid) propertyName) rid = object [ "requestId" .= rid
                                                                                                       , "requestType" .= JSON.String "GetInputPropertiesListPropertyItems"
                                                                                                       , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                                 , "propertyName" .= propertyName
                                                                                                                                 ]
                                                                                                       ]
toRequestData (PressInputPropertiesButton (Just inputName) Nothing propertyName) rid = object [ "requestId" .= rid
                                                                                              , "requestType" .= JSON.String "PressInputPropertiesButton"
                                                                                              , "requestData" .= object [ "inputName" .= inputName
                                                                                                                        , "propertyName" .= propertyName
                                                                                                                        ]
                                                                                              ]
toRequestData (PressInputPropertiesButton Nothing (Just inputUuid) propertyName) rid = object [ "requestId" .= rid
                                                                                              , "requestType" .= JSON.String "PressInputPropertiesButton"
                                                                                              , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                        , "propertyName" .= propertyName
                                                                                                                        ]
                                                                                              ]
toRequestData GetTransitionKindList rid = object [ "requestId" .= rid
                                                 , "requestType" .= JSON.String "GetTransitionKindList"
                                                 ]
toRequestData GetSceneTransitionList rid = object [ "requestId" .= rid
                                                  , "requestType" .= JSON.String "GetSceneTransitionList"
                                                  ]
toRequestData GetCurrentSceneTransition rid = object [ "requestId" .= rid
                                                     , "requestType" .= JSON.String "GetCurrentSceneTransition"
                                                     ]
toRequestData (SetCurrentSceneTransition transitionName) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "SetCurrentSceneTransition"
                                                                      , "requestData" .= object [ "transitionName" .= transitionName ]
                                                                      ]
toRequestData (SetCurrentSceneTransitionDuration transitionDuration) rid = object [ "requestId" .= rid
                                                                                  , "requestType" .= JSON.String "SetCurrentSceneTransitionDuration"
                                                                                  , "requestData" .= object [ "transitionDuration" .= transitionDuration ]
                                                                                  ]
toRequestData (SetCurrentSceneTransitionSettings transitionSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                 , "requestType" .= JSON.String "SetCurrentSceneTransitionSettings"
                                                                                                 , "requestData" .= object [ "transitionSettings" .= transitionSettings
                                                                                                                           , "overlay" .= overlay
                                                                                                                           ]
                                                                                                 ]
toRequestData (SetCurrentSceneTransitionSettings transitionSettings Nothing) rid = object [ "requestId" .= rid
                                                                                                 , "requestType" .= JSON.String "SetCurrentSceneTransitionSettings"
                                                                                                 , "requestData" .= object [ "transitionSettings" .= transitionSettings ]
                                                                                                 ]
toRequestData GetCurrentSceneTransitionCursor rid = object [ "requestId" .= rid
                                                           , "requestType" .= JSON.String "GetCurrentSceneTransitionCursor"
                                                           ]
toRequestData TriggerStudioModeTransition rid = object [ "requestId" .= rid
                                                       , "requestType" .= JSON.String "TriggerStudioModeTransition"
                                                       ]
toRequestData (SetTBarPosition position (Just release)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetTBarPosition"
                                                                     , "requestData" .= object [ "position" .= position
                                                                                               , "release" .= release
                                                                                               ]
                                                                     ]
toRequestData (SetTBarPosition position Nothing) rid = object [ "requestId" .= rid
                                                              , "requestType" .= JSON.String "SetTBarPosition"
                                                              , "requestData" .= object [ "position" .= position ]
                                                              ]
toRequestData GetSourceFilterKindList rid = object [ "requestId" .= rid
                                                   , "requestType" .= JSON.String "GetSourceFilterKindList"
                                                   ]
toRequestData (GetSourceFilterList (Just sourceName) Nothing) rid = object [ "requestId" .= rid
                                                                           , "requestType" .= JSON.String "GetSourceFilterList"
                                                                           , "requestData" .= object [ "sourceName" .= sourceName ]
                                                                           ]
toRequestData (GetSourceFilterList Nothing (Just sourceUuid)) rid = object [ "requestId" .= rid
                                                                           , "requestType" .= JSON.String "GetSourceFilterList"
                                                                           , "requestData" .= object [ "sourceUuid" .= sourceUuid ]
                                                                           ]
toRequestData (GetSourceFilterDefaultSettings filterKind) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetSourceFilterDefaultSettings"
                                                                       , "requestData" .= object [ "filterKind" .= filterKind ]
                                                                       ]
toRequestData (CreateSourceFilter (Just sourceName) Nothing filterName filterKind filterSettings) rid = object [ "requestId" .= rid
                                                                                                               , "requestType" .= JSON.String "CreateSourceFilter"
                                                                                                               , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                         , "filterName" .= filterName
                                                                                                                                         , "filterKind" .= filterKind
                                                                                                                                         , "filterSettings" .= filterSettings
                                                                                                                                         ]
                                                                                                               ]
toRequestData (CreateSourceFilter Nothing (Just sourceUuid) filterName filterKind filterSettings) rid = object [ "requestId" .= rid
                                                                                                               , "requestType" .= JSON.String "CreateSourceFilter"
                                                                                                               , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                         , "filterName" .= filterName
                                                                                                                                         , "filterKind" .= filterKind
                                                                                                                                         , "filterSettings" .= filterSettings
                                                                                                                                         ]
                                                                                                               ]
toRequestData (RemoveSourceFilter (Just sourceName) Nothing filterName) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "RemoveSourceFilter"
                                                                                     , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                               , "filterName" .= filterName
                                                                                                               ]
                                                                                     ]
toRequestData (RemoveSourceFilter Nothing (Just sourceUuid) filterName) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "RemoveSourceFilter"
                                                                                     , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                               , "filterName" .= filterName
                                                                                                               ]
                                                                                     ]
toRequestData (SetSourceFilterName (Just sourceName) Nothing filterName newFilterName) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetSourceFilterName"
                                                                                                    , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                              , "filterName" .= filterName
                                                                                                                              , "newFilterName" .= newFilterName
                                                                                                                              ]
                                                                                                    ]
toRequestData (SetSourceFilterName Nothing (Just sourceUuid) filterName newFilterName) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetSourceFilterName"
                                                                                                    , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                              , "filterName" .= filterName
                                                                                                                              , "newFilterName" .= newFilterName
                                                                                                                              ]
                                                                                                    ]
toRequestData (GetSourceFilter (Just sourceName) Nothing filterName) rid = object [ "requestId" .= rid
                                                                                  , "requestType" .= JSON.String "GetSourceFilter"
                                                                                  , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                            , "filterName" .= filterName
                                                                                                            ]
                                                                                  ]
toRequestData (GetSourceFilter Nothing (Just sourceUuid) filterName) rid = object [ "requestId" .= rid
                                                                                  , "requestType" .= JSON.String "GetSourceFilter"
                                                                                  , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                            , "filterName" .= filterName
                                                                                                            ]
                                                                                  ]
toRequestData (SetSourceFilterIndex (Just sourceName) Nothing filterName filterIndex) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSourceFilterIndex"
                                                                                                   , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                             , "filterName" .= filterName
                                                                                                                             , "filterIndex" .= filterIndex
                                                                                                                             ]
                                                                                                   ]
toRequestData (SetSourceFilterIndex Nothing (Just sourceUuid) filterName filterIndex) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSourceFilterIndex"
                                                                                                   , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                             , "filterName" .= filterName
                                                                                                                             , "filterIndex" .= filterIndex
                                                                                                                             ]
                                                                                                   ]
toRequestData (SetSourceFilterSettings (Just sourceName) Nothing filterName filterSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                                       , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                                       , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                 , "filterName" .= filterName
                                                                                                                                                 , "filterSettings" .= filterSettings
                                                                                                                                                 , "overlay" .= overlay
                                                                                                                                                 ]
                                                                                                                       ]
toRequestData (SetSourceFilterSettings (Just sourceName) Nothing filterName filterSettings Nothing) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                                , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                          , "filterName" .= filterName
                                                                                                                                          , "filterSettings" .= filterSettings
                                                                                                                                          ]
                                                                                                                ]
toRequestData (SetSourceFilterSettings Nothing (Just sourceUuid) filterName filterSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                                       , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                                       , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                                 , "filterName" .= filterName
                                                                                                                                                 , "filterSettings" .= filterSettings
                                                                                                                                                 , "overlay" .= overlay
                                                                                                                                                 ]
                                                                                                                       ]
toRequestData (SetSourceFilterSettings Nothing (Just sourceUuid) filterName filterSettings Nothing) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                                , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                          , "filterName" .= filterName
                                                                                                                                          , "filterSettings" .= filterSettings
                                                                                                                                          ]
                                                                                                                ]
toRequestData (SetSourceFilterEnabled (Just sourceName) Nothing filterName filterEnabled) rid = object [ "requestId" .= rid
                                                                                                      , "requestType" .= JSON.String "SetSourceFilterEnabled"
                                                                                                      , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                , "filterName" .= filterName
                                                                                                                                , "filterEnabled" .= filterEnabled
                                                                                                                                ]
                                                                                                      ]
toRequestData (SetSourceFilterEnabled Nothing (Just sourceUuid) filterName filterEnabled) rid = object [ "requestId" .= rid
                                                                                                      , "requestType" .= JSON.String "SetSourceFilterEnabled"
                                                                                                      , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                , "filterName" .= filterName
                                                                                                                                , "filterEnabled" .= filterEnabled
                                                                                                                                ]
                                                                                                      ]
toRequestData (GetSceneItemList (Just sceneName) Nothing) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetSceneItemList"
                                                                       , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                       ]
toRequestData (GetSceneItemList Nothing (Just sceneUuid)) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetSceneItemList"
                                                                       , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                       ]
toRequestData (GetGroupSceneItemList (Just sceneName) Nothing) rid = object [ "requestId" .= rid
                                                                            , "requestType" .= JSON.String "GetGroupSceneItemList"
                                                                            , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                            ]
toRequestData (GetGroupSceneItemList Nothing (Just sceneUuid)) rid = object [ "requestId" .= rid
                                                                            , "requestType" .= JSON.String "GetGroupSceneItemList"
                                                                            , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                            ]
toRequestData (GetSceneItemId (Just sceneName) Nothing sourceName (Just searchOffset)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "GetSceneItemId"
                                                                                                    , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                              , "sourceName" .= sourceName
                                                                                                                              , "searchOffset" .= searchOffset
                                                                                                                              ]
                                                                                                    ]
toRequestData (GetSceneItemId (Just sceneName) Nothing sourceName Nothing) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "GetSceneItemId"
                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                  , "sourceName" .= sourceName
                                                                                                                  ]
                                                                                        ]
toRequestData (GetSceneItemId Nothing (Just sceneUuid) sourceName (Just searchOffset)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "GetSceneItemId"
                                                                                                    , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                              , "sourceName" .= sourceName
                                                                                                                              , "searchOffset" .= searchOffset
                                                                                                                              ]
                                                                                                    ]
toRequestData (GetSceneItemId Nothing (Just sceneUuid) sourceName Nothing) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "GetSceneItemId"
                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                  , "sourceName" .= sourceName
                                                                                                                  ]
                                                                                        ]
toRequestData (GetSceneItemSource (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "GetSceneItemSource"
                                                                                     , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                               , "sceneItemId" .= sceneItemId
                                                                                                               ]
                                                                                     ]
toRequestData (GetSceneItemSource Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "GetSceneItemSource"
                                                                                     , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                               , "sceneItemId" .= sceneItemId
                                                                                                               ]
                                                                                     ]
toRequestData (CreateSceneItem (Just sceneName) Nothing (Just sourceName) Nothing (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                                  , "sourceName" .= sourceName
                                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (CreateSceneItem (Just sceneName) Nothing (Just sourceName) Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                  , "sourceName" .= sourceName
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem (Just sceneName) Nothing Nothing (Just sourceUuid) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (CreateSceneItem (Just sceneName) Nothing Nothing (Just sourceUuid) Nothing) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem Nothing (Just sceneUuid) (Just sourceName) Nothing (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                                  , "sourceName" .= sourceName
                                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (CreateSceneItem Nothing (Just sceneUuid) (Just sourceName) Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                  , "sourceName" .= sourceName
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem Nothing (Just sceneUuid) Nothing (Just sourceUuid) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (CreateSceneItem Nothing (Just sceneUuid) Nothing (Just sourceUuid) Nothing) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                                  ]
                                                                                                        ]
toRequestData (RemoveSceneItem (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                  , "requestType" .= JSON.String "RemoveSceneItem"
                                                                                  , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                            , "sceneItemId" .= sceneItemId
                                                                                                            ]
                                                                                  ]
toRequestData (RemoveSceneItem Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                  , "requestType" .= JSON.String "RemoveSceneItem"
                                                                                  , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                            , "sceneItemId" .= sceneItemId
                                                                                                            ]
                                                                                  ]
toRequestData (DuplicateSceneItem (Just sceneName) Nothing sceneItemId (Just destinationSceneName) Nothing) rid = object [ "requestId" .= rid
                                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                                         , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                                   , "destinationSceneName" .= destinationSceneName
                                                                                                                                                   ]
                                                                                                                         ]
toRequestData (DuplicateSceneItem (Just sceneName) Nothing sceneItemId Nothing (Just destinationSceneUuid)) rid = object [ "requestId" .= rid
                                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                                         , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                                   , "destinationSceneUuid" .= destinationSceneUuid
                                                                                                                                                   ]
                                                                                                                         ]
toRequestData (DuplicateSceneItem Nothing (Just sceneUuid) sceneItemId (Just destinationSceneName) Nothing) rid = object [ "requestId" .= rid
                                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                                         , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                                   , "destinationSceneName" .= destinationSceneName
                                                                                                                                                   ]
                                                                                                                         ]
toRequestData (DuplicateSceneItem Nothing (Just sceneUuid) sceneItemId Nothing (Just destinationSceneUuid)) rid = object [ "requestId" .= rid
                                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                                         , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                                   , "destinationSceneUuid" .= destinationSceneUuid
                                                                                                                                                   ]
                                                                                                                         ]
toRequestData (GetSceneItemTransform (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "GetSceneItemTransform"
                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                  , "sceneItemId" .= sceneItemId
                                                                                                                  ]
                                                                                        ]
toRequestData (GetSceneItemTransform Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "GetSceneItemTransform"
                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                  , "sceneItemId" .= sceneItemId
                                                                                                                  ]
                                                                                        ]
toRequestData (SetSceneItemTransform (Just sceneName) Nothing sceneItemId sceneItemTransform) rid = object [ "requestId" .= rid
                                                                                                           , "requestType" .= JSON.String "SetSceneItemTransform"
                                                                                                           , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                     , "sceneItemId" .= sceneItemId
                                                                                                                                     , "sceneItemTransform" .= sceneItemTransform
                                                                                                                                     ]
                                                                                                           ]
toRequestData (SetSceneItemTransform Nothing (Just sceneUuid) sceneItemId sceneItemTransform) rid = object [ "requestId" .= rid
                                                                                                           , "requestType" .= JSON.String "SetSceneItemTransform"
                                                                                                           , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                     , "sceneItemId" .= sceneItemId
                                                                                                                                     , "sceneItemTransform" .= sceneItemTransform
                                                                                                                                     ]
                                                                                                           ]
toRequestData (GetSceneItemEnabled (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                      , "requestType" .= JSON.String "GetSceneItemEnabled"
                                                                                      , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                , "sceneItemId" .= sceneItemId
                                                                                                                ]
                                                                                      ]
toRequestData (GetSceneItemEnabled Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                      , "requestType" .= JSON.String "GetSceneItemEnabled"
                                                                                      , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                , "sceneItemId" .= sceneItemId
                                                                                                                ]
                                                                                      ]
toRequestData (SetSceneItemEnabled (Just sceneName) Nothing sceneItemId sceneItemEnabled) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "SetSceneItemEnabled"
                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                  , "sceneItemId" .= sceneItemId
                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                  ]
                                                                                                        ]
toRequestData (SetSceneItemEnabled Nothing (Just sceneUuid) sceneItemId sceneItemEnabled) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "SetSceneItemEnabled"
                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                  , "sceneItemId" .= sceneItemId
                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                  ]
                                                                                                        ]
toRequestData (GetSceneItemLocked (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "GetSceneItemLocked"
                                                                                     , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                               , "sceneItemId" .= sceneItemId
                                                                                                               ]
                                                                                     ]
toRequestData (GetSceneItemLocked Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "GetSceneItemLocked"
                                                                                     , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                               , "sceneItemId" .= sceneItemId
                                                                                                               ]
                                                                                     ]
toRequestData (SetSceneItemLocked (Just sceneName) Nothing sceneItemId sceneItemLocked) rid = object [ "requestId" .= rid
                                                                                                     , "requestType" .= JSON.String "SetSceneItemLocked"
                                                                                                     , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                               , "sceneItemId" .= sceneItemId
                                                                                                                               , "sceneItemLocked" .= sceneItemLocked
                                                                                                                               ]
                                                                                                     ]
toRequestData (SetSceneItemLocked Nothing (Just sceneUuid) sceneItemId sceneItemLocked) rid = object [ "requestId" .= rid
                                                                                                     , "requestType" .= JSON.String "SetSceneItemLocked"
                                                                                                     , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                               , "sceneItemId" .= sceneItemId
                                                                                                                               , "sceneItemLocked" .= sceneItemLocked
                                                                                                                               ]
                                                                                                     ]
toRequestData (GetSceneItemIndex (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                    , "requestType" .= JSON.String "GetSceneItemIndex"
                                                                                    , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                              , "sceneItemId" .= sceneItemId
                                                                                                              ]
                                                                                    ]
toRequestData (GetSceneItemIndex Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                    , "requestType" .= JSON.String "GetSceneItemIndex"
                                                                                    , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                              , "sceneItemId" .= sceneItemId
                                                                                                              ]
                                                                                    ]
toRequestData (SetSceneItemIndex (Just sceneName) Nothing sceneItemId sceneItemIndex) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSceneItemIndex"
                                                                                                   , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                             , "sceneItemId" .= sceneItemId
                                                                                                                             , "sceneItemIndex" .= sceneItemIndex
                                                                                                                             ]
                                                                                                   ]
toRequestData (SetSceneItemIndex Nothing (Just sceneUuid) sceneItemId sceneItemIndex) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSceneItemIndex"
                                                                                                   , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                             , "sceneItemId" .= sceneItemId
                                                                                                                             , "sceneItemIndex" .= sceneItemIndex
                                                                                                                             ]
                                                                                                   ]
toRequestData (GetSceneItemBlendMode (Just sceneName) Nothing sceneItemId) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "GetSceneItemBlendMode"
                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                  , "sceneItemId" .= sceneItemId
                                                                                                                  ]
                                                                                        ]
toRequestData (GetSceneItemBlendMode Nothing (Just sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "GetSceneItemBlendMode"
                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                  , "sceneItemId" .= sceneItemId
                                                                                                                  ]
                                                                                        ]
toRequestData (SetSceneItemBlendMode (Just sceneName) Nothing sceneItemId sceneItemBlendMode) rid = object [ "requestId" .= rid
                                                                                                           , "requestType" .= JSON.String "SetSceneItemBlendMode"
                                                                                                           , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                     , "sceneItemId" .= sceneItemId
                                                                                                                                     , "sceneItemBlendMode" .= sceneItemBlendMode
                                                                                                                                     ]
                                                                                                           ]
toRequestData (SetSceneItemBlendMode Nothing (Just sceneUuid) sceneItemId sceneItemBlendMode) rid = object [ "requestId" .= rid
                                                                                                           , "requestType" .= JSON.String "SetSceneItemBlendMode"
                                                                                                           , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                     , "sceneItemId" .= sceneItemId
                                                                                                                                     , "sceneItemBlendMode" .= sceneItemBlendMode
                                                                                                                                     ]
                                                                                                           ]
toRequestData GetVirtualCamStatus rid = object [ "requestId" .= rid
                                               , "requestType" .= JSON.String "GetVirtualCamStatus"
                                               ]
toRequestData ToggleVirtualCam rid = object [ "requestId" .= rid
                                            , "requestType" .= JSON.String "ToggleVirtualCam"
                                            ]
toRequestData StartVirtualCam rid = object [ "requestId" .= rid
                                           , "requestType" .= JSON.String "StartVirtualCam"
                                           ]
toRequestData StopVirtualCam rid = object [ "requestId" .= rid
                                          , "requestType" .= JSON.String "StopVirtualCam"
                                          ]
toRequestData GetReplayBufferStatus rid = object [ "requestId" .= rid
                                                 , "requestType" .= JSON.String "GetReplayBufferStatus"
                                                 ]
toRequestData ToggleReplayBuffer rid = object [ "requestId" .= rid
                                              , "requestType" .= JSON.String "ToggleReplayBuffer"
                                              ]
toRequestData StartReplayBuffer rid = object [ "requestId" .= rid
                                             , "requestType" .= JSON.String "StartReplayBuffer"
                                             ]
toRequestData StopReplayBuffer rid = object [ "requestId" .= rid
                                            , "requestType" .= JSON.String "StopReplayBuffer"
                                            ]
toRequestData SaveReplayBuffer rid = object [ "requestId" .= rid
                                            , "requestType" .= JSON.String "SaveReplayBuffer"
                                            ]
toRequestData GetLastReplayBufferReplay rid = object [ "requestId" .= rid
                                                     , "requestType" .= JSON.String "GetLastReplayBufferReplay"
                                                     ]
toRequestData GetOutputList rid = object [ "requestId" .= rid
                                         , "requestType" .= JSON.String "GetOutputList"
                                         ]
toRequestData (GetOutputStatus outputName) rid = object [ "requestId" .= rid
                                                        , "requestType" .= JSON.String "GetOutputStatus"
                                                        , "requestData" .= object [ "outputName" .= outputName ]
                                                        ]
toRequestData (ToggleOutput outputName) rid = object [ "requestId" .= rid
                                                     , "requestType" .= JSON.String "ToggleOutput"
                                                     , "requestData" .= object [ "outputName" .= outputName ]
                                                     ]
toRequestData (StartOutput outputName) rid = object [ "requestId" .= rid
                                                    , "requestType" .= JSON.String "StartOutput"
                                                    , "requestData" .= object [ "outputName" .= outputName ]
                                                    ]
toRequestData (StopOutput outputName) rid = object [ "requestId" .= rid
                                                   , "requestType" .= JSON.String "StopOutput"
                                                   , "requestData" .= object [ "outputName" .= outputName ]
                                                   ]
toRequestData (GetOutputSettings outputName) rid = object [ "requestId" .= rid
                                                          , "requestType" .= JSON.String "GetOutputSettings"
                                                          , "requestData" .= object [ "outputName" .= outputName ]
                                                          ]
toRequestData (SetOutputSettings outputName outputSettings) rid = object [ "requestId" .= rid
                                                                         , "requestType" .= JSON.String "SetOutputSettings"
                                                                         , "requestData" .= object [ "outputName" .= outputName
                                                                                                   , "outputSettings" .= outputSettings
                                                                                                   ]
                                                                         ]
toRequestData GetStreamStatus rid = object [ "requestId" .= rid
                                           , "requestType" .= JSON.String "GetStreamStatus"
                                           ]
toRequestData ToggleStream rid = object [ "requestId" .= rid
                                        , "requestType" .= JSON.String "ToggleStream"
                                        ]
toRequestData StartStream rid = object [ "requestId" .= rid
                                       , "requestType" .= JSON.String "StartStream"
                                       ]
toRequestData StopStream rid = object [ "requestId" .= rid
                                      , "requestType" .= JSON.String "StopStream"
                                      ]
toRequestData (SendStreamCaption captionText) rid = object [ "requestId" .= rid
                                                           , "requestType" .= JSON.String "SendStreamCaption"
                                                           , "requestData" .= object [ "captionText" .= captionText ]
                                                           ]
toRequestData GetRecordStatus rid = object [ "requestId" .= rid
                                           , "requestType" .= JSON.String "GetRecordStatus"
                                           ]
toRequestData ToggleRecord rid = object [ "requestId" .= rid
                                        , "requestType" .= JSON.String "ToggleRecord"
                                        ]
toRequestData StartRecord rid = object [ "requestId" .= rid
                                       , "requestType" .= JSON.String "StartRecord"
                                       ]
toRequestData StopRecord rid = object [ "requestId" .= rid
                                      , "requestType" .= JSON.String "StopRecord"
                                      ]
toRequestData ToggleRecordPause rid = object [ "requestId" .= rid
                                             , "requestType" .= JSON.String "ToggleRecordPause"
                                             ]
toRequestData PauseRecord rid = object [ "requestId" .= rid
                                       , "requestType" .= JSON.String "PauseRecord"
                                       ]
toRequestData ResumeRecord rid = object [ "requestId" .= rid
                                        , "requestType" .= JSON.String "ResumeRecord"
                                        ]
toRequestData (GetMediaInputStatus (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "GetMediaInputStatus"
                                                                          , "requestData" .= object [ "inputName" .= inputName ]
                                                                          ]
toRequestData (GetMediaInputStatus Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "GetMediaInputStatus"
                                                                          , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                          ]
toRequestData (SetMediaInputCursor (Just inputName) Nothing mediaCursor) rid = object [ "requestId" .= rid
                                                                                       , "requestType" .= JSON.String "SetMediaInputCursor"
                                                                                       , "requestData" .= object [ "inputName" .= inputName
                                                                                                                 , "mediaCursor" .= mediaCursor
                                                                                                                 ]
                                                                                       ]
toRequestData (SetMediaInputCursor Nothing (Just inputUuid) mediaCursor) rid = object [ "requestId" .= rid
                                                                                       , "requestType" .= JSON.String "SetMediaInputCursor"
                                                                                       , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                 , "mediaCursor" .= mediaCursor
                                                                                                                 ]
                                                                                       ]
toRequestData (OffsetMediaInputCursor (Just inputName) Nothing mediaCursorOffset) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "OffsetMediaInputCursor"
                                                                                               , "requestData" .= object [ "inputName" .= inputName
                                                                                                                         , "mediaCursorOffset" .= mediaCursorOffset
                                                                                                                         ]
                                                                                               ]
toRequestData (OffsetMediaInputCursor Nothing (Just inputUuid) mediaCursorOffset) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "OffsetMediaInputCursor"
                                                                                               , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                         , "mediaCursorOffset" .= mediaCursorOffset
                                                                                                                         ]
                                                                                               ]
toRequestData (TriggerMediaInputAction (Just inputName) Nothing mediaAction) rid = object [ "requestId" .= rid
                                                                                          , "requestType" .= JSON.String "TriggerMediaInputAction"
                                                                                          , "requestData" .= object [ "inputName" .= inputName
                                                                                                                    , "mediaAction" .= mediaAction
                                                                                                                    ]
                                                                                          ]
toRequestData (TriggerMediaInputAction Nothing (Just inputUuid) mediaAction) rid = object [ "requestId" .= rid
                                                                                          , "requestType" .= JSON.String "TriggerMediaInputAction"
                                                                                          , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                    , "mediaAction" .= mediaAction
                                                                                                                    ]
                                                                                          ]
toRequestData GetStudioModeEnabled rid = object [ "requestId" .= rid
                                                , "requestType" .= JSON.String "GetStudioModeEnabled"
                                                ]
toRequestData (SetStudioModeEnabled studioModeEnabled) rid = object [ "requestId" .= rid
                                                                    , "requestType" .= JSON.String "SetStudioModeEnabled"
                                                                    , "requestData" .= object [ "studioModeEnabled" .= studioModeEnabled ]
                                                                    ]
toRequestData (OpenInputPropertiesDialog (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "OpenInputPropertiesDialog"
                                                                                , "requestData" .= object [ "inputName" .= inputName ]
                                                                                ]
toRequestData (OpenInputPropertiesDialog Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "OpenInputPropertiesDialog"
                                                                                , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                                ]
toRequestData (OpenInputFiltersDialog (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "OpenInputFiltersDialog"
                                                                             , "requestData" .= object [ "inputName" .= inputName ]
                                                                             ]
toRequestData (OpenInputFiltersDialog Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "OpenInputFiltersDialog"
                                                                             , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                             ]
toRequestData (OpenInputInteractDialog (Just inputName) Nothing) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "OpenInputInteractDialog"
                                                                              , "requestData" .= object [ "inputName" .= inputName ]
                                                                              ]
toRequestData (OpenInputInteractDialog Nothing (Just inputUuid)) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "OpenInputInteractDialog"
                                                                              , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                              ]
toRequestData GetMonitorList rid = object [ "requestId" .= rid
                                          , "requestType" .= JSON.String "GetMonitorList"
                                          ]
toRequestData (OpenVideoMixProjector videoMixType (Just monitorIndex) (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                             , "requestType" .= JSON.String "OpenVideoMixProjector"
                                                                                                             , "requestData" .= object [ "videoMixType" .= videoMixType
                                                                                                                                       , "monitorIndex" .= monitorIndex
                                                                                                                                       , "projectorGeometry" .= projectorGeometry
                                                                                                                                       ]
                                                                                                             ]
toRequestData (OpenVideoMixProjector videoMixType (Just monitorIndex) Nothing) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "OpenVideoMixProjector"
                                                                                            , "requestData" .= object [ "videoMixType" .= videoMixType
                                                                                                                      , "monitorIndex" .= monitorIndex
                                                                                                                      ]
                                                                                            ]
toRequestData (OpenVideoMixProjector videoMixType Nothing (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                 , "requestType" .= JSON.String "OpenVideoMixProjector"
                                                                                                 , "requestData" .= object [ "videoMixType" .= videoMixType
                                                                                                                           , "projectorGeometry" .= projectorGeometry
                                                                                                                           ]
                                                                                                 ]
toRequestData (OpenVideoMixProjector videoMixType Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "OpenVideoMixProjector"
                                                                                , "requestData" .= object [ "videoMixType" .= videoMixType ]
                                                                                ]
toRequestData (OpenSourceProjector (Just sourceName) Nothing (Just monitorIndex) (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                                        , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                  , "monitorIndex" .= monitorIndex
                                                                                                                                                  , "projectorGeometry" .= projectorGeometry
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (OpenSourceProjector (Just sourceName) Nothing (Just monitorIndex) Nothing) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                                        , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                                  , "monitorIndex" .= monitorIndex
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (OpenSourceProjector (Just sourceName) Nothing Nothing (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                            , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                            , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                      , "projectorGeometry" .= projectorGeometry
                                                                                                                                      ]
                                                                                                            ]
toRequestData (OpenSourceProjector (Just sourceName) Nothing Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                           , "requestData" .= object [ "sourceName" .= sourceName ]
                                                                                           ]
toRequestData (OpenSourceProjector Nothing (Just sourceUuid) (Just monitorIndex) (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                                        , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                                  , "monitorIndex" .= monitorIndex
                                                                                                                                                  , "projectorGeometry" .= projectorGeometry
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (OpenSourceProjector Nothing (Just sourceUuid) (Just monitorIndex) Nothing) rid = object [ "requestId" .= rid
                                                                                                                        , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                                        , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                                  , "monitorIndex" .= monitorIndex
                                                                                                                                                  ]
                                                                                                                        ]
toRequestData (OpenSourceProjector Nothing (Just sourceUuid) Nothing (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                            , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                            , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                      , "projectorGeometry" .= projectorGeometry
                                                                                                                                      ]
                                                                                                            ]
toRequestData (OpenSourceProjector Nothing (Just sourceUuid) Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                           , "requestData" .= object [ "sourceUuid" .= sourceUuid ]
                                                                                           ]

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
                   | Request { requestData :: RequestData
                             , requestId :: String
                             }
                   | RequestBatch { batchRequestId :: String
                                  , haltOnFailure :: Maybe Bool
                                  , executionType :: Maybe Integer
                                  , requests :: [JSON.Value]
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
               , "d" .= toRequestData requestData requestId
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
