{-# OPTIONS_GHC -fmax-pmcheck-models=200 #-}

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

data SleepLength = SleepFrames Integer
                 | SleepMillis Integer
                 deriving ( Show )

data Realm = Global | Profile
    deriving ( Show )
instance ToJSON Realm where
    toJSON Global = JSON.String "OBS_WEBSOCKET_DATA_REALM_GLOBAL"
    toJSON Profile = JSON.String "OBS_WEBSOCKET_DATA_REALM_PROFILE"

data NameUuid = Name String
              | Uuid String
              deriving ( Show )

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

type Numerator = Integer
type Denominator = Integer
type FPSSettings = (Numerator, Denominator)

type Width = Integer
type Height = Integer
type CanvasSettings = (Width, Height)
type OutputSettings = (Width, Height)

-- Ergonomics notes: fix before release
-- Types shared between client and server should be extracted
-- Some 2-tuples probably want to be extracted as more explicit types
-- Boolean overlay + a JSON.Value can be extracted as a type
-- Projector Geometry is a Qt object submitted in a base64 encoded format - no reason the developer should have to deal with that, this needs a type
data RequestData = GetVersion
                 | GetStats
                 | BroadcastCustomEvent JSON.Value
                 | CallVendorRequest String String (Maybe JSON.Value)
                 | GetHotkeyList
                 | TriggerHotkeyByName String (Maybe String)
                 | Sleep SleepLength
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
                 | SetVideoSettingsF FPSSettings
                 | SetVideoSettingsC CanvasSettings
                 | SetVideoSettingsO OutputSettings
                 | SetVideoSettingsFC FPSSettings CanvasSettings
                 | SetVideoSettingsFO FPSSettings OutputSettings
                 | SetVideoSettingsCO CanvasSettings OutputSettings
                 | SetVideoSettings FPSSettings CanvasSettings  OutputSettings
                 | GetStreamServiceSettings
                 | SetStreamServiceSettings String JSON.Value
                 | GetRecordDirectory
                 | SetRecordDirectory FilePath
                 | GetSourceActive NameUuid
                 | GetSourceScreenshot String String (Maybe (Integer, Integer)) (Maybe Integer)
                 | SaveSourceScreenshot String String FilePath (Maybe (Integer, Integer)) (Maybe Integer)
                 | GetSceneList
                 | GetGroupList
                 | GetCurrentProgramScene
                 | SetCurrentProgramScene NameUuid
                 | GetCurrentPreviewScene
                 | SetCurrentPreviewScene NameUuid
                 | CreateScene String
                 | RemoveScene NameUuid
                 | SetSceneName NameUuid String
                 | GetSceneSceneTransitionOverride String
                 | SetSceneSceneTransitionOverride String (Maybe String) (Maybe Integer)
                 | GetInputList (Maybe String)
                 | GetInputKindList (Maybe Bool)
                 | GetSpecialInputs
                 | CreateInput String String String (Maybe JSON.Value) (Maybe Bool)
                 | RemoveInput NameUuid
                 | SetInputName NameUuid String
                 | GetInputDefaultSettings String
                 | GetInputSettings NameUuid
                 | SetInputSettings NameUuid JSON.Value (Maybe Bool)
                 | GetInputMute NameUuid
                 | SetInputMute NameUuid Bool
                 | ToggleInputMute NameUuid
                 | GetInputVolume NameUuid
                 | SetInputVolume NameUuid (Maybe (Integer, Integer))
                 | GetInputAudioBalance NameUuid
                 | SetInputAudioBalance NameUuid Float
                 | GetInputAudioSyncOffset NameUuid
                 | SetInputAudioSyncOffset NameUuid Integer
                 | GetInputAudioMonitorType NameUuid
                 | SetInputAudioMonitorType NameUuid MonitorType
                 | GetInputAudioTracks NameUuid
                 | SetInputAudioTracks NameUuid JSON.Value
                 | GetInputPropertiesListPropertyItems NameUuid String
                 | PressInputPropertiesButton NameUuid String
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
                 | GetSourceFilterList NameUuid
                 | GetSourceFilterDefaultSettings String
                 | CreateSourceFilter NameUuid String String JSON.Value
                 | RemoveSourceFilter NameUuid String
                 | SetSourceFilterName NameUuid String String
                 | GetSourceFilter NameUuid String
                 | SetSourceFilterIndex NameUuid String Integer
                 | SetSourceFilterSettings NameUuid String JSON.Value (Maybe Bool)
                 | SetSourceFilterEnabled NameUuid String Bool
                 | GetSceneItemList NameUuid
                 | GetGroupSceneItemList NameUuid
                 | GetSceneItemId NameUuid String (Maybe Integer)
                 | GetSceneItemSource NameUuid Integer
                 | CreateSceneItem NameUuid NameUuid (Maybe Bool)
                 | RemoveSceneItem NameUuid Integer
                 | DuplicateSceneItem NameUuid Integer NameUuid
                 | GetSceneItemTransform NameUuid Integer
                 | SetSceneItemTransform NameUuid Integer JSON.Value
                 | GetSceneItemEnabled NameUuid Integer
                 | SetSceneItemEnabled NameUuid Integer Bool
                 | GetSceneItemLocked NameUuid Integer
                 | SetSceneItemLocked NameUuid Integer Bool
                 | GetSceneItemIndex NameUuid Integer
                 | SetSceneItemIndex NameUuid Integer Integer
                 | GetSceneItemBlendMode NameUuid Integer
                 | SetSceneItemBlendMode NameUuid Integer BlendMode
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
                 | GetMediaInputStatus NameUuid
                 | SetMediaInputCursor NameUuid Integer
                 | OffsetMediaInputCursor NameUuid Integer
                 | TriggerMediaInputAction NameUuid MediaAction
                 | GetStudioModeEnabled
                 | SetStudioModeEnabled Bool
                 | OpenInputPropertiesDialog NameUuid
                 | OpenInputFiltersDialog NameUuid
                 | OpenInputInteractDialog NameUuid
                 | GetMonitorList
                 | OpenVideoMixProjector MixType (Maybe Integer) (Maybe ProjectorGeometry)
                 | OpenSourceProjector NameUuid (Maybe Integer) (Maybe ProjectorGeometry)
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
toRequestData (Sleep (SleepMillis millis)) rid = object [ "requestId" .= rid
                                                        , "requestType" .= JSON.String "Sleep"
                                                        , "requestData" .= object [ "sleepMillis" .= millis ]
                                                        ]
toRequestData (Sleep (SleepFrames frames)) rid = object [ "requestId" .= rid
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
toRequestData (SetVideoSettingsF (fpsNumerator, fpsDenominator)) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "SetVideoSettings"
                                                                              , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                        , "fpsDenominator" .= fpsDenominator
                                                                                                        ]
                                                                              ]
toRequestData (SetVideoSettingsC (baseWidth, baseHeight)) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "SetVideoSettings"
                                                                       , "requestData" .= object [ "baseWidth" .= baseWidth
                                                                                                 , "baseHeight" .= baseHeight
                                                                                                 ]
                                                                       ]
toRequestData (SetVideoSettingsO (outputWidth, outputHeight)) rid = object [ "requestId" .= rid
                                                                           , "requestType" .= JSON.String "SetVideoSettings"
                                                                           , "requestData" .= object [ "outputWidth" .= outputWidth
                                                                                                     , "outputHeight" .= outputHeight
                                                                                                     ]
                                                                           ]
toRequestData (SetVideoSettingsFC (fpsNumerator, fpsDenominator) (baseWidth, baseHeight)) rid = object [ "requestId" .= rid
                                                                                                       , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                       , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                                                 , "fpsDenominator" .= fpsDenominator
                                                                                                                                 , "baseWidth" .= baseWidth
                                                                                                                                 , "baseHeight" .= baseHeight
                                                                                                                                 ]
                                                                                                       ]
toRequestData (SetVideoSettingsFO (fpsNumerator, fpsDenominator) (outputWidth, outputHeight)) rid = object [ "requestId" .= rid
                                                                                                           , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                           , "requestData" .= object [ "fpsNumerator" .= fpsNumerator
                                                                                                                                     , "fpsDenominator" .= fpsDenominator
                                                                                                                                     , "outputWidth" .= outputWidth
                                                                                                                                     , "outputHeight" .= outputHeight
                                                                                                                                     ]
                                                                                                           ]
toRequestData (SetVideoSettingsCO (baseWidth, baseHeight) (outputWidth, outputHeight)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetVideoSettings"
                                                                                                    , "requestData" .= object [ "baseWidth" .= baseWidth
                                                                                                                              , "baseHeight" .= baseHeight
                                                                                                                              , "outputWidth" .= outputWidth
                                                                                                                              , "outputHeight" .= outputHeight
                                                                                                                              ]
                                                                                                    ]

toRequestData (SetVideoSettings (fpsNumerator, fpsDenominator) (baseWidth, baseHeight) (outputWidth, outputHeight)) rid = object [ "requestId" .= rid
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
toRequestData (GetSourceActive (Name sourceName)) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetSourceActive"
                                                               , "requestData" .= object [ "sourceName" .= sourceName ]
                                                               ]
toRequestData (GetSourceActive (Uuid sourceUuid)) rid = object [ "requestId" .= rid
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
toRequestData (SetCurrentProgramScene (Name sceneName)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetCurrentProgramScene"
                                                                     , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                     ]
toRequestData (SetCurrentProgramScene (Uuid sceneUuid)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetCurrentProgramScene"
                                                                     , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                     ]
toRequestData GetCurrentPreviewScene rid = object [ "requestId" .= rid
                                                  , "requestType" .= JSON.String "GetCurrentPreviewScene"
                                                  ]
toRequestData (SetCurrentPreviewScene (Name sceneName)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetCurrentPreviewScene"
                                                                     , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                     ]
toRequestData (SetCurrentPreviewScene (Uuid sceneUuid)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetCurrentPreviewScene"
                                                                     , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                     ]
toRequestData (CreateScene sceneName) rid = object [ "requestId" .= rid
                                                   , "requestType" .= JSON.String "CreateScene"
                                                   , "requestData" .= object [ "sceneName" .= sceneName ]
                                                   ]
toRequestData (RemoveScene (Name sceneName)) rid = object [ "requestId" .= rid
                                                          , "requestType" .= JSON.String "RemoveScene"
                                                          , "requestData" .= object [ "sceneName" .= sceneName ]
                                                          ]
toRequestData (RemoveScene (Uuid sceneUuid)) rid = object [ "requestId" .= rid
                                                          , "requestType" .= JSON.String "RemoveScene"
                                                          , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                          ]
toRequestData (SetSceneName (Name sceneName) newSceneName) rid = object [ "requestId" .= rid
                                                                        , "requestType" .= JSON.String "SetSceneName"
                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                  , "newSceneName" .= newSceneName
                                                                                                  ]
                                                                        ]
toRequestData (SetSceneName (Uuid sceneUuid) newSceneName) rid = object [ "requestId" .= rid
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
toRequestData (RemoveInput (Name inputName)) rid = object [ "requestId" .= rid
                                                          , "requestType" .= JSON.String "RemoveInput"
                                                          , "requestData" .= object [ "inputName" .= inputName ]
                                                          ]
toRequestData (RemoveInput (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                          , "requestType" .= JSON.String "RemoveInput"
                                                          , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                          ]
toRequestData (SetInputName (Name inputName) newInputName) rid = object [ "requestId" .= rid
                                                                        , "requestType" .= JSON.String "SetInputName"
                                                                        , "requestData" .= object [ "inputName" .= inputName
                                                                                                  , "newInputName" .= newInputName
                                                                                                  ]
                                                                        ]
toRequestData (SetInputName (Uuid inputUuid) newInputName) rid = object [ "requestId" .= rid
                                                                        , "requestType" .= JSON.String "SetInputName"
                                                                        , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                  , "newInputName" .= newInputName
                                                                                                  ]
                                                                        ]
toRequestData (GetInputDefaultSettings inputKind) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetInputDefaultSettings"
                                                               , "requestData" .= object [ "inputKind" .= inputKind ]
                                                               ]
toRequestData (GetInputSettings (Name inputName)) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetInputSettings"
                                                               , "requestData" .= object [ "inputName" .= inputName ]
                                                               ]
toRequestData (GetInputSettings (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetInputSettings"
                                                               , "requestUuid" .= object [ "inputName" .= inputUuid ]
                                                               ]
toRequestData (SetInputSettings (Name inputName) inputSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "SetInputSettings"
                                                                                            , "requestData" .= object [ "inputName" .= inputName
                                                                                                                      , "inputSettings" .= inputSettings
                                                                                                                      , "overlay" .= overlay
                                                                                                                      ]
                                                                                            ]
toRequestData (SetInputSettings (Name inputName) inputSettings Nothing) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "SetInputSettings"
                                                                                     , "requestData" .= object [ "inputName" .= inputName
                                                                                                               , "inputSettings" .= inputSettings
                                                                                                               ]
                                                                                     ]
toRequestData (SetInputSettings (Uuid inputUuid) inputSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "SetInputSettings"
                                                                                            , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                      , "inputSettings" .= inputSettings
                                                                                                                      , "overlay" .= overlay
                                                                                                                      ]
                                                                                            ]
toRequestData (SetInputSettings (Uuid inputUuid) inputSettings Nothing) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "SetInputSettings"
                                                                                     , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                               , "inputSettings" .= inputSettings
                                                                                                               ]
                                                                                     ]
toRequestData (GetInputMute (Name inputName)) rid = object [ "requestId" .= rid
                                                           , "requestType" .= JSON.String "GetInputMute"
                                                           , "requestData" .= object [ "inputName" .= inputName ]
                                                           ]
toRequestData (GetInputMute (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                           , "requestType" .= JSON.String "GetInputMute"
                                                           , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                           ]
toRequestData (SetInputMute (Name inputName) inputMuted) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "SetInputMute"
                                                                      , "requestData" .= object [ "inputName" .= inputName
                                                                                                , "inputMuted" .= inputMuted
                                                                                                ]
                                                                      ]
toRequestData (SetInputMute (Uuid inputUuid) inputMuted) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "SetInputMute"
                                                                      , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                , "inputMuted" .= inputMuted
                                                                                                ]
                                                                      ]
toRequestData (ToggleInputMute (Name inputName)) rid = object [ "requestId" .= rid
                                                              , "requestType" .= JSON.String "ToggleInputMute"
                                                              , "requestData" .= object [ "inputName" .= inputName ]
                                                              ]
toRequestData (ToggleInputMute (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                              , "requestType" .= JSON.String "ToggleInputMute"
                                                              , "requestUuid" .= object [ "inputName" .= inputUuid ]
                                                              ]
toRequestData (GetInputVolume (Name inputName)) rid = object [ "requestId" .= rid
                                                             , "requestType" .= JSON.String "GetInputVolume"
                                                             , "requestData" .= object [ "inputName" .= inputName ]
                                                             ]
toRequestData (GetInputVolume (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                             , "requestType" .= JSON.String "GetInputVolume"
                                                             , "requestUuid" .= object [ "inputName" .= inputUuid ]
                                                             ]
toRequestData (SetInputVolume (Name inputName) (Just (inputVolumeMul, inputVolumeDb))) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetInputVolume"
                                                                                                    , "requestData" .= object [ "inputName" .= inputName
                                                                                                                              , "inputVolumeMul" .= inputVolumeMul
                                                                                                                              , "inputVolumeDb" .= inputVolumeDb
                                                                                                                              ]
                                                                                                    ]
toRequestData (SetInputVolume (Name inputName) Nothing) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetInputVolume"
                                                                     , "requestData" .= object [ "inputName" .= inputName ]
                                                                     ]
toRequestData (SetInputVolume (Uuid inputUuid) (Just (inputVolumeMul, inputVolumeDb))) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "SetInputVolume"
                                                                                                    , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                              , "inputVolumeMul" .= inputVolumeMul
                                                                                                                              , "inputVolumeDb" .= inputVolumeDb
                                                                                                                              ]
                                                                                                    ]
toRequestData (SetInputVolume (Uuid inputUuid) Nothing) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "SetInputVolume"
                                                                     , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                     ]
toRequestData (GetInputAudioBalance (Name inputName)) rid = object [ "requestId" .= rid
                                                                   , "requestType" .= JSON.String "GetInputAudioBalance"
                                                                   , "requestData" .= object [ "inputName" .= inputName ]
                                                                   ]
toRequestData (GetInputAudioBalance (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                   , "requestType" .= JSON.String "GetInputAudioBalance"
                                                                   , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                   ]
toRequestData (SetInputAudioBalance (Name inputName) inputAudioBalance) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "SetInputAudioBalance"
                                                                                     , "requestData" .= object [ "inputName" .= inputName
                                                                                                               , "inputAudioBalance" .= inputAudioBalance
                                                                                                               ]
                                                                                     ]
toRequestData (SetInputAudioBalance (Uuid inputUuid) inputAudioBalance) rid = object [ "requestId" .= rid
                                                                                     , "requestType" .= JSON.String "SetInputAudioBalance"
                                                                                     , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                               , "inputAudioBalance" .= inputAudioBalance
                                                                                                               ]
                                                                                     ]
toRequestData (GetInputAudioSyncOffset (Name inputName)) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "GetInputAudioSyncOffset"
                                                                      , "requestData" .= object [ "inputName" .= inputName ]
                                                                      ]
toRequestData (GetInputAudioSyncOffset (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "GetInputAudioSyncOffset"
                                                                      , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                      ]
toRequestData (SetInputAudioSyncOffset (Name inputName) inputAudioSyncOffset) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetInputAudioSyncOffset"
                                                                                           , "requestData" .= object [ "inputName" .= inputName
                                                                                                                     , "inputAudioSyncOffset" .= inputAudioSyncOffset
                                                                                                                     ]
                                                                                           ]
toRequestData (SetInputAudioSyncOffset (Uuid inputUuid) inputAudioSyncOffset) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetInputAudioSyncOffset"
                                                                                           , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                     , "inputAudioSyncOffset" .= inputAudioSyncOffset
                                                                                                                     ]
                                                                                           ]
toRequestData (GetInputAudioMonitorType (Name inputName)) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetInputAudioMonitorType"
                                                                       , "requestData" .= object [ "inputName" .= inputName ]
                                                                       ]
toRequestData (GetInputAudioMonitorType (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetInputAudioMonitorType"
                                                                       , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                       ]
toRequestData (SetInputAudioMonitorType (Name inputName) monitorType) rid = object [ "requestId" .= rid
                                                                                   , "requestType" .= JSON.String "SetInputAudioMonitorType"
                                                                                   , "requestData" .= object [ "inputName" .= inputName
                                                                                                             , "monitorType" .= monitorType
                                                                                                             ]
                                                                                   ]
toRequestData (SetInputAudioMonitorType (Uuid inputUuid) monitorType) rid = object [ "requestId" .= rid
                                                                                   , "requestType" .= JSON.String "SetInputAudioMonitorType"
                                                                                   , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                             , "monitorType" .= monitorType
                                                                                                             ]
                                                                                   ]
toRequestData (GetInputAudioTracks (Name inputName)) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "GetInputAudioTracks"
                                                                  , "requestData" .= object [ "inputName" .= inputName ]
                                                                  ]
toRequestData (GetInputAudioTracks (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "GetInputAudioTracks"
                                                                  , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                  ]
toRequestData (SetInputAudioTracks (Name inputName) inputAudioTracks) rid = object [ "requestId" .= rid
                                                                                   , "requestType" .= JSON.String "SetInputAudioTracks"
                                                                                   , "requestData" .= object [ "inputName" .= inputName
                                                                                                             , "inputAudioTracks" .= inputAudioTracks
                                                                                                             ]
                                                                                   ]
toRequestData (SetInputAudioTracks (Uuid inputUuid) inputAudioTracks) rid = object [ "requestId" .= rid
                                                                                   , "requestType" .= JSON.String "SetInputAudioTracks"
                                                                                   , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                             , "inputAudioTracks" .= inputAudioTracks
                                                                                                             ]
                                                                                   ]
toRequestData (GetInputPropertiesListPropertyItems (Name inputName) propertyName) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "GetInputPropertiesListPropertyItems"
                                                                                               , "requestData" .= object [ "inputName" .= inputName
                                                                                                                         , "propertyName" .= propertyName
                                                                                                                         ]
                                                                                               ]
toRequestData (GetInputPropertiesListPropertyItems (Uuid inputUuid) propertyName) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "GetInputPropertiesListPropertyItems"
                                                                                               , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                         , "propertyName" .= propertyName
                                                                                                                         ]
                                                                                               ]
toRequestData (PressInputPropertiesButton (Name inputName) propertyName) rid = object [ "requestId" .= rid
                                                                                      , "requestType" .= JSON.String "PressInputPropertiesButton"
                                                                                      , "requestData" .= object [ "inputName" .= inputName
                                                                                                                , "propertyName" .= propertyName
                                                                                                                ]
                                                                                      ]
toRequestData (PressInputPropertiesButton (Uuid inputUuid) propertyName) rid = object [ "requestId" .= rid
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
toRequestData (GetSourceFilterList (Name sourceName)) rid = object [ "requestId" .= rid
                                                                   , "requestType" .= JSON.String "GetSourceFilterList"
                                                                   , "requestData" .= object [ "sourceName" .= sourceName ]
                                                                   ]
toRequestData (GetSourceFilterList (Uuid sourceUuid)) rid = object [ "requestId" .= rid
                                                                   , "requestType" .= JSON.String "GetSourceFilterList"
                                                                   , "requestData" .= object [ "sourceUuid" .= sourceUuid ]
                                                                   ]
toRequestData (GetSourceFilterDefaultSettings filterKind) rid = object [ "requestId" .= rid
                                                                       , "requestType" .= JSON.String "GetSourceFilterDefaultSettings"
                                                                       , "requestData" .= object [ "filterKind" .= filterKind ]
                                                                       ]
toRequestData (CreateSourceFilter (Name sourceName) filterName filterKind filterSettings) rid = object [ "requestId" .= rid
                                                                                                       , "requestType" .= JSON.String "CreateSourceFilter"
                                                                                                       , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                 , "filterName" .= filterName
                                                                                                                                 , "filterKind" .= filterKind
                                                                                                                                 , "filterSettings" .= filterSettings
                                                                                                                                 ]
                                                                                                       ]
toRequestData (CreateSourceFilter (Uuid sourceUuid) filterName filterKind filterSettings) rid = object [ "requestId" .= rid
                                                                                                       , "requestType" .= JSON.String "CreateSourceFilter"
                                                                                                       , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                 , "filterName" .= filterName
                                                                                                                                 , "filterKind" .= filterKind
                                                                                                                                 , "filterSettings" .= filterSettings
                                                                                                                                 ]
                                                                                                       ]
toRequestData (RemoveSourceFilter (Name sourceName) filterName) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "RemoveSourceFilter"
                                                                             , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                       , "filterName" .= filterName
                                                                                                       ]
                                                                             ]
toRequestData (RemoveSourceFilter (Uuid sourceUuid) filterName) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "RemoveSourceFilter"
                                                                             , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                       , "filterName" .= filterName
                                                                                                       ]
                                                                             ]
toRequestData (SetSourceFilterName (Name sourceName) filterName newFilterName) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "SetSourceFilterName"
                                                                                            , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                      , "filterName" .= filterName
                                                                                                                      , "newFilterName" .= newFilterName
                                                                                                                      ]
                                                                                            ]
toRequestData (SetSourceFilterName (Uuid sourceUuid) filterName newFilterName) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "SetSourceFilterName"
                                                                                            , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                      , "filterName" .= filterName
                                                                                                                      , "newFilterName" .= newFilterName
                                                                                                                      ]
                                                                                            ]
toRequestData (GetSourceFilter (Name sourceName) filterName) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "GetSourceFilter"
                                                                          , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                    , "filterName" .= filterName
                                                                                                    ]
                                                                          ]
toRequestData (GetSourceFilter (Uuid sourceUuid) filterName) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "GetSourceFilter"
                                                                          , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                    , "filterName" .= filterName
                                                                                                    ]
                                                                          ]
toRequestData (SetSourceFilterIndex (Name sourceName) filterName filterIndex) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetSourceFilterIndex"
                                                                                           , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                     , "filterName" .= filterName
                                                                                                                     , "filterIndex" .= filterIndex
                                                                                                                     ]
                                                                                           ]
toRequestData (SetSourceFilterIndex (Uuid sourceUuid) filterName filterIndex) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetSourceFilterIndex"
                                                                                           , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                     , "filterName" .= filterName
                                                                                                                     , "filterIndex" .= filterIndex
                                                                                                                     ]
                                                                                           ]
toRequestData (SetSourceFilterSettings (Name sourceName) filterName filterSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                                , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                          , "filterName" .= filterName
                                                                                                                                          , "filterSettings" .= filterSettings
                                                                                                                                          , "overlay" .= overlay
                                                                                                                                          ]
                                                                                                                ]
toRequestData (SetSourceFilterSettings (Name sourceName) filterName filterSettings Nothing) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                         , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                   , "filterName" .= filterName
                                                                                                                                   , "filterSettings" .= filterSettings
                                                                                                                                   ]
                                                                                                         ]
toRequestData (SetSourceFilterSettings (Uuid sourceUuid) filterName filterSettings (Just overlay)) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                                , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                          , "filterName" .= filterName
                                                                                                                                          , "filterSettings" .= filterSettings
                                                                                                                                          , "overlay" .= overlay
                                                                                                                                          ]
                                                                                                                ]
toRequestData (SetSourceFilterSettings (Uuid sourceUuid) filterName filterSettings Nothing) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "SetSourceFilterSettings"
                                                                                                         , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                   , "filterName" .= filterName
                                                                                                                                   , "filterSettings" .= filterSettings
                                                                                                                                   ]
                                                                                                         ]
toRequestData (SetSourceFilterEnabled (Name sourceName) filterName filterEnabled) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "SetSourceFilterEnabled"
                                                                                               , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                         , "filterName" .= filterName
                                                                                                                         , "filterEnabled" .= filterEnabled
                                                                                                                         ]
                                                                                               ]
toRequestData (SetSourceFilterEnabled (Uuid sourceUuid) filterName filterEnabled) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "SetSourceFilterEnabled"
                                                                                               , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                         , "filterName" .= filterName
                                                                                                                         , "filterEnabled" .= filterEnabled
                                                                                                                         ]
                                                                                               ]
toRequestData (GetSceneItemList (Name sceneName)) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetSceneItemList"
                                                               , "requestData" .= object [ "sceneName" .= sceneName ]
                                                               ]
toRequestData (GetSceneItemList (Uuid sceneUuid)) rid = object [ "requestId" .= rid
                                                               , "requestType" .= JSON.String "GetSceneItemList"
                                                               , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                               ]
toRequestData (GetGroupSceneItemList (Name sceneName)) rid = object [ "requestId" .= rid
                                                                    , "requestType" .= JSON.String "GetGroupSceneItemList"
                                                                    , "requestData" .= object [ "sceneName" .= sceneName ]
                                                                    ]
toRequestData (GetGroupSceneItemList (Uuid sceneUuid)) rid = object [ "requestId" .= rid
                                                                    , "requestType" .= JSON.String "GetGroupSceneItemList"
                                                                    , "requestData" .= object [ "sceneUuid" .= sceneUuid ]
                                                                    ]
toRequestData (GetSceneItemId (Name sceneName) sourceName (Just searchOffset)) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "GetSceneItemId"
                                                                                            , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                      , "sourceName" .= sourceName
                                                                                                                      , "searchOffset" .= searchOffset
                                                                                                                      ]
                                                                                            ]
toRequestData (GetSceneItemId (Name sceneName) sourceName Nothing) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "GetSceneItemId"
                                                                                , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                          , "sourceName" .= sourceName
                                                                                                          ]
                                                                                ]
toRequestData (GetSceneItemId (Uuid sceneUuid) sourceName (Just searchOffset)) rid = object [ "requestId" .= rid
                                                                                            , "requestType" .= JSON.String "GetSceneItemId"
                                                                                            , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                      , "sourceName" .= sourceName
                                                                                                                      , "searchOffset" .= searchOffset
                                                                                                                      ]
                                                                                            ]
toRequestData (GetSceneItemId (Uuid sceneUuid) sourceName Nothing) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "GetSceneItemId"
                                                                                , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                          , "sourceName" .= sourceName
                                                                                                          ]
                                                                                ]
toRequestData (GetSceneItemSource (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "GetSceneItemSource"
                                                                             , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                       , "sceneItemId" .= sceneItemId
                                                                                                       ]
                                                                             ]
toRequestData (GetSceneItemSource (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "GetSceneItemSource"
                                                                             , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                       , "sceneItemId" .= sceneItemId
                                                                                                       ]
                                                                             ]
toRequestData (CreateSceneItem (Name sceneName) (Name sourceName) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                  , "sourceName" .= sourceName
                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem (Name sceneName) (Name sourceName) Nothing) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                  , "sourceName" .= sourceName
                                                                                                                  ]
                                                                                        ]
toRequestData (CreateSceneItem (Name sceneName) (Uuid sourceUuid) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem (Name sceneName) (Uuid sourceUuid) Nothing) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                        , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                  ]
                                                                                        ]
toRequestData (CreateSceneItem (Uuid sceneUuid) (Name sourceName) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                  , "sourceName" .= sourceName
                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem (Uuid sceneUuid) (Name sourceName) Nothing) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                  , "sourceName" .= sourceName
                                                                                                                  ]
                                                                                        ]
toRequestData (CreateSceneItem (Uuid sceneUuid) (Uuid sourceUuid) (Just sceneItemEnabled)) rid = object [ "requestId" .= rid
                                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                                  , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                                  ]
                                                                                                        ]
toRequestData (CreateSceneItem (Uuid sceneUuid) (Uuid sourceUuid) Nothing) rid = object [ "requestId" .= rid
                                                                                        , "requestType" .= JSON.String "CreateSceneItem"
                                                                                        , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                  , "sourceUuid" .= sourceUuid
                                                                                                                  ]
                                                                                        ]
toRequestData (RemoveSceneItem (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "RemoveSceneItem"
                                                                          , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                    , "sceneItemId" .= sceneItemId
                                                                                                    ]
                                                                          ]
toRequestData (RemoveSceneItem (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                          , "requestType" .= JSON.String "RemoveSceneItem"
                                                                          , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                    , "sceneItemId" .= sceneItemId
                                                                                                    ]
                                                                          ]
toRequestData (DuplicateSceneItem (Name sceneName) sceneItemId (Name destinationSceneName)) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                         , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                   , "destinationSceneName" .= destinationSceneName
                                                                                                                                   ]
                                                                                                         ]
toRequestData (DuplicateSceneItem (Name sceneName) sceneItemId (Uuid destinationSceneUuid)) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                         , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                   , "destinationSceneUuid" .= destinationSceneUuid
                                                                                                                                   ]
                                                                                                         ]
toRequestData (DuplicateSceneItem (Uuid sceneUuid) sceneItemId (Name destinationSceneName)) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                         , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                   , "destinationSceneName" .= destinationSceneName
                                                                                                                                   ]
                                                                                                         ]
toRequestData (DuplicateSceneItem (Uuid sceneUuid) sceneItemId (Uuid destinationSceneUuid)) rid = object [ "requestId" .= rid
                                                                                                         , "requestType" .= JSON.String "DuplicateSceneItem"
                                                                                                         , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                                   , "sceneItemId" .= sceneItemId
                                                                                                                                   , "destinationSceneUuid" .= destinationSceneUuid
                                                                                                                                   ]
                                                                                                         ]
toRequestData (GetSceneItemTransform (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "GetSceneItemTransform"
                                                                                , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                          , "sceneItemId" .= sceneItemId
                                                                                                          ]
                                                                                ]
toRequestData (GetSceneItemTransform (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "GetSceneItemTransform"
                                                                                , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                          , "sceneItemId" .= sceneItemId
                                                                                                          ]
                                                                                ]
toRequestData (SetSceneItemTransform (Name sceneName) sceneItemId sceneItemTransform) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSceneItemTransform"
                                                                                                   , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                             , "sceneItemId" .= sceneItemId
                                                                                                                             , "sceneItemTransform" .= sceneItemTransform
                                                                                                                             ]
                                                                                                   ]
toRequestData (SetSceneItemTransform (Uuid sceneUuid) sceneItemId sceneItemTransform) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSceneItemTransform"
                                                                                                   , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                             , "sceneItemId" .= sceneItemId
                                                                                                                             , "sceneItemTransform" .= sceneItemTransform
                                                                                                                             ]
                                                                                                   ]
toRequestData (GetSceneItemEnabled (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "GetSceneItemEnabled"
                                                                              , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                        , "sceneItemId" .= sceneItemId
                                                                                                        ]
                                                                              ]
toRequestData (GetSceneItemEnabled (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "GetSceneItemEnabled"
                                                                              , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                        , "sceneItemId" .= sceneItemId
                                                                                                        ]
                                                                              ]
toRequestData (SetSceneItemEnabled (Name sceneName) sceneItemId sceneItemEnabled) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "SetSceneItemEnabled"
                                                                                               , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                         , "sceneItemId" .= sceneItemId
                                                                                                                         , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                         ]
                                                                                               ]
toRequestData (SetSceneItemEnabled (Uuid sceneUuid) sceneItemId sceneItemEnabled) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "SetSceneItemEnabled"
                                                                                               , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                         , "sceneItemId" .= sceneItemId
                                                                                                                         , "sceneItemEnabled" .= sceneItemEnabled
                                                                                                                         ]
                                                                                               ]
toRequestData (GetSceneItemLocked (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "GetSceneItemLocked"
                                                                             , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                       , "sceneItemId" .= sceneItemId
                                                                                                       ]
                                                                             ]
toRequestData (GetSceneItemLocked (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                             , "requestType" .= JSON.String "GetSceneItemLocked"
                                                                             , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                       , "sceneItemId" .= sceneItemId
                                                                                                       ]
                                                                             ]
toRequestData (SetSceneItemLocked (Name sceneName) sceneItemId sceneItemLocked) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetSceneItemLocked"
                                                                                             , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                       , "sceneItemId" .= sceneItemId
                                                                                                                       , "sceneItemLocked" .= sceneItemLocked
                                                                                                                       ]
                                                                                             ]
toRequestData (SetSceneItemLocked (Uuid sceneUuid) sceneItemId sceneItemLocked) rid = object [ "requestId" .= rid
                                                                                             , "requestType" .= JSON.String "SetSceneItemLocked"
                                                                                             , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                       , "sceneItemId" .= sceneItemId
                                                                                                                       , "sceneItemLocked" .= sceneItemLocked
                                                                                                                       ]
                                                                                             ]
toRequestData (GetSceneItemIndex (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                            , "requestType" .= JSON.String "GetSceneItemIndex"
                                                                            , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                      , "sceneItemId" .= sceneItemId
                                                                                                      ]
                                                                            ]
toRequestData (GetSceneItemIndex (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                            , "requestType" .= JSON.String "GetSceneItemIndex"
                                                                            , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                      , "sceneItemId" .= sceneItemId
                                                                                                      ]
                                                                            ]
toRequestData (SetSceneItemIndex (Name sceneName) sceneItemId sceneItemIndex) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetSceneItemIndex"
                                                                                           , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                     , "sceneItemId" .= sceneItemId
                                                                                                                     , "sceneItemIndex" .= sceneItemIndex
                                                                                                                     ]
                                                                                           ]
toRequestData (SetSceneItemIndex (Uuid sceneUuid) sceneItemId sceneItemIndex) rid = object [ "requestId" .= rid
                                                                                           , "requestType" .= JSON.String "SetSceneItemIndex"
                                                                                           , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                                     , "sceneItemId" .= sceneItemId
                                                                                                                     , "sceneItemIndex" .= sceneItemIndex
                                                                                                                     ]
                                                                                           ]
toRequestData (GetSceneItemBlendMode (Name sceneName) sceneItemId) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "GetSceneItemBlendMode"
                                                                                , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                          , "sceneItemId" .= sceneItemId
                                                                                                          ]
                                                                                ]
toRequestData (GetSceneItemBlendMode (Uuid sceneUuid) sceneItemId) rid = object [ "requestId" .= rid
                                                                                , "requestType" .= JSON.String "GetSceneItemBlendMode"
                                                                                , "requestData" .= object [ "sceneUuid" .= sceneUuid
                                                                                                          , "sceneItemId" .= sceneItemId
                                                                                                          ]
                                                                                ]
toRequestData (SetSceneItemBlendMode (Name sceneName) sceneItemId sceneItemBlendMode) rid = object [ "requestId" .= rid
                                                                                                   , "requestType" .= JSON.String "SetSceneItemBlendMode"
                                                                                                   , "requestData" .= object [ "sceneName" .= sceneName
                                                                                                                             , "sceneItemId" .= sceneItemId
                                                                                                                             , "sceneItemBlendMode" .= sceneItemBlendMode
                                                                                                                             ]
                                                                                                   ]
toRequestData (SetSceneItemBlendMode (Uuid sceneUuid) sceneItemId sceneItemBlendMode) rid = object [ "requestId" .= rid
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
toRequestData (GetMediaInputStatus (Name inputName)) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "GetMediaInputStatus"
                                                                  , "requestData" .= object [ "inputName" .= inputName ]
                                                                  ]
toRequestData (GetMediaInputStatus (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                  , "requestType" .= JSON.String "GetMediaInputStatus"
                                                                  , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                  ]
toRequestData (SetMediaInputCursor (Name inputName) mediaCursor) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "SetMediaInputCursor"
                                                                              , "requestData" .= object [ "inputName" .= inputName
                                                                                                        , "mediaCursor" .= mediaCursor
                                                                                                        ]
                                                                              ]
toRequestData (SetMediaInputCursor (Uuid inputUuid) mediaCursor) rid = object [ "requestId" .= rid
                                                                              , "requestType" .= JSON.String "SetMediaInputCursor"
                                                                              , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                        , "mediaCursor" .= mediaCursor
                                                                                                        ]
                                                                              ]
toRequestData (OffsetMediaInputCursor (Name inputName) mediaCursorOffset) rid = object [ "requestId" .= rid
                                                                                       , "requestType" .= JSON.String "OffsetMediaInputCursor"
                                                                                       , "requestData" .= object [ "inputName" .= inputName
                                                                                                                 , "mediaCursorOffset" .= mediaCursorOffset
                                                                                                                 ]
                                                                                       ]
toRequestData (OffsetMediaInputCursor (Uuid inputUuid) mediaCursorOffset) rid = object [ "requestId" .= rid
                                                                                       , "requestType" .= JSON.String "OffsetMediaInputCursor"
                                                                                       , "requestData" .= object [ "inputUuid" .= inputUuid
                                                                                                                 , "mediaCursorOffset" .= mediaCursorOffset
                                                                                                                 ]
                                                                                       ]
toRequestData (TriggerMediaInputAction (Name inputName) mediaAction) rid = object [ "requestId" .= rid
                                                                                  , "requestType" .= JSON.String "TriggerMediaInputAction"
                                                                                  , "requestData" .= object [ "inputName" .= inputName
                                                                                                            , "mediaAction" .= mediaAction
                                                                                                            ]
                                                                                  ]
toRequestData (TriggerMediaInputAction (Uuid inputUuid) mediaAction) rid = object [ "requestId" .= rid
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
toRequestData (OpenInputPropertiesDialog (Name inputName)) rid = object [ "requestId" .= rid
                                                                        , "requestType" .= JSON.String "OpenInputPropertiesDialog"
                                                                        , "requestData" .= object [ "inputName" .= inputName ]
                                                                        ]
toRequestData (OpenInputPropertiesDialog (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                        , "requestType" .= JSON.String "OpenInputPropertiesDialog"
                                                                        , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                        ]
toRequestData (OpenInputFiltersDialog (Name inputName)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "OpenInputFiltersDialog"
                                                                     , "requestData" .= object [ "inputName" .= inputName ]
                                                                     ]
toRequestData (OpenInputFiltersDialog (Uuid inputUuid)) rid = object [ "requestId" .= rid
                                                                     , "requestType" .= JSON.String "OpenInputFiltersDialog"
                                                                     , "requestData" .= object [ "inputUuid" .= inputUuid ]
                                                                     ]
toRequestData (OpenInputInteractDialog (Name inputName)) rid = object [ "requestId" .= rid
                                                                      , "requestType" .= JSON.String "OpenInputInteractDialog"
                                                                      , "requestData" .= object [ "inputName" .= inputName ]
                                                                      ]
toRequestData (OpenInputInteractDialog (Uuid inputUuid)) rid = object [ "requestId" .= rid
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
toRequestData (OpenSourceProjector (Name sourceName) (Just monitorIndex) (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                                , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                                          , "monitorIndex" .= monitorIndex
                                                                                                                                          , "projectorGeometry" .= projectorGeometry
                                                                                                                                          ]
                                                                                                                ]
toRequestData (OpenSourceProjector (Name sourceName) (Just monitorIndex) Nothing) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                               , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                         , "monitorIndex" .= monitorIndex
                                                                                                                         ]
                                                                                               ]
toRequestData (OpenSourceProjector (Name sourceName) Nothing (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                    , "requestData" .= object [ "sourceName" .= sourceName
                                                                                                                              , "projectorGeometry" .= projectorGeometry
                                                                                                                              ]
                                                                                                    ]
toRequestData (OpenSourceProjector (Name sourceName) Nothing Nothing) rid = object [ "requestId" .= rid
                                                                                   , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                   , "requestData" .= object [ "sourceName" .= sourceName ]
                                                                                   ]
toRequestData (OpenSourceProjector (Uuid sourceUuid) (Just monitorIndex) (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                                , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                                , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                                          , "monitorIndex" .= monitorIndex
                                                                                                                                          , "projectorGeometry" .= projectorGeometry
                                                                                                                                          ]
                                                                                                                ]
toRequestData (OpenSourceProjector (Uuid sourceUuid) (Just monitorIndex) Nothing) rid = object [ "requestId" .= rid
                                                                                               , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                               , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                         , "monitorIndex" .= monitorIndex
                                                                                                                         ]
                                                                                               ]
toRequestData (OpenSourceProjector (Uuid sourceUuid) Nothing (Just projectorGeometry)) rid = object [ "requestId" .= rid
                                                                                                    , "requestType" .= JSON.String "OpenSourceProjector"
                                                                                                    , "requestData" .= object [ "sourceUuid" .= sourceUuid
                                                                                                                              , "projectorGeometry" .= projectorGeometry
                                                                                                                              ]
                                                                                                    ]
toRequestData (OpenSourceProjector (Uuid sourceUuid) Nothing Nothing) rid = object [ "requestId" .= rid
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

data ClientMessage = Identify Integer [EventType] (Maybe String)
                   | Reidentify [EventType]
                   | Request String RequestData
                   | RequestBatch String Bool (Maybe Integer) [ClientMessage]
                   deriving ( Show )

instance ToJSON ClientMessage where
    toJSON (Identify clientRPCVersion eventSubscriptions authenticationRequest) =
        object [ "op" .= (1 :: Integer)
               , "d" .= object [ "rpcVersion" .= clientRPCVersion
                               , "eventSubscriptions" .= eventMask eventSubscriptions
                               , "authentication" .= authenticationRequest
                               ]
               ]
    toJSON (Reidentify newEventSubscriptions) =
        object [ "op" .= (3 :: Integer)
               , "d" .= object [ "eventSubscriptions" .= eventMask newEventSubscriptions
                               ]
               ]
    toJSON (Request requestId requestData) =
        object [ "op" .= (6 :: Integer)
               , "d" .= toRequestData requestData requestId
               ]
    toJSON (RequestBatch requestId haltOnFailure executionType requests) =
        object [ "op" .= (8 :: Integer)
               , "d" .= object [ "requestId" .= requestId
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
