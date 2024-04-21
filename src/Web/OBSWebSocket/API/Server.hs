{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import Data.Functor  ( (<&>) )

type OBSMessageParser = Object -> JSON.Types.Parser Message

data AuthChallenge = AuthChallenge { challenge :: String
                                   , salt :: String
                                   } deriving ( Show )
instance FromJSON AuthChallenge where
    parseJSON = withObject "Authentication" $ \o -> do
        challenge <- o .: "challenge"
        salt <- o .: "salt"
        return AuthChallenge{..}

data RequestStatus = RequestStatus { requestType :: String
                                   , requestId :: String
                                   , requestStatus :: Object
                                   } deriving ( Show )
instance FromJSON RequestStatus where
    parseJSON = withObject "RequestStatus" $ \o -> do
        requestType <- o .: "requestType"
        requestId <- o .: "requestId"
        requestStatus <- o .: "requestStatus"
        return RequestStatus{..}

data MonitorType = NoMonitor | MonitorOnly | MonitorAndOutput
    deriving ( Show )

data OutputState = OutputStarted | OutputStarting | OutputStopped | OutputStopping
    deriving ( Show )

data MediaAction = MediaNone | MediaPlay | MediaPause | MediaStop | MediaRestart | MediaNext | MediaPrevious
    deriving ( Show )

data VolumeMeter = VolumeMeter { inputName :: String
                               , inputUuid :: Maybe String
                               , levels :: [(Float, Float, Float)]
                               } deriving ( Show )
instance FromJSON VolumeMeter where
    parseJSON = withObject "InputLevels" $ \o -> do
        inputName <- o .: "inputName"
        inputUuid <- o .:? "inputUuid"
        levels <- o .: "inputLevelsMul"
        return VolumeMeter{..}

data ServerEvent = ExitStarted -- OBS has begun the shutdown process.
                 | VendorEvent { vendorName :: String -- An event has been emitted from a vendor.
                               , eventType :: String -- A vendor is a unique name registered by a third-party plugin or script, which allows for custom requests and events to be added to obs-websocket. If a plugin or script implements vendor requests or events, documentation is expected to be provided with them.
                               , eventData :: Object
                               }
                 | CustomEvent { eventData :: Object } -- Custom event emitted by BroadcastCustomEvent.
                 | CurrentSceneCollectionChanging String -- The current scene collection has begun changing.
                                                         -- Note: We recommend using this event to trigger a pause of all polling requests, as performing any requests during a scene collection change is considered undefined behavior and can cause crashes!
                 | CurrentSceneCollectionChanged String -- The current scene collection has changed.
                                                        -- Note: If polling has been paused during CurrentSceneCollectionChanging, this is the cue to restart polling.
                 | SceneCollectionListChanged [String] -- The scene collection list has changed.
                 | CurrentProfileChanging String -- The current profile has begun changing.
                 | CurrentProfileChanged String -- The current profile has changed.
                 | ProfileListChanged [String] -- The profile list has changed.
                 | SceneCreated { sceneName :: String -- A new scene has been created.
                                , sceneUuid :: Maybe String
                                , isGroup :: Bool
                                }
                 | SceneRemoved { sceneName :: String -- A scene has been removed.
                                , sceneUuid :: Maybe String
                                , isGroup :: Bool
                                }
                 | SceneNameChanged { sceneName :: String
                                    , sceneUuid :: Maybe String -- The name of a scene has changed.
                                    , oldSceneName :: String
                                    }
                 | CurrentProgramSceneChanged { sceneName :: String -- The current program scene has changed.
                                              , sceneUuid :: Maybe String
                                              }
                 | CurrentPreviewSceneChanged { sceneName :: String -- The current preview scene has changed.
                                              , sceneUuid :: Maybe String
                                              }
                 | SceneListChanged [Object] -- The list of scenes has changed.
                 | InputCreated { inputName :: String -- An input has been created.
                                , inputUuid :: Maybe String
                                , inputKind :: String
                                , unversionedInputKind :: String
                                , inputSettings :: Object
                                , defaultInputSettings :: Object
                                }
                 | InputRemoved { inputName :: String -- An input has been removed.
                                , inputUuid :: Maybe String
                                }
                 | InputNameChanged { inputUuid :: Maybe String -- The name of an input has changed.
                                    , oldInputName :: String
                                    , inputName :: String
                                    }
                 | InputSettingsChanged { inputName :: String -- An input's settings have changed (been updated).
                                        , inputUuid :: Maybe String -- Note: On some inputs, changing values in the properties dialog will cause an immediate update. Pressing the "Cancel" button will revert the settings, resulting in another event being fired.
                                        , inputSettings :: Object
                                        }
                 | InputActiveStateChanged { inputName :: String -- An input's active state has changed.
                                           , inputUuid :: Maybe String -- When an input is active, it means it's being shown by the program feed.
                                           , videoActive :: Bool
                                           }
                 | InputShowStateChanged { inputName :: String -- An input's show state has changed.
                                         , inputUuid :: Maybe String -- When an input is showing, it means it's being shown by the preview or a dialog.
                                         , videoShowing :: Bool
                                         }
                 | InputMuteStateChanged { inputName :: String -- An input's mute state has changed.
                                         , inputUuid :: Maybe String
                                         , inputMuted :: Bool
                                         }
                 | InputVolumeChanged { inputName :: String -- An input's volume level has changed.
                                      , inputUuid :: Maybe String
                                      , inputVolumeMul :: Float
                                      , inputVolumeDb :: Float
                                      }
                 | InputAudioBalanceChanged { inputName :: String -- The audio balance value of an input has changed.
                                            , inputUuid :: Maybe String
                                            , inputAudioBalance :: Float
                                            }
                 | InputAudioSyncOffsetChanged { inputName :: String -- The sync offset of an input has changed.
                                               , inputUuid :: Maybe String
                                               , inputAudioSyncOffset :: Float
                                               }
                 | InputAudioTracksChanged { inputName :: String --The audio tracks of an input have changed.
                                           , inputUuid :: Maybe String
                                           , inputAudioTracks :: Object
                                           }
                 | InputAudioMonitorTypeChanged { inputName :: String -- The monitor type of an input has changed.
                                                , inputUuid :: Maybe String
                                                , monitorType :: MonitorType
                                                }
                 | InputVolumeMeters [VolumeMeter] -- A high-volume event providing volume levels of all active inputs every 50 milliseconds.
                 | CurrentSceneTransitionChanged { transitionName :: String -- The current scene transition has changed.
                                                 , transitionUuid :: Maybe String
                                                 }
                 | CurrentSceneTransitionDurationChanged Integer -- The current scene transition duration has changed.
                 | SceneTransitionStarted { transitionName :: String -- A scene transition has started.
                                          , transitionUuid :: Maybe String
                                          }
                 | SceneTransitionEnded { transitionName :: String -- A scene transition has completed fully.
                                        , transitionUuid :: Maybe String -- Note: Does not appear to trigger when the transition is interrupted by the user.
                                        }
                 | SceneTransitionVideoEnded { transitionName :: String -- A scene transition's video has completed fully.
                                             , transitionUuid :: Maybe String -- Useful for stinger transitions to tell when the video actually ends. SceneTransitionEnded only signifies the cut point, not the completion of transition playback.
                                             } -- Note: Appears to be called by every transition, regardless of relevance.
                 | SourceFilterListReindexed { sourceName :: String -- A source's filter list has been reindexed.
                                             , filters :: [Object]
                                             }
                 | SourceFilterCreated { sourceName :: String -- A filter has been added to a source.
                                       , filterName :: String
                                       , filterKind :: String
                                       , filterIndex :: Integer
                                       , filterSettings :: Object
                                       , defaultFilterSettings :: Object
                                       }
                 | SourceFilterRemoved { sourceName :: String -- A filter has been removed from a source.
                                       , filterName :: String
                                       }
                 | SourceFilterNameChanged { sourceName :: String -- The name of a source filter has changed.
                                           , oldFilterName :: String
                                           , filterName :: String
                                           }
                 | SourceFilterSettingsChanged { sourceName :: String -- An source filter's settings have changed (been updated).
                                               , filterName :: String
                                               , filterSettings :: Object
                                               }
                 | SourceFilterEnableStateChanged { sourceName :: String -- A source filter's enable state has changed.
                                                  , filterName :: String
                                                  , filterEnabled :: Bool
                                                  }
                 | SceneItemCreated { sceneName :: String -- A scene item has been created.
                                    , sceneUuid :: Maybe String
                                    , sourceName :: String
                                    , sourceUuid :: Maybe String
                                    , sceneItemId :: Integer
                                    , sceneItemIndex :: Integer
                                    }
                 | SceneItemRemoved { sceneName :: String -- A scene item has been removed.
                                    , sceneUuid :: Maybe String -- This event is not emitted when the scene the item is in is removed.
                                    , sourceName :: String
                                    , sourceUuid :: Maybe String
                                    , sceneItemId :: Integer
                                    }
                 | SceneItemListReindexed { sceneName :: String -- A scene's item list has been reindexed.
                                          , sceneUuid :: Maybe String
                                          , sceneItems :: [Object]
                                          }
                 | SceneItemEnableStateChanged { sceneName :: String -- A scene item's enable state has changed.
                                               , sceneUuid :: Maybe String
                                               , sceneItemId :: Integer
                                               , sceneItemEnabled :: Bool
                                               }
                 | SceneItemLockStateChanged { sceneName :: String -- A scene item's lock state has changed.
                                             , sceneUuid :: Maybe String
                                             , sceneItemId :: Integer
                                             , sceneItemLocked :: Bool
                                             }
                 | SceneItemSelected { sceneName :: String -- A scene item has been selected in the Ui.
                                     , sceneUuid :: Maybe String
                                     , sceneItemId :: Integer
                                     }
                 | SceneItemTransformChanged { sceneName :: String -- The transform/crop of a scene item has changed.
                                             , sceneUuid :: Maybe String
                                             , sceneItemId :: Integer
                                             , sceneItemTransform :: Object
                                             }
                 | StreamStateChanged { outputActive :: Bool -- The state of the stream output has changed.
                                      , outputState :: OutputState
                                      }
                 | RecordStateChanged { outputActive :: Bool -- The state of the record output has changed.
                                      , outputState :: OutputState
                                      , outputPath :: Maybe FilePath
                                      }
                 | ReplayBufferStateChanged { outputActive :: Bool -- The state of the replay buffer output has changed.
                                            , outputState :: OutputState
                                            }
                 | VirtualcamStateChanged { outputActive :: Bool -- The state of the virtualcam output has changed.
                                          , outputState :: OutputState
                                          }
                 | ReplayBufferSaved FilePath -- The replay buffer has been saved.
                 | MediaInputPlaybackStarted { inputName :: String -- A media input has started playing.
                                             , inputUuid :: Maybe String
                                             }
                 | MediaInputPlaybackEnded { inputName :: String -- A media input has finished playing.
                                           , inputUuid :: Maybe String
                                           }
                 | MediaInputActionTriggered { inputName :: String -- An action has been performed on an input.
                                             , inputUuid :: Maybe String
                                             , mediaAction :: MediaAction
                                             }
                 | StudioModeStateChanged Bool -- Studio mode has been enabled or disabled.
                 | ScreenshotSaved FilePath -- A screenshot has been saved via Settings -> Hotkeys -> Screenshot ONLY.  Applications using Get/SaveSourceScreenshot must implement a CustomEvent.
                 deriving ( Show )
instance FromJSON ServerEvent where
    parseJSON = withObject "Event" $ \d -> do
        (e :: String) <- d .: "eventType"
        case e of
          "ExitStarted" -> return ExitStarted
          "VendorEvent" -> do
              o <- d .: "eventData"
              vendorName <- o .: "vendorName"
              eventType <- o .: "eventType"
              eventData <- o .: "eventData"
              return VendorEvent{..}
          "CustomEvent" -> do
              o <- d .: "eventData"
              eventData <- o .: "eventData"
              return CustomEvent{..}
          "CurrentSceneCollectionChanging" -> do
              o <- d .: "eventData"
              sceneCollectionName <- o .: "sceneCollectionName"
              return $ CurrentSceneCollectionChanging sceneCollectionName
          "CurrentSceneCollectionChanged" -> do
              o <- d .: "eventData"
              sceneCollectionName <- o .: "sceneCollectionName"
              return $ CurrentSceneCollectionChanged sceneCollectionName
          "SceneCollectionListChanged" -> do
              o <- d .: "eventData"
              sceneCollections <- o .: "sceneCollections"
              return $ SceneCollectionListChanged sceneCollections
          "CurrentProfileChanging" -> do
              o <- d .: "eventData"
              profileName <- o .: "profileName"
              return $ CurrentProfileChanging profileName
          "CurrentProfileChanged" -> do
              o <- d .: "eventData"
              profileName <- o .: "profileName"
              return $ CurrentProfileChanged profileName
          "ProfileListChanged" -> do
              o <- d .: "eventData"
              profiles <- o .: "profiles"
              return $ ProfileListChanged profiles
          "SceneCreated" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              isGroup <- o .: "isGroup"
              return SceneCreated{..}
          "SceneRemoved" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              isGroup <- o .: "isGroup"
              return SceneRemoved{..}
          "SceneNameChanged" -> do
              o <- d .: "eventData"
              sceneUuid <- o .:? "sceneUuid"
              oldSceneName <- o .: "oldSceneName"
              sceneName <- o .: "sceneName"
              return SceneNameChanged{..}
          "CurrentProgramSceneChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              return CurrentProgramSceneChanged{..}
          "CurrentPreviewSceneChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              return CurrentPreviewSceneChanged{..}
          "SceneListChanged" -> do
              o <- d .: "eventData"
              scenes <- o .: "scenes"
              return $ SceneListChanged scenes
          "InputCreated" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputKind <- o .: "inputKind"
              unversionedInputKind <- o .: "unversionedInputKind"
              inputSettings <- o .: "inputSettings"
              defaultInputSettings <- o .: "defaultInputSettings"
              return InputCreated{..}
          "InputRemoved" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              return InputRemoved{..}
          "InputNameChanged" -> do
              o <- d .: "eventData"
              inputUuid <- o .:? "inputUuid"
              oldInputName <- o .: "oldInputName"
              inputName <- o .: "inputName"
              return InputNameChanged{..}
          "InputSettingsChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputSettings <- o .: "inputSettings"
              return InputSettingsChanged{..}
          "InputActiveStateChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              videoActive <- o .: "videoActive"
              return InputActiveStateChanged{..}
          "InputShowStateChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              videoShowing <- o .: "videoShowing"
              return InputShowStateChanged{..}
          "InputMuteStateChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputMuted <- o .: "inputMuted"
              return InputMuteStateChanged{..}
          "InputVolumeChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputVolumeMul <- o .: "inputVolumeMul"
              inputVolumeDb <- o .: "inputVolumeDb"
              return InputVolumeChanged{..}
          "InputAudioBalanceChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputAudioBalance <- o .: "inputAudioBalance"
              return InputAudioBalanceChanged{..}
          "InputAudioSyncOffsetChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputAudioSyncOffset <- o .: "inputAudioSyncOffset"
              return InputAudioSyncOffsetChanged{..}
          "InputAudioTracksChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputAudioTracks <- o .: "inputAudioTracks"
              return InputAudioTracksChanged{..}
          "InputAudioMonitorTypeChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              (mt :: String) <- o .: "monitorType"
              case mt of
                  "OBS_MONITORING_TYPE_MONITOR_ONLY" -> do
                      let monitorType = MonitorOnly
                      return InputAudioMonitorTypeChanged{..}
                  "OBS_MONITORING_TYPE_MONITOR_AND_OUTPUT" -> do
                      let monitorType = MonitorAndOutput
                      return InputAudioMonitorTypeChanged{..}
                  "OBS_MONITORING_TYPE_NONE" -> do
                      let monitorType = NoMonitor
                      return InputAudioMonitorTypeChanged{..}
                  _ -> mzero
          "InputVolumeMeters" -> do
              o <- d .: "eventData"
              inputs <- o .: "inputs"
              return $ InputVolumeMeters inputs
          "CurrentSceneTransitionChanged" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return CurrentSceneTransitionChanged{..}
          "CurrentSceneTransitionDurationChanged" -> do
              o <- d .: "eventData"
              transitionDuration <- o .: "transitionDuration"
              return $ CurrentSceneTransitionDurationChanged transitionDuration
          "SceneTransitionStarted" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return SceneTransitionStarted{..}
          "SceneTransitionEnded" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return SceneTransitionEnded{..}
          "SceneTransitionVideoEnded" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return SceneTransitionVideoEnded{..}
          "SourceFilterListReindexed" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filters <- o .: "filters"
              return SourceFilterListReindexed{..}
          "SourceFilterCreated" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              filterKind <- o .: "filterKind"
              filterIndex <- o .: "filterIndex"
              filterSettings <- o .: "filterSettings"
              defaultFilterSettings <- o .: "defaultFilterSettings"
              return SourceFilterCreated{..}
          "SourceFilterRemoved" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              return SourceFilterRemoved{..}
          "SourceFilterNameChanged" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              oldFilterName <- o .: "oldFilterName"
              filterName <- o .: "filterName"
              return SourceFilterNameChanged{..}
          "SourceFilterSettingsChanged" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              filterSettings <- o .: "filterSettings"
              return SourceFilterSettingsChanged{..}
          "SourceFilterEnableStateChanged" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              filterEnabled <- o .: "filterEnabled"
              return SourceFilterEnableStateChanged{..}
          "SceneItemCreated" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sourceName <- o .: "sourceName"
              sourceUuid <- o .:? "sourceUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemIndex <- o .: "sceneItemIndex"
              return SceneItemCreated{..}
          "SceneItemRemoved" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sourceName <- o .: "sourceName"
              sourceUuid <- o .:? "sourceUuid"
              sceneItemId <- o .: "sceneItemId"
              return SceneItemRemoved{..}
          "SceneItemListReindexed" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItems <- o .: "sceneItems"
              return SceneItemListReindexed{..}
          "SceneItemEnableStateChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemEnabled <- o .: "sceneItemEnabled"
              return SceneItemEnableStateChanged{..}
          "SceneItemLockStateChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemLocked <- o .: "sceneItemLocked"
              return SceneItemLockStateChanged{..}
          "SceneItemSelected" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              return SceneItemSelected{..}
          "SceneItemTransformChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemTransform <- o .: "sceneItemTransform"
              return SceneItemTransformChanged{..}
          "StreamStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> do
                    let outputState = OutputStarted
                    return StreamStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> do
                    let outputState = OutputStarting
                    return StreamStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> do
                    let outputState = OutputStopped
                    return StreamStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> do
                    let outputState = OutputStopping
                    return StreamStateChanged{..}
                _ -> mzero
          "RecordStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              outputPath <- o .:? "outputPath"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> do
                    let outputState = OutputStarted
                    return RecordStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> do
                    let outputState = OutputStarting
                    return RecordStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> do
                    let outputState = OutputStopped
                    return RecordStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> do
                    let outputState = OutputStopping
                    return RecordStateChanged{..}
                _ -> mzero
          "ReplayBufferStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> do
                    let outputState = OutputStarted
                    return ReplayBufferStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> do
                    let outputState = OutputStarting
                    return ReplayBufferStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> do
                    let outputState = OutputStopped
                    return ReplayBufferStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> do
                    let outputState = OutputStopping
                    return ReplayBufferStateChanged{..}
                _ -> mzero
          "VirtualcamStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> do
                    let outputState = OutputStarted
                    return VirtualcamStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> do
                    let outputState = OutputStarting
                    return VirtualcamStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> do
                    let outputState = OutputStopped
                    return VirtualcamStateChanged{..}
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> do
                    let outputState = OutputStopping
                    return VirtualcamStateChanged{..}
                _ -> mzero
          "ReplayBufferSaved" -> do
              o <- d .: "eventData"
              savedReplayPath <- o .: "savedReplayPath"
              return $ ReplayBufferSaved savedReplayPath
          "MediaInputPlaybackStarted" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              return MediaInputPlaybackStarted{..}
          "MediaInputPlaybackEnded" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              return MediaInputPlaybackEnded{..}
          "MediaInputActionTriggered" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              (mo :: String) <- o .: "mediaAction"
              case mo of
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_NONE" -> do
                    let mediaAction = MediaNone
                    return MediaInputActionTriggered{..}
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PLAY" -> do
                    let mediaAction = MediaPlay
                    return MediaInputActionTriggered{..}
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PAUSE" -> do
                    let mediaAction = MediaPause
                    return MediaInputActionTriggered{..}
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_STOP" -> do
                    let mediaAction = MediaStop
                    return MediaInputActionTriggered{..}
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_RESTART" -> do
                    let mediaAction = MediaRestart
                    return MediaInputActionTriggered{..}
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_NEXT" -> do
                    let mediaAction = MediaNext
                    return MediaInputActionTriggered{..}
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PREVIOUS" -> do
                    let mediaAction = MediaPrevious
                    return MediaInputActionTriggered{..}
                _ -> mzero
          "StudioModeStateChanged" -> do
              o <- d .: "eventData"
              studioModeEnabled <- o .: "studioModeEnabled"
              return $ StudioModeStateChanged studioModeEnabled
          "ScreenshotSaved" -> do
              o <- d .: "eventData"
              savedScreenshotPath <- o .: "savedScreenshotPath"
              return $ ScreenshotSaved savedScreenshotPath
          _ -> mzero

data Message = Hello { obsWebSocketVersion :: String
                     , remoteRPCVersion :: Integer
                     , authenticationChallenge :: Maybe AuthChallenge
                     }
             | Identified { negotiatedRPCVersion :: Integer }
             | Event ServerEvent
             | RequestResponse { status :: RequestStatus
                               , responseData :: Maybe Object
                               }
             | RequestBatchResponse { batchResponseId :: String
                                    , results :: [Object]
                                    } deriving ( Show )
instance FromJSON Message where
    parseJSON = withObject "OBSMessage" $ \o -> do
        o .: "op" >>= \(opcode :: Integer) ->
            case opcode of
              0 -> o .: "d" >>= hello
              2 -> o .: "d" >>= identified
              5 -> (o .: "d") <&> Event
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
    negotiatedRPCVersion <- o .: "negotiatedRpcVersion"
    return Identified{..}

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
