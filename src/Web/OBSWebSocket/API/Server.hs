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

-- GetVersion
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetVersion\",\"responseData\":{\"availableRequests\":[\"OpenSourceProjector\",\"OpenInputInteractDialog\",\"OpenInputFiltersDialog\",\"OpenInputPropertiesDialog\",\"OffsetMediaInputCursor\",\"GetMediaInputStatus\",\"ResumeRecord\",\"StopRecord\",\"SetInputAudioSyncOffset\",\"GetSceneSceneTransitionOverride\",\"SetInputSettings\",\"GetInputDefaultSettings\",\"StopOutput\",\"StopStream\",\"GetInputSettings\",\"SetCurrentSceneTransitionDuration\",\"SetInputName\",\"CreateInput\",\"TriggerHotkeyByKeySequence\",\"SetSceneSceneTransitionOverride\",\"SetSceneName\",\"RemoveScene\",\"GetInputList\",\"SetCurrentPreviewScene\",\"BroadcastCustomEvent\",\"ToggleRecordPause\",\"GetVirtualCamStatus\",\"SetCurrentSceneTransitionSettings\",\"PauseRecord\",\"GetSceneItemTransform\",\"GetCurrentProgramScene\",\"SetCurrentSceneTransition\",\"GetSceneItemPrivateSettings\",\"SetSourceFilterSettings\",\"SetInputVolume\",\"GetStreamServiceSettings\",\"GetCurrentSceneTransition\",\"SaveReplayBuffer\",\"SetSourcePrivateSettings\",\"Sleep\",\"StartRecord\",\"StartOutput\",\"StopReplayBuffer\",\"SetStudioModeEnabled\",\"GetHotkeyList\",\"GetSceneItemList\",\"GetStats\",\"GetRecordStatus\",\"ToggleReplayBuffer\",\"GetCurrentPreviewScene\",\"RemoveInput\",\"DuplicateSceneItem\",\"SendStreamCaption\",\"SetCurrentSceneCollection\",\"GetPersistentData\",\"CallVendorRequest\",\"SaveSourceScreenshot\",\"SetInputAudioBalance\",\"GetSceneCollectionList\",\"GetSourceActive\",\"GetGroupList\",\"GetVersion\",\"CreateProfile\",\"GetSourcePrivateSettings\",\"GetProfileParameter\",\"GetSourceScreenshot\",\"SetProfileParameter\",\"OpenVideoMixProjector\",\"GetInputPropertiesListPropertyItems\",\"GetStudioModeEnabled\",\"GetGroupSceneItemList\",\"GetSceneItemEnabled\",\"RemoveSourceFilter\",\"TriggerHotkeyByName\",\"SetVideoSettings\",\"SetMediaInputCursor\",\"SetSourceFilterIndex\",\"SetStreamServiceSettings\",\"SetInputAudioTracks\",\"GetSceneItemId\",\"GetInputAudioBalance\",\"GetRecordDirectory\",\"GetReplayBufferStatus\",\"GetStreamStatus\",\"GetInputMute\",\"SetCurrentProfile\",\"RemoveProfile\",\"ToggleInputMute\",\"SetRecordDirectory\",\"GetInputAudioMonitorType\",\"SetInputAudioMonitorType\",\"GetInputAudioTracks\",\"GetSceneTransitionList\",\"PressInputPropertiesButton\",\"GetInputVolume\",\"GetTransitionKindList\",\"GetMonitorList\",\"CreateSceneCollection\",\"TriggerStudioModeTransition\",\"SetTBarPosition\",\"GetProfileList\",\"SetSceneItemBlendMode\",\"SetInputMute\",\"GetLastReplayBufferReplay\",\"GetSourceFilterKindList\",\"ToggleStream\",\"GetInputKindList\",\"GetSourceFilterList\",\"GetSourceFilterDefaultSettings\",\"GetInputAudioSyncOffset\",\"SetSceneItemLocked\",\"StartStream\",\"CreateSourceFilter\",\"SetSourceFilterName\",\"StartReplayBuffer\",\"GetSourceFilter\",\"SetSourceFilterEnabled\",\"GetSceneItemSource\",\"GetSceneList\",\"SetPersistentData\",\"CreateSceneItem\",\"RemoveSceneItem\",\"TriggerMediaInputAction\",\"SetSceneItemEnabled\",\"SetSceneItemIndex\",\"GetSceneItemLocked\",\"GetSceneItemIndex\",\"GetSceneItemBlendMode\",\"SetSceneItemPrivateSettings\",\"CreateScene\",\"GetCurrentSceneTransitionCursor\",\"ToggleVirtualCam\",\"GetVideoSettings\",\"StartVirtualCam\",\"StopVirtualCam\",\"GetOutputList\",\"GetOutputStatus\",\"SetSceneItemTransform\",\"ToggleOutput\",\"GetOutputSettings\",\"GetSpecialInputs\",\"SetCurrentProgramScene\",\"SetOutputSettings\",\"ToggleRecord\"],\"obsVersion\":\"30.1.2\",\"obsWebSocketVersion\":\"5.4.2\",\"platform\":\"arch\",\"platformDescription\":\"Arch Linux\",\"rpcVersion\":1,\"supportedImageFormats\":[\"bmp\",\"cur\",\"icns\",\"ico\",\"jp2\",\"jpeg\",\"jpg\",\"pbm\",\"pgm\",\"png\",\"ppm\",\"tif\",\"tiff\",\"wbmp\",\"webp\",\"xbm\",\"xpm\"]}},\"op\":7}"
-- Response Fields:
-- Name     Type    Description
-- obsVersion   String  Current OBS Studio version
-- obsWebSocketVersion  String  Current obs-websocket version
-- rpcVersion   Number  Current latest obs-websocket RPC version
-- availableRequests    Array<String>   Array of available RPC requests for the currently negotiated RPC version
-- supportedImageFormats    Array<String>   Image formats available in GetSourceScreenshot and SaveSourceScreenshot requests.
-- platform     String  Name of the platform. Usually windows, macos, or ubuntu (linux flavor). Not guaranteed to be any of those
-- platformDescription  String  Description of the platform, like Windows 10 (10.0)
--
--
-- GetStats
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetStats\",\"responseData\":{\"activeFps\":60.0000024000001,\"availableDiskSpace\":12135541.21484375,\"averageFrameRenderTime\":0.694396,\"cpuUsage\":0.22358791700727937,\"memoryUsage\":413.2578125,\"outputSkippedFrames\":0,\"outputTotalFrames\":0,\"renderSkippedFrames\":5,\"renderTotalFrames\":5209,\"webSocketSessionIncomingMessages\":3,\"webSocketSessionOutgoingMessages\":3}},\"op\":7}"
-- Response Fields:
-- Name     Type    Description
-- cpuUsage     Number  Current CPU usage in percent
-- memoryUsage  Number  Amount of memory in MB currently being used by OBS
-- availableDiskSpace   Number  Available disk space on the device being used for recording storage
-- activeFps    Number  Current FPS being rendered
-- averageFrameRenderTime   Number  Average time in milliseconds that OBS is taking to render a frame
-- renderSkippedFrames  Number  Number of frames skipped by OBS in the render thread
-- renderTotalFrames    Number  Total number of frames outputted by the render thread
-- outputSkippedFrames  Number  Number of frames skipped by OBS in the output thread
-- outputTotalFrames    Number  Total number of frames outputted by the output thread
-- webSocketSessionIncomingMessages     Number  Total number of messages received by obs-websocket from the client
-- webSocketSessionOutgoingMessages     Number  Total number of messages sent by obs-websocket to the client
--
-- GetHotkeyList
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetHotkeyList\",\"responseData\":{\"hotkeys\":[\"OBSBasic.StartStreaming\",\"OBSBasic.StopStreaming\",\"OBSBasic.ForceStopStreaming\",\"OBSBasic.StartRecording\",\"OBSBasic.StopRecording\",\"OBSBasic.PauseRecording\",\"OBSBasic.UnpauseRecording\",\"OBSBasic.SplitFile\",\"OBSBasic.StartReplayBuffer\",\"OBSBasic.StopReplayBuffer\",\"OBSBasic.StartVirtualCam\",\"OBSBasic.StopVirtualCam\",\"OBSBasic.EnablePreview\",\"OBSBasic.DisablePreview\",\"OBSBasic.EnablePreviewProgram\",\"OBSBasic.DisablePreviewProgram\",\"OBSBasic.ShowContextBar\",\"OBSBasic.HideContextBar\",\"OBSBasic.Transition\",\"OBSBasic.ResetStats\",\"OBSBasic.Screenshot\",\"OBSBasic.SelectedSourceScreenshot\",\"OBSBasic.SelectScene\",\"OBSBasic.SelectScene\",\"OBSBasic.SelectScene\",\"OBSBasic.SelectScene\",\"libobs.mute\",\"libobs.unmute\",\"libobs.push-to-mute\",\"libobs.push-to-talk\",\"OBSBasic.SelectScene\",\"libobs.mute\",\"libobs.unmute\",\"libobs.push-to-mute\",\"libobs.push-to-talk\",\"OBSBasic.SelectScene\",\"OBSBasic.SelectScene\",\"OBSBasic.SelectScene\",\"OBSBasic.SelectScene\",\"libobs.mute\",\"libobs.unmute\",\"libobs.push-to-mute\",\"libobs.push-to-talk\",\"ObsBrowser.Refresh\",\"libobs.show_scene_item.4\",\"libobs.hide_scene_item.4\",\"libobs.show_scene_item.3\",\"libobs.hide_scene_item.3\",\"libobs.show_scene_item.5\",\"libobs.hide_scene_item.5\",\"libobs.show_scene_item.2\",\"libobs.hide_scene_item.2\",\"libobs.show_scene_item.3\",\"libobs.hide_scene_item.3\",\"libobs.show_scene_item.3\",\"libobs.hide_scene_item.3\",\"libobs.show_scene_item.1\",\"libobs.hide_scene_item.1\",\"libobs.show_scene_item.6\",\"libobs.hide_scene_item.6\",\"libobs.show_scene_item.1\",\"libobs.hide_scene_item.1\",\"libobs.show_scene_item.1\",\"libobs.hide_scene_item.1\",\"libobs.show_scene_item.2\",\"libobs.hide_scene_item.2\",\"libobs.show_scene_item.4\",\"libobs.hide_scene_item.4\",\"libobs.show_scene_item.1\",\"libobs.hide_scene_item.1\",\"libobs.show_scene_item.2\",\"libobs.hide_scene_item.2\",\"libobs.show_scene_item.3\",\"libobs.hide_scene_item.3\",\"libobs.show_scene_item.8\",\"libobs.hide_scene_item.8\",\"libobs.show_scene_item.2\",\"libobs.hide_scene_item.2\",\"libobs.show_scene_item.4\",\"libobs.hide_scene_item.4\",\"libobs.show_scene_item.1\",\"libobs.hide_scene_item.1\",\"libobs.show_scene_item.2\",\"libobs.hide_scene_item.2\",\"libobs.show_scene_item.4\",\"libobs.hide_scene_item.4\",\"libobs.show_scene_item.4\",\"libobs.hide_scene_item.4\",\"OBSBasic.QuickTransition.4\",\"OBSBasic.QuickTransition.5\",\"OBSBasic.QuickTransition.6\"]}},\"op\":7}"
-- Response Fields:
-- Name     Type    Description
-- hotkeys  Array<String>   Array of hotkey names
--
-- TriggerHotkeyByName
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"TriggerHotkeyByName\"},\"op\":7}"
--
-- Sleep
--
-- SetPersistentData
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetPersistentData\"},\"op\":7}"
--
-- GetPersistentData
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetPersistentData\",\"responseData\":{\"slotValue\":\"Hello!\"}},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetPersistentData\",\"responseData\":{\"slotValue\":null}},\"op\":7}"
--
-- GetSceneCollectionList
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetSceneCollectionList\",\"responseData\":{\"currentSceneCollectionName\":\"Video Capture\",\"sceneCollections\":[\"Baduk (Composited)\",\"Camera Only\",\"Kusogrande\",\"Speedrun Ragnarok 2023 Restream\",\"Speedrun Ragnarok 2023 Runner\",\"Stream (Composited)\",\"Stream (Composited - No Cam)\",\"Video Capture\",\"z - GDQ Runner Scene Collection\"]}},\"op\":7}"
--
-- CreateSceneCollection
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"CreateSceneCollection\"},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":601,\"result\":false},\"requestType\":\"CreateSceneCollection\"},\"op\":7}"
--
-- SetCurrentSceneCollection
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetCurrentSceneCollection\"},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":600,\"result\":false},\"requestType\":\"SetCurrentSceneCollection\"},\"op\":7}"
--
-- GetProfileList
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetProfileList\",\"responseData\":{\"currentProfileName\":\"Local Capture\",\"profiles\":[\"Camera Capture\",\"z-GDQ RTMP Hotfix\",\"GDQ Hotfix\",\"Kusogrande\",\"Local Capture\",\"Local Capture (Secondary)\",\"RTMP Test\",\"Speedrun Ragnarok\",\"Stream\",\"z - 1 - GDQ Runner Stream Setup High Resolution\",\"z - GDQ Runner Stream Setup\",\"z - GDQ Stream Real Style\"]}},\"op\":7}"
--
-- CreateProfile
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"CreateProfile\"},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":601,\"result\":false},\"requestType\":\"CreateProfile\"},\"op\":7}"
--
-- SetCurrentProfile
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetCurrentProfile\"},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":600,\"result\":false},\"requestType\":\"SetCurrentProfile\"},\"op\":7}"
--
-- RemoveProfile
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":600,\"result\":false},\"requestType\":\"RemoveProfile\"},\"op\":7}"
--
--
-- SetProfileParameter
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetProfileParameter\"},\"op\":7}"
--
-- GetProfileParameter
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetProfileParameter\",\"responseData\":{\"defaultParameterValue\":null,\"parameterValue\":\"Value\"}},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetProfileParameter\",\"responseData\":{\"defaultParameterValue\":null,\"parameterValue\":null}},\"op\":7}"
--
-- GetVideoSettings
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetVideoSettings\",\"responseData\":{\"baseHeight\":1080,\"baseWidth\":1920,\"fpsDenominator\":1,\"fpsNumerator\":60,\"outputHeight\":1080,\"outputWidth\":1920}},\"op\":7}"
--
-- SetVideoSettings
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetVideoSettings\"},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":402,\"comment\":\"The field value of `baseHeight` is below the minimum of `8.000000`\",\"result\":false},\"requestType\":\"SetVideoSettings\"},\"op\":7}"
--
-- GetStreamServiceSettings
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetStreamServiceSettings\",\"responseData\":{\"streamServiceSettings\":{\"bwtest\":false,\"key\":\"5\",\"server\":\"rtmp://45.33.94.88/ap\",\"use_auth\":false},\"streamServiceType\":\"rtmp_custom\"}},\"op\":7}"
--
-- SetStreamServiceSettings
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetStreamServiceSettings\"},\"op\":7}"
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":700,\"comment\":\"Failed to create the stream service with the requested streamServiceType. It may be an invalid type.\",\"result\":false},\"requestType\":\"SetStreamServiceSettings\"},\"op\":7}"
--
-- GetRecordDirectory
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"GetRecordDirectory\",\"responseData\":{\"recordDirectory\":\"/home/wuest\"}},\"op\":7}"
--
-- SetRecordDirectory
-- "{\"d\":{\"requestId\":\"Hello\",\"requestStatus\":{\"code\":100,\"result\":true},\"requestType\":\"SetRecordDirectory\"},\"op\":7}"
--

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

data HelloMessage = HelloMessage { obsWebSocketVersion :: String
                                 , remoteRPCVersion :: Integer
                                 , authenticationChallenge :: Maybe AuthChallenge
                                 } deriving ( Show )

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

data RequestResponse = RequestResponse { status :: RequestStatus
                                       , responseData :: Maybe Object
                                       } deriving ( Show )

data RequestBatchResponse = RequestBatchResponse { batchResponseId :: String
                                                 , results :: [Object]
                                                 } deriving ( Show )

data Message = Hello HelloMessage
             | Identified Integer
             | Event ServerEvent
             | Response RequestResponse
             | BatchResponse RequestBatchResponse
             deriving ( Show )
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
    return $ Hello $ HelloMessage{..}

identified :: OBSMessageParser
identified o = do
    negotiatedRPCVersion <- o .: "negotiatedRpcVersion"
    return $ Identified negotiatedRPCVersion

response :: OBSMessageParser
response o = do
    status <- o .: ""
    responseData <- o .:? "responseData"
    return $ Response RequestResponse{..}

batchResponse :: OBSMessageParser
batchResponse o = do
    batchResponseId <- o .: "requestId"
    results <- o .: "results"
    return $ BatchResponse RequestBatchResponse{..}
