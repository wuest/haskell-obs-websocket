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

data VendorData = VendorData { vendorName :: String
                             , eventType :: String
                             , eventData :: Object
                             } deriving ( Show )

data SceneData = SceneData { sceneName :: String
                           , sceneUuid :: Maybe String
                           } deriving ( Show )

data InputData = InputData { inputName :: String
                           , inputUuid :: Maybe String
                           } deriving ( Show )

data TransitionData = TransitionData { transitionName :: String
                                     , transitionUuid :: Maybe String
                                     } deriving ( Show )

data SourceFilter = SourceFilter { sourceName :: String
                                 , filterName :: String
                                 } deriving ( Show )

data SourceData = SourceData { sourceName :: String
                             , sourceUuid :: Maybe String
                             } deriving ( Show )

data ServerEvent = ExitStarted -- OBS has begun the shutdown process.
                 | VendorEvent VendorData -- An event has been emitted from a vendor.
                                          -- A vendor is a unique name registered by a third-party plugin or script, which allows for custom requests and events to be added to obs-websocket. If a plugin or script implements vendor requests or events, documentation is expected to be provided with them.
                 | CustomEvent Object -- Custom event emitted by BroadcastCustomEvent.
                 | CurrentSceneCollectionChanging String -- The current scene collection has begun changing.
                                                         -- Note: We recommend using this event to trigger a pause of all polling requests, as performing any requests during a scene collection change is considered undefined behavior and can cause crashes!
                 | CurrentSceneCollectionChanged String -- The current scene collection has changed.
                                                        -- Note: If polling has been paused during CurrentSceneCollectionChanging, this is the cue to restart polling.
                 | SceneCollectionListChanged [String] -- The scene collection list has changed.
                 | CurrentProfileChanging String -- The current profile has begun changing.
                 | CurrentProfileChanged String -- The current profile has changed.
                 | ProfileListChanged [String] -- The profile list has changed.
                 | SceneCreated SceneData Bool -- A new scene has been created.
                 | SceneRemoved SceneData Bool -- A scene has been removed.
                 | SceneNameChanged SceneData String -- The name of a scene has changed.
                 | CurrentProgramSceneChanged SceneData -- The current program scene has changed.
                 | CurrentPreviewSceneChanged SceneData -- The current preview scene has changed.
                 | SceneListChanged [Object] -- The list of scenes has changed.
                 | InputCreated InputData String String Object Object -- An input has been created.
                 | InputRemoved InputData -- An input has been removed.
                 | InputNameChanged InputData String  -- The name of an input has changed.
                 | InputSettingsChanged InputData Object -- An input's settings have changed (been updated).
                                                         -- Note: On some inputs, changing values in the properties dialog will cause an immediate update. Pressing the "Cancel" button will revert the settings, resulting in another event being fired.
                 | InputActiveStateChanged InputData Bool -- An input's active state has changed.
                                                          -- When an input is active, it means it's being shown by the program feed.
                 | InputShowStateChanged InputData Bool -- An input's show state has changed.
                                                        -- When an input is showing, it means it's being shown by the preview or a dialog.
                 | InputMuteStateChanged InputData Bool -- An input's mute state has changed.
                 | InputVolumeChanged InputData Float Float -- An input's volume level has changed.
                 | InputAudioBalanceChanged InputData Float -- The audio balance value of an input has changed.
                 | InputAudioSyncOffsetChanged InputData Float -- The sync offset of an input has changed.
                 | InputAudioTracksChanged InputData Object --The audio tracks of an input have changed.
                 | InputAudioMonitorTypeChanged InputData MonitorType -- The monitor type of an input has changed.
                 | InputVolumeMeters [VolumeMeter] -- A high-volume event providing volume levels of all active inputs every 50 milliseconds.
                 | CurrentSceneTransitionChanged TransitionData -- The current scene transition has changed.
                 | CurrentSceneTransitionDurationChanged Integer -- The current scene transition duration has changed.
                 | SceneTransitionStarted TransitionData -- A scene transition has started.
                 | SceneTransitionEnded TransitionData -- A scene transition has completed fully.
                                                       -- Note: Does not appear to trigger when the transition is interrupted by the user.
                 | SceneTransitionVideoEnded TransitionData -- A scene transition's video has completed fully.
                                                            -- Useful for stinger transitions to tell when the video actually ends. SceneTransitionEnded only signifies the cut point, not the completion of transition playback.
                                                            -- Note: Appears to be called by every transition, regardless of relevance.
                 | SourceFilterListReindexed String [Object] -- A source's filter list has been reindexed.
                 | SourceFilterCreated SourceFilter String Integer Object Object -- A filter has been added to a source.
                 | SourceFilterRemoved SourceFilter -- A filter has been removed from a source.
                 | SourceFilterNameChanged SourceFilter String -- The name of a source filter has changed.
                 | SourceFilterSettingsChanged SourceFilter Object -- An source filter's settings have changed (been updated).
                 | SourceFilterEnableStateChanged SourceFilter Bool -- A source filter's enable state has changed.
                 | SceneItemCreated SceneData SourceData Integer Integer -- A scene item has been created.
                 | SceneItemRemoved SceneData SourceData Integer -- A scene item has been removed.
                                                                 -- This event is not emitted when the scene the item is in is removed.
                 | SceneItemListReindexed SceneData [Object] -- A scene's item list has been reindexed.
                 | SceneItemEnableStateChanged SceneData Integer Bool -- A scene item's enable state has changed.
                 | SceneItemLockStateChanged SceneData Integer Bool -- A scene item's lock state has changed.
                 | SceneItemSelected SceneData Integer -- A scene item has been selected in the Ui.
                 | SceneItemTransformChanged SceneData Integer Object -- The transform/crop of a scene item has changed.
                 | StreamStateChanged OutputState Bool -- The state of the stream output has changed.
                 | RecordStateChanged OutputState Bool (Maybe FilePath) -- The state of the record output has changed.
                 | ReplayBufferStateChanged OutputState Bool -- The state of the replay buffer output has changed.
                 | VirtualcamStateChanged OutputState Bool -- The state of the virtualcam output has changed.
                 | ReplayBufferSaved FilePath -- The replay buffer has been saved.
                 | MediaInputPlaybackStarted InputData -- A media input has started playing.
                 | MediaInputPlaybackEnded InputData -- A media input has finished playing.
                 | MediaInputActionTriggered InputData MediaAction -- An action has been performed on an input.
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
              return $ VendorEvent VendorData{..}
          "CustomEvent" -> do
              o <- d .: "eventData"
              eventData <- o .: "eventData"
              return $ CustomEvent eventData
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
              return $ SceneCreated SceneData{..} isGroup
          "SceneRemoved" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              isGroup <- o .: "isGroup"
              return $ SceneRemoved SceneData{..} isGroup
          "SceneNameChanged" -> do
              o <- d .: "eventData"
              sceneUuid <- o .:? "sceneUuid"
              oldSceneName <- o .: "oldSceneName"
              sceneName <- o .: "sceneName"
              return $ SceneNameChanged SceneData{..} oldSceneName
          "CurrentProgramSceneChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              return $ CurrentProgramSceneChanged SceneData{..}
          "CurrentPreviewSceneChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              return $ CurrentPreviewSceneChanged SceneData{..}
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
              return $ InputCreated InputData{..} inputKind unversionedInputKind inputSettings defaultInputSettings -- TODO: Ergonomics, probably two types to extract here
          "InputRemoved" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              return $ InputRemoved InputData{..}
          "InputNameChanged" -> do
              o <- d .: "eventData"
              inputUuid <- o .:? "inputUuid"
              oldInputName <- o .: "oldInputName"
              inputName <- o .: "inputName"
              return $ InputNameChanged InputData{..} oldInputName
          "InputSettingsChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputSettings <- o .: "inputSettings"
              return $ InputSettingsChanged InputData{..} inputSettings
          "InputActiveStateChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              videoActive <- o .: "videoActive"
              return $ InputActiveStateChanged InputData{..} videoActive
          "InputShowStateChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              videoShowing <- o .: "videoShowing"
              return $ InputShowStateChanged InputData{..} videoShowing
          "InputMuteStateChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputMuted <- o .: "inputMuted"
              return $ InputMuteStateChanged InputData{..} inputMuted
          "InputVolumeChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputVolumeMul <- o .: "inputVolumeMul"
              inputVolumeDb <- o .: "inputVolumeDb"
              return $ InputVolumeChanged InputData{..} inputVolumeMul inputVolumeDb
          "InputAudioBalanceChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputAudioBalance <- o .: "inputAudioBalance"
              return $ InputAudioBalanceChanged InputData{..} inputAudioBalance
          "InputAudioSyncOffsetChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputAudioSyncOffset <- o .: "inputAudioSyncOffset"
              return $ InputAudioSyncOffsetChanged InputData{..} inputAudioSyncOffset
          "InputAudioTracksChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              inputAudioTracks <- o .: "inputAudioTracks"
              return $ InputAudioTracksChanged InputData{..} inputAudioTracks
          "InputAudioMonitorTypeChanged" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              (mt :: String) <- o .: "monitorType"
              case mt of
                  "OBS_MONITORING_TYPE_MONITOR_ONLY" -> do
                      let monitorType = MonitorOnly
                      return $ InputAudioMonitorTypeChanged InputData{..} monitorType
                  "OBS_MONITORING_TYPE_MONITOR_AND_OUTPUT" -> do
                      let monitorType = MonitorAndOutput
                      return $ InputAudioMonitorTypeChanged InputData{..} monitorType
                  "OBS_MONITORING_TYPE_NONE" -> do
                      let monitorType = NoMonitor
                      return $ InputAudioMonitorTypeChanged InputData{..} monitorType
                  _ -> mzero
          "InputVolumeMeters" -> do
              o <- d .: "eventData"
              inputs <- o .: "inputs"
              return $ InputVolumeMeters inputs
          "CurrentSceneTransitionChanged" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return $ CurrentSceneTransitionChanged TransitionData{..}
          "CurrentSceneTransitionDurationChanged" -> do
              o <- d .: "eventData"
              transitionDuration <- o .: "transitionDuration"
              return $ CurrentSceneTransitionDurationChanged transitionDuration
          "SceneTransitionStarted" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return $ SceneTransitionStarted TransitionData{..}
          "SceneTransitionEnded" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return $ SceneTransitionEnded TransitionData{..}
          "SceneTransitionVideoEnded" -> do
              o <- d .: "eventData"
              transitionName <- o .: "transitionName"
              transitionUuid <- o .:? "transitionUuid"
              return $ SceneTransitionVideoEnded TransitionData{..}
          "SourceFilterListReindexed" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filters <- o .: "filters"
              return $ SourceFilterListReindexed sourceName filters
          "SourceFilterCreated" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              filterKind <- o .: "filterKind"
              filterIndex <- o .: "filterIndex"
              filterSettings <- o .: "filterSettings"
              defaultFilterSettings <- o .: "defaultFilterSettings"
              return $ SourceFilterCreated SourceFilter{..} filterKind filterIndex filterSettings defaultFilterSettings
          "SourceFilterRemoved" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              return $ SourceFilterRemoved SourceFilter{..}
          "SourceFilterNameChanged" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              oldFilterName <- o .: "oldFilterName"
              filterName <- o .: "filterName"
              return $ SourceFilterNameChanged SourceFilter{..} oldFilterName
          "SourceFilterSettingsChanged" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              filterSettings <- o .: "filterSettings"
              return $ SourceFilterSettingsChanged SourceFilter{..} filterSettings
          "SourceFilterEnableStateChanged" -> do
              o <- d .: "eventData"
              sourceName <- o .: "sourceName"
              filterName <- o .: "filterName"
              filterEnabled <- o .: "filterEnabled"
              return $ SourceFilterEnableStateChanged SourceFilter{..} filterEnabled
          "SceneItemCreated" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sourceName <- o .: "sourceName"
              sourceUuid <- o .:? "sourceUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemIndex <- o .: "sceneItemIndex"
              return $ SceneItemCreated SceneData{..} SourceData{..} sceneItemId sceneItemIndex
          "SceneItemRemoved" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sourceName <- o .: "sourceName"
              sourceUuid <- o .:? "sourceUuid"
              sceneItemId <- o .: "sceneItemId"
              return $ SceneItemRemoved SceneData{..} SourceData{..} sceneItemId
          "SceneItemListReindexed" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItems <- o .: "sceneItems"
              return $ SceneItemListReindexed SceneData{..} sceneItems
          "SceneItemEnableStateChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemEnabled <- o .: "sceneItemEnabled"
              return $ SceneItemEnableStateChanged SceneData{..} sceneItemId sceneItemEnabled
          "SceneItemLockStateChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemLocked <- o .: "sceneItemLocked"
              return $ SceneItemLockStateChanged SceneData{..} sceneItemId sceneItemLocked
          "SceneItemSelected" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              return $ SceneItemSelected SceneData{..} sceneItemId
          "SceneItemTransformChanged" -> do
              o <- d .: "eventData"
              sceneName <- o .: "sceneName"
              sceneUuid <- o .:? "sceneUuid"
              sceneItemId <- o .: "sceneItemId"
              sceneItemTransform <- o .: "sceneItemTransform"
              return $ SceneItemTransformChanged SceneData{..} sceneItemId sceneItemTransform
          "StreamStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> return $ StreamStateChanged OutputStarted outputActive
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> return $ StreamStateChanged OutputStarting outputActive
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> return $ StreamStateChanged OutputStopped outputActive
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> return $ StreamStateChanged OutputStopping outputActive
                _ -> mzero
          "RecordStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              outputPath <- o .:? "outputPath"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> return $ RecordStateChanged OutputStarted outputActive outputPath
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> return $ RecordStateChanged OutputStarting outputActive outputPath
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> return $ RecordStateChanged OutputStopped outputActive outputPath
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> return $ RecordStateChanged OutputStopping outputActive outputPath
                _ -> mzero
          "ReplayBufferStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> return $ ReplayBufferStateChanged OutputStarted outputActive
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> return $ ReplayBufferStateChanged OutputStarting outputActive
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> return $ ReplayBufferStateChanged OutputStopped outputActive
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> return $ ReplayBufferStateChanged OutputStopping outputActive
                _ -> mzero
          "VirtualcamStateChanged" -> do
              o <- d .: "eventData"
              outputActive <- o .: "outputActive"
              (os :: String) <- o .: "outputState"
              case os of
                "OBS_WEBSOCKET_OUTPUT_STARTED" -> return $ VirtualcamStateChanged OutputStarted outputActive
                "OBS_WEBSOCKET_OUTPUT_STARTING" -> return $ VirtualcamStateChanged OutputStarting outputActive
                "OBS_WEBSOCKET_OUTPUT_STOPPED" -> return $ VirtualcamStateChanged OutputStopped outputActive
                "OBS_WEBSOCKET_OUTPUT_STOPPING" -> return $ VirtualcamStateChanged OutputStopping outputActive
                _ -> mzero
          "ReplayBufferSaved" -> do
              o <- d .: "eventData"
              savedReplayPath <- o .: "savedReplayPath"
              return $ ReplayBufferSaved savedReplayPath
          "MediaInputPlaybackStarted" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              return $ MediaInputPlaybackStarted InputData{..}
          "MediaInputPlaybackEnded" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              return $ MediaInputPlaybackEnded InputData{..}
          "MediaInputActionTriggered" -> do
              o <- d .: "eventData"
              inputName <- o .: "inputName"
              inputUuid <- o .:? "inputUuid"
              (mo :: String) <- o .: "mediaAction"
              case mo of
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_NONE" -> return $ MediaInputActionTriggered InputData{..} MediaNone
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PLAY" -> return $ MediaInputActionTriggered InputData{..} MediaPlay
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PAUSE" -> return $ MediaInputActionTriggered InputData{..} MediaPause
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_STOP" -> return $ MediaInputActionTriggered InputData{..} MediaStop
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_RESTART" -> return $ MediaInputActionTriggered InputData{..} MediaRestart
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_NEXT" -> return $ MediaInputActionTriggered InputData{..} MediaNext
                "OBS_WEBSOCKET_MEDIA_INPUT_ACTION_PREVIOUS" -> return $ MediaInputActionTriggered InputData{..} MediaPrevious
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
