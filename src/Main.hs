module Main where

import Prelude

import Control.Monad.Trans ( liftIO )
import Data.Functor        ( (<&>) )
--import Data.Text           ( Text )
import Network.Socket      ( withSocketsDo )

import qualified Data.Text.Lazy          as Text ( toStrict )
import qualified Data.Text.Lazy.Encoding as Text ( decodeUtf8 )

import qualified Data.Aeson         as JSON
--import qualified Data.Text          as Text
import qualified Network.WebSockets as WS

import qualified Opts
import qualified Web.OBSWebSocket.API.Client as Client
import qualified Web.OBSWebSocket.API.Server as OBS


app :: Maybe String -> WS.ClientApp ()
app password conn = do
    (x :: Maybe OBS.Message) <- WS.receiveData conn <&> JSON.decode
    liftIO $ print x
    _ <- WS.sendTextData conn . Text.toStrict . Text.decodeUtf8 . JSON.encode $ authenticate password x
    (y :: Maybe OBS.Message) <- WS.receiveData conn <&> JSON.decode
    liftIO $ print y
    _ <- WS.sendTextData conn . Text.toStrict . Text.decodeUtf8 . JSON.encode $ Just Client.Reidentify { Client.newEventSubscriptions = [ ] }
    _ <- WS.sendTextData conn . Text.toStrict . Text.decodeUtf8 . JSON.encode $
        Just Client.Request { Client.requestId = "Hello", Client.requestData = Client.SetRecordDirectory "/home/wuest/Videos" }

    eatMyData conn -- debugging :')

eatMyData :: WS.Connection -> IO ()
eatMyData conn = do
    y <- WS.receiveData conn
    liftIO $ print y
    putStrLn ""
    let (z :: Maybe OBS.Message) = JSON.decode y
    liftIO $ print z
    eatMyData conn

authenticate :: Maybe String -> Maybe OBS.Message -> Maybe Client.ClientMessage
authenticate (Just pass) (Just (OBS.Hello OBS.HelloMessage { OBS.authenticationChallenge = Just (OBS.AuthChallenge { OBS.challenge = challenge, OBS.salt = salt }) })) =
    Just Client.Identify { Client.clientRPCVersion = 1
                         , Client.authenticationRequest = Just $ Client.genAuthString salt challenge pass
                         , Client.eventSubscriptions = []
                         }
authenticate _ (Just (OBS.Hello OBS.HelloMessage { OBS.authenticationChallenge = Nothing })) =
    Just Client.Identify { Client.clientRPCVersion = 1
                         , Client.authenticationRequest = Nothing
                         , Client.eventSubscriptions = []
                         }
authenticate _ _ = Nothing

main :: IO ()
main = do
    opts <- Opts.getOpts

    let port = Opts.port opts
        host = Opts.host opts
        pass = Opts.password opts
    print host
    print port
    withSocketsDo $ WS.runClient host port "/" $ app pass
