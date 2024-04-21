{-# LANGUAGE OverloadedStrings #-}

module Opts ( Options
            , getOpts
            , optVerbose
            , port
            , host
            , password
            ) where

import Prelude
import System.Console.GetOpt
import System.Environment    ( getProgName, getArgs )
import System.Exit           ( exitSuccess )
import System.IO             ( hPutStrLn, stderr )

import qualified Const

data Options = Options { port :: Int
                       , host :: String
                       , password :: Maybe String
                       , optVerbose :: Bool
                       }

defaultOptions :: IO Options
defaultOptions =
    return $ Options { port = 4455
                     , host = "localhost"
                     , password = Nothing
                     , optVerbose = False
                     }

printVersion :: Options -> IO Options
printVersion _ = do
    hPutStrLn stderr $ Const.applicationName ++ " " ++ Const.version
    exitSuccess

printHelp :: Options -> IO Options
printHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

verbose :: Options -> IO Options
verbose opt = return opt { optVerbose = True }

setHost :: String -> Options -> IO Options
setHost newHost opt = return opt { host = newHost }

setPort :: String -> Options -> IO Options
setPort newPort opt = return opt { port = read newPort :: Int }

setPass :: String -> Options -> IO Options
setPass newPass opt = return opt { password = Just newPass }

blank :: OptDescr (Options -> IO Options)
blank = Option [] [] (NoArg return) ""

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['v'] ["verbose"]
        (NoArg verbose) "Enable verbose messages"

    , Option ['V'] ["version"]
        (NoArg printVersion) "Print version"

    , Option ['?'] ["help"]
        (NoArg printHelp) "Show help"

    , blank

    , Option ['h'] ["host"]
        (ReqArg setHost "HOST") "OBS Websocket host"
    , Option ['p'] ["port"]
        (ReqArg setPort "PORT") "OBS Websocket port"
    , Option [] ["password"]
        (ReqArg setPass "PASSWORD") "OBS Websocket password"
    ]

getOpts :: IO Options
getOpts = do
    args <- getArgs
    let (actions, _nonoptions, _errors) = getOpt RequireOrder options args
    foldl (>>=) defaultOptions actions
