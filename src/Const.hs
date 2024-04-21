{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Const where

import Prelude ( String )
import Data.Text.Internal ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.FileEmbed  ( embedFile )

applicationName :: String
applicationName = "obs-client"

version :: String
version = "0.0.0.1"

license :: Text
license = decodeUtf8 $(embedFile "LICENSE")
