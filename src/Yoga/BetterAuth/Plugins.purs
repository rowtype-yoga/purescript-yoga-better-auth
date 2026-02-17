module Yoga.BetterAuth.Plugins
  ( bearer
  ) where

import Yoga.BetterAuth.Types (Plugin)

foreign import bearerPlugin :: Plugin

bearer :: Plugin
bearer = bearerPlugin
