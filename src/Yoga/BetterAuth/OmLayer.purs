module Yoga.BetterAuth.OmLayer
  ( BetterAuthL
  , AuthClientL
  , betterAuthLayer
  , betterAuthLayer'
  , authClientLayer
  ) where

import Prelude

import Effect.Class (liftEffect)
import Prim.Row (class Union)
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.BetterAuth (BetterAuthOptionsImpl)
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Types (Auth, AuthClient)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer)

type BetterAuthL r = (auth :: Auth | r)

betterAuthLayer
  :: forall r opts opts_
   . Union opts opts_ BetterAuthOptionsImpl
  => OmLayer (betterAuthConfig :: { | opts } | r) (BetterAuthL ()) ()
betterAuthLayer = makeLayer do
  { betterAuthConfig } <- Om.ask
  auth <- Server.betterAuth betterAuthConfig # liftEffect
  pure { auth }

betterAuthLayer'
  :: forall r opts opts_
   . Union opts opts_ BetterAuthOptionsImpl
  => { | opts }
  -> OmLayer r (BetterAuthL ()) ()
betterAuthLayer' config = makeLayer do
  auth <- Server.betterAuth config # liftEffect
  pure { auth }

type AuthClientL r = (authClient :: AuthClient | r)

authClientLayer
  :: forall r
   . OmLayer (authClientConfig :: { baseURL :: String }, auth :: Auth | r) (AuthClientL ()) ()
authClientLayer = makeLayer do
  { authClientConfig, auth } <- Om.ask
  client <- Client.createTestClient authClientConfig.baseURL auth # liftEffect
  pure { authClient: client }
