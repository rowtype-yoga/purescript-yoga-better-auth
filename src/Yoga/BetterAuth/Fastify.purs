module Yoga.BetterAuth.Fastify
  ( registerAuth
  , RegisterAuthOptionsImpl
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Prim.Row (class Union)
import Yoga.BetterAuth.Types (Auth)
import Yoga.Fastify.Fastify (Fastify)

type RegisterAuthOptionsImpl =
  ( basePath :: String
  )

foreign import registerAuthImpl :: forall opts. EffectFn3 Auth Fastify { | opts } Unit

registerAuth :: forall opts opts_. Union opts opts_ RegisterAuthOptionsImpl => { | opts } -> Auth -> Fastify -> Effect Unit
registerAuth opts auth app = runEffectFn3 registerAuthImpl auth app opts
