module Test.BetterAuth.Helpers where

import Effect.Uncurried (EffectFn4, runEffectFn4)
import Effect (Effect)
import Foreign (Foreign)

foreign import mkRequestImpl :: EffectFn4 String String Foreign String Foreign

mkRequest :: String -> String -> Foreign -> String -> Effect Foreign
mkRequest url method headers body = runEffectFn4 mkRequestImpl url method headers body

foreign import getSetCookieHeaders :: Foreign -> String

foreign import mkHeadersImpl :: String -> Foreign

mkHeaders :: String -> Foreign
mkHeaders = mkHeadersImpl
