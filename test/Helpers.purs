module Test.BetterAuth.Helpers where

import Effect.Uncurried (EffectFn4, runEffectFn4)
import Effect (Effect)
import Foreign (Foreign)
import Yoga.Fetch (Headers)

foreign import mkRequestImpl :: EffectFn4 String String Headers String Foreign

mkRequest :: String -> String -> Headers -> String -> Effect Foreign
mkRequest url method hdrs body = runEffectFn4 mkRequestImpl url method hdrs body
