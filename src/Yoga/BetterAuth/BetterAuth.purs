module Yoga.BetterAuth.BetterAuth
  ( module Yoga.BetterAuth.Types
  , betterAuth
  , handler
  , api
  , BetterAuthOptionsImpl
  , EmailAndPassword
  , emailAndPassword
  , EmailAndPasswordImpl
  , getSession
  , signInEmail
  , signUpEmail
  , signOut
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Promise (Promise)
import Promise.Aff as Promise
import Effect.Aff (Aff)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.BetterAuth.Types (Api, Auth, SessionWithUser, SignUpResult, SignInResult)
import Yoga.BetterAuth.Types (Api, Auth, AuthClient, User, Session, Account, SessionWithUser, SignUpResult, SignInResult) as Yoga.BetterAuth.Types
import Yoga.Fetch (Response) as Fetch

type BetterAuthOptionsImpl =
  ( appName :: String
  , baseURL :: String
  , basePath :: String
  , secret :: String
  , database :: Foreign
  , emailAndPassword :: EmailAndPassword
  , socialProviders :: Foreign
  , trustedOrigins :: Array String
  , plugins :: Array Foreign
  )

foreign import betterAuthImpl :: forall opts. EffectFn1 { | opts } Auth

betterAuth :: forall opts opts_. Union opts opts_ BetterAuthOptionsImpl => { | opts } -> Effect Auth
betterAuth = runEffectFn1 betterAuthImpl

newtype EmailAndPassword = EmailAndPassword Foreign

type EmailAndPasswordImpl =
  ( enabled :: Boolean
  , minPasswordLength :: Int
  , maxPasswordLength :: Int
  , requireEmailVerification :: Boolean
  , autoSignIn :: Boolean
  , disableSignUp :: Boolean
  )

emailAndPassword :: forall opts opts_. Union opts opts_ EmailAndPasswordImpl => { | opts } -> EmailAndPassword
emailAndPassword opts = EmailAndPassword (unsafeCoerce opts)

foreign import handlerImpl :: EffectFn2 Auth Foreign (Promise Fetch.Response)

handler :: Auth -> Foreign -> Aff Fetch.Response
handler auth request = runEffectFn2 handlerImpl auth request # Promise.toAffE

foreign import apiImpl :: EffectFn1 Auth Api

api :: Auth -> Effect Api
api = runEffectFn1 apiImpl

foreign import getSessionImpl :: EffectFn2 Api { headers :: Foreign } (Promise SessionWithUser)

getSession :: { headers :: Foreign } -> Api -> Aff SessionWithUser
getSession opts a = runEffectFn2 getSessionImpl a opts # Promise.toAffE

foreign import signInEmailImpl :: EffectFn2 Api { body :: { email :: String, password :: String } } (Promise SignInResult)

signInEmail :: { email :: String, password :: String } -> Api -> Aff SignInResult
signInEmail body a = runEffectFn2 signInEmailImpl a { body } # Promise.toAffE

foreign import signUpEmailImpl :: EffectFn2 Api { body :: { email :: String, password :: String, name :: String } } (Promise SignUpResult)

signUpEmail :: { email :: String, password :: String, name :: String } -> Api -> Aff SignUpResult
signUpEmail body a = runEffectFn2 signUpEmailImpl a { body } # Promise.toAffE

foreign import signOutImpl :: EffectFn2 Api { headers :: Foreign } (Promise { success :: Boolean })

signOut :: { headers :: Foreign } -> Api -> Aff { success :: Boolean }
signOut opts a = runEffectFn2 signOutImpl a opts # Promise.toAffE
