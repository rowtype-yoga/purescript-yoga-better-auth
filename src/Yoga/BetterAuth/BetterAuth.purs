module Yoga.BetterAuth.BetterAuth
  ( module Yoga.BetterAuth.Types
  , betterAuth
  , handler
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
import Yoga.BetterAuth.Types (Auth, SessionWithUser, SignUpResult, SignInResult)
import Yoga.BetterAuth.Types (Auth, AuthClient, User, Session, Account, SessionWithUser, SignUpResult, SignInResult) as Yoga.BetterAuth.Types

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
betterAuth opts = runEffectFn1 betterAuthImpl opts

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

foreign import handlerImpl :: EffectFn2 Auth Foreign (Promise Foreign)

handler :: Auth -> Foreign -> Aff Foreign
handler auth request = runEffectFn2 handlerImpl auth request # Promise.toAffE

foreign import getSessionImpl :: EffectFn2 Auth { headers :: Foreign } (Promise SessionWithUser)

getSession :: { headers :: Foreign } -> Auth -> Aff SessionWithUser
getSession opts auth = runEffectFn2 getSessionImpl auth opts # Promise.toAffE

foreign import signInEmailImpl :: EffectFn2 Auth { email :: String, password :: String } (Promise SignInResult)

signInEmail :: { email :: String, password :: String } -> Auth -> Aff SignInResult
signInEmail body auth = runEffectFn2 signInEmailImpl auth body # Promise.toAffE

foreign import signUpEmailImpl :: EffectFn2 Auth { email :: String, password :: String, name :: String } (Promise SignUpResult)

signUpEmail :: { email :: String, password :: String, name :: String } -> Auth -> Aff SignUpResult
signUpEmail body auth = runEffectFn2 signUpEmailImpl auth body # Promise.toAffE

foreign import signOutImpl :: EffectFn2 Auth { headers :: Foreign } (Promise Unit)

signOut :: { headers :: Foreign } -> Auth -> Aff Unit
signOut opts auth = runEffectFn2 signOutImpl auth opts # Promise.toAffE
