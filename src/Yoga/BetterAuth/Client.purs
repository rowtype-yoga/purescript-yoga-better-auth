module Yoga.BetterAuth.Client
  ( module Yoga.BetterAuth.Types
  , createAuthClient
  , AuthClientOptionsImpl
  , clientSignInEmail
  , clientSignUpEmail
  , clientSignOut
  , clientGetSession
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Promise (Promise)
import Promise.Aff as Promise
import Effect.Aff (Aff)
import Prim.Row (class Union)
import Yoga.BetterAuth.Types (AuthClient)
import Yoga.BetterAuth.Types (Auth, AuthClient, User, Session, Account, SessionWithUser, SignUpResult, SignInResult) as Yoga.BetterAuth.Types

type AuthClientOptionsImpl =
  ( baseURL :: String
  , plugins :: Array Foreign
  )

foreign import createAuthClientImpl :: forall opts. EffectFn1 { | opts } AuthClient

createAuthClient :: forall opts opts_. Union opts opts_ AuthClientOptionsImpl => { | opts } -> Effect AuthClient
createAuthClient opts = runEffectFn1 createAuthClientImpl opts

foreign import clientSignInEmailImpl :: EffectFn2 AuthClient { email :: String, password :: String } (Promise Foreign)

clientSignInEmail :: { email :: String, password :: String } -> AuthClient -> Aff Foreign
clientSignInEmail body client = runEffectFn2 clientSignInEmailImpl client body # Promise.toAffE

foreign import clientSignUpEmailImpl :: EffectFn2 AuthClient { email :: String, password :: String, name :: String } (Promise Foreign)

clientSignUpEmail :: { email :: String, password :: String, name :: String } -> AuthClient -> Aff Foreign
clientSignUpEmail body client = runEffectFn2 clientSignUpEmailImpl client body # Promise.toAffE

foreign import clientSignOutImpl :: EffectFn1 AuthClient (Promise Unit)

clientSignOut :: AuthClient -> Aff Unit
clientSignOut client = runEffectFn1 clientSignOutImpl client # Promise.toAffE

foreign import clientGetSessionImpl :: EffectFn1 AuthClient (Promise Foreign)

clientGetSession :: AuthClient -> Aff Foreign
clientGetSession client = runEffectFn1 clientGetSessionImpl client # Promise.toAffE
