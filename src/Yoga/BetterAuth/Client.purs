module Yoga.BetterAuth.Client
  ( module Yoga.BetterAuth.Types
  , createAuthClient
  , createTestClient
  , createCookieJarFetchOptions
  , AuthClientOptionsImpl
  , FetchOptions
  , signUpEmail
  , signInEmail
  , signOut
  , getSession
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Prim.Row (class Union)
import Promise (Promise)
import Promise.Aff as Promise
import Yoga.BetterAuth.Types (Auth, AuthClient, ClientError, ClientSessionWithUser, ClientSignInResult, ClientSignUpResult, Plugin)
import Yoga.BetterAuth.Types (Auth, AuthClient, ClientError, ClientUser, ClientSession, ClientSessionWithUser, ClientSignUpResult, ClientSignInResult, User, Session, Account, SessionWithUser, SignUpResult, SignInResult) as Yoga.BetterAuth.Types

foreign import data FetchOptions :: Type

type AuthClientOptionsImpl =
  ( baseURL :: String
  , plugins :: Array Plugin
  , fetchOptions :: FetchOptions
  )

foreign import createAuthClientImpl :: forall opts. EffectFn1 { | opts } AuthClient

createAuthClient :: forall opts opts_. Union opts opts_ AuthClientOptionsImpl => { | opts } -> Effect AuthClient
createAuthClient = runEffectFn1 createAuthClientImpl

foreign import createCookieJarFetchOptionsImpl :: EffectFn1 Auth FetchOptions

createCookieJarFetchOptions :: Auth -> Effect FetchOptions
createCookieJarFetchOptions = runEffectFn1 createCookieJarFetchOptionsImpl

createTestClient :: String -> Auth -> Effect AuthClient
createTestClient baseURL auth = do
  fetchOptions <- createCookieJarFetchOptions auth
  createAuthClient { baseURL, fetchOptions }

type RawResponse a =
  { data :: Nullable a
  , error :: Nullable { message :: Nullable String, status :: Int, statusText :: String }
  }

normalizeError :: { message :: Nullable String, status :: Int, statusText :: String } -> ClientError
normalizeError raw = do
  let
    message = fold (toMaybe raw.message)
  { message, status: raw.status, statusText: raw.statusText }

unwrapResponse :: forall a. Aff (RawResponse a) -> Aff (Either ClientError a)
unwrapResponse aff = do
  res <- aff
  pure case toMaybe res.data, toMaybe res.error of
    Just d, _ -> Right d
    _, Just e -> Left (normalizeError e)
    Nothing, Nothing -> Left { message: "Unknown error", status: 0, statusText: "Unknown" }

foreign import clientSignUpEmailImpl :: EffectFn2 AuthClient { email :: String, password :: String, name :: String } (Promise (RawResponse ClientSignUpResult))

signUpEmail :: { email :: String, password :: String, name :: String } -> AuthClient -> Aff (Either ClientError ClientSignUpResult)
signUpEmail body client = unwrapResponse do
  runEffectFn2 clientSignUpEmailImpl client body # Promise.toAffE

foreign import clientSignInEmailImpl :: EffectFn2 AuthClient { email :: String, password :: String } (Promise (RawResponse ClientSignInResult))

signInEmail :: { email :: String, password :: String } -> AuthClient -> Aff (Either ClientError ClientSignInResult)
signInEmail body client = unwrapResponse do
  runEffectFn2 clientSignInEmailImpl client body # Promise.toAffE

foreign import clientGetSessionImpl :: EffectFn1 AuthClient (Promise (RawResponse ClientSessionWithUser))

getSession :: AuthClient -> Aff (Either ClientError ClientSessionWithUser)
getSession client = unwrapResponse do
  runEffectFn1 clientGetSessionImpl client # Promise.toAffE

foreign import clientSignOutImpl :: EffectFn1 AuthClient (Promise (RawResponse { success :: Boolean }))

signOut :: AuthClient -> Aff (Either ClientError { success :: Boolean })
signOut client = unwrapResponse do
  runEffectFn1 clientSignOutImpl client # Promise.toAffE
