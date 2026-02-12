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

import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Promise (Promise)
import Promise.Aff as Promise
import Effect.Aff (Aff)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.BetterAuth.Types (Api, Auth, Database, Email(..), ISODateString(..), Plugin, SessionId(..), SessionWithUser, User, Session, SignUpResult, SignInResult, SocialProviders, Token(..), UserId(..), WebHeaders, WebRequest)
import Yoga.BetterAuth.Types (Api, Auth, AuthClient, Database, Plugin, User, Session, Account, SessionWithUser, SignUpResult, SignInResult, SocialProviders, WebHeaders, WebRequest) as Yoga.BetterAuth.Types
import Yoga.Fetch (Response) as Fetch

type BetterAuthOptionsImpl =
  ( appName :: String
  , baseURL :: String
  , basePath :: String
  , secret :: String
  , database :: Database
  , emailAndPassword :: EmailAndPassword
  , socialProviders :: SocialProviders
  , trustedOrigins :: Array String
  , plugins :: Array Plugin
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

foreign import handlerImpl :: EffectFn2 Auth WebRequest (Promise Fetch.Response)

handler :: Auth -> WebRequest -> Aff Fetch.Response
handler auth request = runEffectFn2 handlerImpl auth request # Promise.toAffE

foreign import apiImpl :: EffectFn1 Auth Api

api :: Auth -> Effect Api
api = runEffectFn1 apiImpl

-- Raw FFI types (Nullable at JS boundary)

type RawUser =
  { id :: String
  , email :: String
  , name :: String
  , image :: Nullable String
  , emailVerified :: Boolean
  , createdAt :: String
  , updatedAt :: String
  }

type RawSession =
  { id :: String
  , userId :: String
  , token :: String
  , expiresAt :: String
  , ipAddress :: Nullable String
  , userAgent :: Nullable String
  , createdAt :: String
  , updatedAt :: String
  }

fromRawUser :: RawUser -> User
fromRawUser r = r
  { id = UserId r.id
  , email = Email r.email
  , image = toMaybe r.image
  , createdAt = ISODateString r.createdAt
  , updatedAt = ISODateString r.updatedAt
  }

fromRawSession :: RawSession -> Session
fromRawSession r = r
  { id = SessionId r.id
  , userId = UserId r.userId
  , token = Token r.token
  , expiresAt = ISODateString r.expiresAt
  , ipAddress = toMaybe r.ipAddress
  , userAgent = toMaybe r.userAgent
  , createdAt = ISODateString r.createdAt
  , updatedAt = ISODateString r.updatedAt
  }

foreign import getSessionImpl :: EffectFn2 Api { headers :: WebHeaders } (Promise { session :: RawSession, user :: RawUser })

getSession :: { headers :: WebHeaders } -> Api -> Aff SessionWithUser
getSession opts a = do
  raw <- runEffectFn2 getSessionImpl a opts # Promise.toAffE
  pure { session: fromRawSession raw.session, user: fromRawUser raw.user }

foreign import signInEmailImpl :: EffectFn2 Api { body :: { email :: String, password :: String } } (Promise { token :: String, user :: RawUser, redirect :: Boolean })

signInEmail :: { email :: String, password :: String } -> Api -> Aff SignInResult
signInEmail body a = do
  raw <- runEffectFn2 signInEmailImpl a { body } # Promise.toAffE
  pure { token: Token raw.token, user: fromRawUser raw.user, redirect: raw.redirect }

foreign import signUpEmailImpl :: EffectFn2 Api { body :: { email :: String, password :: String, name :: String } } (Promise { token :: String, user :: RawUser })

signUpEmail :: { email :: String, password :: String, name :: String } -> Api -> Aff SignUpResult
signUpEmail body a = do
  raw <- runEffectFn2 signUpEmailImpl a { body } # Promise.toAffE
  pure { token: Token raw.token, user: fromRawUser raw.user }

foreign import signOutImpl :: EffectFn2 Api { headers :: WebHeaders } (Promise { success :: Boolean })

signOut :: { headers :: WebHeaders } -> Api -> Aff { success :: Boolean }
signOut opts a = runEffectFn2 signOutImpl a opts # Promise.toAffE
