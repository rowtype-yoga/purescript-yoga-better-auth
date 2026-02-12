module Yoga.BetterAuth.Types where

import Prelude

import Data.JSDate (JSDate)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype UserId = UserId String

derive instance Newtype UserId _
derive newtype instance Eq UserId
derive newtype instance Show UserId

newtype SessionId = SessionId String

derive instance Newtype SessionId _
derive newtype instance Eq SessionId
derive newtype instance Show SessionId

newtype Token = Token String

derive instance Newtype Token _
derive newtype instance Eq Token
derive newtype instance Show Token

newtype Email = Email String

derive instance Newtype Email _
derive newtype instance Eq Email
derive newtype instance Show Email

foreign import data Auth :: Type

foreign import data Api :: Type

foreign import data AuthClient :: Type

foreign import data Plugin :: Type

foreign import data Database :: Type

foreign import data SocialProviders :: Type

foreign import data WebHeaders :: Type

foreign import data WebRequest :: Type

type User =
  { id :: UserId
  , email :: Email
  , name :: String
  , image :: Maybe String
  , emailVerified :: Boolean
  , createdAt :: String
  , updatedAt :: String
  }

type Session =
  { id :: SessionId
  , userId :: UserId
  , token :: Token
  , expiresAt :: String
  , ipAddress :: Maybe String
  , userAgent :: Maybe String
  , createdAt :: String
  , updatedAt :: String
  }

type Account =
  { id :: String
  , userId :: UserId
  , providerId :: String
  , accountId :: String
  , accessToken :: Maybe Token
  , refreshToken :: Maybe Token
  , idToken :: Maybe Token
  , scope :: Maybe String
  , createdAt :: String
  , updatedAt :: String
  }

type SessionWithUser =
  { session :: Session
  , user :: User
  }

type SignUpResult =
  { token :: Token
  , user :: User
  }

type SignInResult =
  { token :: Token
  , user :: User
  , redirect :: Boolean
  }

-- Client types (dates are JSDate because better-auth's client parser converts ISO strings)

type ClientError =
  { message :: String
  , status :: Int
  , statusText :: String
  }

type ClientUser =
  { id :: UserId
  , email :: Email
  , name :: String
  , image :: Maybe String
  , emailVerified :: Boolean
  , createdAt :: JSDate
  , updatedAt :: JSDate
  }

type ClientSession =
  { id :: SessionId
  , userId :: UserId
  , token :: Token
  , expiresAt :: JSDate
  , ipAddress :: Maybe String
  , userAgent :: Maybe String
  , createdAt :: JSDate
  , updatedAt :: JSDate
  }

type ClientSessionWithUser =
  { session :: ClientSession
  , user :: ClientUser
  }

type ClientSignUpResult =
  { token :: Maybe Token
  , user :: ClientUser
  }

type ClientSignInResult =
  { token :: Token
  , user :: ClientUser
  , redirect :: Boolean
  }
