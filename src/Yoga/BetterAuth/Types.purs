module Yoga.BetterAuth.Types where

import Data.JSDate (JSDate)
import Data.Maybe (Maybe)

foreign import data Auth :: Type

foreign import data Api :: Type

foreign import data AuthClient :: Type

foreign import data Plugin :: Type

foreign import data Database :: Type

foreign import data SocialProviders :: Type

foreign import data WebHeaders :: Type

foreign import data WebRequest :: Type

type User =
  { id :: String
  , email :: String
  , name :: String
  , image :: Maybe String
  , emailVerified :: Boolean
  , createdAt :: String
  , updatedAt :: String
  }

type Session =
  { id :: String
  , userId :: String
  , token :: String
  , expiresAt :: String
  , ipAddress :: Maybe String
  , userAgent :: Maybe String
  , createdAt :: String
  , updatedAt :: String
  }

type Account =
  { id :: String
  , userId :: String
  , providerId :: String
  , accountId :: String
  , accessToken :: Maybe String
  , refreshToken :: Maybe String
  , idToken :: Maybe String
  , scope :: Maybe String
  , createdAt :: String
  , updatedAt :: String
  }

type SessionWithUser =
  { session :: Session
  , user :: User
  }

type SignUpResult =
  { token :: String
  , user :: User
  }

type SignInResult =
  { token :: String
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
  { id :: String
  , email :: String
  , name :: String
  , image :: Maybe String
  , emailVerified :: Boolean
  , createdAt :: JSDate
  , updatedAt :: JSDate
  }

type ClientSession =
  { id :: String
  , userId :: String
  , token :: String
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
  { token :: Maybe String
  , user :: ClientUser
  }

type ClientSignInResult =
  { token :: String
  , user :: ClientUser
  , redirect :: Boolean
  }
