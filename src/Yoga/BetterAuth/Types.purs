module Yoga.BetterAuth.Types where

import Data.JSDate (JSDate)
import Data.Nullable (Nullable)

foreign import data Auth :: Type

foreign import data Api :: Type

foreign import data AuthClient :: Type

type User =
  { id :: String
  , email :: String
  , name :: String
  , image :: Nullable String
  , emailVerified :: Boolean
  , createdAt :: String
  , updatedAt :: String
  }

type Session =
  { id :: String
  , userId :: String
  , token :: String
  , expiresAt :: String
  , ipAddress :: Nullable String
  , userAgent :: Nullable String
  , createdAt :: String
  , updatedAt :: String
  }

type Account =
  { id :: String
  , userId :: String
  , providerId :: String
  , accountId :: String
  , accessToken :: Nullable String
  , refreshToken :: Nullable String
  , idToken :: Nullable String
  , scope :: Nullable String
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
  , image :: Nullable String
  , emailVerified :: Boolean
  , createdAt :: JSDate
  , updatedAt :: JSDate
  }

type ClientSession =
  { id :: String
  , userId :: String
  , token :: String
  , expiresAt :: JSDate
  , ipAddress :: Nullable String
  , userAgent :: Nullable String
  , createdAt :: JSDate
  , updatedAt :: JSDate
  }

type ClientSessionWithUser =
  { session :: ClientSession
  , user :: ClientUser
  }

type ClientSignUpResult =
  { token :: Nullable String
  , user :: ClientUser
  }

type ClientSignInResult =
  { token :: String
  , user :: ClientUser
  , redirect :: Boolean
  }
