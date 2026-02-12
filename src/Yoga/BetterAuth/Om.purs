module Yoga.BetterAuth.Om
  ( module Yoga.BetterAuth.Types
  , signUpEmail
  , signInEmail
  , signOut
  , getSession
  , clientSignUpEmail
  , clientSignInEmail
  , clientSignOut
  , clientGetSession
  ) where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Types (Auth, AuthClient, ClientError, ClientSessionWithUser, ClientSignInResult, ClientSignUpResult, Email, Password, UserName, SessionWithUser, SignInResult, SignUpResult, WebHeaders)
import Yoga.BetterAuth.Types (Auth, AuthClient, ClientError, ClientUser, ClientSession, ClientSessionWithUser, ClientSignUpResult, ClientSignInResult, Email(..), Password(..), UserName(..), ISODateString(..), SessionId(..), Token(..), UserId(..), User, Session, Account, SessionWithUser, SignUpResult, SignInResult) as Yoga.BetterAuth.Types
import Yoga.Om as Om
import Yoga.Om (throwLeftAs)

-- Server-side operations (require auth :: Auth in context)

signUpEmail
  :: forall r err
   . { email :: Email, password :: Password, name :: UserName }
  -> Om.Om { auth :: Auth | r } err SignUpResult
signUpEmail body = do
  { auth } <- Om.ask
  api' <- Server.api auth # liftEffect
  Server.signUpEmail body api' # liftAff

signInEmail
  :: forall r err
   . { email :: Email, password :: Password }
  -> Om.Om { auth :: Auth | r } err SignInResult
signInEmail body = do
  { auth } <- Om.ask
  api' <- Server.api auth # liftEffect
  Server.signInEmail body api' # liftAff

getSession
  :: forall r err
   . { headers :: WebHeaders }
  -> Om.Om { auth :: Auth | r } err SessionWithUser
getSession opts = do
  { auth } <- Om.ask
  api' <- Server.api auth # liftEffect
  Server.getSession opts api' # liftAff

signOut
  :: forall r err
   . { headers :: WebHeaders }
  -> Om.Om { auth :: Auth | r } err { success :: Boolean }
signOut opts = do
  { auth } <- Om.ask
  api' <- Server.api auth # liftEffect
  Server.signOut opts api' # liftAff

-- Client-side operations (require authClient :: AuthClient in context)

clientSignUpEmail
  :: forall r err
   . { email :: Email, password :: Password, name :: UserName }
  -> Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) ClientSignUpResult
clientSignUpEmail body = do
  { authClient } <- Om.ask
  result <- Client.signUpEmail body authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

clientSignInEmail
  :: forall r err
   . { email :: Email, password :: Password }
  -> Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) ClientSignInResult
clientSignInEmail body = do
  { authClient } <- Om.ask
  result <- Client.signInEmail body authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

clientGetSession
  :: forall r err
   . Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) ClientSessionWithUser
clientGetSession = do
  { authClient } <- Om.ask
  result <- Client.getSession authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

clientSignOut
  :: forall r err
   . Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) { success :: Boolean }
clientSignOut = do
  { authClient } <- Om.ask
  result <- Client.signOut authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })
