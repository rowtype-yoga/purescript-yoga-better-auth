module Yoga.BetterAuth.Om.Client
  ( clientSignUpEmail
  , clientSignInEmail
  , clientSignOut
  , clientGetSession
  , clientSignInSocial
  ) where

import Prelude

import Effect.Aff.Class (liftAff)
import Prim.Row (class Union)
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.SocialProviders (ProviderId, SignInSocialOptionsImpl, SignInSocialResult)
import Yoga.BetterAuth.Types (AuthClient, ClientError, ClientSessionWithUser, ClientSignInResult, ClientSignUpResult, Email, Password, UserName)
import Yoga.Om as Om
import Yoga.Om (throwLeftAs)

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

clientSignInSocial
  :: forall r err opts opts_
   . Union opts opts_ SignInSocialOptionsImpl
  => { provider :: ProviderId | opts }
  -> Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) SignInSocialResult
clientSignInSocial body = do
  { authClient } <- Om.ask
  result <- Client.signInSocial body authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })
