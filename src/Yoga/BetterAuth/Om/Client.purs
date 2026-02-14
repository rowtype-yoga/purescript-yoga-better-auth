module Yoga.BetterAuth.Om.Client
  ( signUpEmail
  , signInEmail
  , signOut
  , getSession
  , signInSocial
  ) where

import Prelude

import Effect.Aff.Class (liftAff)
import Prim.Row (class Union)
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.SocialProviders (ProviderId, SignInSocialOptionsImpl, SignInSocialResult)
import Yoga.BetterAuth.Types (AuthClient, ClientError, ClientSessionWithUser, ClientSignInResult, ClientSignUpResult, Email, Password, UserName)
import Yoga.Om as Om
import Yoga.Om (throwLeftAs)

signUpEmail
  :: forall r err
   . { email :: Email, password :: Password, name :: UserName }
  -> Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) ClientSignUpResult
signUpEmail body = do
  { authClient } <- Om.ask
  result <- Client.signUpEmail body authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

signInEmail
  :: forall r err
   . { email :: Email, password :: Password }
  -> Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) ClientSignInResult
signInEmail body = do
  { authClient } <- Om.ask
  result <- Client.signInEmail body authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

getSession
  :: forall r err
   . Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) ClientSessionWithUser
getSession = do
  { authClient } <- Om.ask
  result <- Client.getSession authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

signOut
  :: forall r err
   . Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) { success :: Boolean }
signOut = do
  { authClient } <- Om.ask
  result <- Client.signOut authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })

signInSocial
  :: forall r err opts opts_
   . Union opts opts_ SignInSocialOptionsImpl
  => { provider :: ProviderId | opts }
  -> Om.Om { authClient :: AuthClient | r } (authError :: ClientError | err) SignInSocialResult
signInSocial body = do
  { authClient } <- Om.ask
  result <- Client.signInSocial body authClient # liftAff
  result # throwLeftAs (Om.error <<< { authError: _ })
