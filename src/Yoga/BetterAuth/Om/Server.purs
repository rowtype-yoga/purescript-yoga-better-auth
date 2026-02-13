module Yoga.BetterAuth.Om.Server
  ( signUpEmail
  , signInEmail
  , signOut
  , getSession
  , signInSocial
  ) where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Prim.Row (class Union)
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.SocialProviders (ProviderId, SignInSocialOptionsImpl, SignInSocialResult)
import Yoga.BetterAuth.Types (Api, Auth, Email, Password, UserName, SessionWithUser, SignInResult, SignUpResult, WebHeaders)
import Yoga.Om as Om

askApi :: forall r err. Om.Om { auth :: Auth | r } err Api
askApi = do
  { auth } <- Om.ask
  Server.api auth # liftEffect

signUpEmail
  :: forall r err
   . { email :: Email, password :: Password, name :: UserName }
  -> Om.Om { auth :: Auth | r } err SignUpResult
signUpEmail body = do
  api' <- askApi
  Server.signUpEmail body api' # liftAff

signInEmail
  :: forall r err
   . { email :: Email, password :: Password }
  -> Om.Om { auth :: Auth | r } err SignInResult
signInEmail body = do
  api' <- askApi
  Server.signInEmail body api' # liftAff

getSession
  :: forall r err
   . { headers :: WebHeaders }
  -> Om.Om { auth :: Auth | r } err SessionWithUser
getSession opts = do
  api' <- askApi
  Server.getSession opts api' # liftAff

signOut
  :: forall r err
   . { headers :: WebHeaders }
  -> Om.Om { auth :: Auth | r } err { success :: Boolean }
signOut opts = do
  api' <- askApi
  Server.signOut opts api' # liftAff

signInSocial
  :: forall r err opts opts_
   . Union opts opts_ SignInSocialOptionsImpl
  => { provider :: ProviderId | opts }
  -> Om.Om { auth :: Auth | r } err SignInSocialResult
signInSocial body = do
  api' <- askApi
  Server.signInSocial body api' # liftAff
