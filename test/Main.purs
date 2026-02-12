module Test.BetterAuth.Main where

import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeToForeign)
import Test.BetterAuth.Helpers (getSetCookieHeaders, mkHeaders, mkRequest)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.BetterAuth.BetterAuth (api, betterAuth, emailAndPassword, getSession, handler, signInEmail, signOut, signUpEmail)
import Yoga.BetterAuth.Types (Auth)

mkAuth :: Effect Auth
mkAuth = betterAuth
  { secret: "test-secret-that-is-at-least-32-chars-long!!"
  , baseURL: "http://localhost:3000"
  , emailAndPassword: emailAndPassword { enabled: true }
  }

mkSignUpRequest :: String -> Effect Foreign
mkSignUpRequest email = mkRequest
  "http://localhost:3000/api/auth/sign-up/email"
  "POST"
  (unsafeToForeign { "content-type": "application/json", "origin": "http://localhost:3000" })
  ("""{"email":""" <> "\"" <> email <> "\"" <> ""","password":"password123","name":"Test User"}""")

sessionHeaders :: Foreign -> Foreign
sessionHeaders response = mkHeaders (getSetCookieHeaders response)

signUpWithSession :: Auth -> String -> Aff { headers :: Foreign }
signUpWithSession auth email = do
  request <- mkSignUpRequest email # liftEffect
  response <- handler auth request
  pure { headers: sessionHeaders response }

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do
    describe "Yoga.BetterAuth" do
      describe "betterAuth" do
        it "creates an auth instance" do
          _ <- mkAuth # liftEffect
          pure unit

      describe "api" do
        it "extracts an api from an auth instance" do
          auth <- mkAuth # liftEffect
          _ <- api auth # liftEffect
          pure unit

      describe "signUpEmail" do
        it "returns a token" do
          authApi <- mkAuth >>= api # liftEffect
          result <- signUpEmail
            { email: "signup-token@test.com", password: "password123", name: "Test User" }
            authApi
          result.token `shouldSatisfy` (not <<< String.null)

        it "returns user.email matching input" do
          authApi <- mkAuth >>= api # liftEffect
          result <- signUpEmail
            { email: "signup-email@test.com", password: "password123", name: "Test User" }
            authApi
          result.user.email `shouldEqual` "signup-email@test.com"

        it "returns user.name matching input" do
          authApi <- mkAuth >>= api # liftEffect
          result <- signUpEmail
            { email: "signup-name@test.com", password: "password123", name: "My Name" }
            authApi
          result.user.name `shouldEqual` "My Name"

        it "returns user.id" do
          authApi <- mkAuth >>= api # liftEffect
          result <- signUpEmail
            { email: "signup-id@test.com", password: "password123", name: "Test User" }
            authApi
          result.user.id `shouldSatisfy` (not <<< String.null)

        it "returns user.emailVerified as false" do
          authApi <- mkAuth >>= api # liftEffect
          result <- signUpEmail
            { email: "signup-verified@test.com", password: "password123", name: "Test User" }
            authApi
          result.user.emailVerified `shouldEqual` false

      describe "signInEmail" do
        it "returns a token" do
          authApi <- mkAuth >>= api # liftEffect
          _ <- signUpEmail
            { email: "signin-token@test.com", password: "password123", name: "Test User" }
            authApi
          result <- signInEmail
            { email: "signin-token@test.com", password: "password123" }
            authApi
          result.token `shouldSatisfy` (not <<< String.null)

        it "returns user.email matching input" do
          authApi <- mkAuth >>= api # liftEffect
          _ <- signUpEmail
            { email: "signin-email@test.com", password: "password123", name: "Test User" }
            authApi
          result <- signInEmail
            { email: "signin-email@test.com", password: "password123" }
            authApi
          result.user.email `shouldEqual` "signin-email@test.com"

        it "returns user.name" do
          authApi <- mkAuth >>= api # liftEffect
          _ <- signUpEmail
            { email: "signin-name@test.com", password: "password123", name: "Sign In Name" }
            authApi
          result <- signInEmail
            { email: "signin-name@test.com", password: "password123" }
            authApi
          result.user.name `shouldEqual` "Sign In Name"

        it "returns user.id" do
          authApi <- mkAuth >>= api # liftEffect
          _ <- signUpEmail
            { email: "signin-id@test.com", password: "password123", name: "Test User" }
            authApi
          result <- signInEmail
            { email: "signin-id@test.com", password: "password123" }
            authApi
          result.user.id `shouldSatisfy` (not <<< String.null)

        it "returns user.emailVerified" do
          authApi <- mkAuth >>= api # liftEffect
          _ <- signUpEmail
            { email: "signin-ev@test.com", password: "password123", name: "Test User" }
            authApi
          result <- signInEmail
            { email: "signin-ev@test.com", password: "password123" }
            authApi
          result.user.emailVerified `shouldEqual` false

        it "returns redirect as false" do
          authApi <- mkAuth >>= api # liftEffect
          _ <- signUpEmail
            { email: "signin-redir@test.com", password: "password123", name: "Test User" }
            authApi
          result <- signInEmail
            { email: "signin-redir@test.com", password: "password123" }
            authApi
          result.redirect `shouldEqual` false

      describe "handler" do
        it "handles a sign-up request and sets cookies" do
          auth <- mkAuth # liftEffect
          request <- mkSignUpRequest "handler@test.com" # liftEffect
          response <- handler auth request
          let cookies = getSetCookieHeaders response
          cookies `shouldSatisfy` (not <<< String.null)

      describe "getSession" do
        it "returns session.id" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "getsess-id@test.com"
          result <- getSession opts authApi
          result.session.id `shouldSatisfy` (not <<< String.null)

        it "returns session.token" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "getsess-tok@test.com"
          result <- getSession opts authApi
          result.session.token `shouldSatisfy` (not <<< String.null)

        it "returns session.userId matching user.id" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "getsess-match@test.com"
          result <- getSession opts authApi
          result.session.userId `shouldEqual` result.user.id

        it "returns user.email" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "getsess-email@test.com"
          result <- getSession opts authApi
          result.user.email `shouldEqual` "getsess-email@test.com"

        it "returns user.name" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "getsess-name@test.com"
          result <- getSession opts authApi
          result.user.name `shouldEqual` "Test User"

        it "returns user.emailVerified" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "getsess-ev@test.com"
          result <- getSession opts authApi
          result.user.emailVerified `shouldEqual` false

      describe "signOut" do
        it "returns success true" do
          auth <- mkAuth # liftEffect
          authApi <- api auth # liftEffect
          opts <- signUpWithSession auth "signout@test.com"
          result <- signOut opts authApi
          result.success `shouldEqual` true
