module Test.BetterAuth.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (un)
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Om as AuthOm
import Yoga.BetterAuth.Types (AuthClient, Email(..), SessionId(..), Token(..), UserId(..))
import Yoga.Om as Om

mkClient :: Effect AuthClient
mkClient = do
  auth <- Server.betterAuth
    { secret: "test-secret-that-is-at-least-32-chars-long!!"
    , baseURL: "http://localhost:3000"
    , emailAndPassword: Server.emailAndPassword { enabled: true }
    }
  Client.createTestClient "http://localhost:3000" auth

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do

    describe "Client" do

      it "sign up, sign in, get session, sign out" do
        client <- mkClient # liftEffect

        signUp <- Client.signUpEmail { email: "alice@test.com", password: "password123", name: "Alice" } client
        case signUp of
          Left e -> fail e.message
          Right r -> do
            r.user.email `shouldEqual` Email "alice@test.com"
            r.user.name `shouldEqual` "Alice"
            r.user.emailVerified `shouldEqual` false
            un UserId r.user.id `shouldSatisfy` (not <<< String.null)

        signIn <- Client.signInEmail { email: "alice@test.com", password: "password123" } client
        case signIn of
          Left e -> fail e.message
          Right r -> do
            un Token r.token `shouldSatisfy` (not <<< String.null)
            r.user.email `shouldEqual` Email "alice@test.com"
            r.user.name `shouldEqual` "Alice"
            r.redirect `shouldEqual` false

        session <- Client.getSession client
        case session of
          Left e -> fail e.message
          Right r -> do
            un SessionId r.session.id `shouldSatisfy` (not <<< String.null)
            un Token r.session.token `shouldSatisfy` (not <<< String.null)
            r.session.userId `shouldEqual` r.user.id
            r.user.email `shouldEqual` Email "alice@test.com"

        logout <- Client.signOut client
        case logout of
          Left e -> fail e.message
          Right r -> r.success `shouldEqual` true

      it "sign in with wrong password returns Left" do
        client <- mkClient # liftEffect
        result <- Client.signInEmail { email: "nobody@test.com", password: "wrong" } client
        case result of
          Right _ -> fail "Expected Left"
          Left e -> e.status `shouldSatisfy` (_ > 0)

    describe "Om" do

      it "sign up, get session, sign out" do
        client <- mkClient # liftEffect
        result <- Om.runReader { authClient: client } do
          { user } <- AuthOm.clientSignUpEmail { email: "bob@test.com", password: "password123", name: "Bob" }
          { session } <- AuthOm.clientGetSession
          { success } <- AuthOm.clientSignOut
          pure { user, session, success }
        case result of
          Left _ -> fail "Expected Right"
          Right r -> do
            r.user.email `shouldEqual` Email "bob@test.com"
            r.user.name `shouldEqual` "Bob"
            r.session.userId `shouldEqual` r.user.id
            r.success `shouldEqual` true

      it "sign in with wrong password throws authError" do
        client <- mkClient # liftEffect
        result <- Om.runReader { authClient: client } do
          AuthOm.clientSignInEmail { email: "nobody@test.com", password: "wrong" }
        case result of
          Right _ -> fail "Expected Left"
          Left _ -> pure unit
