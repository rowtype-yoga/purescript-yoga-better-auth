module Test.BetterAuth.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (un)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Om as AuthOm
import Yoga.BetterAuth.Types (AuthClient, Email(..), Password(..), UserName(..), SessionId(..), Token(..), UserId(..))
import Partial.Unsafe as Partial.Unsafe
import Yoga.Om as Om

mkClient :: Effect AuthClient
mkClient = do
  auth <- Server.betterAuth
    { secret: "test-secret-that-is-at-least-32-chars-long!!"
    , baseURL: "http://localhost:3000"
    , emailAndPassword: Server.emailAndPassword { enabled: true }
    }
  Client.createTestClient "http://localhost:3000" auth

runAuth :: forall a. AuthClient -> Om.Om { authClient :: AuthClient } (authError :: Client.ClientError) a -> Aff a
runAuth client = Om.runOm { authClient: client }
  { exception: \e -> fail ("Unexpected exception: " <> show e) *> unsafeCrashWith "unreachable"
  , authError: \e -> fail ("Unexpected auth error: " <> e.message) *> unsafeCrashWith "unreachable"
  }
  where
  unsafeCrashWith :: forall b. String -> Aff b
  unsafeCrashWith = liftEffect <<< Partial.Unsafe.unsafeCrashWith

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do

    describe "Yoga.BetterAuth.Om" do

      it "sign up returns the user" do
        client <- mkClient # liftEffect
        { user } <- runAuth client do
          AuthOm.clientSignUpEmail { email: Email "alice@test.com", password: Password "password123", name: UserName "Alice" }
        user.email `shouldEqual` Email "alice@test.com"
        user.name `shouldEqual` UserName "Alice"
        user.emailVerified `shouldEqual` false
        un UserId user.id `shouldSatisfy` (not <<< String.null)

      it "sign in returns token and user" do
        client <- mkClient # liftEffect
        runAuth client do
          void $ AuthOm.clientSignUpEmail { email: Email "bob@test.com", password: Password "password123", name: UserName "Bob" }
        { token, user, redirect } <- runAuth client do
          AuthOm.clientSignInEmail { email: Email "bob@test.com", password: Password "password123" }
        un Token token `shouldSatisfy` (not <<< String.null)
        user.email `shouldEqual` Email "bob@test.com"
        redirect `shouldEqual` false

      it "get session after sign up" do
        client <- mkClient # liftEffect
        { session, user } <- runAuth client do
          void $ AuthOm.clientSignUpEmail { email: Email "carol@test.com", password: Password "password123", name: UserName "Carol" }
          AuthOm.clientGetSession
        un SessionId session.id `shouldSatisfy` (not <<< String.null)
        un Token session.token `shouldSatisfy` (not <<< String.null)
        session.userId `shouldEqual` user.id
        user.email `shouldEqual` Email "carol@test.com"

      it "sign out after sign up" do
        client <- mkClient # liftEffect
        { success } <- runAuth client do
          void $ AuthOm.clientSignUpEmail { email: Email "dave@test.com", password: Password "password123", name: UserName "Dave" }
          AuthOm.clientSignOut
        success `shouldEqual` true

      it "sign in with wrong password throws authError" do
        client <- mkClient # liftEffect
        result <- Om.runReader { authClient: client } do
          AuthOm.clientSignInEmail { email: Email "nobody@test.com", password: Password "wrong" }
        case result of
          Right _ -> fail "Expected authError"
          Left _ -> pure unit
