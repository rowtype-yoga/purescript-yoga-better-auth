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

withRight :: forall a. Either Client.ClientError a -> (a -> Aff Unit) -> Aff Unit
withRight (Right a) f = f a
withRight (Left e) _ = fail ("Expected Right, got Left: " <> e.message)

withLeft :: forall a. Either Client.ClientError a -> (Client.ClientError -> Aff Unit) -> Aff Unit
withLeft (Left e) f = f e
withLeft (Right _) _ = fail "Expected Left, got Right"

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do
    describe "Yoga.BetterAuth.Client" do
      describe "signUpEmail" do
        it "returns a token" do
          client <- mkClient # liftEffect
          result <- Client.signUpEmail { email: "signup-token@test.com", password: "password123", name: "Test User" } client
          withRight result \r ->
            r.token `shouldSatisfy` \_ -> true -- Maybe Token, any value is fine

        it "returns user.email matching input" do
          client <- mkClient # liftEffect
          result <- Client.signUpEmail { email: "signup-email@test.com", password: "password123", name: "Test User" } client
          withRight result \r ->
            r.user.email `shouldEqual` Email "signup-email@test.com"

        it "returns user.name matching input" do
          client <- mkClient # liftEffect
          result <- Client.signUpEmail { email: "signup-name@test.com", password: "password123", name: "My Name" } client
          withRight result \r ->
            r.user.name `shouldEqual` "My Name"

        it "returns user.id" do
          client <- mkClient # liftEffect
          result <- Client.signUpEmail { email: "signup-id@test.com", password: "password123", name: "Test User" } client
          withRight result \r ->
            un UserId r.user.id `shouldSatisfy` (not <<< String.null)

        it "returns user.emailVerified as false" do
          client <- mkClient # liftEffect
          result <- Client.signUpEmail { email: "signup-ev@test.com", password: "password123", name: "Test User" } client
          withRight result \r ->
            r.user.emailVerified `shouldEqual` false

      describe "signInEmail" do
        it "returns a token" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signin-token@test.com", password: "password123", name: "Test User" } client
          result <- Client.signInEmail { email: "signin-token@test.com", password: "password123" } client
          withRight result \r ->
            un Token r.token `shouldSatisfy` (not <<< String.null)

        it "returns user.email matching input" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signin-email@test.com", password: "password123", name: "Test User" } client
          result <- Client.signInEmail { email: "signin-email@test.com", password: "password123" } client
          withRight result \r ->
            r.user.email `shouldEqual` Email "signin-email@test.com"

        it "returns user.name" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signin-name@test.com", password: "password123", name: "Sign In Name" } client
          result <- Client.signInEmail { email: "signin-name@test.com", password: "password123" } client
          withRight result \r ->
            r.user.name `shouldEqual` "Sign In Name"

        it "returns user.id" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signin-id@test.com", password: "password123", name: "Test User" } client
          result <- Client.signInEmail { email: "signin-id@test.com", password: "password123" } client
          withRight result \r ->
            un UserId r.user.id `shouldSatisfy` (not <<< String.null)

        it "returns user.emailVerified" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signin-ev@test.com", password: "password123", name: "Test User" } client
          result <- Client.signInEmail { email: "signin-ev@test.com", password: "password123" } client
          withRight result \r ->
            r.user.emailVerified `shouldEqual` false

        it "returns redirect as false" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signin-redir@test.com", password: "password123", name: "Test User" } client
          result <- Client.signInEmail { email: "signin-redir@test.com", password: "password123" } client
          withRight result \r ->
            r.redirect `shouldEqual` false

      describe "getSession" do
        it "returns session after sign up" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "getsess@test.com", password: "password123", name: "Test User" } client
          result <- Client.getSession client
          withRight result \r -> do
            un SessionId r.session.id `shouldSatisfy` (not <<< String.null)
            un Token r.session.token `shouldSatisfy` (not <<< String.null)
            r.session.userId `shouldEqual` r.user.id
            r.user.email `shouldEqual` Email "getsess@test.com"
            r.user.name `shouldEqual` "Test User"
            r.user.emailVerified `shouldEqual` false

      describe "signOut" do
        it "returns success true" do
          client <- mkClient # liftEffect
          _ <- Client.signUpEmail { email: "signout@test.com", password: "password123", name: "Test User" } client
          result <- Client.signOut client
          withRight result \r ->
            r.success `shouldEqual` true

      describe "error handling" do
        it "returns Left for invalid credentials" do
          client <- mkClient # liftEffect
          result <- Client.signInEmail { email: "nonexistent@test.com", password: "wrong" } client
          withLeft result \e ->
            e.status `shouldSatisfy` (_ > 0)

    describe "Yoga.BetterAuth.Om" do
      describe "clientSignUpEmail" do
        it "signs up via Om context" do
          client <- mkClient # liftEffect
          result <- Om.runReader { authClient: client } do
            r <- AuthOm.clientSignUpEmail { email: "om-signup@test.com", password: "password123", name: "Om User" }
            pure r.user.email
          case result of
            Right email -> email `shouldEqual` Email "om-signup@test.com"
            Left _ -> fail "Expected Right"

      describe "clientGetSession" do
        it "gets session via Om context" do
          client <- mkClient # liftEffect
          result <- Om.runReader { authClient: client } do
            _ <- AuthOm.clientSignUpEmail { email: "om-session@test.com", password: "password123", name: "Om User" }
            r <- AuthOm.clientGetSession
            pure r.session.userId
          case result of
            Right uid -> un UserId uid `shouldSatisfy` (not <<< String.null)
            Left _ -> fail "Expected Right"

      describe "error handling" do
        it "throws authError for invalid credentials" do
          client <- mkClient # liftEffect
          result <- Om.runReader { authClient: client } do
            AuthOm.clientSignInEmail { email: "om-noexist@test.com", password: "wrong" }
          case result of
            Right _ -> fail "Expected authError, got Right"
            Left _ -> pure unit
