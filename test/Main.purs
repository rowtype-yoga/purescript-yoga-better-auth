module Test.BetterAuth.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Class (liftEffect)
import Yoga.BetterAuth.BetterAuth (betterAuth, emailAndPassword, api, signUpEmail, signInEmail)
import Data.String as String

mkApi :: Effect _
mkApi = do
  auth <- betterAuth
    { secret: "test-secret-that-is-at-least-32-chars-long!!"
    , baseURL: "http://localhost:3000"
    , emailAndPassword: emailAndPassword { enabled: true }
    }
  api auth

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do
    describe "Yoga.BetterAuth" do
      describe "Server" do
        it "creates an auth instance" do
          _ <- mkApi # liftEffect
          pure unit

        it "signs up a user via api" do
          authApi <- mkApi # liftEffect
          result <- signUpEmail
            { email: "test@example.com"
            , password: "password123"
            , name: "Test User"
            }
            authApi
          result.user.email `shouldEqual` "test@example.com"
          result.user.name `shouldEqual` "Test User"
          result.user.id `shouldSatisfy` (not <<< String.null)
          result.token `shouldSatisfy` (not <<< String.null)

        it "signs in a user via api" do
          authApi <- mkApi # liftEffect
          _ <- signUpEmail
            { email: "signin@example.com"
            , password: "password123"
            , name: "Sign In User"
            }
            authApi
          result <- signInEmail
            { email: "signin@example.com"
            , password: "password123"
            }
            authApi
          result.user.email `shouldEqual` "signin@example.com"
          result.token `shouldSatisfy` (not <<< String.null)
