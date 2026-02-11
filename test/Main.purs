module Test.BetterAuth.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect.Class (liftEffect)
import Yoga.BetterAuth.BetterAuth (betterAuth, emailAndPassword, signUpEmail, signInEmail)
import Data.String as String

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do
    describe "Yoga.BetterAuth" do
      describe "Server" do
        it "creates an auth instance" do
          _ <- liftEffect $ betterAuth
            { secret: "test-secret-that-is-at-least-32-chars-long!!"
            , baseURL: "http://localhost:3000"
            , emailAndPassword: emailAndPassword { enabled: true }
            }
          pure unit

        it "signs up a user via api" do
          auth <- liftEffect $ betterAuth
            { secret: "test-secret-that-is-at-least-32-chars-long!!"
            , baseURL: "http://localhost:3000"
            , emailAndPassword: emailAndPassword { enabled: true }
            }
          result <- signUpEmail
            { email: "test@example.com"
            , password: "password123"
            , name: "Test User"
            } auth
          result.user.email `shouldEqual` "test@example.com"
          result.user.name `shouldEqual` "Test User"
          result.user.id `shouldSatisfy` (not <<< String.null)
          result.token `shouldSatisfy` (not <<< String.null)

        it "signs in a user via api" do
          auth <- liftEffect $ betterAuth
            { secret: "test-secret-that-is-at-least-32-chars-long!!"
            , baseURL: "http://localhost:3000"
            , emailAndPassword: emailAndPassword { enabled: true }
            }
          _ <- signUpEmail
            { email: "signin@example.com"
            , password: "password123"
            , name: "Sign In User"
            } auth
          result <- signInEmail
            { email: "signin@example.com"
            , password: "password123"
            } auth
          result.user.email `shouldEqual` "signin@example.com"
          result.token `shouldSatisfy` (not <<< String.null)
