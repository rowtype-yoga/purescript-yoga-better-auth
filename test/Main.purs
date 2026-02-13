module Test.BetterAuth.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecPure')
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.BetterAuth (EmailAndPassword)
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Om as AuthOm
import Yoga.BetterAuth.OmLayer as OmLayer
import Yoga.BetterAuth.Types (Auth, AuthClient, Database, Email(..), Password(..), UserName(..), SessionId(..), Token(..), UserId(..))
import Yoga.Om.Layer (OmLayer, runLayer, (>->))
import Yoga.Test.Docker as Docker
import Yoga.Om as Om

type BetterAuthConfig = (secret :: String, baseURL :: String, emailAndPassword :: EmailAndPassword)

type TestStackLayer = OmLayer
  (connectionString :: String, baseURL :: String, betterAuthConfig :: { | BetterAuthConfig })
  (auth :: Auth, database :: Database, authClient :: AuthClient)
  ()

type AuthFullLayer = OmLayer
  (connectionString :: String, betterAuthConfig :: { | BetterAuthConfig })
  (auth :: Auth, database :: Database)
  ()

type ComposedLayer = OmLayer
  (connectionString :: String)
  (auth :: Auth)
  ()

mkClient :: Effect AuthClient
mkClient = do
  auth <- Server.betterAuth
    { secret: "test-secret-that-is-at-least-32-chars-long!!"
    , baseURL: "http://localhost:3000"
    , emailAndPassword: Server.emailAndPassword { enabled: true }
    }
  Client.createTestClient "http://localhost:3000" auth

mkPostgresClient :: Aff { client :: AuthClient, db :: Server.Database }
mkPostgresClient = do
  let connectionString = "postgresql://test:test@localhost:5433/better_auth_test"
  db <- Server.pgPool connectionString # liftEffect
  auth <-
    Server.betterAuth
      { secret: "test-secret-that-is-at-least-32-chars-long!!"
      , baseURL: "http://localhost:3000"
      , database: db
      , emailAndPassword: Server.emailAndPassword { enabled: true }
      } # liftEffect
  Server.runMigrations auth
  client <- Client.createTestClient "http://localhost:3000" auth # liftEffect
  pure { client, db }

runAuth :: forall a. AuthClient -> Om.Om { authClient :: AuthClient } (authError :: Client.ClientError) a -> Aff a
runAuth client = Om.runOm { authClient: client }
  { exception: \e -> throwError (error ("Unexpected exception: " <> show e))
  , authError: \e -> throwError (error ("Unexpected auth error: " <> e.message))
  }

composeFile :: Docker.ComposeFile
composeFile = Docker.ComposeFile "docker-compose.test.yml"

main :: Effect Unit
main = launchAff_ do
  let config = defaultConfig { timeout = Just (Milliseconds 60000.0) }

  runSpecPure' config [ consoleReporter ] do

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

    describe "Yoga.BetterAuth.Om (Postgres)" do

      it "sign up and sign in with real Postgres" do
        Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
        { client, db } <- mkPostgresClient
        { user } <- runAuth client do
          AuthOm.clientSignUpEmail { email: Email "pg@test.com", password: Password "password123", name: UserName "PgUser" }
        user.email `shouldEqual` Email "pg@test.com"
        user.name `shouldEqual` UserName "PgUser"
        { token } <- runAuth client do
          AuthOm.clientSignInEmail { email: Email "pg@test.com", password: Password "password123" }
        un Token token `shouldSatisfy` (not <<< String.null)
        Server.pgPoolEnd db
        Docker.stopService composeFile

    describe "Yoga.BetterAuth.OmLayer" do

      let
        betterAuthConfig =
          { secret: "test-secret-that-is-at-least-32-chars-long!!"
          , baseURL: "http://localhost:3000"
          , emailAndPassword: Server.emailAndPassword { enabled: true }
          }

      it "testStackLive provides auth + client from config" do
        Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
        let
          ctx =
            { connectionString: "postgresql://test:test@localhost:5433/better_auth_test"
            , baseURL: "http://localhost:3000"
            , betterAuthConfig
            }
        let layer = OmLayer.testStackLive :: TestStackLayer
        { authClient, database } <- Om.runOm ctx
          { exception: \e -> throwError (error ("Layer failed: " <> show e)) }
          (runLayer ctx layer)
        { user } <- runAuth authClient do
          AuthOm.clientSignUpEmail { email: Email "layer@test.com", password: Password "password123", name: UserName "LayerUser" }
        user.email `shouldEqual` Email "layer@test.com"
        Server.pgPoolEnd database
        Docker.stopService composeFile

      it "betterAuthLive' composes with databaseLive via >->" do
        Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
        let ctx = { connectionString: "postgresql://test:test@localhost:5433/better_auth_test" }
        let layer = OmLayer.betterAuthLive' betterAuthConfig >-> OmLayer.databaseLive :: ComposedLayer
        { auth } <- Om.runOm ctx
          { exception: \e -> throwError (error ("Layer failed: " <> show e)) }
          (runLayer ctx layer)
        Server.runMigrations auth
        client <- Client.createTestClient "http://localhost:3000" auth # liftEffect
        { user } <- runAuth client do
          AuthOm.clientSignUpEmail { email: Email "compose@test.com", password: Password "password123", name: UserName "ComposeUser" }
        user.email `shouldEqual` Email "compose@test.com"
        Docker.stopService composeFile

      it "authFullLive sets up database + auth + migrations" do
        Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
        let
          ctx =
            { connectionString: "postgresql://test:test@localhost:5433/better_auth_test"
            , betterAuthConfig
            }
        let layer = OmLayer.authFullLive :: AuthFullLayer
        { auth, database } <- Om.runOm ctx
          { exception: \e -> throwError (error ("Layer failed: " <> show e)) }
          (runLayer ctx layer)
        client <- Client.createTestClient "http://localhost:3000" auth # liftEffect
        { user } <- runAuth client do
          AuthOm.clientSignUpEmail { email: Email "full@test.com", password: Password "password123", name: UserName "FullUser" }
        user.email `shouldEqual` Email "full@test.com"
        Server.pgPoolEnd database
        Docker.stopService composeFile
