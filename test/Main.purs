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
import Test.Spec (Spec, describe, it)
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
import Yoga.Om.Layer (OmLayer, class CheckAllProvided, runLayer, (>->))
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

secret :: String
secret = "test-secret-that-is-at-least-32-chars-long!!"

baseURL :: String
baseURL = "http://localhost:3000"

connectionString :: String
connectionString = "postgresql://test:test@localhost:5433/better_auth_test"

betterAuthConfig :: { | BetterAuthConfig }
betterAuthConfig =
  { secret
  , baseURL
  , emailAndPassword: Server.emailAndPassword { enabled: true }
  }

composeFile :: Docker.ComposeFile
composeFile = Docker.ComposeFile "docker-compose.test.yml"

mkClient :: Effect AuthClient
mkClient = do
  auth <- Server.betterAuth betterAuthConfig
  Client.createTestClient baseURL auth

runAuth :: forall a. AuthClient -> Om.Om { authClient :: AuthClient } (authError :: Client.ClientError) a -> Aff a
runAuth client = Om.runOm { authClient: client }
  { exception: \e -> throwError (error ("Unexpected exception: " <> show e))
  , authError: \e -> throwError (error ("Unexpected auth error: " <> e.message))
  }

runLayerOm :: forall ctx prov. CheckAllProvided ctx ctx => { | ctx } -> OmLayer ctx prov () -> Aff { | prov }
runLayerOm ctx layer = Om.runOm ctx
  { exception: \e -> throwError (error ("Layer failed: " <> show e)) }
  (runLayer ctx layer)

withDocker :: Aff Unit -> Aff Unit
withDocker action = do
  Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
  action
  Docker.stopService composeFile

main :: Effect Unit
main = launchAff_ do
  let config = defaultConfig { timeout = Just (Milliseconds 60000.0) }
  runSpecPure' config [ consoleReporter ] do
    omClientSpec
    omPostgresSpec
    omLayerSpec

omClientSpec :: Spec Unit
omClientSpec = describe "Yoga.BetterAuth.Om" do

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

omPostgresSpec :: Spec Unit
omPostgresSpec = describe "Yoga.BetterAuth.Om (Postgres)" do

  it "sign up and sign in with real Postgres" do
    withDocker do
      { client, db } <- mkPostgresClient
      signUp <- runAuth client do
        AuthOm.clientSignUpEmail { email: Email "pg@test.com", password: Password "password123", name: UserName "PgUser" }
      signUp.user.email `shouldEqual` Email "pg@test.com"
      signUp.user.name `shouldEqual` UserName "PgUser"
      signIn <- runAuth client do
        AuthOm.clientSignInEmail { email: Email "pg@test.com", password: Password "password123" }
      un Token signIn.token `shouldSatisfy` (not <<< String.null)
      Server.pgPoolEnd db
  where
  mkPostgresClient = do
    db <- Server.pgPool connectionString # liftEffect
    auth <- Server.betterAuth { database: db, secret, baseURL, emailAndPassword: Server.emailAndPassword { enabled: true } } # liftEffect
    Server.runMigrations auth
    client <- Client.createTestClient baseURL auth # liftEffect
    pure { client, db }

omLayerSpec :: Spec Unit
omLayerSpec = describe "Yoga.BetterAuth.OmLayer" do

  it "testStackLive provides auth + client from config" do
    withDocker testStackLiveTest

  it "betterAuthLive' composes with databaseLive via >->" do
    let ctx = { connectionString }
    let layer = OmLayer.betterAuthLive' betterAuthConfig >-> OmLayer.databaseLive :: ComposedLayer
    withDocker do
      { auth } <- runLayerOm ctx layer
      Server.runMigrations auth
      client <- Client.createTestClient baseURL auth # liftEffect
      { user } <- runAuth client do
        AuthOm.clientSignUpEmail { email: Email "compose@test.com", password: Password "password123", name: UserName "ComposeUser" }
      user.email `shouldEqual` Email "compose@test.com"

  it "authFullLive sets up database + auth + migrations" do
    let ctx = { connectionString, betterAuthConfig }
    let layer = OmLayer.authFullLive :: AuthFullLayer
    withDocker do
      { auth, database } <- runLayerOm ctx layer
      client <- Client.createTestClient baseURL auth # liftEffect
      { user } <- runAuth client do
        AuthOm.clientSignUpEmail { email: Email "full@test.com", password: Password "password123", name: UserName "FullUser" }
      user.email `shouldEqual` Email "full@test.com"
      Server.pgPoolEnd database

testStackLiveTest :: Aff Unit
testStackLiveTest = do
  provided <- OmLayer.testStackLive # runLayerOm ctx
  { user } <- Om.runOm provided
    { exception: \e -> throwError (error ("Unexpected: " <> show e))
    , authError: \e -> throwError (error ("Auth error: " <> e.message))
    }
    do AuthOm.clientSignUpEmail { email, password, name }
  user.email `shouldEqual` email
  Server.pgPoolEnd provided.database
  where
  ctx = { connectionString, baseURL, betterAuthConfig }
  email = Email "layer@test.com"
  password = Password "password123"
  name = UserName "LayerUser"
