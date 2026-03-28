module Test.BetterAuth.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, finally, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Config (defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecPure')
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.BetterAuth (EmailAndPassword)
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Fastify as BetterAuth.Fastify
import Yoga.BetterAuth.Om.Client as AuthClient
import Yoga.BetterAuth.OmHandler (handleAuth)
import Yoga.BetterAuth.OmLayer as OmLayer
import Yoga.BetterAuth.Plugins as BetterAuth.Plugins
import Yoga.BetterAuth.Types (Api, Auth, AuthClient, Email(..), Password(..), UserName(..), SessionId(..), Token(..), UserId(..))
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Om.API (registerAPILayer)
import Yoga.Fastify.Om.Route (GET, Route, Handler, handle, respond, BearerToken)
import Yoga.Fetch as Fetch
import Yoga.Fetch.Impl.Node (nodeFetch)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, withScoped, runLayer, (>->))
import Yoga.OpenTelemetry.OpenTelemetry as OTel
import Yoga.OpenTelemetry.OmLayer as OTelOmLayer
import Yoga.Test.Docker as Docker

type BetterAuthConfig = (secret :: String, baseURL :: String, emailAndPassword :: EmailAndPassword)

type ComposedLayer = OmLayer
  (scope :: Scope)
  ()
  { auth :: Auth }

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

withDocker :: Aff Unit -> Aff Unit
withDocker action = do
  Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
  finally (Docker.stopService composeFile) action

main :: Effect Unit
main = launchAff_ do
  let config = defaultConfig { timeout = Just (Milliseconds 60000.0) }
  runSpecPure' config [ consoleReporter ] do
    omClientSpec
    omPostgresSpec
    omLayerSpec
    omHandlerSpec

omClientSpec :: Spec Unit
omClientSpec = describe "Yoga.BetterAuth.Om" do

  it "sign up returns the user" do
    client <- mkClient # liftEffect
    { user } <- runAuth client do
      AuthClient.signUpEmail { email: Email "alice@test.com", password: Password "password123", name: UserName "Alice" }
    user.email `shouldEqual` Email "alice@test.com"
    user.name `shouldEqual` UserName "Alice"
    user.emailVerified `shouldEqual` false
    un UserId user.id `shouldSatisfy` (not <<< String.null)

  it "sign in returns token and user" do
    client <- mkClient # liftEffect
    runAuth client do
      void $ AuthClient.signUpEmail { email: Email "bob@test.com", password: Password "password123", name: UserName "Bob" }
    { token, user, redirect } <- runAuth client do
      AuthClient.signInEmail { email: Email "bob@test.com", password: Password "password123" }
    un Token token `shouldSatisfy` (not <<< String.null)
    user.email `shouldEqual` Email "bob@test.com"
    redirect `shouldEqual` false

  it "get session after sign up" do
    client <- mkClient # liftEffect
    { session, user } <- runAuth client do
      void $ AuthClient.signUpEmail { email: Email "carol@test.com", password: Password "password123", name: UserName "Carol" }
      AuthClient.getSession
    un SessionId session.id `shouldSatisfy` (not <<< String.null)
    un Token session.token `shouldSatisfy` (not <<< String.null)
    session.userId `shouldEqual` user.id
    user.email `shouldEqual` Email "carol@test.com"

  it "sign out after sign up" do
    client <- mkClient # liftEffect
    { success } <- runAuth client do
      void $ AuthClient.signUpEmail { email: Email "dave@test.com", password: Password "password123", name: UserName "Dave" }
      AuthClient.signOut
    success `shouldEqual` true

  it "sign in with wrong password throws authError" do
    client <- mkClient # liftEffect
    result <- Om.runReader { authClient: client } do
      AuthClient.signInEmail { email: Email "nobody@test.com", password: Password "wrong" }
    case result of
      Right _ -> fail "Expected authError"
      Left _ -> pure unit

omPostgresSpec :: Spec Unit
omPostgresSpec = describe "Yoga.BetterAuth.Om (Postgres)" do

  it "sign up and sign in with real Postgres" do
    withDocker do
      withScoped (OmLayer.authFullLive { connectionString, betterAuthConfig }) \{ auth } -> do
        client <- Client.createTestClient baseURL auth # liftEffect
        signUp <- runAuth client do
          AuthClient.signUpEmail { email: Email "pg@test.com", password: Password "password123", name: UserName "PgUser" }
        signUp.user.email `shouldEqual` Email "pg@test.com"
        signUp.user.name `shouldEqual` UserName "PgUser"
        signIn <- runAuth client do
          AuthClient.signInEmail { email: Email "pg@test.com", password: Password "password123" }
        un Token signIn.token `shouldSatisfy` (not <<< String.null)

omLayerSpec :: Spec Unit
omLayerSpec = describe "Yoga.BetterAuth.OmLayer" do

  it "testStackLive provides auth + client from config" testStackLiveTest

  it "betterAuthLive' composes with databaseLive via >->" do
    let layer = OmLayer.betterAuthLive' betterAuthConfig >-> OmLayer.databaseLive' connectionString :: ComposedLayer
    withDocker do
      withScoped layer \{ auth } -> do
        Server.runMigrations auth
        client <- Client.createTestClient baseURL auth # liftEffect
        { user } <- runAuth client do
          AuthClient.signUpEmail { email: Email "compose@test.com", password: Password "password123", name: UserName "ComposeUser" }
        user.email `shouldEqual` Email "compose@test.com"

testStackLiveTest :: Aff Unit
testStackLiveTest = withDocker do
  withScoped (OmLayer.testStackLive { connectionString, baseURL, betterAuthConfig }) \provided -> do
    { user } <- runAuth' provided do
      AuthClient.signUpEmail { email, password, name }
    user.email `shouldEqual` email
    user.name `shouldEqual` name

    { token } <- runAuth' provided do
      AuthClient.signInEmail { email, password }
    un Token token `shouldSatisfy` (not <<< String.null)

    { session, user: sessionUser } <- runAuth' provided AuthClient.getSession
    sessionUser.id `shouldEqual` user.id
    un SessionId session.id `shouldSatisfy` (not <<< String.null)

    { success } <- runAuth' provided AuthClient.signOut
    success `shouldEqual` true
  where
  email = Email "layer@test.com"
  password = Password "password123"
  name = UserName "LayerUser"

runAuth'
  :: forall a r
   . { authClient :: AuthClient | r }
  -> Om.Om { authClient :: AuthClient | r } (authError :: Client.ClientError) a
  -> Aff a
runAuth' provided = Om.runOm provided
  { exception: \e -> throwError (error ("Unexpected: " <> show e))
  , authError: \e -> throwError (error ("Auth error: " <> e.message))
  }

-- handleAuth integration test

type ProtectedRoute = Route GET "protected"
  { headers :: { authorization :: BearerToken } }
  ( ok :: { body :: { greeting :: String } }
  , unauthorized :: { body :: { error :: String } }
  )

type PublicRoute = Route GET "public" {}
  (ok :: { body :: { status :: String } })

type TestAPI =
  { protected :: ProtectedRoute
  , public :: PublicRoute
  }

protectedHandler :: Handler ProtectedRoute (authApi :: Api)
protectedHandler = handleAuth \session -> do
  respond @"ok" { greeting: "hello " <> un UserName session.user.name }

publicHandler :: Handler PublicRoute ()
publicHandler = handle do
  respond @"ok" { status: "ok" }

testPort :: Int
testPort = 3457

testAPILayer :: OmLayer (fastify :: F.Fastify, authApi :: Api) () {}
testAPILayer = registerAPILayer @TestAPI
  { protected: protectedHandler
  , public: publicHandler
  }

otelConfig :: OTelOmLayer.OTelConfig
otelConfig =
  { serviceName: OTel.ServiceName "yoga-better-auth-test"
  , serviceVersion: OTel.ServiceVersion "0.2.2"
  , serviceNamespace: OTel.ServiceNamespace "yoga-better-auth"
  , logsEndpoint: "http://localhost:4318/v1/logs"
  , tracesEndpoint: "http://localhost:4318/v1/traces"
  , tracerName: OTel.TracerName "better-auth-test"
  }

omHandlerSpec :: Spec Unit
omHandlerSpec = describe "Yoga.BetterAuth.OmHandler" do

  it "handleAuth accepts valid tokens and rejects invalid ones" do
    withScoped (OTelOmLayer.otelLive' otelConfig) \{ tracer } -> do
      Docker.startService composeFile (Docker.Timeout (Milliseconds 30000.0))
      authApi /\ fastify <- startServer
      finally (F.close fastify *> Docker.stopService composeFile) do
        rootSpan <- OTel.startSpan (OTel.SpanName "handleAuth-integration-test") tracer # liftEffect

        { token } <- withChild "sign-up-user" rootSpan tracer do
          Server.signUpEmail { email: Email "handler@test.com", password: Password "password123", name: UserName "Handler" } authApi

        withChild "fetch-protected-valid-token" rootSpan tracer do
          resp <- fetchProtected (un Token token)
          Fetch.statusCode resp `shouldEqual` 200
          body <- Fetch.text resp # liftAff
          body `shouldSatisfy` \b -> String.contains (String.Pattern "Handler") b

        withChild "fetch-protected-bad-token" rootSpan tracer do
          badResp <- fetchProtected "invalid-token"
          Fetch.statusCode badResp `shouldEqual` 401

        OTel.endSpan rootSpan # liftEffect
  where
  startServer :: Aff (Api /\ F.Fastify)
  startServer = do
    database <- Server.pgPool connectionString # liftEffect
    auth <- Server.betterAuth
      { secret
      , baseURL
      , database
      , emailAndPassword: Server.emailAndPassword { enabled: true }
      , plugins: [ BetterAuth.Plugins.bearer ]
      }
      # liftEffect
    Server.runMigrations auth # liftAff
    authApi <- Server.api auth # liftEffect
    fastify <- F.fastify {} # liftEffect
    BetterAuth.Fastify.registerAuth {} auth fastify # liftEffect
    let ctx = { fastify, authApi }
    _ <- Om.runOm ctx { exception: throwError } $ runLayer ctx testAPILayer
    _ <- F.listen { port: F.Port testPort, host: F.Host "127.0.0.1" } fastify
    pure (authApi /\ fastify)

  withChild :: forall a. String -> OTel.Span -> OTel.Tracer -> Aff a -> Aff a
  withChild name = OTel.withChildSpanAff (OTel.SpanName name)

  fetchProtected token = Fetch.fetch nodeFetch
    (Fetch.URL $ "http://127.0.0.1:" <> show testPort <> "/protected")
    { method: Fetch.getMethod, headers: Fetch.makeHeaders { authorization: "Bearer " <> token } }
