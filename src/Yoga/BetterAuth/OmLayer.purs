module Yoga.BetterAuth.OmLayer
  ( -- Individual layers (open rows, composable)
    databaseLive
  , databaseLive'
  , betterAuthLive
  , betterAuthLive'
  , migrationsLive
  , testClientLive
  , testClientLive'
  , browserClientLive
  -- Convenience compositions (via wireLayers)
  , authFullLive
  , testStackLive
  -- Type aliases
  , DatabaseL
  , BetterAuthL
  , AuthClientL
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Prim.Row (class Lacks, class Union)
import Yoga.BetterAuth.BetterAuth as Server
import Yoga.BetterAuth.BetterAuth (BetterAuthOptionsImpl)
import Yoga.BetterAuth.Client as Client
import Yoga.BetterAuth.Types (Auth, AuthClient, Database)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, makeScopedLayer, acquireRelease)

type DatabaseL r = (database :: Database | r)

type BetterAuthL r = (auth :: Auth | r)

type AuthClientL r = (authClient :: AuthClient | r)

mkAuth
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => Database
  -> { | opts }
  -> Effect Auth
mkAuth = runEffectFn2 betterAuthWithDatabaseImpl

foreign import betterAuthWithDatabaseImpl :: forall r. EffectFn2 Database { | r } Auth

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Individual Layers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

databaseLive :: forall r. OmLayer (scope :: Scope, connectionString :: String | r) () { database :: Database }
databaseLive = makeScopedLayer acquire release
  where
  acquire = do
    { connectionString } <- Om.ask
    database <- Server.pgPool connectionString # liftEffect
    pure { database }
  release { database } = Server.pgPoolEnd database

databaseLive' :: String -> OmLayer (scope :: Scope) () { database :: Database }
databaseLive' connectionString = makeLayer do
  database <- acquireDatabase connectionString
  pure { database }

betterAuthLive
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => OmLayer (betterAuthConfig :: { | opts }, database :: Database | r) () { auth :: Auth }
betterAuthLive = makeLayer do
  { betterAuthConfig, database } <- Om.ask
  auth <- mkAuth database betterAuthConfig # liftEffect
  pure { auth }

betterAuthLive'
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => { | opts }
  -> OmLayer (database :: Database | r) () { auth :: Auth }
betterAuthLive' betterAuthConfig = makeLayer do
  { database } <- Om.ask
  auth <- mkAuth database betterAuthConfig # liftEffect
  pure { auth }

migrationsLive :: forall r. OmLayer (auth :: Auth | r) () {}
migrationsLive = makeLayer do
  { auth } <- Om.ask
  Server.runMigrations auth # liftAff
  pure {}

testClientLive :: forall r. OmLayer (baseURL :: String, auth :: Auth | r) () { authClient :: AuthClient }
testClientLive = makeLayer do
  { baseURL, auth } <- Om.ask
  authClient <- Client.createTestClient baseURL auth # liftEffect
  pure { authClient }

testClientLive' :: forall r. String -> OmLayer (auth :: Auth | r) () { authClient :: AuthClient }
testClientLive' baseURL = makeLayer do
  { auth } <- Om.ask
  authClient <- Client.createTestClient baseURL auth # liftEffect
  pure { authClient }

browserClientLive :: forall r. OmLayer (baseURL :: String | r) () { authClient :: AuthClient }
browserClientLive = makeLayer do
  { baseURL } <- Om.ask
  authClient <- Client.createAuthClient { baseURL } # liftEffect
  pure { authClient }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Convenience Compositions (via wireLayers)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

authFullLive
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => { connectionString :: String, betterAuthConfig :: { | opts } }
  -> OmLayer (scope :: Scope) () { auth :: Auth, database :: Database }
authFullLive { connectionString, betterAuthConfig } = makeLayer do
  database <- acquireDatabase connectionString
  auth <- mkAuth database betterAuthConfig # liftEffect
  Server.runMigrations auth # liftAff
  pure { auth, database }

testStackLive
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => { connectionString :: String, baseURL :: String, betterAuthConfig :: { | opts } }
  -> OmLayer (scope :: Scope) () { auth :: Auth, database :: Database, authClient :: AuthClient }
testStackLive { connectionString, baseURL, betterAuthConfig } = makeLayer do
  database <- acquireDatabase connectionString
  auth <- mkAuth database betterAuthConfig # liftEffect
  Server.runMigrations auth # liftAff
  authClient <- Client.createTestClient baseURL auth # liftEffect
  pure { auth, database, authClient }

acquireDatabase :: forall r. String -> Om.Om { scope :: Scope | r } () Database
acquireDatabase connectionString =
  acquireRelease
    (Server.pgPool connectionString # liftEffect)
    Server.pgPoolEnd
