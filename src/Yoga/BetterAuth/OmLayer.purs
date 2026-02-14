module Yoga.BetterAuth.OmLayer
  ( -- Individual layers (open rows, composable)
    databaseLive
  , databaseLive'
  , betterAuthLive
  , betterAuthLive'
  , migrationsLive
  , testClientLive
  , browserClientLive
  -- Convenience compositions
  , authWithDatabaseLive
  , authFullLive
  , testStackLive
  -- Type aliases
  , DatabaseL
  , BetterAuthL
  , AuthClientL
  -- Backward compatible
  , betterAuthLayer
  , betterAuthLayer'
  , authClientLayer
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

foreign import betterAuthWithDatabaseImpl :: forall r. EffectFn2 Database { | r } Auth

mkAuth
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => Database
  -> { | opts }
  -> Effect Auth
mkAuth database betterAuthConfig =
  runEffectFn2 betterAuthWithDatabaseImpl database betterAuthConfig

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
databaseLive' connectionString = makeScopedLayer acquire release
  where
  acquire = do
    database <- Server.pgPool connectionString # liftEffect
    pure { database }
  release { database } = Server.pgPoolEnd database

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

browserClientLive :: forall r. OmLayer (baseURL :: String | r) () { authClient :: AuthClient }
browserClientLive = makeLayer do
  { baseURL } <- Om.ask
  authClient <- Client.createAuthClient { baseURL } # liftEffect
  pure { authClient }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Convenience Compositions
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

authWithDatabaseLive
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => String
  -> { | opts }
  -> OmLayer (scope :: Scope) () { auth :: Auth, database :: Database }
authWithDatabaseLive connectionString betterAuthConfig = makeLayer do
  database <- acquireRelease
    (Server.pgPool connectionString # liftEffect)
    Server.pgPoolEnd
  auth <- mkAuth database betterAuthConfig # liftEffect
  pure { auth, database }

authFullLive
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => String
  -> { | opts }
  -> OmLayer (scope :: Scope) () { auth :: Auth, database :: Database }
authFullLive connectionString betterAuthConfig = makeLayer do
  database <- acquireRelease
    (Server.pgPool connectionString # liftEffect)
    Server.pgPoolEnd
  auth <- mkAuth database betterAuthConfig # liftEffect
  Server.runMigrations auth # liftAff
  pure { auth, database }

testStackLive
  :: forall opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => String
  -> String
  -> { | opts }
  -> OmLayer (scope :: Scope) () { auth :: Auth, database :: Database, authClient :: AuthClient }
testStackLive connectionString baseURL betterAuthConfig = makeLayer do
  database <- acquireRelease
    (Server.pgPool connectionString # liftEffect)
    Server.pgPoolEnd
  auth <- mkAuth database betterAuthConfig # liftEffect
  Server.runMigrations auth # liftAff
  authClient <- Client.createTestClient baseURL auth # liftEffect
  pure { auth, database, authClient }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Backward Compatible
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

betterAuthLayer
  :: forall r opts opts_
   . Union opts opts_ BetterAuthOptionsImpl
  => OmLayer (betterAuthConfig :: { | opts } | r) () { auth :: Auth }
betterAuthLayer = makeLayer do
  { betterAuthConfig } <- Om.ask
  auth <- Server.betterAuth betterAuthConfig # liftEffect
  pure { auth }

betterAuthLayer'
  :: forall r opts opts_
   . Union opts opts_ BetterAuthOptionsImpl
  => { | opts }
  -> OmLayer r () { auth :: Auth }
betterAuthLayer' config = makeLayer do
  auth <- Server.betterAuth config # liftEffect
  pure { auth }

authClientLayer
  :: forall r
   . OmLayer (authClientConfig :: { baseURL :: String }, auth :: Auth | r) () { authClient :: AuthClient }
authClientLayer = makeLayer do
  { authClientConfig, auth } <- Om.ask
  client <- Client.createTestClient authClientConfig.baseURL auth # liftEffect
  pure { authClient: client }
