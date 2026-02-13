module Yoga.BetterAuth.OmLayer
  ( -- Individual layers (open rows, composable)
    databaseLive
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
import Yoga.Om.Layer (OmLayer, makeLayer)

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

databaseLive :: forall r. OmLayer (connectionString :: String | r) (DatabaseL ()) ()
databaseLive = makeLayer do
  { connectionString } <- Om.ask
  database <- Server.pgPool connectionString # liftEffect
  pure { database }

betterAuthLive
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => OmLayer (betterAuthConfig :: { | opts }, database :: Database | r) (BetterAuthL ()) ()
betterAuthLive = makeLayer do
  { betterAuthConfig, database } <- Om.ask
  auth <- mkAuth database betterAuthConfig # liftEffect
  pure { auth }

betterAuthLive'
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => { | opts }
  -> OmLayer (database :: Database | r) (BetterAuthL ()) ()
betterAuthLive' betterAuthConfig = makeLayer do
  { database } <- Om.ask
  auth <- mkAuth database betterAuthConfig # liftEffect
  pure { auth }

migrationsLive :: forall r. OmLayer (auth :: Auth | r) () ()
migrationsLive = makeLayer do
  { auth } <- Om.ask
  Server.runMigrations auth # liftAff
  pure {}

testClientLive :: forall r. OmLayer (baseURL :: String, auth :: Auth | r) (AuthClientL ()) ()
testClientLive = makeLayer do
  { baseURL, auth } <- Om.ask
  authClient <- Client.createTestClient baseURL auth # liftEffect
  pure { authClient }

browserClientLive :: forall r. OmLayer (baseURL :: String | r) (AuthClientL ()) ()
browserClientLive = makeLayer do
  { baseURL } <- Om.ask
  authClient <- Client.createAuthClient { baseURL } # liftEffect
  pure { authClient }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Convenience Compositions
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

authWithDatabaseLive
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => OmLayer
       (connectionString :: String, betterAuthConfig :: { | opts } | r)
       (auth :: Auth, database :: Database)
       ()
authWithDatabaseLive = makeLayer do
  { connectionString, betterAuthConfig } <- Om.ask
  database <- Server.pgPool connectionString # liftEffect
  auth <- mkAuth database betterAuthConfig # liftEffect
  pure { auth, database }

authFullLive
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => OmLayer
       (connectionString :: String, betterAuthConfig :: { | opts } | r)
       (auth :: Auth, database :: Database)
       ()
authFullLive = makeLayer do
  { connectionString, betterAuthConfig } <- Om.ask
  database <- Server.pgPool connectionString # liftEffect
  auth <- mkAuth database betterAuthConfig # liftEffect
  Server.runMigrations auth # liftAff
  pure { auth, database }

testStackLive
  :: forall r opts opts_
   . Lacks "database" opts
  => Union opts opts_ BetterAuthOptionsImpl
  => OmLayer
       (connectionString :: String, baseURL :: String, betterAuthConfig :: { | opts } | r)
       (auth :: Auth, database :: Database, authClient :: AuthClient)
       ()
testStackLive = makeLayer do
  { connectionString, baseURL, betterAuthConfig } <- Om.ask
  database <- Server.pgPool connectionString # liftEffect
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
  => OmLayer (betterAuthConfig :: { | opts } | r) (BetterAuthL ()) ()
betterAuthLayer = makeLayer do
  { betterAuthConfig } <- Om.ask
  auth <- Server.betterAuth betterAuthConfig # liftEffect
  pure { auth }

betterAuthLayer'
  :: forall r opts opts_
   . Union opts opts_ BetterAuthOptionsImpl
  => { | opts }
  -> OmLayer r (BetterAuthL ()) ()
betterAuthLayer' config = makeLayer do
  auth <- Server.betterAuth config # liftEffect
  pure { auth }

authClientLayer
  :: forall r
   . OmLayer (authClientConfig :: { baseURL :: String }, auth :: Auth | r) (AuthClientL ()) ()
authClientLayer = makeLayer do
  { authClientConfig, auth } <- Om.ask
  client <- Client.createTestClient authClientConfig.baseURL auth # liftEffect
  pure { authClient: client }
