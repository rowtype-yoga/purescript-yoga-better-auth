module Yoga.BetterAuth.OmHandler (handleAuth) where

import Prelude

import Control.Monad.Reader.Trans (ask)
import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect.Aff (try)
import Effect.Aff.Class (liftAff)
import Prim.Row as Row
import Prim.RowList as RL
import Web.Fetch.Headers as Headers
import Yoga.BetterAuth.BetterAuth as BetterAuth
import Yoga.BetterAuth.Types (Api, SessionWithUser)
import Yoga.Fastify.Om.Route.OmHandler (Handler, handle, reject, class SplitResponseRL)
import Yoga.HTTP.API.Route.BearerToken (BearerToken)
import Yoga.HTTP.API.Route.HeaderValue (printHeader)
import Yoga.HTTP.API.Route.Response (Response)
import Yoga.HTTP.API.Route.RouteHandler (class RouteHandler)
import Yoga.Om (Om)

handleAuth
  :: forall @route pathParams queryParams extraHeaders body respVariant
       successRow errorRow extraCtx rl
   . RouteHandler route pathParams queryParams (authorization :: BearerToken | extraHeaders) body respVariant
  => RL.RowToList respVariant rl
  => SplitResponseRL rl successRow (unauthorized :: Response () { error :: String } | errorRow)
  => Row.Lacks "_respondNow" errorRow
  => Row.Lacks "path" extraCtx
  => Row.Lacks "query" extraCtx
  => Row.Lacks "headers" extraCtx
  => Row.Lacks "body" extraCtx
  => ( SessionWithUser
       -> Om
            { authApi :: Api
            , body :: body
            , headers :: { authorization :: BearerToken | extraHeaders }
            , path :: Record pathParams
            , query :: Record queryParams
            | extraCtx
            }
            ( _respondNow :: Variant successRow
            , unauthorized :: Response () { error :: String }
            | errorRow
            )
            (Variant successRow)
     )
  -> Handler route (authApi :: Api | extraCtx)
handleAuth f = handle do
  { headers: { authorization }, authApi } <- ask
  let webHeaders = Headers.fromRecord { authorization: printHeader authorization }
  sessionResult <- try (BetterAuth.getSession { headers: webHeaders } authApi) # liftAff
  case sessionResult of
    Left _ -> reject @"unauthorized" { error: "Invalid or expired session" }
    Right session -> f session
