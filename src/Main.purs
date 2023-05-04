module Main where

import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import HTTPurple (class Generic, ExtRequest, RequestR, Response, RouteDuplex', ServerM, internalServerError, mkRoute, noArgs, ok, serve, (/))
import Prelude (bind, pure, show, ($), (<>), (>>>))
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Record.Studio.Keys (class KeysRL)
import Yoga.Om (Om)
import Yoga.Om as Om

data Route = Hello

derive instance Generic Route _
type User = String
type UserApi = { getUser :: Aff (Maybe User) }

withToken
  :: forall route requestWithoutUser requestWithUser furtherCtx exception
   . Nub (RequestR route requestWithUser) (RequestR route requestWithUser)
  => Union requestWithoutUser (token :: Maybe String) requestWithUser
  => (ExtRequest route requestWithUser -> Om furtherCtx exception Response)
  -> ExtRequest route requestWithoutUser
  -> Om furtherCtx exception Response
withToken router request = do
  router $ merge request { token: Just "token-123" }

withUser
  :: forall route requestWithoutToken requestWithToken requestWithUser furtherCtx furtherCtxRL exception
   . Union requestWithoutToken (token :: Maybe String) requestWithToken -- our middleware extends a request without user to a request with user
  => Nub (RequestR route requestWithUser) (RequestR route requestWithUser)
  => Union requestWithToken (user :: Maybe String) requestWithUser -- our middleware extends a request without user to a request with user
  => RowToList furtherCtx furtherCtxRL -- technical requirement
  => KeysRL furtherCtxRL -- technical requirement
  => Union furtherCtx (userApi :: UserApi) (userApi :: UserApi | furtherCtx) -- this says that the ctx of our authenticator needs a userApi
  => (ExtRequest route requestWithUser -> Om { | furtherCtx } exception Response)
  -> ExtRequest route requestWithToken -- our authenticator expects a request without user, because we are going to add the user
  -> Om { userApi :: UserApi | furtherCtx } exception Response -- our authenticator runs in an Om that expects the userApi in its ctx
withUser router request = do
  { userApi } <- Reader.ask
  maybeUser <- liftAff userApi.getUser
  Om.expandCtx $ router $ merge request { user: maybeUser }

route :: RouteDuplex' Route
route = mkRoute
  { "Hello": "hello" / noArgs
  }

main :: ServerM
main = do
  let
    userApi :: UserApi
    userApi = { getUser: pure $ Just "John Doe" }
  serve { port: 8080 }
    { route
    , router:
        -- we wrap our basic request handler in the authenticator, thus giving it access to the Maybe User
        withToken (withUser router)
          -- then we run the Om, giving it the previously created userAPi
          >>> Om.runOm { userApi }
            {
              -- and handle the generic exception using an internalServerError
              exception: \e -> internalServerError $ show e
            }
    }
  where
  -- A basic router running in Om, so you can additionally give it dependencies in the Om ctx, like further APIs
  router :: ExtRequest Route (token :: Maybe String, user :: Maybe User) -> Om {} () Response
  router { route: Hello, user, token } = ok $ "hello " <> fromMaybe "Unknown" user <> ", I'm using token " <> fromMaybe "Unknown" token
