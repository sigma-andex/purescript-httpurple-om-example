module Main where

import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import HTTPurple (class Generic, ExtRequest, RequestR, Response, RouteDuplex', ServerM, internalServerError, mkRoute, noArgs, ok, serve, (/))
import Prelude (bind, pure, show, ($), (<>), (>>>))
import Prim.Row (class Nub, class Union)
import Record (merge)
import Yoga.Om (Om)
import Yoga.Om as Om

data Route = Hello

derive instance Generic Route _
type User = String
type UserApi = { getUser :: Aff (Maybe User) }

authenticator
  :: forall route extIn extOut exception
   . Nub (RequestR route extOut) (RequestR route extOut)
  => Union extIn (user :: Maybe String) extOut
  => (ExtRequest route extOut -> Om {} exception Response)
  -> ExtRequest route extIn
  -> Om { userApi :: UserApi } exception Response
authenticator router request = do
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
  serve { port: 8080 } { route, router: authenticator router >>> Om.runOm { userApi } { exception: \e -> internalServerError $ show e } }
  where
  router :: ExtRequest Route (user :: Maybe User) -> Om {} () Response
  router { route: Hello, user } = ok $ "hello " <> fromMaybe "Unknown" user
