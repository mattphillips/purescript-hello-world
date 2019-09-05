module Test.Express where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref, new)
import Node.Express.App (App)
import Node.Express.Types (Application, Request, Response)

foreign import mkTestApplication :: Effect Application
foreign import mountApp :: App -> Application -> Application

foreign import sendRequest ::
  Application -> Request -> Response -> Effect Unit

type RequestOptions = {
  method :: String,
  url :: String
}

foreign import mkRequest :: RequestOptions -> Request

foreign import _mkResponse :: ∀ a. a -> Response

mkResponse :: Effect (Ref Response)
mkResponse = new (_mkResponse unit)

foreign import getStatus :: Response -> Int
foreign import getBody :: ∀ a. Response -> a
