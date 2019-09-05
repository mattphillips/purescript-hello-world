module Test.Express where

import Effect (Effect)
import Node.Express.App (App)
import Node.Express.Types (Application, Request, Response)

foreign import mkTestApplication :: Effect Application
foreign import mountApp :: App -> Application -> Application

foreign import sendRequest :: Application -> Request -> Effect Response

type RequestOptions = {
  method :: String,
  url :: String
}

foreign import mkRequest :: RequestOptions -> Request

foreign import getStatus :: Response -> Int
foreign import getBody :: ∀ a. Response -> a
