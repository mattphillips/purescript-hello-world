module Node.Express.Request.BodyParser where

import Prelude

import Data.Function.Uncurried (Fn3)
import Effect (Effect)
import Node.Express.Types (Response, Request)

foreign import bodyParser ::
  Fn3 Request Response (Effect Unit) (Effect Unit)
