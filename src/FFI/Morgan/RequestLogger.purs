module Morgan.RequestLogger where

import Prelude

import Data.Function.Uncurried (Fn3)
import Effect (Effect)
import Node.Express.Types (Response, Request)

foreign import requestLogger ::
  Fn3 Request Response (Effect Unit) (Effect Unit)
