module Node.Express.Response.SendStatus (sendStatus) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Types (Response)

foreign import _sendStatus :: forall a. Fn2 Response a (Effect Unit)

sendStatus :: Int -> Handler
sendStatus data_ = HandlerM \_ resp _ ->
    liftEffect $ runFn2 _sendStatus resp data_
