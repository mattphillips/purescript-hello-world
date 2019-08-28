module Config where

import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Node.Process (lookupEnv)
import Prelude (bind, pure, (<$>), (<<<), (>>=))

type Config = {
  port :: Int
}

createConfig :: Effect Config
createConfig = do
  port <- (fromMaybe 8080 <<< (_ >>= fromString)) <$> lookupEnv "PORT"
  pure { port }
