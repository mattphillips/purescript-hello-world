module Id (Id, genId, parseId, toString) where

import Prelude

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Data.UUID as Uuid
import Effect (Effect)

newtype Id a = Id Uuid.UUID

instance showId :: Show (Id a) where
  show (Id uuid) = show (Uuid.toString uuid)

derive instance eqId :: Eq a => Eq (Id a)

instance hashableId :: Hashable a => Hashable (Id a) where
  hash (Id uuid) = hash (Uuid.toString uuid)

genId :: ∀ a. Effect (Id a)
genId = Id <$> Uuid.genUUID

parseId :: ∀ a. String -> Maybe (Id a)
parseId = map Id <<< Uuid.parseUUID

toString :: ∀ a. Id a -> String
toString (Id id) = Uuid.toString id
