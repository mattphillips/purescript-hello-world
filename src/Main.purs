module Main (main) where

import Data.HashMap (HashMap, empty, insert, lookup)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Number.Format (toString)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Effect.Ref (Ref, modify_, new, read)
import Prelude (class Eq, class Show, Unit, bind, discard, pure, unit, (<$>))

newtype Name = Name String 
derive newtype instance showName :: Show Name

newtype PersonId = PersonId String
derive newtype instance showPersonId :: Show PersonId
derive newtype instance eqPersonId :: Eq PersonId
derive newtype instance hashablePersonId :: Hashable PersonId

type Person a = { name :: Name, id :: a }

type PersonRepo m =
  { create :: forall a. Person a -> m (Person PersonId)
  , get    :: PersonId -> m (Maybe(Person PersonId))
  }

type Store = Ref (HashMap PersonId (Person PersonId))

createInMemoryPersonRepo :: Effect (PersonRepo Effect)
createInMemoryPersonRepo = inMemoryPersonRepo <$> new empty
  where
  inMemoryPersonRepo :: Store -> PersonRepo Effect
  inMemoryPersonRepo store = { create, get }
    where
    create :: forall a. Person a -> Effect (Person PersonId)
    create {name} = do
      id <- random
      let p = { name, id: PersonId(toString id) }
      modify_ (\db -> insert p.id p db) store
      pure p

    get :: PersonId -> Effect (Maybe(Person PersonId))
    get id = (\db -> lookup id db) <$> read store

main :: Effect Unit
main = do
  repo <- createInMemoryPersonRepo
  p <- repo.create { id: unit, name: Name("Matt") }
  mp <- repo.get p.id
  logShow mp
