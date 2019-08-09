module Main (main) where

import Data.Array (filter)
import Data.Eq
import Data.HashMap as HM
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
derive newtype instance eqName :: Eq Name

newtype PersonId = PersonId String
derive newtype instance showPersonId :: Show PersonId
derive newtype instance eqPersonId :: Eq PersonId
derive newtype instance hashablePersonId :: Hashable PersonId

type Person a = { name :: Name, id :: a }

type PersonRepo m =
  { create :: forall a. Person a -> m (Person PersonId)
  , get :: PersonId -> m (Maybe (Person PersonId))
  , getByName :: Name -> m (Array (Person PersonId))
  , update :: PersonId -> (forall a. Person a -> Person a) -> m (Maybe (Person PersonId))
  }

type Store = Ref (HM.HashMap PersonId (Person PersonId))

createInMemoryPersonRepo :: Effect (PersonRepo Effect)
createInMemoryPersonRepo = inMemoryPersonRepo <$> new HM.empty
  where
  inMemoryPersonRepo :: Store -> PersonRepo Effect
  inMemoryPersonRepo store = { create, get, getByName, update }
    where
    create :: forall a. Person a -> Effect (Person PersonId)
    create {name} = do
      id <- random
      let p = { name, id: PersonId(toString id) }
      modify_ (\db -> HM.insert p.id p db) store
      pure p

    get :: PersonId -> Effect (Maybe (Person PersonId))
    get id = (\db -> HM.lookup id db) <$> read store

    getByName :: Name -> Effect (Array (Person PersonId))
    getByName n = do
      db <- read store
      let ps = HM.values db
      pure (filter (\{name} -> n == name) ps)

    update :: PersonId -> (forall a. Person a -> Person a) -> Effect (Maybe (Person PersonId))
    update id f = do
      person <- get id
      modify_ (\db -> HM.update (\_ -> f <$> person) id db) store
      pure person

main :: Effect Unit
main = do
  repo <- createInMemoryPersonRepo
  p <- repo.create { id: unit, name: Name("Matt") }
  logShow p
  mp <- repo.get p.id
  logShow mp
  _ <- repo.update p.id \pp -> { id: pp.id, name: Name("Matthew") }
  ps <- repo.getByName (Name("Matthew"))
  logShow ps
