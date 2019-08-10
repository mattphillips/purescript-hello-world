module Person.Repository (PersonRepo, Person, PersonId, Name(..), createInMemoryPersonRepo) where

import Data.Eq

import Data.Array (filter)
import Data.Function (const)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Data.Number.Format (toString)
import Effect (Effect)

import Effect.Random (random)
import Effect.Ref as Ref
import Prelude (class Eq, class Show, bind, discard, pure, (<$>))

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

type Store = Ref.Ref (HM.HashMap PersonId (Person PersonId))

createInMemoryPersonRepo :: Effect (PersonRepo Effect)
createInMemoryPersonRepo = inMemoryPersonRepo <$> Ref.new HM.empty
  where
  inMemoryPersonRepo :: Store -> PersonRepo Effect
  inMemoryPersonRepo store = { create, get, getByName, update }
    where
    create :: forall a. Person a -> Effect (Person PersonId)
    create {name} = do
      id <- random
      let p = { name, id: PersonId(toString id) }
      Ref.modify_ (HM.insert p.id p) store
      pure p

    get :: PersonId -> Effect (Maybe (Person PersonId))
    get id = (HM.lookup id) <$> Ref.read store

    getByName :: Name -> Effect (Array (Person PersonId))
    getByName n = do
      values <- HM.values <$> Ref.read store
      pure (filter (\{name} -> n == name) values)

    update :: PersonId -> (forall a. Person a -> Person a) -> Effect (Maybe (Person PersonId))
    update id f = do
      person <- get id
      let updatedPerson = f <$> person
      Ref.modify_ (HM.update (const updatedPerson) id) store
      pure updatedPerson
