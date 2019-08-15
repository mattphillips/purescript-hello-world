module Person.InMemoryInterpreters (createInMemoryPersonRepo) where

import Data.Eq

import Data.Array (filter)
import Data.Function (const)
import Data.HashMap as HM

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref as Ref
import Id (genId)
import Person.Repository (Name, Person, PersonId, PersonRepo)
import Prelude (bind, discard, pure, (<$>))

type Store = Ref.Ref (HM.HashMap PersonId (Person PersonId))

createInMemoryPersonRepo :: Effect (PersonRepo Effect)
createInMemoryPersonRepo = inMemoryPersonRepo <$> Ref.new HM.empty
  where
  inMemoryPersonRepo :: Store -> PersonRepo Effect
  inMemoryPersonRepo store = { create, get, getByName, update }
    where
    create :: ∀ a. Person a -> Effect (Person PersonId)
    create {name} = do
      id <- genId
      let p = { name, id }
      Ref.modify_ (HM.insert p.id p) store
      pure p

    get :: PersonId -> Effect (Maybe (Person PersonId))
    get id = (HM.lookup id) <$> Ref.read store

    getByName :: Name -> Effect (Array (Person PersonId))
    getByName n = do
      values <- HM.values <$> Ref.read store
      pure (filter (\{name} -> n == name) values)

    update :: PersonId -> (∀ a. Person a -> Person a) -> Effect (Maybe (Person PersonId))
    update id f = do
      person <- get id
      let updatedPerson = f <$> person
      Ref.modify_ (HM.update (const updatedPerson) id) store
      pure updatedPerson
