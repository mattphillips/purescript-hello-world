module Person.Repository (PersonRepo, Person, PersonId, Name(..)) where

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Id (Id)
import Prelude (class Eq, class Show, Unit)

newtype Name = Name String 
derive newtype instance showName :: Show Name
derive newtype instance eqName :: Eq Name
instance hasableName :: Hashable Name where
  hash (Name n) = hash n

type Person a = { name :: Name, id :: a }

type PersonId = Id (Person Unit)
type PersonRepo m =
  { create :: ∀ a. Person a -> m (Person PersonId)
  , get :: PersonId -> m (Maybe (Person PersonId))
  , getByName :: Name -> m (Array (Person PersonId))
  , update :: PersonId -> (∀ a. Person a -> Person a) -> m (Maybe (Person PersonId))
  }
