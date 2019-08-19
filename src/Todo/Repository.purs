module Todo.Repository (TodoRepo, Todo, TodoId, Description(..), Status(..)) where

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Id (Id)
import Prelude (class Eq, class Show, Unit)

newtype Description = Description String 
derive newtype instance showDescription :: Show Description
derive newtype instance eqDescription :: Eq Description
instance hashableDescription :: Hashable Description where
  hash (Description n) = hash n

data Status = Active | Completed
instance showStatus :: Show Status where
  show Active = "Active"
  show Completed = "Completed"

derive instance eqStatus :: Eq Status
instance hashableStatus :: Hashable Status where
  hash Active = hash 0
  hash Completed = hash 1

type Todo a =
  { id :: a
  , description :: Description
  , status :: Status
  }

type TodoId = Id (Todo Unit)

type TodoRepo m =
  { create :: Description -> m (Todo TodoId)
  , get :: TodoId -> m (Maybe (Todo TodoId))
  , getByStatus :: Status -> m (Array (Todo TodoId))
  , update :: TodoId -> (∀ a. Todo a -> Todo a) -> m (Maybe (Todo TodoId))
  , delete :: TodoId -> m Unit -- TODO: should this be TodoId -> Maybe (Todo TodoId)?
  }
