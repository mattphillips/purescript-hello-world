module Todo.Repository (TodoRepo, Todo, TodoId, Description(..)) where

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Id (Id)
import Prelude (class Eq, class Show, Unit)

newtype Description = Description String 
derive newtype instance showDescription :: Show Description
derive newtype instance eqDescription :: Eq Description
instance hasableDescription :: Hashable Description where
  hash (Description n) = hash n

type Todo a = { description :: Description, id :: a }

type TodoId = Id (Todo Unit)
type TodoRepo m =
  { create :: Description -> m (Todo TodoId)
  , get :: TodoId -> m (Maybe (Todo TodoId))
  , update :: TodoId -> (∀ a. Todo a -> Todo a) -> m (Maybe (Todo TodoId))
  }
