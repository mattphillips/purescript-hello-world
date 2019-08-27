module Todo.Repository (TodoRepo, Todo, TodoId, Description(..), Filter) where

import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe, maybe)
import Foreign (ForeignError(..), fail, readString)
import Foreign.Class (class Decode)
import Foreign.Internal (readObject)
import Foreign.Object (lookup)
import Id (Id)
import Prelude (class Eq, class Show, Unit, ($), (<$>), (>>=), (>>>))

newtype Description = Description String 
derive newtype instance showDescription :: Show Description
derive newtype instance eqDescription :: Eq Description

instance decodeDescription :: Decode Description where
  decode f = readObject f >>= lookup "description" >>> maybe failure success
    where
      failure = fail $ ForeignError "Property description is missing"
      success d = Description <$> readString d

instance hashableDescription :: Hashable Description where
  hash (Description n) = hash n

type Todo a =
  { id :: a
  , description :: Description
  , isComplete :: Boolean
  }

type TodoId = Id (Todo Unit)

type Filter = { isComplete :: Boolean }

type TodoRepo m =
  { create :: Description -> m (Todo TodoId)
  , get :: TodoId -> m (Maybe (Todo TodoId))
  , getAll :: m (Array (Todo TodoId))
  , filter :: Filter -> m (Array (Todo TodoId))
  , update :: TodoId -> (∀ a. Todo a -> Todo a) -> m (Maybe (Todo TodoId))
  , delete :: TodoId -> m Unit -- TODO: should this be TodoId -> Maybe (Todo TodoId)?
  }
