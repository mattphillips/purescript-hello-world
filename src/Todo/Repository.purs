module Todo.Repository (
  TodoRepo,
  Todo(..),
  TodoRecord,
  TodoId,
  Description(..),
  IsComplete(..),
  Filter,
  updateTodo,
  getId,
  getDescription,
  getIsComplete
) where

import Control.Monad.Except (mapExcept)
import Data.Either (Either(..), either)
import Data.Hashable (class Hashable, hash)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe, maybe)
import Foreign (Foreign, ForeignError(..), F, fail, readBoolean, readString)
import Foreign.Class (class Decode)
import Foreign.Internal (readObject)
import Foreign.Object (lookup)
import Id (Id)
import Prelude (class Eq, class Show, Unit, const, ($), (<>), (>>=), (>>>))

newtype Description = Description String 
derive newtype instance showDescription :: Show Description
derive newtype instance eqDescription :: Eq Description

instance decodeDescription :: Decode Description where
  decode = decodeValueAtKey "description" readString Description

instance hashableDescription :: Hashable Description where
  hash (Description n) = hash n

newtype IsComplete = IsComplete Boolean

derive newtype instance eqIsComplete :: Eq IsComplete
derive newtype instance showIsComplete :: Show IsComplete
instance hashableIsComplete :: Hashable IsComplete where
  hash (IsComplete b) = hash b

instance decodeIsComplete :: Decode IsComplete where
  decode = decodeValueAtKey "isComplete" readBoolean IsComplete

decodeValueAtKey :: ∀ a b. String -> (Foreign -> F a) -> (a -> b) -> Foreign -> F b
decodeValueAtKey key parse construct f = readObject f >>= lookup key >>> maybe handleMissingKey parseValue
  where
    handleMissingKey = fail $ ForeignError ("Property " <> key <> " is missing.")
    parseValue = parse >>> mapExcept (either handleParseError (construct >>> Right))
      where
      handleParseError = const $ Left $ singleton $ ForeignError $ "Could not parse " <> key <> "."

type TodoRecord a = { id :: a , description :: Description , isComplete :: IsComplete }
-- TODO: maybe don't expose the Todo constructor
newtype Todo a = Todo (TodoRecord a)

derive newtype instance showTodo :: Show a => Show (Todo a)
derive newtype instance eqTodo :: Eq a => Eq (Todo a)
instance hashableTodo :: Hashable a => Hashable (Todo a) where
  hash (Todo t) = hash t

getId :: ∀ a. Todo a -> a
getId (Todo t) = t.id

getDescription :: ∀ a. Todo a -> Description
getDescription (Todo t) = t.description

getIsComplete :: ∀ a. Todo a -> IsComplete
getIsComplete (Todo t) = t.isComplete

updateTodo :: forall a. (TodoRecord a -> TodoRecord a) -> Todo a -> Todo a
updateTodo f (Todo t) = Todo (f t)

type TodoId = Id (Todo Unit)

type Filter = { isComplete :: IsComplete }

type TodoRepo m =
  { create :: Description -> m (Todo TodoId)
  , get :: TodoId -> m (Maybe (Todo TodoId))
  , getAll :: m (Array (Todo TodoId))
  , filter :: Filter -> m (Array (Todo TodoId))
  , update :: TodoId -> (∀ a. Todo a -> Todo a) -> m (Maybe (Todo TodoId))
  , delete :: TodoId -> m Unit -- TODO: should this be TodoId -> Maybe (Todo TodoId)?
  }
