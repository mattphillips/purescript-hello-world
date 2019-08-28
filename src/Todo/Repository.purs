module Todo.Repository (TodoRepo, Todo, TodoId, Description(..), IsComplete(..), Filter) where

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

type Todo a =
  { id :: a
  , description :: Description
  , isComplete :: IsComplete
  }

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
