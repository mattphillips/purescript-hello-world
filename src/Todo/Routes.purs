module Todo.Routes (createTodoRoutes, TodoRoutes, TodoError(..)) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.List.NonEmpty (head)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign (ForeignError)
import Foreign.Class (class Decode)
import Id (parseId, toString)
import Node.Express.Handler (Handler, HandlerM, nextThrow)
import Node.Express.Request (getBody, getQueryParam, getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Response.SendStatus (sendStatus)
import Todo.Repository (IsComplete(..), TodoId, TodoRepo)

data TodoError = InvalidTodoId | TodoNotFound TodoId | InvalidArgument (NonEmptyList ForeignError)

type TodoRoutes =
  { getAll :: Handler
  , create :: Handler
  , delete :: Handler
  , update :: Handler
  }

createTodoRoutes :: TodoRepo Effect -> TodoRoutes
createTodoRoutes repo = { getAll, create, delete, update }
  where
  getAll :: Handler
  getAll = do
    isCompleteParam <- getIsCompleteParam
    todos <- liftEffect case isCompleteParam of
      Nothing -> repo.getAll
      Just isComplete -> (repo.filter {isComplete: (IsComplete isComplete)})
    sendJson $ todos
  
  create :: Handler
  create = getPayload \des -> (liftEffect $ repo.create des) >>= sendJson

  delete :: Handler
  delete = getIdParam \id -> (liftEffect $ repo.delete id) >>= const (sendStatus 204)

  update :: Handler
  update = getIdParam \id ->
    getPayload \description ->
      getPayload \isComplete ->
        (liftEffect $ repo.update id (\todo -> { id: todo.id, isComplete, description })) >>= maybe (raiseError (TodoNotFound id)) sendJson

getPayload :: ∀ a. Decode a => (a -> Handler) -> Handler
getPayload f = getBody >>= runExcept >>> either handleInvalidArgumentError f

raiseError :: TodoError -> Handler
raiseError = case _ of
  InvalidTodoId -> nextError 400 "Bad Request"
  TodoNotFound id -> nextError 404 ("Todo with id: " <> (toString id) <> " not found.")
  InvalidArgument msg -> nextError 400 ("Invalid Request. " <> show (head msg)) -- TODO: serialise foreign error properly
  where
    nextError status msg = setStatus status >>= const (nextThrow $ error msg)

handleInvalidArgumentError :: NonEmptyList ForeignError -> Handler
handleInvalidArgumentError = InvalidArgument >>> raiseError

getIsCompleteParam :: HandlerM (Maybe Boolean)
getIsCompleteParam = do
  isCompleteParam <- getQueryParam "isComplete"
  pure case isCompleteParam of
    (Just "true") -> Just (true)
    (Just "false") ->  (Just (false))
    _ ->  Nothing

getIdParam :: (TodoId -> Handler) -> Handler
getIdParam f = getRouteParam "id" >>= (_ >>= parseId) >>> maybe (raiseError InvalidTodoId) f
