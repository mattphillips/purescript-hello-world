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
  create = getBody >>= runExcept >>> either handleInvalidArgumentError \d -> (liftEffect $ repo.create d) >>= sendJson

  delete :: Handler
  delete = getIdParam' \id -> (liftEffect $ repo.delete id) >>= const (sendStatus 204)

  update :: Handler
  update = do
    idParam <- getIdParam
    case idParam of
      Nothing -> raiseError InvalidTodoId
      Just id -> do
        description <- runExcept <$> getBody
        either handleInvalidArgumentError handleSuccess description
        where
          handleSuccess d = do
            isComplete <- runExcept <$> getBody
            either handleInvalidArgumentError handleSuccess' isComplete
            where
              handleSuccess' ic = do
                todo <- liftEffect $ repo.update id (\todo -> { id: todo.id, isComplete: ic, description: d })
                case todo of
                  Nothing -> raiseError (TodoNotFound id)
                  Just t -> sendJson t

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

getIdParam :: HandlerM (Maybe TodoId)
getIdParam = (_ >>= parseId) <$> getRouteParam "id"

getIdParam' :: (TodoId -> Handler) -> Handler
getIdParam' f = getRouteParam "id" >>= (_ >>= parseId) >>> maybe (raiseError InvalidTodoId) f
