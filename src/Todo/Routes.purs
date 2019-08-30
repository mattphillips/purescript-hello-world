module Todo.Routes (createTodoRoutes, TodoError(..)) where

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
import Node.Express.App (App, delete, get, post, put)
import Node.Express.Handler (Handler, HandlerM, nextThrow)
import Node.Express.Request (getBody, getQueryParam, getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Response.SendStatus (sendStatus)
import Todo (IsComplete(..), TodoId, updateTodo)
import Todo.Repository (TodoRepo)

data TodoError = InvalidTodoId | TodoNotFound TodoId | InvalidArgument (NonEmptyList ForeignError)

createTodoRoutes :: TodoRepo Effect -> App
createTodoRoutes repo = do
  get "/" getAll

  post "/" create
  
  put "/:id" update
  
  delete "/:id" deleteHandler

  where
  getAll = do
    isCompleteParam <- getIsCompleteParam
    todos <- liftEffect case isCompleteParam of
      Nothing -> repo.getAll
      Just isComplete -> (repo.filter {isComplete: (IsComplete isComplete)})
    sendJson $ todos
  
  create = getPayload \des -> (liftEffect $ repo.create des) >>= sendJson

  deleteHandler :: Handler
  deleteHandler = getIdParam \id -> (liftEffect $ repo.delete id) >>= const (sendStatus 204)

  update = getIdParam \id ->
    getPayload \d ->
      getPayload \isC ->
        (liftEffect $ repo.update id (updateTodo \t -> t { isComplete = isC, description = d })) >>=
          maybe (raiseError (TodoNotFound id)) sendJson

getPayload :: ∀ a. Decode a => (a -> Handler) -> Handler
getPayload f = getBody >>= runExcept >>> either (InvalidArgument >>> raiseError) f

raiseError :: TodoError -> Handler
raiseError = case _ of
  InvalidTodoId -> nextError 400 "Bad Request"
  TodoNotFound id -> nextError 404 ("Todo with id: " <> (toString id) <> " not found.")
  InvalidArgument msg -> nextError 400 ("Invalid Request. " <> show (head msg)) -- TODO: serialise foreign error properly
  where
    nextError status msg = setStatus status >>= const (nextThrow $ error msg)

getIsCompleteParam :: HandlerM (Maybe Boolean)
getIsCompleteParam = do
  isCompleteParam <- getQueryParam "isComplete"
  pure case isCompleteParam of
    (Just "true") -> Just (true)
    (Just "false") ->  (Just (false))
    _ ->  Nothing

getIdParam :: (TodoId -> Handler) -> Handler
getIdParam f = getRouteParam "id" >>= (_ >>= parseId) >>> maybe (raiseError InvalidTodoId) f
