module Todo.Routes (createTodoRoutes, TodoRoutes) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (parseId)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Request (getQueryParam, getRouteParam)
import Node.Express.Response (sendJson)
import Node.Express.Response.SendStatus (sendStatus)
import Todo.Repository (TodoRepo, TodoId)

type TodoRoutes =
  { getAll :: Handler
  , delete :: Handler
  }

createTodoRoutes :: TodoRepo Effect -> TodoRoutes
createTodoRoutes repo = { getAll, delete }
  where
  getAll :: Handler
  getAll = do
    isCompleteParam <- getIsCompleteParam
    todos <- liftEffect case isCompleteParam of
      Nothing -> repo.getAll
      Just isComplete -> (repo.filter {isComplete})
    sendJson $ todos

  delete :: Handler
  delete = do
    idParam <- getIdParam
    case idParam of
      Nothing -> sendStatus 400
      Just id -> do
        _ <- liftEffect (repo.delete id)
        sendStatus 204

getIsCompleteParam :: HandlerM (Maybe Boolean)
getIsCompleteParam = do
  isCompleteParam <- getQueryParam "isComplete"
  pure case isCompleteParam of
    (Just "true") -> Just (true)
    (Just "false") ->  (Just (false))
    _ ->  Nothing

getIdParam :: HandlerM (Maybe TodoId)
getIdParam = (_ >>= parseId) <$> getRouteParam "id"
