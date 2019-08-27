module Todo.Routes (createTodoRoutes, TodoRoutes) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Request (getQueryParam)
import Node.Express.Response (sendJson)
import Todo.Repository (TodoRepo)

type TodoRoutes =
  { getAll :: Handler
  }

createTodoRoutes :: TodoRepo Effect -> TodoRoutes
createTodoRoutes repo = { getAll }
  where
  getAll :: Handler
  getAll = do
    isCompleteParam <- getIsCompleteParam
    todos <- liftEffect case isCompleteParam of
      Nothing -> repo.getAll
      Just isComplete -> (repo.filter {isComplete})
    sendJson $ todos

getIsCompleteParam :: HandlerM (Maybe Boolean)
getIsCompleteParam = do
  isCompleteParam <- getQueryParam "isComplete"
  pure case isCompleteParam of
    (Just "true") -> Just (true)
    (Just "false") ->  (Just (false))
    _ ->  Nothing
