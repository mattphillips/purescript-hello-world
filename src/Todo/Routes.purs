module Todo.Routes (createTodoRoutes, TodoRoutes) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Id (parseId)
import Node.Express.Handler (Handler, HandlerM)
import Node.Express.Request (getBody, getQueryParam, getRouteParam)
import Node.Express.Response (send, sendJson, setStatus)
import Node.Express.Response.SendStatus (sendStatus)
import Todo.Repository (IsComplete(..), TodoId, TodoRepo)

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
  create = getBody >>= runExcept >>> either handleError handleSuccess
    where
      handleError errs = setStatus 400 >>= const (send errs) -- TODO add a json serialiser here
      handleSuccess d = (liftEffect $ repo.create d) >>= sendJson

  delete :: Handler
  delete = do
    idParam <- getIdParam
    case idParam of
      Nothing -> sendStatus 400
      Just id -> do
        _ <- liftEffect (repo.delete id)
        sendStatus 204

  update :: Handler
  update = do
    idParam <- getIdParam
    case idParam of
      Nothing -> sendStatus 404
      Just id -> do
        description <- runExcept <$> getBody
        either handleError handleSuccess description
        where
          handleError errs = setStatus 400 >>= const (send errs) -- TODO add a json serialiser here
          handleSuccess d = do
            isComplete <- runExcept <$> getBody
            either handleError' handleSuccess' isComplete
            where
              handleError' errs = setStatus 400 >>= const (send errs) -- TODO add a json serialiser here
              handleSuccess' ic = do
                todo <- liftEffect $ repo.update id (\todo -> { id: todo.id, isComplete: ic, description: d })
                case todo of
                  Nothing -> sendStatus 404
                  Just t -> sendJson t

getIsCompleteParam :: HandlerM (Maybe Boolean)
getIsCompleteParam = do
  isCompleteParam <- getQueryParam "isComplete"
  pure case isCompleteParam of
    (Just "true") -> Just (true)
    (Just "false") ->  (Just (false))
    _ ->  Nothing

getIdParam :: HandlerM (Maybe TodoId)
getIdParam = (_ >>= parseId) <$> getRouteParam "id"
