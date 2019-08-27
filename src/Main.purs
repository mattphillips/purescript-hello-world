module Main (main) where

import Effect (Effect)
import Effect.Class.Console (log)
import Node.Express.App (App, delete, get, listenHttp, post, useExternal)
import Node.Express.Request.BodyParser (bodyParser)
import Node.HTTP (Server)
import Prelude (bind, discard, show, ($), (<>))
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)
import Todo.Repository (Description(..), IsComplete(..))
import Todo.Routes (TodoRoutes, createTodoRoutes)

appSetup :: TodoRoutes  -> App
appSetup routes = do
  useExternal bodyParser
  get "/" routes.getAll
  post "/" routes.create
  delete "/:id" routes.delete
  -- useOnError        (errorHandler      state)

main :: Effect Server
main = do
  repo <- createInMemoryTodoRepo
  t <- repo.create (Description("One"))
  _ <- repo.create (Description("two"))
  _ <- repo.update t.id \pp -> { id: pp.id, description: pp.description, isComplete: (IsComplete true) }
  _ <- repo.create (Description("three"))
  let routes = createTodoRoutes repo
  -- port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp (appSetup routes) 8080 \_ ->
    log $ "Listening on " <> show 8080
