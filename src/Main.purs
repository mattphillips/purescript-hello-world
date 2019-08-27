module Main (main) where

import Effect (Effect)
import Effect.Class.Console (log)
import Node.Express.App (App, get, listenHttp)
import Node.HTTP (Server)
import Prelude (bind, show, ($), (<>))
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)
import Todo.Repository (Description(..))
import Todo.Routes (TodoRoutes, createTodoRoutes)

appSetup :: TodoRoutes  -> App
appSetup routes = do
  get "/" routes.getAll

main :: Effect Server
main = do
  repo <- createInMemoryTodoRepo
  t <- repo.create (Description("One"))
  _ <- repo.create (Description("two"))
  _ <- repo.update t.id \pp -> { id: pp.id, description: pp.description, isComplete: true }
  _ <- repo.create (Description("three"))
  let routes = createTodoRoutes repo
  listenHttp (appSetup routes) 8080 \_ ->
    log $ "Listening on " <> show 8080
