module Main (main) where

import Config (createConfig)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (message)
import Morgan.RequestLogger (requestLogger)
import Node.Express.App (App, delete, get, listenHttp, post, put, useExternal, useOnError)
import Node.Express.Request.BodyParser (bodyParser)
import Node.Express.Response (send)
import Node.HTTP (Server)
import Prelude (bind, discard, show, ($), (<>))
import Todo (Description(..), IsComplete(..), getId, updateTodo)
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)
import Todo.Routes (TodoRoutes, createTodoRoutes)

appSetup :: TodoRoutes -> App
appSetup routes = do
  useExternal requestLogger
  useExternal bodyParser
  get "/" routes.getAll
  post "/" routes.create
  put "/:id" routes.update
  delete "/:id" routes.delete
  useOnError \e -> send { error: message e }

main :: Effect Server
main = do
  config <- createConfig
  repo <- createInMemoryTodoRepo
  t <- repo.create (Description("One"))
  _ <- repo.create (Description("two"))
  _ <- repo.update (getId t) (updateTodo \tt -> tt { isComplete = (IsComplete true) })
  _ <- repo.create (Description("three"))
  let routes = createTodoRoutes repo
  listenHttp (appSetup routes) config.port \_ ->
    log $ "Listening on " <> show config.port
