module Test.Todo.Routes where

import Prelude

import Config (createConfig)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (length)
import Effect (Effect)
import Effect.Aff (Aff, message)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (readArray)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.Express.App (App, listenHttp, useExternal, useOnError)
import Node.Express.Request.BodyParser (bodyParser)
import Node.Express.Response (send)
import Node.HTTP (Server, close)
import Test.Spec (Spec, after, before, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Todo (Description(..))
import Todo.InMemoryInterpreters (createInMemoryTodoRepo)
import Todo.Routes (createTodoRoutes)

fetch :: M.Fetch
fetch = M.fetch nodeFetch

appSetup :: App -> App
appSetup routes = do
  useExternal bodyParser
  routes
  useOnError \e -> send { error: message e }

run :: Effect Server
run = do
  config <- createConfig
  repo <- createInMemoryTodoRepo
  _ <- repo.create (Description "Matt")
  let routes = createTodoRoutes repo
  listenHttp (appSetup routes) config.port \_ ->
    log $ "Listening on " <> show config.port

runServer :: Aff Server
runServer = liftEffect $ run

closeServer :: Server -> Aff Unit
closeServer s = liftEffect $ close s (pure unit)

todoRoutesTest :: Spec Unit
todoRoutesTest = 
  describe "TodoRoutes" do
    describe "GET /" do

      before runServer $ after closeServer do
        it "returns new todo with generated id" \server -> do
          _response <- Aff.attempt $ fetch (M.URL "http://localhost:8080") M.defaultFetchOptions
          case _response of
            Left e -> do
              fail $ "failed with " <> show e
            Right response -> do
              stuff <- M.json response
              let code = M.statusCode response
              code `shouldEqual` 200
              case runExcept(readArray stuff) of
                Right d -> do
                  length d `shouldEqual` 1
                Left errs -> do
                  fail $ "failed with " <> show errs
